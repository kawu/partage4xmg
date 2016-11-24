{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


-- | Parsing sentences from input and printing trees.


module NLP.Partage4Xmg.Parse
( ParseCfg (..)
, parseAll
) where


import           Control.Monad              (forM_, unless, when)
import           Control.Monad.Trans.Maybe

import           Data.List                  (intercalate)
import           Data.Maybe                 (maybeToList)
import qualified Data.Map.Strict            as M
import qualified Data.MemoCombinators       as Memo
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as L
import qualified Data.Text.Lazy.IO          as L
import qualified Data.Tree                  as R
import Data.IORef
import           Pipes
import qualified Pipes.Prelude              as Pipes

import qualified NLP.Partage.DAG            as DAG
import qualified NLP.Partage.Earley         as Earley
import qualified NLP.Partage.Tree           as Parsed
import qualified NLP.Partage.FS             as FS
import qualified NLP.Partage.Auto.Trie      as Trie
import qualified NLP.Partage.Tree.Comp      as C
import qualified NLP.Partage.Tree.Other     as O
import qualified NLP.Partage.Earley.Deriv   as Deriv

import qualified NLP.Partage4Xmg.Lexicon    as Lex
import qualified NLP.Partage4Xmg.Morph      as Morph
import qualified NLP.Partage4Xmg.Grammar    as Gram
import qualified NLP.Partage4Xmg.Ensemble   as Ens


--------------------------------------------------
-- Configuration
--------------------------------------------------


-- | Parsing configuration.
data ParseCfg = ParseCfg
    { maxSize     :: Maybe Int
      -- ^ Optional limit on the sentence size
    , startSym    :: String
      -- ^ The starting symbol of trees
    , printParsed :: Int
      -- ^ Print the set of parsed trees?
    , useFS       :: Bool
      -- ^ Use feature structures?
    , printDeriv  :: Bool
      -- ^ Print derivations instead of derived trees
    } deriving (Show, Read, Eq, Ord)


-- | If the list longer than the given length?
longerThan :: [a] -> Maybe Int -> Bool
longerThan _ Nothing   = False
longerThan xs (Just n) = length xs > n


--------------------------------------------------
-- Parsing
--------------------------------------------------


-- | Create an automaton from a list of lexicalized elementary trees and the corresponding
-- feature structures.
mkAutoFS
  :: [(Ens.Tree T.Text, C.Comp a)]
  -> Earley.Auto T.Text T.Text a
mkAutoFS gram =
  let dag = DAG.mkGram gram
      tri = Trie.fromGram (DAG.factGram dag)
  in  Earley.mkAuto (DAG.dagGram dag) tri


-- | Create an automaton from a list of lexicalized elementary trees.
mkAuto :: [Ens.Tree T.Text] -> Earley.Auto T.Text T.Text Ens.ClosedFS
mkAuto =
  let dummy = C.Comp (const $ Just []) C.dummyTopDown
  in  mkAutoFS . map (,dummy)


-- | Parse the given sentence from the given start symbol with the given grammar.
parseWith
  :: Ens.Grammar
  -- ^ The TAG grammar
  -> [T.Text]
  -- ^ The sentence to parse
  -> IO (Earley.Hype T.Text T.Text Ens.ClosedFS)
parseWith gram sent0 = do
  let interps = map (Ens.getInterps gram . L.fromStrict) sent0
      elemTrees = concat
        [ map fst $ Ens.getTreesFS gram interp
        | interpSet <- interps
        , interp <- S.toList interpSet ]
      auto = mkAuto elemTrees
      input =
        [ S.fromList
          . map (\interp -> (L.toStrict $ Morph.lemma interp, []))
          . S.toList
          $ interpSet
        | interpSet <- interps ]

--   -- logging
--   forM_ (S.toList elemTrees) $
--     putStrLn . R.drawTree . fmap show
--   putStrLn ""
--   forM_ input print

  -- parsing
  Earley.earleyAuto auto . Earley.fromSets $ input


-- | Like `parseWith` but with FS unification.
parseWithFS
  :: Ens.Grammar
  -- ^ The TAG grammar
  -> [T.Text]
  -- ^ The sentence to parse
  -> IO (Earley.Hype T.Text T.Text Ens.ClosedFS)
parseWithFS gram sent0 = do
  let interps = map (Ens.getInterps gram . L.fromStrict) sent0
      elemTrees = concat
        [ Ens.getTreesFS gram interp
        | interpSet <- interps
        , interp <- S.toList interpSet ]
      auto = mkAutoFS elemTrees
      input =
        [ S.fromList
          . map (\interp ->
                   ( L.toStrict $ Morph.lemma interp
                   , Ens.closeAVM $ Morph.avm interp )
                )
          . S.toList
          $ interpSet
        | interpSet <- interps ]
  -- Earley.parseAuto auto begSym . Earley.fromSets $ input
  Earley.earleyAuto auto . Earley.fromSets $ input


-- -- -- | Like `parseWith` but with FS unification.
-- -- parseWithFS'
-- --   :: Ens.Grammar
-- --   -- ^ The TAG grammar
-- --   -> T.Text
-- --   -- ^ Start symbol
-- --   -> [T.Text]
-- --   -- ^ The sentence to parse
-- --   -> IO [Deriv.TokDeriv T.Text T.Text (FS.ClosedFS T.Text T.Text)]
-- parseWithFS' gram begSym sent0 = do
--   let interps = map (Ens.getInterps gram . L.fromStrict) sent0
--       elemTrees = concat
--         [ Ens.getTreesFS gram interp
--         | interpSet <- interps
--         , interp <- S.toList interpSet ]
--       auto = mkAutoFS elemTrees
--       input =
--         [ S.fromList
--           . map (\interp ->
--                    ( L.toStrict $ Morph.lemma interp
--                    , Ens.closeAVM $ Morph.avm interp )
--                 )
--           . S.toList
--           $ interpSet
--         | interpSet <- interps ]
--   Earley.earleyAuto auto . Earley.fromSets $ input
--   -- return $ Deriv.derivTrees hype begSym (length input)



--------------------------------------------------
-- Parsing
--------------------------------------------------


-- | Read the grammar from the input file, sentences to parse from
-- std input, and perform the experiment.
parseAll
  :: ParseCfg
  -> Ens.GramCfg
  -> IO ()
parseAll ParseCfg{..} gramCfg = do
  gram <- Ens.readGrammar gramCfg
  lines <- map L.toStrict . L.lines <$> L.getContents
  let parseIt = if useFS then parseWithFS else parseWith
  forM_ lines $ \line -> do
    let begSym = T.pack startSym
        input  = T.words line
    hype <- parseIt gram input
    if printDeriv then do
      let parseSet = Deriv.derivTrees hype begSym (length input)
      forM_ (take printParsed $ parseSet) $ \t0 -> do
        let t = fmap (fmap showClosedFS) t0
        putStrLn . R.drawTree . fmap show . Deriv.deriv4show $ t
    else do
      let parseSet = Earley.parsedTrees hype begSym (length input)
      forM_ (take printParsed $ parseSet) $ \t -> do
        putStrLn . R.drawTree . fmap show . O.encode . Left $ t


-- -- | Read the grammar from the input file, sentences to parse from
-- -- std input, and perform the experiment.
-- parseAll'
--   :: ParseCfg
--   -> Ens.GramCfg
--   -> IO ()
-- parseAll' ParseCfg{..} gramCfg = do
--   gram <- Ens.readGrammar gramCfg
--   lines <- map L.toStrict . L.lines <$> L.getContents
--   let parseIt = parseWithFS'
--   forM_ lines $ \line -> do
--     parseSet <- parseIt gram (T.pack startSym) (T.words line)
--     forM_ (take printParsed $ parseSet) $ \t -> do
--       putStrLn . R.drawTree . fmap show . O.encode . Left $ t


--------------------------------------------------
-- Utils
--------------------------------------------------


showClosedFS :: Ens.ClosedFS -> String
showClosedFS
  = between "{" "}"
  . intercalate ","
  . map showPair
  where
    showPair (keySet, mayValAlt) = showKeys keySet ++ "=" ++
      case mayValAlt of
        Nothing  -> "_"
        Just alt -> showVals alt
    showKeys = intercalate "&" . map T.unpack . S.toList
    showVals = intercalate "|" . map T.unpack . S.toList
    between x y z = x ++ z ++ y
