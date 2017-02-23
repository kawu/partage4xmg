{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE GADTs             #-}


-- | Parsing sentences from input and printing trees.


module NLP.Partage4Xmg.Parse
( ParseCfg (..)
, printETs
, parseAll
) where


import           Control.Monad              (forM_, unless, when)
import           Control.Monad.Trans.Maybe

import           Data.List                  (intercalate)
import           Data.Maybe                 (maybeToList, mapMaybe)
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
import qualified NLP.Partage.FSTree2        as FSTree
import qualified NLP.Partage.Env            as Env
import qualified NLP.Partage.Auto.Trie      as Trie
import qualified NLP.Partage.Tree.Other     as O
import           NLP.Partage.Earley.Base    (Tok(..))
import qualified NLP.Partage.Earley.Comp    as C
import qualified NLP.Partage.Earley.Deriv   as Deriv

import qualified NLP.Partage4Xmg.Lexicon    as Lex
import qualified NLP.Partage4Xmg.Morph      as Morph
import qualified NLP.Partage4Xmg.Grammar    as Gram
import qualified NLP.Partage4Xmg.Ensemble as Ens
import           NLP.Partage4Xmg.Ensemble
                 (Tree, FSTree, NonTerm, Term, Val) --, CFS)

-- import Debug.Trace (trace)


--------------------------------------------------
-- Configuration
--------------------------------------------------


-- | Parsing configuration.
data ParseCfg = ParseCfg
    { startSym    :: String
      -- ^ The starting symbol
    , printParsed :: Int
      -- ^ Print the set of parsed trees?  How many?
    , useFS       :: Bool
      -- ^ Use feature structures?
    , printDeriv  :: Bool
      -- ^ Print derivations instead of derived trees
    } deriving (Show, Read, Eq, Ord)


--------------------------------------------------
-- Parsing
--------------------------------------------------


-- | Create an automaton from a list of lexicalized elementary trees and the
-- corresponding feature structures.
mkAutoFS :: [(Tree Term, C.Comp a)] -> Earley.Auto NonTerm Term a
mkAutoFS gram =
  let dag = DAG.mkGramWith C.or gram
      tri = Trie.fromGram (DAG.factGram dag)
  in  Earley.mkAuto (DAG.dagGram dag) tri


-- | Create an automaton from a list of lexicalized elementary trees.
mkAuto :: [Tree Term] -> Earley.Auto NonTerm Term (FS.CFS key Val)
mkAuto =
  let dummy = C.Comp (const [M.empty]) C.dummyTopDown
  in  mkAutoFS . map (,dummy)


-- | Retieve the set of ETs for the given grammar and the given terminal.
gramOn
  :: (Ord avm, Ord key, Show key)
  => Ens.AvmTyp avm key
  -> Ens.Grammar avm
  -- ^ The TAG grammar
  -> Term
  -- ^ The terminal
  -- -> M.Map (Tree Term) (C.Comp CFS)
  -> [Env.EnvM Val (FSTree Term key)]
gramOn avmTyp gram word =
  -- trace (show interpSet) $ elemTrees
  elemTrees
  where
    interpSet = Ens.getInterps gram word
    elemTrees = concat
      [ Ens.getTrees avmTyp gram word interp
      | interp <- S.toList interpSet ]


-- | Compile the given tree into a grammar tree and the corresponding
-- computation.
compile
  :: (Ord key, Show key)
  => Env.EnvM Val (FSTree Term key)
  -> Maybe (Tree Term, C.Comp (FS.CFS key Val))
compile = Ens.splitTree


-- | Extract the underlying FSTree.
extract
  :: Env.EnvM Val (FSTree Term k)
  -> Maybe (R.Tree (Ens.Node Term, FS.CFS k Val))
extract = FSTree.extract


-- | Parse the given sentence from the given start symbol with the given grammar.
parseWith
  :: (Ord avm, Ord key, Show key)
  => Ens.AvmTyp avm key
  -> Ens.Grammar avm
  -- ^ The TAG grammar
  -> [T.Text]
  -- ^ The sentence to parse
  -> IO (Earley.Hype NonTerm Term (FS.CFS key Val))
parseWith avmTyp gram sent = do
  let elemTrees
        = S.toList . S.fromList
        . map fst
        . mapMaybe compile
        . concatMap (gramOn avmTyp gram)
        $ sent
      auto = mkAuto elemTrees
      input = map (S.singleton . (, M.empty :: FS.CFS key Val)) sent
  Earley.earleyAuto auto . Earley.fromSets $ input


-- | Like `parseWith` but with FS unification.
parseWithFS
  :: (Ord avm, Ord key, Show key)
  => Ens.AvmTyp avm key
  -> Ens.Grammar avm
  -- ^ The TAG grammar
  -> [T.Text]
  -- ^ The sentence to parse
  -> IO (Earley.Hype T.Text T.Text (FS.CFS key Val))
parseWithFS avmTyp gram sent = do
  let elemTrees
        -- = M.fromList <- this was WRONG
        = mapMaybe compile
        . concatMap (gramOn avmTyp gram)
        $ sent
      auto = mkAutoFS elemTrees -- (M.toList elemTrees)
      input = [S.singleton (x, M.empty) | x <- sent]
--       input =
--         [ S.fromList
--           . map (\interp ->
--                    ( Morph.lemma interp
--                    , Ens.closeTopAVM $ Morph.avm interp )
--                 )
--           . S.toList
--           $ interpSet
--         | interpSet <- interps ]
  Earley.earleyAuto auto . Earley.fromSets $ input


--------------------------------------------------
-- Showing trees
--------------------------------------------------


-- | Read the grammar from the input file, sentences to parse from std input,
-- and show the extracted grammar trees (no FSs, though).
printETs
  :: (Ord avm, Show avm, Ord key, Show key)
  => Ens.GramCfg
  -> Ens.AvmTyp avm key
  -> IO ()
printETs gramCfg avmTyp = do
  gram <- Ens.readGrammar gramCfg avmTyp
  lines <- map L.toStrict . L.lines <$> L.getContents
  forM_ lines $ \line -> do
    let input  = T.words line
    forM_ input $ \word -> do
      putStrLn $ "<<WORD: " ++ T.unpack word ++ ">>"
      putStrLn ""
      let elemTrees = mapMaybe extract $ gramOn avmTyp gram word
      forM_ elemTrees $
        putStrLn . R.drawTree . fmap showPair
      -- putStrLn ""
  where
    showPair (node, avm) =
      O.showNode T.unpack T.unpack node ++ " " ++
      showCFS avmTyp avm


--------------------------------------------------
-- Parsing
--------------------------------------------------


-- | Read the grammar from the input file, sentences to parse from
-- std input, and perform the experiment.
parseAll
  :: (Ord avm, Show avm, Ord key, Show key)
  => ParseCfg
  -> Ens.GramCfg
  -> Ens.AvmTyp avm key
  -> IO ()
parseAll ParseCfg{..} gramCfg avmTyp = do
  gram <- Ens.readGrammar gramCfg avmTyp
  lines <- map L.toStrict . L.lines <$> L.getContents
  let parseIt = if useFS then parseWithFS  else parseWith
  forM_ lines $ \line -> do
    let begSym = T.pack startSym
        input  = T.words line
    hype <- parseIt avmTyp gram input
    if printDeriv then do
      let parseSet () = Deriv.derivTrees hype begSym (length input)
      forM_ (take printParsed $ parseSet ()) $ \t0 -> do
        let showPrintNode = Deriv.showPrintNode showPair
            showPair (node, mayAvm) =
              O.showNode T.unpack showTok node ++ " " ++
              showCFS avmTyp (maybe M.empty id mayAvm)
            showTok Tok{..} = show position ++ " " ++ T.unpack terminal
        -- let t = fmap (fmap $ showCFS avmTyp) t0
        putStrLn . R.drawTree . fmap showPrintNode . Deriv.deriv4show $ t0
      reportTreeNum line printParsed (parseSet ())
    else do
      let parseSet () = Earley.parsedTrees hype begSym (length input)
      forM_ (take printParsed $ parseSet ()) $ \t -> do
        -- putStrLn . R.drawTree . fmap show . O.encode . Left $ t
        putStrLn . R.drawTree . fmap (O.showNode T.unpack T.unpack) . O.encode . Left $ t
      reportTreeNum line printParsed (parseSet ())


-- | Report the number of parsed (or derivation) trees.
reportTreeNum :: T.Text -> Int -> [a] -> IO ()
reportTreeNum input maxNum parseSet = do
  let n = length parseSet
      treeStr = if n > 1 || n == 0 then "trees" else "tree"
  if n < maxNum
    then putStr $ show n ++ " " ++ treeStr ++ " found"
    else putStr $ show n ++ " (or more) trees found"
  putStrLn $ " for \"" ++ T.unpack input ++ "\""
  putStrLn ""


--------------------------------------------------
-- Utils
--------------------------------------------------


showCFS :: Ens.AvmTyp avm key -> FS.CFS key Val -> String
showCFS avmTyp = case avmTyp of
  Ens.Simple -> showGenCFS T.unpack
  Ens.TopBot -> showGenCFS show


showGenCFS :: (key -> String) -> FS.CFS key Val -> String
showGenCFS showKey
  = between "{" "}"
  . intercalate ","
  . map showPair
  . M.toList
  where
    showPair (key, FS.Val{..}) =
      -- "(" ++ show valID ++ ")" ++ showKey key ++
      showKey key ++ "=" ++
      -- showKey key ++ "@" ++ show valID ++
      (case valAlt of
        Nothing  -> ""
        Just alt -> showVals alt)
      ++ "(" ++ show valID ++ ")"
    -- showKeys = intercalate "&" . map T.unpack . S.toList
    showVals = intercalate "|" . map T.unpack . S.toList
--     showKey key = case key of
--       FSTree.Top x -> "t." ++ T.unpack x
--       FSTree.Bot x -> "b." ++ T.unpack x
    between x y z = x ++ z ++ y

-- showCFS =
--   = between "{" "}"
--   . intercalate ","
--   . map showPair
--   where
--     showPair (keySet, mayValAlt) = showKeys keySet ++ "=" ++
--       case mayValAlt of
--         Nothing  -> "_"
--         Just alt -> showVals alt
--     -- showKeys = intercalate "&" . map T.unpack . S.toList
--     showKeys = intercalate "&" . map showKey . S.toList
--     showVals = intercalate "|" . map T.unpack . S.toList
--     showKey key = case key of
--       FSTree.Top x -> "t." ++ T.unpack x
--       FSTree.Bot x -> "b." ++ T.unpack x
--     between x y z = x ++ z ++ y
