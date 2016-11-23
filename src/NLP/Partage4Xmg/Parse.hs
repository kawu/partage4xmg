{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


-- | Parsing sentences from input and printing trees.


module NLP.Partage4Xmg.Parse
( ParseCfg (..)
, parseAll
) where


import           Control.Monad              (forM_, unless, when)
import qualified Control.Monad.State.Strict as E
import           Control.Monad.Trans.Maybe

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
import qualified NLP.Partage.Auto.Trie      as Trie
import qualified NLP.Partage.Tree.Other     as O

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
    } deriving (Show, Read, Eq, Ord)


-- | If the list longer than the given length?
longerThan :: [a] -> Maybe Int -> Bool
longerThan _ Nothing   = False
longerThan xs (Just n) = length xs > n


--------------------------------------------------
-- Parsing
--------------------------------------------------



-- | Create an automaton from a list of lexicalized elementary trees.
mkAuto :: [Ens.Tree T.Text] -> Earley.Auto T.Text T.Text ()
mkAuto gram0 =
  let gram = map (, const $ Just ()) gram0
      dag = DAG.mkGram gram
      tri = Trie.fromGram (DAG.factGram dag)
  in  Earley.mkAuto (DAG.dagGram dag) tri


-- | Parse the given sentence from the given start symbol with the given grammar.
parseWith :: Ens.Grammar -> T.Text -> [T.Text]  -> IO [Parsed.Tree T.Text T.Text]
parseWith gram begSym sent0 = do
  let interps = map (Ens.getInterps gram . L.fromStrict) sent0
      elemTrees = S.unions
        [ Ens.getTrees gram interp
        | interpSet <- interps
        , interp <- S.toList interpSet ]
      auto = mkAuto $ S.toList elemTrees
      input =
        [ S.fromList
          . map (\interp -> (L.toStrict $ Morph.name interp, ()))
          . S.toList
          $ interpSet
        | interpSet <- interps ]

--   -- logging
--   forM_ (S.toList elemTrees) $
--     putStrLn . R.drawTree . fmap show
--   putStrLn ""
--   forM_ input print

  -- parsing
  Earley.parseAuto auto begSym . Earley.fromSets $ input


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
  forM_ lines $ \line -> do
    parseSet <- parseWith gram (T.pack startSym) (T.words line)
    forM_ (take printParsed $ parseSet) $ \t -> do
      putStrLn . R.drawTree . fmap show . O.encode . Left $ t


-- -- | Read the grammar from the input file, sentences to parse from
-- -- std input, and perform the experiment.
-- statsOn
--     :: StatCfg
--     -> B.BuildData
--     -- -> FilePath         -- ^ Grammar
--     -- -> Maybe FilePath   -- ^ Lexicon (if present)
--     -> IO ()
-- statsOn StatCfg{..} buildData = do
--     -- extract the grammar and build the automaton
--     (dag, gramAuto) <- B.buildAuto buildData
--     let auto = Earley.mkAuto dag gramAuto
--     -- read sentences from input
--     let thePipe = hoist lift sentPipe
--     statMap <- flip E.execStateT M.empty . runEffect . for thePipe $
--         \sent -> unless (sent `longerThan` maxSize) $ do
--             stat <- liftIO $ do
--                 (stat, parseSet) <- parseEarley auto sent
--                 putStr "### "
--                 putStr (show sent)
--                 putStr " => " >> print stat
--                 putStrLn ""
--                 forM_ (take printParsed $ parseSet) $ \t -> do
--                     putStrLn . R.drawTree . fmap show . O.encode . Left $ t
--                 return stat
--             E.modify $ M.insertWith addStat
--                 (length sent) (newStat stat)
--     liftIO $ do
--         putStrLn ""
--         putStrLn "length,sentences,parsed,nodes,edges"
--         forM_ (M.toList statMap) $ \(n, stat) -> do
--             putStr (show n ++ ",")
--             printStat stat
--             putStrLn ""
--     liftIO $ do
--         putStrLn ""
--         putStrLn " === TOTAL === "
--         putStrLn ""
--         putStrLn "sentences,parsed,nodes,edges"
--         printStat $ foldl1 addStat (M.elems statMap)
--         putStrLn ""
-- 
-- 
--   where
-- 
--     -- | Parse with Earley version.
--     parseEarley auto sent = do
--         let input = Earley.fromList $ map (,()) sent
--         hype <- Earley.earleyAuto auto input
--         let treeSet = Earley.parsedTrees hype
--                         (T.pack startSym) (length sent)
--         stat <- if not (null treeSet) then do
--                     return $ Just
--                         ( Earley.hyperNodesNum hype
--                         , Earley.hyperEdgesNum hype )
--                  else do return Nothing
--         return (stat, treeSet)


--------------------------------------------------
-- Misc
--------------------------------------------------
