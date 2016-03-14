{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


-- Parsing sentences from input and computing stats.


module NLP.FrenchTAG.Stats
( StatCfg (..)
, statsOn

-- * Tmp
, parseWei
) where


import           Control.Monad (unless, forM_, when)
import qualified Control.Monad.State.Strict   as E

import qualified Data.Text.Lazy as L
import qualified Data.Tree as R
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.MemoCombinators as Memo
import qualified Pipes.Prelude as Pipes
import           Pipes

import qualified NLP.Partage.FactGram.Weighted as W
import qualified NLP.Partage.Earley as Earley
import qualified NLP.Partage.Earley.Prob.AutoAP as AStar
import qualified NLP.Partage.Tree.Other as T

import qualified NLP.FrenchTAG.Gen as G
import qualified NLP.FrenchTAG.Build as B


--------------------------------------------------
-- Configuration
--------------------------------------------------


-- | Configuration.
data StatCfg = StatCfg
    { maxSize       :: Maybe Int
    -- ^ Optional limit on the sentence size
    , buildCfg      :: B.BuildCfg
    -- ^ Grammar construction configuration
    , startSym      :: String
    -- ^ The starting symbol of trees
    , printParsed   :: Int
    -- ^ Print the set of parsed trees?
    } deriving (Show, Read, Eq, Ord)


-- | If the list longer than the given length?
longerThan :: [a] -> Maybe Int -> Bool
longerThan _ Nothing   = False
longerThan xs (Just n) = length xs > n


--------------------------------------------------
-- Std input
--------------------------------------------------


-- | Produce sentence in the given file.
sentPipe :: Producer [[G.Term]] IO ()
sentPipe = Pipes.stdinLn >-> Pipes.map read


--------------------------------------------------
-- Stats entry
--------------------------------------------------


-- | Stats for a given sentence length.
data Stat = Stat
    { nodeNum   :: Int
    , edgeNum   :: Int
    , statNum   :: Int
    , parsNum   :: Int
    } deriving (Show, Eq, Ord)


-- | Create new `Stat` based on (number of nodes, number of edges).
newStat :: Maybe (Int, Int) -> Stat
newStat (Just (n, m)) = Stat
    { nodeNum   = n
    , edgeNum   = m
    , statNum   = 1
    , parsNum   = 1 }
newStat Nothing = Stat
    { nodeNum   = 0
    , edgeNum   = 0
    , statNum   = 1
    , parsNum   = 0 }


-- | Add to `Stat`s.
addStat :: Stat -> Stat -> Stat
addStat x y = Stat
    { nodeNum   = nodeNum x + nodeNum y
    , edgeNum   = edgeNum x + edgeNum y
    , statNum   = statNum x + statNum y
    , parsNum   = parsNum x + parsNum y }


--------------------------------------------------
-- Stats computation
--------------------------------------------------


-- | Read the grammar from the input file, sentences to parse from
-- std input, and perform the experiment.
statsOn
    :: StatCfg
    -> FilePath         -- ^ Grammar
    -> Maybe FilePath   -- ^ Lexicon (if present)
    -> IO ()
statsOn StatCfg{..} gramPath mayLexPath = do
    -- extract the grammar and build the automaton
    (dag, gramAuto) <- B.buildAuto buildCfg gramPath mayLexPath
    let auto = Earley.mkAuto dag gramAuto
    -- read sentences from input
    let thePipe = hoist lift sentPipe
    statMap <- flip E.execStateT M.empty . runEffect . for thePipe $
        \sent -> unless (sent `longerThan` maxSize) $ do
            stat <- liftIO $ do
                (stat, parseSet) <- parseEarley auto sent
                putStr "### "
                putStr (show sent)
                putStr " => " >> print stat
                putStrLn ""
                forM_ (take printParsed $ parseSet) $ \t -> do
                    putStrLn . R.drawTree . fmap show . T.encode . Left $ t
                return stat
            E.modify $ M.insertWith addStat
                (length sent) (newStat stat)
    liftIO $ do
        putStrLn ""
        putStrLn "length,sentences,parsed,nodes,edges"
        forM_ (M.toList statMap) $ \(n, stat) -> do
            putStr (show n ++ ",")
            printStat stat
            putStrLn ""
    liftIO $ do
        putStrLn ""
        putStrLn " === TOTAL === "
        putStrLn ""
        putStrLn "sentences,parsed,nodes,edges"
        printStat $ foldl1 addStat (M.elems statMap)
        putStrLn ""


  where

    -- | Parse with Earley version.
    parseEarley auto sent = do
        let input = Earley.fromSets $ map S.fromList sent
        hype <- Earley.earleyAuto auto input
        let treeSet = Earley.parsedTrees hype
                        (L.pack startSym) (length sent)
        stat <- if not (null treeSet) then do
                    return $ Just
                        ( Earley.hyperNodesNum hype
                        , Earley.hyperEdgesNum hype )
                 else do return Nothing
        return (stat, treeSet)

    printStat Stat{..} = do
        putStr (show statNum ++ ",")
        putStr (show parsNum ++ ",")
        putStr (show (nodeNum `divide` statNum) ++ ",")
        putStr (show (edgeNum `divide` statNum))


--------------------------------------------------
-- Temp section
--------------------------------------------------


-- | Read the grammar from the input files, sentences to parse from
-- std input, and perform the experiment.
parseWei
    :: FilePath         -- ^ Grammar
    -> Maybe FilePath   -- ^ Lexicon (if present)
    -> String           -- ^ Start symbol
    -> IO ()
parseWei gramPath mayLexPath begSym = do
    -- extract the grammar and build the automaton
    auto <- AStar.mkAuto . W.mkGram
          -- . map ((,1) . T.decode)
          . map (,1)
          . S.toList
        <$> G.getTrees gramPath mayLexPath
    -- read sentences from input
    runEffect . for sentPipe
        $ liftIO
        . parseAStar auto
        . map head

  where

    -- | Parse with Prob.AutoAP (A*) version.
    parseAStar :: AStar.Auto G.NonTerm G.Term -> [G.Term] -> IO ()
    parseAStar auto sent = do
        let input = AStar.fromList sent
        b <- AStar.recognizeFromAuto
                termMemo auto (L.pack begSym) input
        putStrLn  $ ">>> " ++ show b
    termMemo = Memo.wrap read show $ Memo.list Memo.char


--------------------------------------------------
-- Misc
--------------------------------------------------


divide :: (Integral a, Integral b) => a -> b -> Double
divide x y = (fromIntegral x :: Double) / fromIntegral y
