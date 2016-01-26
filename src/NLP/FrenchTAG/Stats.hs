{-# LANGUAGE RecordWildCards #-}


-- Parsing sentences from input and computing stats.


module NLP.FrenchTAG.Stats
( StatCfg (..)
, statsOn
) where


import           Control.Monad (unless, forM_)
import qualified Control.Monad.State.Strict   as E

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Pipes.Prelude as Pipes
import           Pipes

import qualified NLP.Partage.Earley as Earley

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
    auto <- Earley.mkAuto =<<
        B.buildAuto buildCfg gramPath mayLexPath
    -- read sentences from input
    let thePipe = hoist lift sentPipe
    statMap <- flip E.execStateT M.empty . runEffect . for thePipe $
        \sent -> unless (sent `longerThan` maxSize) $ do
            stat <- liftIO $ do
                putStr . show $ sent
                parseEarley auto
                    $ Earley.fromSets
                    $ map S.fromList sent
            liftIO $ putStr " => " >> print stat
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
        rec <- Earley.recognizeAuto auto sent
        if rec then do
            earSt <- Earley.earleyAuto auto sent
            return $ Just
                ( Earley.hyperNodesNum earSt
                , Earley.hyperEdgesNum earSt )
        else do return Nothing

    printStat Stat{..} = do
        putStr (show statNum ++ ",")
        putStr (show parsNum ++ ",")
        putStr (show (nodeNum `divide` statNum) ++ ",")
        putStr (show (edgeNum `divide` statNum))


divide :: (Integral a, Integral b) => a -> b -> Double
divide x y = (fromIntegral x :: Double) / fromIntegral y
