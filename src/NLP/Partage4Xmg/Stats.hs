{-# LANGUAGE RecordWildCards #-}


-- Parsing sentences from input and computing stats.


module NLP.Partage4Xmg.Stats
( StatCfg (..)
, statsOn
) where


import           Control.Monad              (forM_, unless)
import qualified Control.Monad.State.Strict as E

import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Pipes
import qualified Pipes.Prelude              as Pipes
import qualified System.TimeIt              as TimeIt

import qualified NLP.Partage.Earley         as Earley

import qualified NLP.Partage4Xmg.Build      as B
import qualified NLP.Partage4Xmg.Gen        as G


--------------------------------------------------
-- Configuration
--------------------------------------------------


-- | Configuration.
data StatCfg = StatCfg
    { maxSize  :: Maybe Int
    -- ^ Optional limit on the sentence size
    , buildCfg :: B.BuildCfg
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
sentPipe :: Producer [G.Term] IO ()
sentPipe = Pipes.stdinLn >-> Pipes.map read


--------------------------------------------------
-- Stats entry
--------------------------------------------------


-- | Stats for a given sentence length.
data Stat = Stat
    { nodeNum  :: Int
    , edgeNum  :: Int
    , procTime :: Double
    -- ^ Processing time in seconds
    , statNum  :: Int
    } deriving (Show, Eq, Ord)


-- -- | Create new `Stat` based on (number of nodes, number of edges).
-- newStat :: (Int, Int, Double) -> Stat
-- newStat (n, m) = Stat
--     { nodeNum   = n
--     , edgeNum   = m
--     , statNum   = 1 }


-- | Add to `Stat`s.
addStat :: Stat -> Stat -> Stat
addStat x y = Stat
    { nodeNum   = nodeNum x + nodeNum y
    , edgeNum   = edgeNum x + edgeNum y
    , procTime  = procTime x + procTime y
    , statNum   = statNum x + statNum y }


--------------------------------------------------
-- Stats computation
--------------------------------------------------


-- | Read the grammar from the input file, sentences to parse from
-- std input, and perform the experiment.
statsOn :: StatCfg -> FilePath -> IO ()
statsOn StatCfg{..} gramPath = do
    -- extract the grammar and build the automaton
    auto <- B.buildAuto buildCfg gramPath
    -- read sentences from input
    let thePipe = hoist lift sentPipe
    statMap <- flip E.execStateT M.empty . runEffect . for thePipe $
        \sent -> unless (sent `longerThan` maxSize) $ do
            stat <- liftIO $ do
                putStr . show $ sent
                parseEarley auto (map S.singleton sent)
            liftIO $ putStr " => " >> print stat
            E.modify $ M.insertWith addStat
                (length sent) stat
    liftIO $ do
        putStrLn ""
        putStrLn "length,nodes,edges,time"
        let divide x y = (fromIntegral x :: Double) / fromIntegral y
        forM_ (M.toList statMap) $ \(n, Stat{..}) -> do
            putStr (show n ++ ",")
            putStr (show (nodeNum `divide` statNum) ++ ",")
            putStr (show (edgeNum `divide` statNum) ++ ",")
            putStr (show (procTime / fromIntegral statNum))
            putStrLn ""

  where

    -- | Parse with Earley version.
    parseEarley auto sent = do
        (execTime, _) <- TimeIt.timeItT $ do
          rec <- Earley.recognizeAuto auto sent
          unless rec $
            error "parseEarley: didn't recognize the sentence!"
        earSt <- Earley.earleyAuto auto sent
        return $ Stat
          { nodeNum  = Earley.hyperNodesNum earSt
          , edgeNum  = Earley.hyperEdgesNum earSt
          , procTime = execTime
          , statNum  = 1 }
