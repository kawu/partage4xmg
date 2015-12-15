{-# LANGUAGE RecordWildCards #-}


-- Parsing sentences from input and computing stats.


module NLP.FrenchTAG.Stats
( StatCfg (..)
, ParseMethod (..)
, statsOn
) where


import           Control.Monad (unless, forM_)
import qualified Control.Monad.State.Strict   as E

import qualified Data.Set as S
import qualified Data.Map.Strict as M
-- import qualified Data.Text.Lazy      as L
import qualified Pipes.Prelude as Pipes
import           Pipes

import qualified NLP.TAG.Vanilla.Tree.Other as O
import qualified NLP.TAG.Vanilla.Rule as Rule
import qualified NLP.TAG.Vanilla.SubtreeSharing as LS
import qualified NLP.TAG.Vanilla.Automaton as LA
import qualified NLP.TAG.Vanilla.Earley.Auto as Auto
import qualified NLP.TAG.Vanilla.Earley.AutoAP as AutoAP
import qualified NLP.TAG.Vanilla.Earley.TreeGen as TreeGen

import qualified NLP.FrenchTAG.Gen as G


--------------------------------------------------
-- Configuration
--------------------------------------------------


-- | Configuration.
data StatCfg = StatCfg
    { maxSize       :: Maybe Int
    -- ^ Optional limit on the sentence size
    , shareTrees    :: Bool
    -- ^ Subtree sharing
    , howParse      :: ParseMethod
    -- ^ Chosen parsing method
    } deriving (Show, Read, Eq, Ord)


-- | Parsing method selection.
data ParseMethod
    = AutoAP
    -- ^ Automaton with active/passive distinction
    | Auto
    -- ^ Automaton with no active/passive distinction
    | TreeGen
    -- ^ Version with prefix sharing
    deriving (Show, Read, Eq, Ord)


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
    { nodeNum   :: Int
    , edgeNum   :: Int
    , statNum   :: Int
    } deriving (Show, Eq, Ord)


-- | Create new `Stat` based on (number of nodes, number of edges).
newStat :: (Int, Int) -> Stat
newStat (n, m) = Stat
    { nodeNum   = n
    , edgeNum   = m
    , statNum   = 1 }


-- | Add to `Stat`s.
addStat :: Stat -> Stat -> Stat
addStat x y = Stat
    { nodeNum   = nodeNum x + nodeNum y
    , edgeNum   = edgeNum x + edgeNum y
    , statNum   = statNum x + statNum y }


--------------------------------------------------
-- Stats computation
--------------------------------------------------


-- | Read the grammar from the input file, sentences to parse from
-- std input, and perform the experiment.
statsOn :: StatCfg -> FilePath -> IO ()
statsOn StatCfg{..} gramPath = do
    -- extract the grammar
    gram <- G.getTrees gramPath

    -- build the automaton
    let compile = if shareTrees
            then LS.compile
            else Rule.compile
    ruleSet <- compile . map O.decode . S.toList $ gram
    let auto = LA.buildAuto ruleSet

    -- read sentences from input
    let thePipe = hoist lift sentPipe
    statMap <- flip E.execStateT M.empty . runEffect . for thePipe $
        \sent -> unless (sent `longerThan` maxSize) $ do
            stat <- liftIO $ do
                putStr . show $ sent
                case howParse of
                    AutoAP  -> parseAutoAP auto sent
                    Auto    -> parseAuto auto sent
                    TreeGen -> parseTreeGen ruleSet sent
            liftIO $ putStr " => " >> print stat
            E.modify $ M.insertWith addStat
                (length sent) (newStat stat)

    liftIO $ do
        putStrLn ""
        putStrLn "length,nodes,edges"
        let divide x y = (fromIntegral x :: Double) / fromIntegral y
        forM_ (M.toList statMap) $ \(n, Stat{..}) -> do
            putStr (show n ++ ",")
            putStr (show (nodeNum `divide` statNum) ++ ",")
            putStr (show (edgeNum `divide` statNum))
            putStrLn ""

  where

    -- | Parse with AutoAP version.
    parseAutoAP auto sent = do
        earSt <- AutoAP.earleyAuto auto sent
        unless (AutoAP.isRecognized sent earSt)
            $ error "parseAutoAP: didn't recognize the sentence!"
        return
            ( AutoAP.hyperNodesNum earSt
            , AutoAP.hyperEdgesNum earSt )

    -- | Parse with Auto version.
    parseAuto auto sent = do
        earSt <- Auto.earleyAuto auto sent
        unless (Auto.isRecognized earSt sent)
            $ error "parseAuto: didn't recognize the sentence!"
        return
            ( Auto.hyperNodesNum earSt
            , Auto.hyperEdgesNum earSt )

    -- | Parse with TreeGen version.
    parseTreeGen ruleSet sent = do
        earSt <- TreeGen.earley ruleSet sent
        return
            ( TreeGen.hyperNodesNum earSt
            , TreeGen.hyperEdgesNum earSt )


--         -- results for base version
--         baseEarSt <- LPG.earley ruleSet sent
--         putStr " => (BASE: "
--         -- putStr $ show (LPA.isRecognized  baseEarSt sent) ++ ", "
--         putStr $ show (LPG.hyperNodesNum baseEarSt) ++ ", "
--         putStr $ show (LPG.hyperEdgesNum baseEarSt) ++ ")"
--
--         -- results for automaton
--         autoEarSt <- LPA.earleyAuto auto sent
--         putStr " # (AUTO: "
--         putStr $ show (LPA.isRecognized sent autoEarSt) ++ ", "
--         putStr $ show (LPA.hyperNodesNum autoEarSt) ++ ", "
--         putStr $ show (LPA.hyperEdgesNum autoEarSt) ++ ")"
--
--         putStrLn ""

