{-# LANGUAGE RecordWildCards #-}


-- Parsing sentences from input and computing stats.


module NLP.FrenchTAG.Stats
( statsOn
) where


import qualified Data.Set as S
-- import qualified Data.Text.Lazy      as L
import qualified Pipes.Prelude as Pipes
import           Pipes

import qualified NLP.TAG.Vanilla.Tree.Other as O
import qualified NLP.TAG.Vanilla.SubtreeSharing as LS
import qualified NLP.TAG.Vanilla.Automaton as LA
import qualified NLP.TAG.Vanilla.Earley.Auto as LPA
import qualified NLP.TAG.Vanilla.Earley.TreeGen as LPG

import qualified NLP.FrenchTAG.Gen as G


-- | Produce sentence in the given file.
sentPipe :: Producer [G.Term] IO ()
sentPipe = Pipes.stdinLn >-> Pipes.map read


-- | Read the grammar from the input file, sentences to parse from std input,
-- and perform the experiment.
statsOn :: FilePath -> IO ()
statsOn gramPath = do
    -- extract the grammar
    gram <- G.getTrees gramPath

    -- build the automaton
    ruleSet <- LS.compile . map O.decode . S.toList $ gram
    let auto = LA.buildAuto ruleSet

    -- read sentences from input
    runEffect . for sentPipe $ \sent -> liftIO $ do
        putStr . show $ sent

        -- results for base version
        baseEarSt <- LPG.earley ruleSet sent
        putStr $ " => (BASE: "
        -- putStr $ show (LPA.isRecognized  baseEarSt sent) ++ ", "
        putStr $ show (LPG.hyperNodesNum baseEarSt) ++ ", "
        putStr $ show (LPG.hyperEdgesNum baseEarSt) ++ ")"

        -- results for automaton
        autoEarSt <- LPA.earleyAuto auto sent
        putStr $ " (AUTO: "
        putStr $ show (LPA.isRecognized  autoEarSt sent) ++ ", "
        putStr $ show (LPA.hyperNodesNum autoEarSt) ++ ", "
        putStr $ show (LPA.hyperEdgesNum autoEarSt) ++ ") "

        putStrLn ""
