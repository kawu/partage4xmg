

-- Build automaton.


module NLP.FrenchTAG.Build
( buildAuto
) where


import qualified Data.Set as S

import qualified NLP.TAG.Vanilla.Tree.Other as O
import qualified NLP.TAG.Vanilla.SubtreeSharing as LS
import qualified NLP.TAG.Vanilla.Automaton as LA

import qualified NLP.FrenchTAG.Gen as G


-- | Build automaton and print the individual edges.
buildAuto :: FilePath -> IO ()
buildAuto gramPath = do
    -- extract the grammar
    gram <- G.getTrees gramPath
    -- build the automaton
    ruleSet <- LS.compile . map O.decode . S.toList $ gram
    let auto = LA.buildAuto ruleSet
    mapM_ print (LA.edges auto)
