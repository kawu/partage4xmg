

-- Build automaton.


module NLP.FrenchTAG.Build
( buildAuto
, buildTrie
) where


import qualified Data.Set as S

import qualified NLP.TAG.Vanilla.Tree.Other as O
import qualified NLP.TAG.Vanilla.SubtreeSharing as LS
import qualified NLP.TAG.Vanilla.Auto.Mini as Mini
import qualified NLP.TAG.Vanilla.Auto.DAWG as DAWG
import qualified NLP.TAG.Vanilla.Auto.Trie as Trie

import qualified NLP.FrenchTAG.Gen as G


-- | Build automaton and print the individual edges.
buildAuto :: FilePath -> IO ()
buildAuto gramPath = do
    -- extract the grammar
    gram <- G.getTrees gramPath
    -- build the automaton
    ruleSet <- LS.compile . map O.decode . S.toList $ gram
    let auto = DAWG.buildAuto ruleSet
    mapM_ print (DAWG.edges auto)


-- | Build automaton and print the individual edges.
buildTrie :: FilePath -> IO ()
buildTrie gramPath = do
    -- extract the grammar
    gram <- G.getTrees gramPath
    -- build the automaton
    ruleSet <- LS.compile . map O.decode . S.toList $ gram
    let trie = Trie.buildTrie ruleSet
    mapM_ print . Mini.allEdges $ Trie.shell trie
