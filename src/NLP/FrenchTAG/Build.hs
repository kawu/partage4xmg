{-# LANGUAGE RecordWildCards #-}


-- Build automaton.


module NLP.FrenchTAG.Build
(
-- * Config
  BuildCfg (..)
, Compress (..)

-- * Build
, buildAuto
, printAuto
) where


import qualified Data.Set as S

import qualified NLP.TAG.Vanilla.Tree.Other as O
import qualified NLP.TAG.Vanilla.Rule as Rule
import qualified NLP.TAG.Vanilla.SubtreeSharing as LS
import qualified NLP.TAG.Vanilla.Auto.Edge as Edge
import qualified NLP.TAG.Vanilla.Auto.Mini as Mini
import qualified NLP.TAG.Vanilla.Auto.DAWG as DAWG
import qualified NLP.TAG.Vanilla.Auto.Trie as Trie
import qualified NLP.TAG.Vanilla.Auto.List as List
import qualified NLP.TAG.Vanilla.Auto.Set  as Set

import qualified NLP.FrenchTAG.Gen as G


--------------------------------------------------
-- Configuration
--------------------------------------------------


-- | Compression method selection.
data Compress
    = Auto
    -- ^ Minimal automaton
    | Trie
    -- ^ Prefix trie
    | List
    -- ^ List of lists
    | SetAuto
    -- ^ Set of automata, one per rule head symbol
    | SetTrie
    -- ^ Set of tries, one per rule head symbol
    deriving (Show, Read, Eq, Ord)


-- | Building configuration.
data BuildCfg = BuildCfg
    { compLevel     :: Compress
    -- ^ Compression level
    , shareTrees    :: Bool
    -- ^ Subtree sharing
    } deriving (Show, Read, Eq, Ord)


--------------------------------------------------
-- Contruction
--------------------------------------------------


-- | Local automaton verion.
type Auto = Mini.AutoR G.NonTerm G.Term


-- | Build automaton using the specified compression technique.
buildAuto :: BuildCfg -> FilePath -> IO Auto
buildAuto BuildCfg{..} gramPath = do
    -- extract the grammar
    gram <- G.getTrees gramPath

    -- build the automaton
    let compile = if shareTrees
            then LS.compile
            else Rule.compile
    ruleSet <- compile . map O.decode . S.toList $ gram
    let mkAuto = case compLevel of
            Auto -> DAWG.mkAuto
            Trie -> Trie.mkAuto
            List -> List.mkAuto
            SetAuto -> Set.mkAuto DAWG.mkAuto
            SetTrie -> Set.mkAuto Trie.mkAuto
    return (mkAuto ruleSet)


-- | Build automaton and print the individual edges.
printAuto :: BuildCfg -> FilePath -> IO ()
printAuto cfg gramPath = do
    auto <- buildAuto cfg gramPath
    mapM_ print (Mini.allEdges auto)


-- -- | Build automaton and print the individual edges.
-- buildTrie :: FilePath -> IO ()
-- buildTrie gramPath = do
--     -- extract the grammar
--     gram <- G.getTrees gramPath
--     -- build the automaton
--     ruleSet <- LS.compile . map O.decode . S.toList $ gram
--     let trie = Trie.buildTrie ruleSet
--     mapM_ print . Mini.allEdges $ Trie.shell trie
