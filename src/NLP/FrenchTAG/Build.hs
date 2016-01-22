{-# LANGUAGE RecordWildCards #-}


-- Build automaton.


module NLP.FrenchTAG.Build
(
-- * Config
  BuildCfg (..)
, Compress (..)

-- * Build
, Auto
, buildAuto
, printAuto
-- ** Lexicalized
, LexAuto
, buildLexAuto
, printLexAuto
) where


import qualified Data.Set as S

import qualified NLP.Partage.Tree.Other as O
import qualified NLP.Partage.FactGram as Gram
-- import qualified NLP.Partage.SubtreeSharing as LS
import qualified NLP.Partage.Auto as Auto
import qualified NLP.Partage.Auto.DAWG as DAWG
import qualified NLP.Partage.Auto.Trie as Trie
import qualified NLP.Partage.Auto.List as List
import qualified NLP.Partage.Auto.Set  as Set

import qualified NLP.FrenchTAG.Gen as G
import qualified NLP.FrenchTAG.GenLex as L


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
type Auto = Auto.GramAuto G.NonTerm G.Term


-- | Build automaton using the specified compression technique.
buildAuto :: BuildCfg -> FilePath -> IO Auto
buildAuto BuildCfg{..} gramPath = do
    -- extract the grammar
    gram <- G.getTrees gramPath

    -- build the automaton
    let compile = if shareTrees
            then Gram.flattenWithSharing
            else Gram.flattenNoSharing
    ruleSet <- compile . map O.decode . S.toList $ gram
    let fromGram = case compLevel of
            Auto -> DAWG.fromGram
            Trie -> Trie.fromGram
            List -> List.fromGram
            SetAuto -> Set.fromGram DAWG.fromGram
            SetTrie -> Set.fromGram Trie.fromGram
    return (fromGram ruleSet)


-- | Build automaton and print the individual edges.
printAuto :: BuildCfg -> FilePath -> IO ()
printAuto cfg gramPath = do
    auto <- buildAuto cfg gramPath
    mapM_ print (Auto.allEdges auto)
    putStrLn "\n# Maximum numbers of passive and active items per span #\n"
    putStr "#(PI): " >> print (numberPI auto)
    putStr "#(AI): " >> print (numberAI auto)


--------------------------------------------------
-- Contruction -- Lexicalized Version
--------------------------------------------------


-- | Local, lexicalized automaton verion.
type LexAuto = Auto.GramAuto L.NonTerm L.Term


-- | Build automaton using the specified compression technique.
--
-- TODO: a large part common with `buildAuto`!
buildLexAuto
    :: BuildCfg
    -> FilePath     -- ^ Grammar
    -> FilePath     -- ^ Lexicon
    -> IO LexAuto
buildLexAuto BuildCfg{..} gramPath lexPath = do
    -- extract the grammar
    gram <- L.getTrees gramPath lexPath

    -- build the automaton
    let compile = if shareTrees
            then Gram.flattenWithSharing
            else Gram.flattenNoSharing
    ruleSet <- compile . map O.decode . S.toList $ gram
    let fromGram = case compLevel of
            Auto -> DAWG.fromGram
            Trie -> Trie.fromGram
            List -> List.fromGram
            SetAuto -> Set.fromGram DAWG.fromGram
            SetTrie -> Set.fromGram Trie.fromGram
    return (fromGram ruleSet)


-- | Build automaton and print the individual edges.
printLexAuto :: BuildCfg -> FilePath -> FilePath -> IO ()
printLexAuto cfg gramPath lexPath = do
    auto <- buildLexAuto cfg gramPath lexPath
    mapM_ print (Auto.allEdges auto)
    putStrLn "\n# Maximum numbers of passive and active items per span #\n"
    putStr "#(PI): " >> print (numberPI auto)
    putStr "#(AI): " >> print (numberAI auto)


--------------------------------------------------
-- Stats
--------------------------------------------------


-- | Maximum possible number of passive items per span.
numberPI :: (Ord a, Ord b) => Auto.GramAuto a b -> Int
numberPI auto = S.size $ S.fromList
    [x | (_, Auto.Head x, _) <- Auto.allEdges auto]


-- | Maximum possible number of active items per span.
numberAI :: (Ord a, Ord b) => Auto.GramAuto a b -> Int
numberAI auto = S.size $ S.fromList
    [i | (i, Auto.Body _, _) <- Auto.allEdges auto]
