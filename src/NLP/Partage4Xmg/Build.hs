{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


-- Build automaton.


module NLP.Partage4Xmg.Build
(
-- * Config
  BuildCfg (..)
, Compress (..)

-- * Build
, Auto
, buildAuto
, printAuto
-- ** Weighted
, WeiAuto
, buildWeiAuto
, printWeiAuto

-- * Temp
, printRules
, printWRules
) where


import           Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import qualified NLP.Partage.Tree.Other as O
import qualified NLP.Partage.DAG as DAG
-- import qualified NLP.Partage.FactGram.Weighted as W
-- import qualified NLP.Partage.SubtreeSharing as LS
import qualified NLP.Partage.Auto as Auto
import qualified NLP.Partage.Auto.DAWG as DAWG
import qualified NLP.Partage.Auto.Trie as Trie
import qualified NLP.Partage.Auto.WeiTrie as WeiTrie
import qualified NLP.Partage.Auto.List as List
import qualified NLP.Partage.Auto.Set  as Set

import qualified NLP.Partage4Xmg.Gen as G


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
    -- TODO: this is ignored now!
    } deriving (Show, Read, Eq, Ord)


--------------------------------------------------
-- Automaton Contruction
--------------------------------------------------


-- | Local automaton verion.
type Auto = Auto.GramAuto -- G.NonTerm G.Term
type DAG  = DAG.DAG (O.Node G.NonTerm G.Term) DAG.Weight
type Gram = DAG.Gram G.NonTerm G.Term


-- | Build automaton using the specified compression technique.
buildAuto
    :: BuildCfg
    -> FilePath         -- ^ Grammar
    -> Maybe FilePath   -- ^ Lexicon (if present)
    -> IO (DAG, Auto)
buildAuto BuildCfg{..} gramPath mayLexPath = do
    -- extract the grammar
--     gram <- G.getTrees gramPath mayLexPath
--     -- build the automaton
--     let compile = if shareTrees
--             then Gram.flattenWithSharing
--             else Gram.flattenNoSharing
--     ruleSet <- DAG.factGram . compile . map O.decode . S.toList $ gram
    gram <- buildGram gramPath mayLexPath
    -- ruleSet <- buildRules gramPath mayLexPath
    let fromGram = case compLevel of
            Auto -> DAWG.fromGram
            Trie -> Trie.fromGram
            List -> List.fromGram
            SetAuto -> Set.fromGram DAWG.fromGram
            SetTrie -> Set.fromGram Trie.fromGram
    return
      ( DAG.dagGram gram
      , fromGram . M.keysSet $ DAG.factGram gram )


-- | Build automaton and print the individual edges.
printAuto :: BuildCfg -> FilePath -> Maybe FilePath -> IO ()
printAuto cfg gramPath mayLexPath = do
    auto <- snd <$> buildAuto cfg gramPath mayLexPath
    mapM_ print (Auto.allEdges auto)
    putStrLn "\n# Maximum numbers of passive and active items per span #\n"
    putStr "#(PI): " >> print (numberPI auto)
    putStr "#(AI): " >> print (numberAI auto)


--------------------------------------------------
-- Weighted Automaton Contruction
--------------------------------------------------


-- | Local automaton verion.
type WeiAuto = Auto.WeiGramAuto G.NonTerm G.Term


-- | Build weighted automaton.
buildWeiAuto
    :: FilePath         -- ^ Grammar
    -> Maybe FilePath   -- ^ Lexicon (if present)
    -> IO WeiAuto
buildWeiAuto gramPath mayLexPath = do
    ruleSet <- buildWRules gramPath mayLexPath
    return (WeiTrie.fromGram ruleSet)


-- | Build automaton and print the individual edges.
printWeiAuto :: FilePath -> Maybe FilePath -> IO ()
printWeiAuto gramPath mayLexPath = do
    weiAuto <- buildWeiAuto gramPath mayLexPath
    let auto = Auto.fromWei weiAuto
    mapM_ print $
        [ (i, x, j, fst . fromJust $ Auto.followWei weiAuto i x)
        | (i, x, j) <- Auto.allEdges auto ]


--------------------------------------------------
-- Stats
--------------------------------------------------


-- | Maximum possible number of passive items per span.
numberPI :: Auto.GramAuto -> Int
numberPI auto = S.size $ S.fromList
    [x | (_, Auto.Head x, _) <- Auto.allEdges auto]


-- | Maximum possible number of active items per span.
numberAI :: Auto.GramAuto -> Int
numberAI auto = S.size $ S.fromList
    [i | (i, Auto.Body _, _) <- Auto.allEdges auto]


--------------------------------------------------
-- Provisional section: no weights
--------------------------------------------------


-- | Weighted rule, local type.
type Rule = DAG.Rule -- G.NonTerm G.Term


-- | Get weighted rules from the given grammar.
buildGram
    :: FilePath         -- ^ Grammar
    -> Maybe FilePath   -- ^ Lexicon (if present)
    -> IO Gram
buildGram gramPath mayLexPath = do
    -- extract the grammar
    DAG.mkGram . map (,0) . S.toList <$>
      G.getTrees gramPath mayLexPath


-- | First `buildRules` and then print them.
printRules
    :: FilePath         -- ^ Grammar
    -> Maybe FilePath   -- ^ Lexicon (if present)
    -> IO ()
printRules gramPath mayLexPath = do
    gram <- buildGram gramPath mayLexPath
    let dag = DAG.dagGram gram
        ruleSet = DAG.factGram gram
    -- mapM_ print $ M.toList (DAG.nodeMap dag)
    mapM_
     (\i -> print (i, DAG.label i dag, DAG.value i dag, DAG.edges i dag))
     (S.toList (DAG.nodeSet dag))
    putStrLn "============"
    mapM_ print (M.toList ruleSet)


--------------------------------------------------
-- Provisional section: weights
--------------------------------------------------


---- | Weighted rule, local type.
--type WRule = W.Rule G.NonTerm G.Term W.Weight


-- | Get weighted rules from the given grammar.
buildWRules
    :: FilePath         -- ^ Grammar
    -> Maybe FilePath   -- ^ Lexicon (if present)
    -> IO (M.Map Rule DAG.Weight)
buildWRules gramPath mayLexPath = do
    -- extract the grammar
    gram <- G.getTrees gramPath mayLexPath
    return
        . DAG.rulesMapFromDAG
        . DAG.dagFromWeightedForest
        . map (,1)
        . S.toList $ gram


-- | First `buildWRules` and then print them.
printWRules
    :: FilePath         -- ^ Grammar
    -> Maybe FilePath   -- ^ Lexicon (if present)
    -> IO ()
printWRules gramPath mayLexPath = do
    ruleMap <- buildWRules gramPath mayLexPath
    mapM_ print (M.toList ruleMap)
