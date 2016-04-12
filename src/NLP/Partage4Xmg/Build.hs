{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


-- Build automaton.


module NLP.Partage4Xmg.Build
(
-- * Config
  BuildCfg (..)
, Compress (..)

-- * Build
, BuildData (..)
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
, getTrees
, printTrees
) where


import           Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Tree as R

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


-- | Building data sources.
data BuildData = BuildData
    { gramPath     :: FilePath
    -- ^ Grammar with anchors
    , mayLexPath   :: Maybe FilePath
    -- ^ Lexicon (if present) defined w.r.t. the grammar
    , mayAuxPath   :: Maybe FilePath
    -- ^ Auxiliary grammar/lexicon file, no anchors
    } deriving (Show, Read, Eq, Ord)




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
-- Automaton Contruction
--------------------------------------------------


-- | Local automaton verion.
type Auto = Auto.GramAuto -- G.NonTerm G.Term
type DAG  = DAG.DAG (O.Node G.NonTerm G.Term) DAG.Weight
type Gram = DAG.Gram G.NonTerm G.Term


-- | Build automaton using the specified compression technique.
buildAuto
    :: BuildCfg
    -> BuildData
    -> IO (DAG, Auto)
buildAuto BuildCfg{..} buildData = do
    -- extract the grammar
    gram <- buildGram shareTrees buildData
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
printAuto :: BuildCfg -> BuildData -> IO ()
printAuto cfg buildData = do
    auto <- snd <$> buildAuto cfg buildData
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
    -- :: FilePath         -- ^ Grammar
    -- -> Maybe FilePath   -- ^ Lexicon (if present)
    :: BuildData
    -> IO WeiAuto
buildWeiAuto buildData = do
    ruleSet <- buildWRules buildData
    return (WeiTrie.fromGram ruleSet)


-- | Build automaton and print the individual edges.
printWeiAuto :: BuildData -> IO ()
printWeiAuto buildData = do
    weiAuto <- buildWeiAuto buildData
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


-- | Get trees for the given data sources.
getTrees :: BuildData -> IO (S.Set G.Tree)
getTrees BuildData{..} = do
  -- extract the trees
  ts  <- G.getTrees gramPath mayLexPath
  ts' <- case mayAuxPath of
    Nothing -> return S.empty
    Just auxPath -> G.getTrees auxPath Nothing
  return $ ts `S.union` ts'


-- | Get weighted rules from the given grammar.
buildGram
    :: Bool       -- ^ Share trees
    -> BuildData
    -> IO Gram
buildGram shareTrees buildData = do
  ts  <- getTrees buildData
  let mkGram = if shareTrees then DAG.mkGram else DAG.mkDummy
  return . mkGram . map (,0) . S.toList $ ts


-- | First `buildRules` and then print them.
printRules
    -- :: FilePath         -- ^ Grammar
    -- -> Maybe FilePath   -- ^ Lexicon (if present)
    :: BuildData
    -> IO ()
printRules buildData = do
    gram <- buildGram True buildData
    let dag = DAG.dagGram gram
        ruleSet = DAG.factGram gram
    -- mapM_ print $ M.toList (DAG.nodeMap dag)
    mapM_
     (\i -> print (i, DAG.label i dag, DAG.value i dag, DAG.edges i dag))
     (S.toList (DAG.nodeSet dag))
    putStrLn "============"
    mapM_ print (M.toList ruleSet)


-- | Print grammar trees after removing FSs.
printTrees :: BuildData -> IO ()
printTrees buildData = do
   ts <- getTrees buildData
   mapM_
    (putStrLn . R.drawTree . fmap show)
    (S.toList ts)
   putStrLn ""
   putStrLn "TREE NUM: " >> print (S.size ts)


--------------------------------------------------
-- Provisional section: weights
--------------------------------------------------


---- | Weighted rule, local type.
--type WRule = W.Rule G.NonTerm G.Term W.Weight


-- | Get weighted rules from the given grammar.
buildWRules
    -- :: FilePath         -- ^ Grammar
    -- -> Maybe FilePath   -- ^ Lexicon (if present)
    :: BuildData
    -> IO (M.Map Rule DAG.Weight)
buildWRules buildData = do
  ts <- getTrees buildData
  return
      . DAG.rulesMapFromDAG
      . DAG.dagFromWeightedForest
      . map (,1)
      . S.toList $ ts


-- | First `buildWRules` and then print them.
printWRules
    -- :: FilePath         -- ^ Grammar
    -- -> Maybe FilePath   -- ^ Lexicon (if present)
    :: BuildData
    -> IO ()
printWRules buildData = do
    ruleMap <- buildWRules buildData
    mapM_ print (M.toList ruleMap)
