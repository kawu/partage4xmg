{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


-- | Building grammar automaton.


module NLP.Partage4Xmg.Build
(
-- * Types
  Term
, NonTerm

-- * Build
, BuildData (..)
, Auto
, buildAuto
, printAuto

-- * Temp
, printRules
, getTrees
, getTreeMap
, printTrees
) where


import qualified Control.Monad.State.Strict as E

import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Tree as R

import qualified NLP.Partage.Tree.Other as O
import qualified NLP.Partage.Tree.Comp as C
import qualified NLP.Partage.DAG as DAG
import qualified NLP.Partage.Auto as Auto
import qualified NLP.Partage.Auto.Trie as Trie

import qualified NLP.Partage4Xmg.Grammar as P
import qualified NLP.Partage4Xmg.Lexicon as Lex


--------------------------------------------------
-- Configuration
--------------------------------------------------


-- | Building data sources.
data BuildData = BuildData
    { gramPath   :: FilePath
    -- ^ Grammar with anchors
    , lexPath    :: FilePath
    -- ^ Lexicon defined for the grammar
    , mayAuxPath :: Maybe FilePath
    -- ^ Auxiliary grammar/lexicon file, no anchors
    } deriving (Show, Read, Eq, Ord)


--------------------------------------------------
-- Types
--------------------------------------------------


-- | Terminal in the grammar is either a normal terminal or a node
-- attached to a non-terminal anchor.
data Term
    = Term T.Text
    | Anchor T.Text
    deriving (Show, Read, Eq, Ord)


-- | Non-terminal is just as in the original grammar.
type NonTerm = T.Text


-- | Type of the node in TAG trees.
type Node = O.Node NonTerm Term


-- | The tree itself.
type Tree = R.Tree Node


-- | Show the given tree.
showTree :: Tree -> String
showTree = R.drawTree . fmap show


-- | Local automaton verion.
type Auto = Auto.GramAuto
type DAG  = DAG.DAG Node (C.Comp ())
type Gram = DAG.Gram NonTerm Term (C.Comp ())


--------------------------------------------------
-- Automaton Contruction
--------------------------------------------------


-- | Build automaton based on input data.
buildAuto :: BuildData -> IO (DAG, Auto)
buildAuto buildData = do
    gram <- buildGram buildData
    let dag = DAG.dagGram gram
        trie = Trie.fromGram (DAG.factGram gram)
    return (dag, trie)


-- | Build automaton and print the individual edges.
printAuto :: BuildData -> IO ()
printAuto buildData = do
    auto <- snd <$> buildAuto buildData
    mapM_ print (Auto.allEdges auto)
    putStrLn "\n# Maximum numbers of passive and active items per span #\n"
    putStr "#(PI): " >> print (numberPI auto)
    putStr "#(AI): " >> print (numberAI auto)


--------------------------------------------------
-- Stats
--------------------------------------------------


-- | Maximum possible number of passive items per span.
numberPI :: Auto -> Int
numberPI auto = S.size $ S.fromList
    [x | (_, Auto.Head x, _) <- Auto.allEdges auto]


-- | Maximum possible number of active items per span.
numberAI :: Auto -> Int
numberAI auto = S.size $ S.fromList
    [i | (i, Auto.Body _, _) <- Auto.allEdges auto]


--------------------------------------------------
-- Provisional section: no weights
--------------------------------------------------


-- -- | Weighted rule, local type.
-- type Rule = DAG.Rule -- G.NonTerm G.Term


-- | Get trees for the given data sources.
getTrees :: BuildData -> IO (S.Set Tree)
getTrees BuildData{..} = do
  -- extract the trees
  ts  <- getLexTrees gramPath lexPath
  ts' <- case mayAuxPath of
    Nothing -> return S.empty
    Just auxPath -> getAncTrees auxPath
  return $ ts `S.union` ts'


-- | Get rules from the given grammar.
buildGram :: BuildData -> IO Gram
buildGram buildData = do
  ts <- getTrees buildData
  let dummy = C.Comp (const $ Just ()) C.dummyTopDown
  return . DAG.mkGram . map (,dummy) . S.toList $ ts


-- | First `buildRules` and then print them.
printRules :: BuildData -> IO ()
printRules buildData = do
    gram <- buildGram buildData
    let dag = DAG.dagGram gram
        ruleSet = DAG.factGram gram
    -- mapM_ print $ M.toList (DAG.nodeMap dag)
    mapM_
     (\i -> print (i, DAG.label i dag, DAG.edges i dag))
     (S.toList (DAG.nodeSet dag))
    putStrLn "============"
    mapM_ print (S.toList ruleSet)


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
-- Grammar extraction
--------------------------------------------------


-- -- | `getAncTrees` or `getLexTrees`, depending on arguments.
-- getTrees
--     :: FilePath         -- ^ Grammar
--     -> Maybe FilePath   -- ^ Lexicon (if present)
--     -> IO (S.Set Tree)
-- getTrees gramPath Nothing =
--     getAncTrees gramPath
-- getTrees gramPath (Just lexPath) =
--     getLexTrees gramPath lexPath


-- | Get the set of lexicalized TAG trees given the grammar file and
-- the lexicon file.
getLexTrees
    :: FilePath     -- ^ Grammar
    -> FilePath     -- ^ Lexicon
    -> IO (S.Set Tree)
getLexTrees gramPath lexPath = do
    treeMap <- getTreeMap gramPath
    words <- Lex.readLexicon lexPath
    let treeList =
          [ anchor (L.toStrict $ Lex.lemma word) tree
          | (word, famSet) <- words
          , family <- S.toList famSet
          , tree   <- maybe [] S.toList (M.lookup family treeMap) ]
    flip E.execStateT S.empty $ E.forM_ treeList $ \tree -> do
        length (showTree tree) `seq`
            E.modify (S.insert tree)


-- | Get the set of TAG trees with anchors. Useful when lexicon is not there.
getAncTrees
    :: FilePath         -- ^ Grammar
    -> IO (S.Set Tree)
getAncTrees path = do
    ts <- P.readGrammar False path
    flip E.execStateT S.empty $ E.forM_ ts $ \(_family, tree) -> do
        let tree' = convert tree
        -- Rather nasty trick, but works.  Otherwise the tree is
        -- not constructed at this precise moment.  Find a better
        -- solution.
        length (showTree tree') `seq`
            E.modify (S.insert tree')


-- | Get the set of TAG trees.
getTreeMap :: FilePath -> IO (M.Map P.Family (S.Set Tree))
getTreeMap path = do
    ts <- P.readGrammar False path
    flip E.execStateT M.empty $ E.forM_ ts $ \(family, tree) -> do
        let tree' = convert tree
        -- Rather nasty trick, but works.  Otherwise the tree is
        -- not constructed at this precise moment.  Find a better
        -- solution.
        length (showTree tree') `seq`
            E.modify (M.insertWith S.union family
                (S.singleton tree'))


-------------------------------------------------
-- Tree conversion
-------------------------------------------------


-- | Convert the parsed tree to the required form.
convert :: P.Tree -> Tree
convert (R.Node P.NonTerm{..} xs) =
  case typ of
    P.Std       -> below $ O.NonTerm sym'
    P.Foot      -> leaf  $ O.Foot sym'
    P.Anchor    -> R.Node (O.NonTerm sym')
      [leaf . O.Term $ Anchor sym']
    P.Lex       -> leaf  . O.Term $ Term sym'
    P.Other _   -> below $ O.NonTerm sym'
  where
    below x = R.Node x $ map convert xs
    leaf x  = R.Node x []
    sym' = L.toStrict sym


-- | Anchor the given tree with the given terminal.
anchor :: T.Text -> Tree -> Tree
anchor a (R.Node n xs) = case n of
  O.NonTerm x -> R.Node (O.NonTerm x) (map (anchor a) xs)
  O.Foot x    -> R.Node (O.Foot x) []
  O.Term t    -> case t of
    Term _   -> R.Node (O.Term t) []
    Anchor _ -> R.Node (O.Term (Term a)) []
