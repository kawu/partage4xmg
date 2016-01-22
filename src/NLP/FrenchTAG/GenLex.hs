{-# LANGUAGE RecordWildCards #-}


-- Datatype prepared for generation when the lexicon is present.


module NLP.FrenchTAG.GenLex
(
-- * Types
  Tree
, Node
, NonTerm
, Term (..)

-- * Conversion
, anchor

-- * Generation
, generateFrom
-- ** Randomized
, genRandFrom

-- * Utils
, getTrees
, printTrees
) where


import           Control.Applicative ((<$>))
-- import           Control.Monad (foldM)
import qualified Control.Monad.State.Strict as E
-- import           Control.Monad.Morph (hoist)

import           Pipes
import qualified Pipes.Prelude as Pipes
import qualified Data.Tree as R
import qualified Data.Text.Lazy      as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import qualified NLP.Partage.Tree.Other as O
import qualified NLP.Partage.Gen as G

import qualified NLP.FrenchTAG.Parse as P
import qualified NLP.FrenchTAG.ParseLex as PL
import qualified NLP.FrenchTAG.Gen as F



-------------------------------------------------
-- Data types
-------------------------------------------------


-- | Terminal is just as in the original grammar.
type Term = L.Text


-- | Non-terminal is just as in the original grammar.
type NonTerm = L.Text


-- | Type of the node in TAG trees.
type Node = O.Node NonTerm Term


-- | The tree itself.
type Tree = R.Tree Node


-- | Show the given tree.
showTree :: Tree -> String
showTree = R.drawTree . fmap show


-------------------------------------------------
-- Tree conversion
-------------------------------------------------


-- | Anchor the given tree with the given terminal.
anchor :: Term -> F.Tree -> Tree
anchor a (R.Node n xs) = case n of
    O.NonTerm x -> R.Node (O.NonTerm x) (map (anchor a) xs)
    O.Foot x    -> R.Node (O.Foot x) []
    O.Term t    -> case t of
        F.Term x    -> R.Node (O.Term x) []
        F.Anchor _  -> R.Node (O.Term a) []


-- -- | Randomly substitute anchors with some corresponding
-- -- lexical entries.
-- terminate :: M.Map


-- -- | Anchor the given tree with the given terminal.
-- anchor :: Term -> G.Tree -> Tree
-- anchor = (R.Node P.NonTerm{..} xs) =
--     case typ of
--         P.Std       -> below $ O.NonTerm sym
--         P.Foot      -> leaf  $ O.Foot sym
--         P.Anchor    -> R.Node (O.NonTerm sym)
--             [leaf . O.Term $ Anchor sym]
--         P.Lex       -> leaf  . O.Term $ Term sym
--         P.Other _   -> below $ O.NonTerm sym
--   where
--     below x = R.Node x $ map convert xs
--     leaf x  = R.Node x []


-------------------------------------------------
-- Grammar extraction
-------------------------------------------------


-- | Get the set of TAG trees given the grammar file and the lexicon
-- file.
getTrees
    :: FilePath     -- ^ Grammar
    -> FilePath     -- ^ Lexicon
    -> IO (S.Set Tree)
getTrees gramPath lexPath = do
    treeMap <- F.getTreeMap gramPath
    lemmas  <- PL.readLexicon lexPath
    let treeList =
          [ anchor (PL.name lemma) tree
          | lemma  <- lemmas
          , family <- S.toList $ PL.treeFams lemma
          , tree   <- maybe [] S.toList (M.lookup family treeMap) ]
    flip E.execStateT S.empty $ E.forM_ treeList $ \tree -> do
        length (showTree tree) `seq`
            E.modify (S.insert tree)


-------------------------------------------------
-- Generation
-------------------------------------------------


-- | Generate size-bounded derived trees based on
-- the grammar under the path.
-- Only final trees are shown.
generateFrom
    :: FilePath     -- ^ Grammar
    -> FilePath     -- ^ Lexicon
    -> Int
    -> IO ()
generateFrom gramPath lexPath sizeMax = do
    gram <- getTrees gramPath lexPath
    let pipe = G.generateAll gram sizeMax
           >-> Pipes.filter O.isFinal
           >-> Pipes.map O.project
    runEffect . for pipe $ liftIO . print


-------------------------------------------------
-- Random generation
-------------------------------------------------


-- | Randomly generate derived sentences.
genRandFrom
    :: F.GenConf
    -> FilePath     -- ^ Grammar
    -> FilePath     -- ^ Lexicon
    -> IO ()
genRandFrom F.GenConf{..} gramPath lexPath = do
    -- extract the grammar
    gram <- getTrees gramPath lexPath
    -- sentence generation pipe
    let conf = G.GenConf
            { genAllSize = maxSize
            , adjProb    = adjProb }
        pipe = G.generateRand gram conf
            >-> Pipes.filter O.isFinal
            >-> Pipes.map O.project
    -- print (conf, treeNum)
    let runPipe = runEffect . for
            (pipe >-> Pipes.take treeNum >-> F.rmDups)
    runPipe $ liftIO . print
