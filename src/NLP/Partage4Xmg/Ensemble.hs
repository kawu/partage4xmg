{-# LANGUAGE RecordWildCards   #-}


module NLP.Partage4Xmg.Ensemble
(
-- * Grammar
  Grammar
, Tree
, getInterps
, getTrees

-- * Reading
, GramCfg(..)
, readGrammar
) where


import qualified Control.Monad.State.Strict as E

import           Data.Maybe                 (maybeToList)
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as L
import qualified Data.Tree                  as R
-- import qualified Data.Traversable           as Trav

import qualified NLP.Partage.DAG            as D
import qualified NLP.Partage.Earley         as Earley
import qualified NLP.Partage.Tree.Other     as O
import qualified NLP.Partage.Env            as Env
import qualified NLP.Partage.FS             as FS
import qualified NLP.Partage.FSTree         as FSTree

import qualified NLP.Partage4Xmg.Lexicon    as Lex
import qualified NLP.Partage4Xmg.Morph      as Morph
import qualified NLP.Partage4Xmg.Grammar    as G


--------------------------------------------------
-- The grammar, lemmas and morphology combined
--------------------------------------------------


-- | All the components of a grammar.
data Grammar = Grammar
  { morphMap :: M.Map L.Text (S.Set Morph.LemmaRef)
  , lexMap   :: M.Map Morph.LemmaRef (S.Set G.Family)
  , treeMap  :: M.Map G.Family (S.Set G.Tree)
  }


-- | Map a given word to the set of its possible interpretations.
getInterps :: Grammar -> L.Text -> S.Set Morph.LemmaRef
getInterps Grammar{..} orth = case M.lookup orth morphMap of
  Nothing -> S.empty
  Just x  -> x


-- | Retrieve the set of grammar trees related to te given interpretation.
_getTrees :: Grammar -> Morph.LemmaRef -> S.Set G.Tree
_getTrees Grammar{..} lemmaRef = S.unions
  [ treeSet
  | famSet <- maybeToList $ M.lookup lemmaRef lexMap
  , family <- S.toList famSet
  , treeSet <- maybeToList $ M.lookup family treeMap
  ]


-- | Retrieve the set of grammar trees related to te given interpretation.
getTrees :: Grammar -> Morph.LemmaRef -> S.Set (Tree T.Text)
getTrees gram lemmaRef
  = S.fromList
  . map simplify
  . map (anchor . L.toStrict $ Morph.name lemmaRef)
  . map convert
  . S.toList
  $ _getTrees gram lemmaRef


--------------------------------------------------
-- Config
--------------------------------------------------


-- | Grammar configuration.
data GramCfg = GramCfg
  { morphPath :: FilePath
  , lexPath   :: FilePath
  , treePath  :: FilePath
  } deriving (Show, Read, Eq, Ord)


-- | Read the grammar given the configuration.
readGrammar :: GramCfg -> IO Grammar
readGrammar GramCfg{..} = do
  xs <- Morph.readMorph morphPath
  ys <- Lex.readLexicon lexPath
  zs <- G.readGrammar treePath
  return Grammar
    { morphMap = M.fromList
      [ (Morph.wordform x, Morph.analyzes x)
      | x <- xs ]
    , lexMap = M.fromList
      [ (Morph.LemmaRef {name = name, cat = cat}, treeFams)
      | Lex.Lemma{..} <- ys ]
    , treeMap = M.fromListWith S.union
      [ (famName, S.singleton tree)
      | (famName, tree) <- zs ]
    }


--------------------------------------------------
-- Local grammar type
--------------------------------------------------


-- | Terminal is either a regular terminal or an anchor.
data ATerm
    = Term T.Text
    | Anchor T.Text
    deriving (Show, Read, Eq, Ord)


-- | Non-terminal is just as in the original grammar.
type NonTerm = T.Text


-- | A simple terminal.
type Term = T.Text


-- | Type of the node in TAG trees.
type Node t = O.Node NonTerm t


-- | The tree itself.
type Tree t = R.Tree (Node t)


-------------------------------------------------
-- Tree conversion
-------------------------------------------------


-- | Remove information about the past anchors.  Fail if there
-- are some anchors left.
simplify :: Tree ATerm -> Tree Term
simplify = fmap $ \node -> case node of
  O.Term (Term x) -> O.Term x
  O.Term (Anchor x) -> error "simlify: cannot simplify, anchors left"
  O.NonTerm x -> O.NonTerm x
  O.Foot x -> O.Foot x


-- | Convert the parsed tree to the required form.
convert :: G.Tree -> Tree ATerm
convert (R.Node G.NonTerm{..} xs) =
  case typ of
    G.Std       -> below $ O.NonTerm sym'
    G.Foot      -> leaf  $ O.Foot sym'
    G.Anchor    -> R.Node (O.NonTerm sym')
      [leaf . O.Term $ Anchor sym']
    G.Lex       -> leaf  . O.Term $ Term sym'
    G.Other _   -> below $ O.NonTerm sym'
  where
    below x = R.Node x $ map convert xs
    leaf x  = R.Node x []
    sym' = L.toStrict sym


-- | Anchor the given tree with the given terminal.
anchor :: T.Text -> Tree ATerm -> Tree ATerm
anchor a (R.Node n xs) = case n of
  O.NonTerm x -> R.Node (O.NonTerm x) (map (anchor a) xs)
  O.Foot x    -> R.Node (O.Foot x) []
  O.Term t    -> case t of
    Term _   -> R.Node (O.Term t) []
    Anchor _ -> R.Node (O.Term (Term a)) []



-------------------------------------------------
-- FS-aware tree conversion
-------------------------------------------------


-- | FS key.
type Key = T.Text


-- | FS value.
type Val = T.Text


-- | FS-aware tree.
type FSTree t = FSTree.FSTree NonTerm t Key Val


-- | A mapping from XMG variables to local variables.
type VarMap = M.Map G.Var Env.Var


-- | Convert the parsed tree to the required form.
convertFS :: G.Tree -> E.StateT VarMap (Env.EnvM Val) (FSTree ATerm)
convertFS (R.Node G.NonTerm{..} xs) =
  case typ of
    G.Std -> below $ O.NonTerm sym'
    G.Foot -> leaf $ O.Foot sym'
-- TODO: FS assigned to the anchor should unify with the one assigned
-- to the terminal in the morphology file.
--     G.Anchor -> do
--       theLeaf <- leaf . O.Term $ Anchor sym'
--       return $ R.Node (O.NonTerm (sym', M.empty)) [theLeaf]
    G.Lex -> leaf . O.Term $ Term sym'
--     G.Other _ -> below $ O.NonTerm sym'
  where
    below x = do
      fs <- convertAVM avm
      R.Node (x, fs) <$> mapM convertFS xs
    leaf x = do
      fs <- convertAVM avm
      return $ R.Node (x, fs) []
    sym' = L.toStrict sym


convertAVM :: G.AVM -> E.StateT VarMap (Env.EnvM Val) (FS.FS Key Val)
convertAVM = undefined
