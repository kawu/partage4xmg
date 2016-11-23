{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections   #-}


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


import           Control.Arrow              (first, second)
import qualified Control.Monad.State.Strict as E

import           Data.Maybe                 (maybeToList)
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as L
import qualified Data.Tree                  as R
-- import qualified Data.Traversable           as Trav
import qualified Pipes                      as P
import qualified Pipes.Prelude              as P


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
simplify = fmap simplifyNode


-- | Remove information about the anchors.  Fail if there are still
-- some anchors left.
simplifyNode :: O.Node NonTerm ATerm -> O.Node NonTerm Term
simplifyNode node = case node of
  O.Term (Term x) -> O.Term x
  O.Term (Anchor x) -> error "simlifyNode: cannot simplify, anchors left"
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
anchor :: Term -> Tree ATerm -> Tree ATerm
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


-- | Remove information about the past anchors.  Fail if there
-- are some anchors left.
simplifyFS :: FSTree ATerm -> FSTree Term
simplifyFS = fmap $ first simplifyNode


-- | Convert the parsed tree to the required form.
convertFS :: G.Tree -> E.StateT VarMap (Env.EnvM Val) (FSTree ATerm)
convertFS (R.Node G.NonTerm{..} xs) =
  case typ of
    G.Std -> below $ O.NonTerm sym'
    G.Foot -> leaf $ O.Foot sym'
    -- TODO: FS assigned to the anchor should unify with the one assigned
    -- to the terminal in the morphology file.
    G.Anchor -> do
      fs <- convertAVM avm
      theLeaf <- leaf . O.Term $ Anchor sym'
      return $ R.Node (O.NonTerm sym', fs) [theLeaf]
    G.Lex -> leaf . O.Term $ Term sym'
    G.Other _ -> below $ O.NonTerm sym'
  where
    below x = do
      fs <- convertAVM avm
      R.Node (x, fs) <$> mapM convertFS xs
    leaf x = do
      fs <- convertAVM avm
      return $ R.Node (x, fs) []
    sym' = L.toStrict sym


-- | Convert an XMG-style AVM to a FS.
convertAVM :: G.AVM -> E.StateT VarMap (Env.EnvM Val) (FS.FS Key Val)
convertAVM avm = fmap M.fromList . runListT $ do
  (key, valVar) <-
    first L.toStrict .
    second (onLeft L.toStrict)
    <$> each (M.toList avm)
  case valVar of
    Left val -> return (key, FS.Val . S.singleton $ val)
    Right var0 -> do
      var <- P.lift $ varFor var0
      return (key, FS.Var var)


-- | Retrieve the corresponding environment variable.
varFor :: G.Var -> E.StateT VarMap (Env.EnvM Val) Env.Var
varFor gramVar = do
  varMay <- E.gets $ M.lookup gramVar
  case varMay of
    Just var -> return var
    Nothing  -> P.lift Env.var


-- | Anchor the given tree with the given terminal and its accompanying FS.
anchorFS
  :: Term                -- ^ Terminal used to replace the anchor
  -> FS.ClosedFS Key Val -- ^ The accompanying FS
  -> FSTree ATerm        -- ^ `FSTree` with an anchor
  -> Env.EnvM Val (FSTree ATerm)
anchorFS anchor newFS (R.Node label@(typ, oldFS) xs) = case typ of
  O.NonTerm x -> R.Node label <$> mapM (anchorFS anchor newFS) xs
  O.Foot x -> return $ R.Node label []
  O.Term t -> case t of
    Term _ -> return $ R.Node label []
    Anchor _ -> do
      newFS' <- FS.reopen newFS
      fs <- FS.unifyFS oldFS newFS'
      return $ R.Node (O.Term (Term anchor), fs) []


-------------------------------------------------
-- Utils
-------------------------------------------------


-- | ListT from a list.
each :: Monad m => [a] -> P.ListT m a
each = P.Select . P.each


-- | Run a ListT computation (unidiomatic Haskell?).
runListT :: (Monad m) => P.ListT m a -> m [a]
runListT = P.toListM . P.enumerate


-- | Map a function on either value.
onEither :: (a -> b) -> Either a a -> Either b b
onEither f (Left x) = Left (f x)
onEither f (Right x) = Right (f x)


-- | Map a function on left value.
onLeft :: (a -> c) -> Either a b -> Either c b
onLeft f (Left x) = Left (f x)
onLeft _ (Right x) = Right x
