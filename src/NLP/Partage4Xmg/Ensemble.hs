{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections   #-}


-- | A version of with support for simple feature structures, with no top/bottom
-- distinction.


module NLP.Partage4Xmg.Ensemble
(
-- * Grammar
  Grammar
, getInterps
, getTreesFS
, splitFSTree

-- * Types
, NonTerm
, Term
, Node
, Tree
-- ** FS-related
, Key
, Val
, OFS
, CFS
, FSTree

-- * Reading
, GramCfg(..)
, readGrammar

-- -- * Utils
-- , closeAVM
) where


import           Control.Applicative        ((<|>))
import           Control.Arrow              (first, second)
import qualified Control.Monad.State.Strict as E

import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy             as L
import qualified Data.Tree                  as R
import qualified Pipes                      as P
import qualified Pipes.Prelude              as P

import qualified NLP.Partage.Tree.Other     as O
import qualified NLP.Partage.Tree.Comp      as C
import qualified NLP.Partage.Env            as Env
import qualified NLP.Partage.FS             as FS
import qualified NLP.Partage.FSTree         as FSTree

import qualified NLP.Partage4Xmg.Lexicon    as Lex
import qualified NLP.Partage4Xmg.Morph      as Morph
import qualified NLP.Partage4Xmg.Grammar    as G
import qualified NLP.Partage4Xmg.Ensemble.Common as EC
import           NLP.Partage4Xmg.Ensemble.Common hiding
  (FSTree, OFS, CFS)

-- import System.IO.Unsafe (unsafePerformIO)
-- import Debug.Trace (trace)


-------------------------------------------------
-- FS-related types
-------------------------------------------------


-- | FS-aware tree.
type FSTree t = EC.FSTree t Key


-- | An open FS.
type OFS = FS.OFS Key


-- | A closed FS.
type CFS = FS.CFS Key Val


--------------------------------------------------
-- FS-related stuff
--------------------------------------------------


-- | Retrieve the FS-aware grammar ETs related to te given interpretation.
getTreesFS
  :: Grammar G.AVM
  -> Term
     -- ^ A wordform
  -> Morph.Ana
     -- ^ The analysis corresponding the wordform
  -- -> [(Tree Term, C.Comp CFS)]
  -> [Env.EnvM Val (FSTree Term)]
getTreesFS = EC.getTrees EC.Simple
-- getTreesFS gram term ana
--   -- = mapMaybe process
--   = map process
--   . S.toList
--   $ _getTrees gram ana
--   where
--     -- process tree = splitFSTree . fmap simplifyFS $ do
--     process tree = fmap EC.simplify $ do
--       converted <- withVarMap (convertFS tree)
--       let fs = closeAVM $ Morph.avm ana
--       EC.anchor term fs converted
-- 
-- 
-- -- | Convert and close the given XMG AVM.
-- closeAVM :: G.AVM -> CFS
-- closeAVM avm = maybe M.empty id . fst . Env.runEnvM $ do
--   fs <- withVarMap (convertAVM avm)
--   FS.close fs


-------------------------------------------------
-- FS-aware tree conversion
-------------------------------------------------


-- | Convert the parsed tree to the required form.
convertFS :: G.Tree G.AVM -> E.StateT VarMap (Env.EnvM Val) (FSTree ATerm)
convertFS = EC.convert convertAVM


-- | Convert an XMG-style AVM to a FS.
convertAVM :: G.AVM -> E.StateT VarMap (Env.EnvM Val) OFS
convertAVM avm = fmap M.fromList . runListT $ do
  (key, valVar) <- each (M.toList avm)
  case valVar of
    Left val -> P.lift . P.lift $ do
      var <- Env.var
      Env.set var val
      return (key, var)
    Right var0 -> do
      var <- P.lift $ varFor var0
      return (key, var)


-- | Retrieve the corresponding environment variable. Create a new one if the
-- XMG variable was not seen previously.
varFor :: G.Var -> E.StateT VarMap (Env.EnvM Val) Env.Var
varFor gramVar = do
  varMay <- E.gets $ M.lookup gramVar
  case varMay of
    Just var -> return var
    Nothing  -> do
      var <- P.lift Env.var
      E.modify' $ M.insert gramVar var
      return var


-- | Extract the tree embedded in the environment and the accompanying computation.
splitFSTree
  :: Env.EnvM Val (FSTree Term)
  -> Maybe (Tree Term, C.Comp CFS)
splitFSTree = EC.splitTree


-------------------------------------------------
-- Utils
-------------------------------------------------


-- | ListT from a list.
each :: Monad m => [a] -> P.ListT m a
each = P.Select . P.each


-- | Run a ListT computation (unidiomatic Haskell?).
runListT :: (Monad m) => P.ListT m a -> m [a]
runListT = P.toListM . P.enumerate
