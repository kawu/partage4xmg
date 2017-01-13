{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


-- | A version of `NLP.Partage4Xmg.Ensemble` with support for dual (top/bottom)
-- feature structures.


module NLP.Partage4Xmg.Ensemble.TopBot
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
-- import qualified NLP.Partage.FSTree         as FSTree
import qualified NLP.Partage.FSTree2         as FSTree

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
type FSTree t = EC.FSTree t (FSTree.Loc Key) -- Val


-- | An open FS.
type OFS = FS.OFS (FSTree.Loc Key)


-- | A closed FS.
type CFS = FS.CFS (FSTree.Loc Key) Val


--------------------------------------------------
-- FS-related stuff
--------------------------------------------------


-- | Retrieve the FS-aware grammar ETs related to te given interpretation.
getTreesFS
  :: Grammar G.AVM2
  -> Term
     -- ^ A wordform
  -> Morph.Ana
     -- ^ The analysis corresponding the wordform
  -- -> [(Tree Term, C.Comp CFS)]
  -> [Env.EnvM Val (FSTree Term)]
getTreesFS = EC.getTrees EC.TopBot
-- getTreesFS gram term ana
--   -- = mapMaybe process
--   = map process
--   . S.toList
--   $ _getTrees gram ana
--   where
--     -- process tree = splitFSTree . fmap simplifyFS $ do
--     process tree = fmap simplifyFS $ do
--       -- let term = Morph.lemma ana
--       converted <- withVarMap (convertFS tree)
--       -- below, we make it bottom only because in the corresponding
--       -- anchor nodes features are assigned to bottom FS only too.
--       let fs = closeAVM . G.botOnly $ Morph.avm ana
--       anchorFS term fs converted
-- 
-- 
-- -- | Convert and close the given XMG AVM.
-- closeAVM :: G.AVM2 -> CFS
-- closeAVM avm = maybe M.empty id . fst . Env.runEnvM $ do
--   fs <- withVarMap (convertAVM avm)
--   FS.close fs


-- -------------------------------------------------
-- -- Tree conversion
-- -------------------------------------------------
--
--
-- -- | Remove information about the past anchors.  Fail if there
-- -- are some anchors left.
-- simplify :: Tree ATerm -> Tree Term
-- simplify = fmap simplifyNode
--
--
-- -- | Convert the parsed tree to the required form.
-- convert :: G.Tree -> Tree ATerm
-- convert (R.Node G.NonTerm{..} xs) =
--   case typ of
--     G.Std       -> below $ O.NonTerm sym'
--     G.Foot      -> leaf  $ O.Foot sym'
--     G.Anchor    -> R.Node (O.NonTerm sym')
--       [leaf . O.Term $ Anchor sym']
--     G.Lex       -> leaf  . O.Term $ Term sym'
--     G.Other _   -> below $ O.NonTerm sym'
--   where
--     below x = R.Node x $ map convert xs
--     leaf x  = R.Node x []
--     sym' = L.toStrict sym
--
--
-- -- | Anchor the given tree with the given terminal.
-- anchor :: Term -> Tree ATerm -> Tree ATerm
-- anchor a (R.Node n xs) = case n of
--   O.NonTerm x -> R.Node (O.NonTerm x) (map (anchor a) xs)
--   O.Foot x    -> R.Node (O.Foot x) []
--   O.Term t    -> case t of
--     Term _   -> R.Node (O.Term t) []
--     Anchor _ -> R.Node (O.Term (Term a)) []


-------------------------------------------------
-- FS-aware tree conversion
-------------------------------------------------


-- -- | Remove information about the past anchors.  Fail if there
-- -- are some anchors left.
-- simplifyFS :: FSTree ATerm -> FSTree Term
-- simplifyFS = EC.simplify


-- -- | Convert the parsed tree to the required form.
-- convertFS :: G.Tree G.AVM2 -> E.StateT VarMap (Env.EnvM Val) (FSTree ATerm)
-- convertFS = EC.convert convertAVM
-- -- convertFS (R.Node G.NonTerm{..} xs) =
-- --   case typ of
-- --     G.Std -> below $ O.NonTerm sym
-- --     G.Foot -> leaf $ O.Foot sym
-- --     G.Anchor -> leaf . O.Term $ Anchor sym
-- -- --       -- fs <- mkFS
-- -- --       fs <- convertAVM avm
-- -- --       theLeaf <- leaf . O.Term $ Anchor sym
-- -- --       return $ R.Node (O.NonTerm sym, fs) [theLeaf]
-- --     G.Lex -> leaf . O.Term $ Term sym
-- --     G.Other _ -> below $ O.NonTerm sym
-- --   where
-- -- --     -- TODO: provisional solutino, top and bot should be distinguished
-- -- --     avm = maybe M.empty id (top <|> bot)
-- --     below x = do
-- --       -- fs <- mkFS
-- --       fs <- convertAVM avm
-- --       R.Node (x, fs) <$> mapM convertFS xs
-- --     leaf x = do
-- --       -- fs <- mkFS
-- --       fs <- convertAVM avm
-- --       return $ R.Node (x, fs) []
-- -- --     mkFS = do
-- -- --       let topList = maybe [] (M.toList top)
-- -- --           botList = maybe [] (M.toList bot)
-- -- -- --       topFS <- convertAVM $ map (first FSTree.Top) topList
-- -- -- --       botFS <- convertAVM $ map (first FSTree.Bot) botList
-- -- --       convertAVM $
-- -- --         map (first FSTree.Top) topList ++
-- -- --         map (first FSTree.Bot) botList
-- -- -- --       -- FS.unify topFS botFS -- not really unification


-- -- | Convert an XMG-style AVM to a FS.
-- convertAVM :: G.AVM2 -> E.StateT VarMap (Env.EnvM Val) OFS
-- -- convertAVM
-- --   :: [(k, (Either G.Val G.Var))]
-- --   -> E.StateT VarMap (Env.EnvM Val) (FS.FS k Val)
-- convertAVM avm = fmap M.fromList . runListT $ do
--   let topList = maybe [] M.toList (G.top avm)
--       botList = maybe [] M.toList (G.bot avm)
--       xs = map (first FSTree.Top) topList ++
--            map (first FSTree.Bot) botList
--   (key, valVar) <- each xs
--   case valVar of
--     -- Left val -> return (key, FS.Val . S.fromList $ S.toList val)
--     -- Left val -> return (key, FS.Val val)
--     Left val -> P.lift . P.lift $ do
--       var <- Env.var
--       Env.set var val
--       return (key, var)
--     Right var0 -> do
--       var <- P.lift $ varFor var0
--       return (key, var)


-- -- | Retrieve the corresponding environment variable. Create a new one if the
-- -- XMG variable was not seen previously.
-- varFor :: G.Var -> E.StateT VarMap (Env.EnvM Val) Env.Var
-- varFor gramVar = do
--   varMay <- E.gets $ M.lookup gramVar
--   case varMay of
--     Just var -> return var
--     Nothing  -> do
--       var <- P.lift Env.var
--       E.modify' $ M.insert gramVar var
--       return var


-- -- | Anchor the given tree with the given terminal and its accompanying FS.
-- anchorFS
--   :: Term                -- ^ Terminal used to replace the anchor
--   -> CFS                 -- ^ The accompanying FS
--   -> FSTree ATerm        -- ^ `FSTree` with an anchor
--   -> Env.EnvM Val (FSTree ATerm)
-- anchorFS = EC.anchor


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


-- -- | Map a function on left value.
-- onLeft :: (a -> c) -> Either a b -> Either c b
-- onLeft f (Left x) = Left (f x)
-- onLeft _ (Right x) = Right x
--
--
-- -- | Remove information about the anchors.  Fail if there are still
-- -- some anchors left.
-- simplifyNode :: O.Node n ATerm -> O.Node n Term
-- simplifyNode node = case node of
--   O.Term (Term x) -> O.Term x
--   O.Term (Anchor _) -> error "simlifyNode: cannot simplify, anchors left"
--   O.NonTerm x -> O.NonTerm x
--   O.Foot x -> O.Foot x
