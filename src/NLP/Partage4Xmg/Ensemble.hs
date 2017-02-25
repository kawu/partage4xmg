{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE GADTs           #-}


module NLP.Partage4Xmg.Ensemble
(
-- * Types
  NonTerm
, ATerm (..)
, Term
, Node
, Tree
-- ** FS-related
, Key
, Val
, FSTree

-- * Grammar
, Grammar (..)
, getInterps
, getTrees

-- * Reading
, GramCfg(..)
, readGrammar

-- * FS-aware tree conversion
, VarMap
, withVarMap
, simplify
, convert
, anchor
, splitTree
) where


import           Control.Arrow              (first)
import           Control.Monad              (guard)
import qualified Control.Monad.State.Strict as E

import qualified Data.Text                  as T
import qualified Data.Tree                  as R
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Data.Maybe                 (maybeToList)
import qualified Pipes                      as P
import qualified Pipes.Prelude              as P

import qualified NLP.Partage.Tree.Other     as O
import qualified NLP.Partage.FS             as FS
import qualified NLP.Partage.FSTree         as FSTree
import qualified NLP.Partage.FSTree2        as FSTree2
import qualified NLP.Partage.Env            as Env
import qualified NLP.Partage.Earley.Comp    as C

import qualified NLP.Partage4Xmg.Lexicon    as Lex
import qualified NLP.Partage4Xmg.Morph      as Morph
import qualified NLP.Partage4Xmg.Grammar    as G


--------------------------------------------------
-- Types
--------------------------------------------------


-- | A non-terminal.
type NonTerm = T.Text


-- | A regular terminal.
type Term = T.Text


-- | Either a regular terminal or an anchor.
data ATerm
    = Term Term
    | Anchor NonTerm
    deriving (Show, Read, Eq, Ord)



-- | Type of the node in TAG trees.
type Node t = O.Node NonTerm t


-- | A TAG tree.
type Tree t = R.Tree (Node t)


-------------------------------------------------
-- FS-related types
-------------------------------------------------


-- | FS key.
type Key = G.Feat


-- | FS value.
type Val = T.Text


-- | FS-aware tree.
type FSTree t k = FSTree.FSTree NonTerm t k


--------------------------------------------------
-- The grammar, lemmas and morphology combined
--------------------------------------------------


-- | All the components of a grammar.
data Grammar = Grammar
  { morphMap :: M.Map T.Text (S.Set Morph.Ana)
  , lexMap   :: M.Map Lex.Word (S.Set G.Family)
  , treeMap  :: M.Map G.Family (S.Set G.Tree)
  }


-- | Map a given word to the set of its possible interpretations.
getInterps :: Grammar -> T.Text -> S.Set Morph.Ana
getInterps Grammar{..} orth =
--   seq printMap $
--   trace (show orth) $
--   trace (T.unpack orth) $
--   trace (show morphMap) $
  case M.lookup orth morphMap of
    Nothing -> S.empty
    Just x  -> x
--   where
--     printMap = unsafePerformIO $ do
--       putStrLn ""
--       E.forM_ (M.toList morphMap) $ \(x, y) -> do
--         print x
--         T.putStrLn x
--         -- print y
--       putStrLn ""


-- | Retrieve the set of grammar trees related to te given interpretation.
_getTrees :: Grammar -> Morph.Ana -> S.Set G.Tree
_getTrees Grammar{..} ana = S.unions
  [ treeSet
  | famSet <- maybeToList $ M.lookup (Morph.word ana) lexMap
  , family <- S.toList famSet
  -- , treeSet <- trace (show family) $ maybeToList $ M.lookup family treeMap
  , treeSet <- maybeToList $ M.lookup family treeMap
  ]


--------------------------------------------------
-- Config
--------------------------------------------------


-- | Grammar configuration.
data GramCfg = GramCfg
  { morphPath :: FilePath
  , useMph    :: Bool
    -- ^ Use the alternative .mph format
  , lexPath   :: FilePath
  , useLex    :: Bool
    -- ^ Use the alternative .lex format
  , treePath  :: FilePath
  } -- deriving (Show, Read, Eq, Ord)


-- | Read the grammar given the configuration.
readGrammar
  :: GramCfg
  -> IO Grammar
readGrammar GramCfg{..} = do
  xs <- if useMph
    then Morph.readMorphMph morphPath
    else Morph.readMorph morphPath
  ys <- if useLex
    then Lex.readLexiconLex lexPath
    else Lex.readLexicon lexPath
  -- zs <- G.readGrammar treePath
  treeMap' <- G.readGrammar treePath
  return Grammar
    { morphMap = M.fromListWith S.union
      [ (Morph.wordform x, Morph.analyzes x)
      | x <- xs ]
    , lexMap = M.fromList ys
    , treeMap = treeMap'
--     , treeMap = M.fromListWith S.union
--       [ (famName, S.singleton tree)
--       | (famName, tree) <- zs ]
    }


--------------------------------------------------
-- FS-aware AVM conversion
--------------------------------------------------


-- | Retrieve the FS-aware grammar ETs related to te given interpretation.
getTrees
  :: Grammar
  -> Term
     -- ^ A wordform
  -> Morph.Ana
     -- ^ The analysis corresponding the wordform
  -- -> [(Tree Term, C.Comp CFS)]
  -> [Env.EnvM Val (FSTree Term Key)]
getTrees gram term ana
  -- = mapMaybe process
  = map process
  . S.toList
  $ _getTrees gram ana
  where
    -- process tree = splitFSTree . fmap simplifyFS $ do
    process tree = fmap simplify $ do
      -- let term = Morph.lemma ana
      converted <- withVarMap (convert tree)
      let fs = closeAVM $ Morph.avm ana
      anchor term (Morph.word ana) fs converted


-- | Convert and close the given XMG AVM.
closeAVM :: G.AVM -> FS.CFS Key Val
closeAVM avm = maybe M.empty id . fst . Env.runEnvM $ do
  fs <- withVarMap $ convertAVM avm
  FS.close fs


-- | Convert an XMG-style AVM to a FS.
convertAVM :: G.AVM -> E.StateT VarMap (Env.EnvM Val) (FS.OFS Key)
convertAVM avm = fmap M.fromList . runListT $ do
  (key, valVar) <- each avm
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


-------------------------------------------------
-- FS-aware tree conversion
-------------------------------------------------


-- | A mapping from XMG variables to local variables.
type VarMap = M.Map G.Var Env.Var


-- | Perform a computation with a variable map (`VarMap`).
withVarMap :: (Monad m) => E.StateT VarMap m a -> m a
withVarMap = flip E.evalStateT M.empty


-- | Remove information about the past anchors.  Fail if there
-- are some anchors left.
simplify :: FSTree ATerm k -> FSTree Term k
simplify = fmap $ first simplifyNode


-- | Convert the parsed tree to the required form.
convert
  :: G.Tree
  -> E.StateT VarMap (Env.EnvM Val) (FSTree ATerm Key)
convert (R.Node G.NonTerm{..} xs) =
  case typ of
    G.Std -> below $ O.NonTerm sym
    G.Foot -> leaf $ O.Foot sym
    G.Anchor -> leaf . O.Term $ Anchor sym
--       -- fs <- mkFS
--       fs <- convertAVM avm
--       theLeaf <- leaf . O.Term $ Anchor sym
--       return $ R.Node (O.NonTerm sym, fs) [theLeaf]
    G.Lex -> leaf . O.Term $ Term sym
    G.Other _ -> below $ O.NonTerm sym
  where
--     -- TODO: provisional solution, top and bot should be distinguished
--     avm = maybe M.empty id (top <|> bot)
    below x = do
      -- fs <- mkFS
      fs <- convertAVM avm
      R.Node (x, fs) <$> mapM convert xs
    leaf x = do
      -- fs <- mkFS
      fs <- convertAVM avm
      return $ R.Node (x, fs) []
--     mkFS = do
--       let topList = maybe [] (M.toList top)
--           botList = maybe [] (M.toList bot)
-- --       topFS <- convertAVM $ map (first FSTree.Top) topList
-- --       botFS <- convertAVM $ map (first FSTree.Bot) botList
--       convertAVM $
--         map (first FSTree.Top) topList ++
--         map (first FSTree.Bot) botList
-- --       -- FS.unify topFS botFS -- not really unification


-- | Anchor the given tree with the given terminal and its accompanying FS.
anchor
  :: (Ord key, Show key)
  => Term                -- ^ Terminal used to replace the anchor
  -> Lex.Word            -- ^ The corresponding word interpretation (containing
                         --   relevant part-of-speech information)
  -> FS.CFS key Val      -- ^ The accompanying FS
  -> FSTree ATerm key    -- ^ `FSTree` with an anchor
  -> Env.EnvM Val (FSTree ATerm key)
anchor anc wordInterp newFS =
  go
  where
    go (R.Node label@(typ, oldFS) xs) = case typ of
      O.NonTerm _ -> R.Node label <$> mapM go xs
      O.Foot _ -> return $ R.Node label []
      O.Term t -> case t of
        Term _ -> return $ R.Node label []
        Anchor sym -> do
          guard $ sym == Lex.cat wordInterp
          newFS' <- FS.reopen newFS
          fs <- FS.unifyFS oldFS newFS'
    --       env <- E.get
    --       trace (show newFS) $ trace (show oldFS) $
    --         trace (show newFS') $ trace (show fs) $ trace (show env) $
          let leaf = R.Node (O.Term (Term anc), M.empty) []
          return $ R.Node (O.NonTerm sym, fs) [leaf]


-- | Extract the tree embedded in the environment and the accompanying computation.
splitTree
  :: Env.EnvM Val (FSTree Term Key)
  -> Maybe (Tree Term, C.Comp (FS.CFS Key Val))
splitTree source = do
  let comp = FSTree2.compile source
  tree <- fmap fst <$> FSTree.extract source
  return (tree, comp)


-------------------------------------------------
-- Utils
-------------------------------------------------


-- | Remove information about the anchors.  Fail if there are still
-- some anchors left.
simplifyNode :: O.Node n ATerm -> O.Node n Term
simplifyNode node = case node of
  O.Term (Term x) -> O.Term x
  O.Term (Anchor _) -> error "simlifyNode: cannot simplify, anchors left"
  O.NonTerm x -> O.NonTerm x
  O.Foot x -> O.Foot x


-- | ListT from a list.
each :: Monad m => [a] -> P.ListT m a
each = P.Select . P.each


-- | Run a ListT computation (unidiomatic Haskell?).
runListT :: (Monad m) => P.ListT m a -> m [a]
runListT = P.toListM . P.enumerate
