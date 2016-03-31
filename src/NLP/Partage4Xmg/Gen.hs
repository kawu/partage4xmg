{-# LANGUAGE RecordWildCards #-}


-- Datatype prepared for generation.


module NLP.Partage4Xmg.Gen
(
-- * Types
  Tree
, Node
, NonTerm
, Term (..)

-- * Conversion
, convert

-- * Generation
, generateFrom
-- ** Randomized
, GenConf (..)
, genRandFrom

-- * Utils
, getTrees
, getTreeMap
, rmDups
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

import qualified Data.Hashable as H

import qualified NLP.Partage.Tree.Other as O
import qualified NLP.Partage.Gen as G

import qualified NLP.Partage4Xmg.Parse as P
import qualified NLP.Partage4Xmg.ParseLex as PL



-------------------------------------------------
-- Data types
-------------------------------------------------


-- | Terminal in the grammar is either a normal terminal or a node
-- attached to a non-terminal anchor.
data Term
    = Term L.Text
    | Anchor L.Text
    deriving (Show, Read, Eq, Ord)

instance H.Hashable Term where
    hashWithSalt salt (Term x) =
        salt     `H.hashWithSalt`
        (0::Int) `H.hashWithSalt` x
    hashWithSalt salt (Anchor x) =
        salt     `H.hashWithSalt`
        (1::Int) `H.hashWithSalt` x

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


-- | Convert the parsed tree into the required form.
convert :: P.Tree -> Tree
convert (R.Node P.NonTerm{..} xs) =
    case typ of
        P.Std       -> below $ O.NonTerm sym
        P.Foot      -> leaf  $ O.Foot sym
        P.Anchor    -> R.Node (O.NonTerm sym)
            [leaf . O.Term $ Anchor sym]
        P.Lex       -> leaf  . O.Term $ Term sym
        P.Other _   -> below $ O.NonTerm sym
  where
    below x = R.Node x $ map convert xs
    leaf x  = R.Node x []


-- | Anchor the given tree with the given terminal.
anchor :: L.Text -> Tree -> Tree
anchor a (R.Node n xs) = case n of
    O.NonTerm x -> R.Node (O.NonTerm x) (map (anchor a) xs)
    O.Foot x    -> R.Node (O.Foot x) []
    O.Term t    -> case t of
            Term _   -> R.Node (O.Term t) []
            Anchor _ -> R.Node (O.Term (Term a)) []


-------------------------------------------------
-- Grammar extraction
-------------------------------------------------


-- | Get the set of TAG trees with anchors.
-- Useful when lexicon is not there.
getAncTrees
    :: FilePath         -- ^ Grammar
    -> IO (S.Set Tree)
getAncTrees path = do
    ts <- P.readGrammar path
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
    ts <- P.readGrammar path
    flip E.execStateT M.empty $ E.forM_ ts $ \(family, tree) -> do
        let tree' = convert tree
        -- Rather nasty trick, but works.  Otherwise the tree is
        -- not constructed at this precise moment.  Find a better
        -- solution.
        length (showTree tree') `seq`
            E.modify (M.insertWith S.union family
                (S.singleton tree'))


-- | Get the set of lexicalized TAG trees given the grammar file and
-- the lexicon file.
getLexTrees
    :: FilePath     -- ^ Grammar
    -> FilePath     -- ^ Lexicon
    -> IO (S.Set Tree)
getLexTrees gramPath lexPath = do
    treeMap <- getTreeMap gramPath
    lemmas  <- PL.readLexicon lexPath
    let treeList =
          [ anchor (PL.name lemma) tree
          | lemma  <- lemmas
          , family <- S.toList $ PL.treeFams lemma
          , tree   <- maybe [] S.toList (M.lookup family treeMap) ]
    flip E.execStateT S.empty $ E.forM_ treeList $ \tree -> do
        length (showTree tree) `seq`
            E.modify (S.insert tree)


-- | `getAncTrees` or `getLexTrees`, depending on arguments.
getTrees
    :: FilePath         -- ^ Grammar
    -> Maybe FilePath   -- ^ Lexicon (if present)
    -> IO (S.Set Tree)
getTrees gramPath Nothing =
    getAncTrees gramPath
getTrees gramPath (Just lexPath) =
    getLexTrees gramPath lexPath


-- | Extract trees with `getTrees` and print them.
printTrees
    :: FilePath         -- ^ Grammar
    -> Maybe FilePath   -- ^ Lexicon (if present)
    -> IO ()
printTrees gramPath lexPath = do
    let printTree = putStrLn . R.drawTree . fmap show
    mapM_ printTree . S.toList =<< getTrees gramPath lexPath


-------------------------------------------------
-- Generation
-------------------------------------------------


-- | Generate size-bounded derived trees based on
-- the grammar under the path.
-- Only final trees are shown.
generateFrom
    :: FilePath         -- ^ Grammar
    -> Maybe FilePath   -- ^ Lexicon (if present)
    -> Int
    -> IO ()
generateFrom gramPath mayLexPath sizeMax = do
    gram <- getTrees gramPath mayLexPath
    let pipe = G.generateAll gram sizeMax
           >-> Pipes.filter O.isFinal
           >-> Pipes.map O.project
    runEffect . for pipe $ liftIO . print


-------------------------------------------------
-- Randomized generation
-------------------------------------------------


-- | Randomized generation configuration.
data GenConf = GenConf {
      maxSize   :: Int
    -- ^ Maximal size of a tree
    , adjProb   :: Double
    -- ^ Adjunction probability
    , treeNum   :: Int
    -- ^ Number of trees to generate
    } deriving (Show, Eq, Ord)


-- | Randomly generate derived sentences.
genRandFrom
    :: GenConf
    -> FilePath         -- ^ Grammar
    -> Maybe FilePath   -- ^ Lexicon
    -> IO ()
genRandFrom GenConf{..} gramPath mayLexPath = do
    -- extract the grammar
    gram <- getTrees gramPath mayLexPath
    -- sentence generation pipe
    let conf = G.GenConf
            { genAllSize = maxSize
            , adjProb    = adjProb }
        pipe = G.generateRand gram conf
            >-> Pipes.filter O.isFinal
            >-> Pipes.map O.project
    -- print (conf, treeNum)
    let runPipe = runEffect . for
            (pipe >-> Pipes.take treeNum >-> rmDups)
    runPipe $ liftIO . print


-------------------------------------------------
-- Utils
-------------------------------------------------


-- -- | Duplication removal pipe.
-- rmDups :: (Monad m, Ord a) => Pipe a a (E.StateT (S.Set a) m) r
-- rmDups = E.forever $ do
--     x <- await
--     isMember <- S.member x <$> lift E.get
--     E.unless isMember $ do
--         yield x
--         lift . E.modify . S.insert $ x


-- | Duplication removal pipe.
rmDups :: (Monad m, Ord a) => Pipe a a m ()
rmDups =
    E.evalStateT pipe S.empty
  where
    pipe = E.forever $ do
        x <- lift await
        isMember <- S.member x <$> E.get
        E.unless isMember $ do
            lift (yield x)
            E.modify (S.insert x)
