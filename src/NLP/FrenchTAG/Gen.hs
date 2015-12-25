{-# LANGUAGE RecordWildCards #-}


-- Datatype prepared for generation.


module NLP.FrenchTAG.Gen
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
, GenConf (..)

-- * Experiments
, genRandFrom

-- * Utils
, getTrees
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

import           NLP.TAG.Vanilla.Core (View(..))
import qualified NLP.TAG.Vanilla.Tree.Other as O
import qualified NLP.TAG.Vanilla.Gen as G
-- import qualified NLP.TAG.Vanilla.SubtreeSharing as LS
-- import qualified NLP.TAG.Vanilla.Automaton as LA
-- import qualified NLP.TAG.Vanilla.Earley.Auto as LP

import qualified NLP.FrenchTAG.Parse as P



-------------------------------------------------
-- Data types
-------------------------------------------------


-- | Terminal in the grammar is either a normal terminal or a node attached to
-- a non-terminal anchor.
data Term
    = Term L.Text
    | Anchor L.Text
    deriving (Show, Read, Eq, Ord)
instance View Term where
    view = show

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


-------------------------------------------------
-- Grammar extraction
-------------------------------------------------


-- | Get the set of TAG trees.
getTrees :: FilePath -> IO (S.Set Tree)
getTrees path = do
    ts <- P.readGrammar path
    flip E.execStateT S.empty $ E.forM_ ts $ \tree -> do
        let tree' = convert tree
        -- Rather nasty trick, but works.  Otherwise the tree is
        -- not constructed at this precise momend.  Find a better
        -- solution.
        length (showTree tree') `seq`
            E.modify (S.insert tree')


-------------------------------------------------
-- Generation
-------------------------------------------------


-- | Generation configuration.
data GenConf = GenConf {
      maxSize   :: Int
    -- ^ Maximal size of a tree
    , adjProb   :: Double
    -- ^ Adjunction probability
    , treeNum   :: Int
    -- ^ Number of trees to generate
    } deriving (Show, Eq, Ord)


-- | Generate size-bounded derived trees based on
-- the grammar under the path.
-- Only final trees are shown.
generateFrom :: FilePath -> Int -> IO ()
generateFrom path sizeMax = do
    gram <- getTrees path
    let pipe = G.generateAll gram sizeMax
           >-> Pipes.filter O.isFinal
           >-> Pipes.map O.proj
    runEffect . for pipe $ liftIO . print
--     let pipe = G.generateRand gram $ G.GenConf
--             { genAllSize = maxSize
--             , adjProb    = adjProb }
--     runEffect . for (pipe >-> Pipes.take treeNum) $ \tree ->
--         lift . putStrLn . R.drawTree . fmap show $ tree


-------------------------------------------------
-- Parsing experiments
-------------------------------------------------


-- | Randomly generate derived sentences.
genRandFrom :: GenConf -> FilePath -> IO ()
genRandFrom GenConf{..} path = do
    -- extract the grammar
    gram <- getTrees path
    -- sentence generation pipe
    let conf = G.GenConf
            { genAllSize = maxSize
            , adjProb    = adjProb }
        pipe = G.generateRand gram conf
            >-> Pipes.filter O.isFinal
            >-> Pipes.map O.proj
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
