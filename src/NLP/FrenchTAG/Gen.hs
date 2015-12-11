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

-- * Experiments
, genAndParseFrom
) where


import           Control.Applicative ((<$>))
import qualified Control.Monad.State.Strict as E

import           Pipes
import qualified Pipes.Prelude as Pipes
import qualified Data.Tree as R
import qualified Data.Text.Lazy      as L
import qualified Data.Set as S

import           NLP.TAG.Vanilla.Core (View(..))
import qualified NLP.TAG.Vanilla.Tree.Other as O
import           NLP.TAG.Vanilla.Gen (generate)
import qualified NLP.TAG.Vanilla.SubtreeSharing as LS
import qualified NLP.TAG.Vanilla.Automaton as LA
import qualified NLP.TAG.Vanilla.Earley.Auto as LP

import qualified NLP.FrenchTAG.Parse as P



-------------------------------------------------
-- Data types
-------------------------------------------------


-- | Terminal in the grammar is either a normal terminal or a node attached to
-- a non-terminal anchor.
data Term
    = Term L.Text
    | Anchor L.Text
    deriving (Show, Eq, Ord)
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


-- | Generate size-bounded derived trees based on
-- the grammar under the path.
-- Only final trees are shown.
generateFrom :: FilePath -> Int -> Double -> IO ()
generateFrom path k p0 = do
    gram <- getTrees path
    let pipe = generate gram k p0
    runEffect . for pipe $ \tree ->
        lift . putStrLn . R.drawTree . fmap show $ tree


-------------------------------------------------
-- Parsing experiments
-------------------------------------------------


-- | Generate size-bounded derived trees based on
-- the grammar under the path.
-- Only final trees are shown.
genAndParseFrom :: FilePath -> Int -> Double -> IO ()
genAndParseFrom path k p0 = do
    -- extract the grammar
    gram <- getTrees path

    -- build the automaton
    ruleSet <- LS.compile . map O.decode . S.toList $ gram
    let auto = LA.buildAuto ruleSet

    -- sentence generation pipe
    let pipe = generate gram k p0
            >-> Pipes.filter O.final
            >-> Pipes.map O.proj
            >-> rmDups

    runEffect . for pipe $ \sent ->
        lift $ do
            print sent
            print =<< LP.recognizeAuto auto sent
            putStrLn ""


-------------------------------------------------
-- Utils
-------------------------------------------------


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
