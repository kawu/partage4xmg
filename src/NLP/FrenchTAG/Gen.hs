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
) where


-- import           Control.Applicative ((<$>))
import qualified Control.Monad.State.Strict as E

import           Pipes
import qualified Data.Tree as R
import qualified Data.Text.Lazy      as L
import qualified Data.Set as S

import qualified NLP.TAG.Vanilla.Tree.Other as O
import           NLP.TAG.Vanilla.Gen (generate)

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
-- Generation
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


-- | Generate size-bounded derived trees based on
-- the grammar under the path.
generateFrom :: FilePath -> Int -> IO ()
generateFrom path k = do
    gram <- getTrees path
    runEffect $ for (generate gram k) $ \tree ->
        lift $ putStrLn . R.drawTree . fmap show $ tree
