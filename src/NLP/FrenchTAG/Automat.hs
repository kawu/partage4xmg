{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- A provisional module responsible for vanilla TAG grammar creation
-- and converions to the automaton.


module NLP.FrenchTAG.Automat where


import           Control.Applicative ((<$>))
import           Control.Monad (msum, (<=<))
import           Control.Monad.State.Strict as E
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Morph (generalize)

import qualified Data.Tree           as R
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import qualified Data.Text.Lazy.IO   as L
import qualified Pipes               as Pipes
import qualified Pipes.Prelude       as Pipes
import           Pipes               (for, (>->), hoist)

import qualified NLP.TAG.Vanilla.Tree as LT
import qualified NLP.TAG.Vanilla.Rule as LR
import qualified NLP.TAG.Vanilla.Earley as LE

import qualified NLP.FrenchTAG.Parse as P


-- | An vanilla TAG tree.
type Tree = LT.Tree P.Sym P.Sym


-- | An LTAG auxiliary tree.
type AuxTree = LT.AuxTree P.Sym P.Sym


-- | Some TAG tree.
type SomeTree = Either Tree AuxTree


-- | Convert the parsed tree into an LTAG tree.
mkTree :: P.Tree -> Tree
mkTree (R.Node nonTerm xs) = case typ of
    P.Lex   -> LT.FNode sym
    _       -> LT.INode
        { LT.labelI = sym
        , LT.subTrees = map mkTree xs }
    where P.NonTerm{..} = nonTerm


-- | Find the path of the foot (if present) in the tree.
findFoot :: P.Tree -> Maybe LT.Path
findFoot (R.Node nonTerm xs) = case typ of
    P.Foot  -> Just []
    _       -> msum
        $ map (uncurry addID)
        $ zip [0..]
        $ map findFoot xs
  where
    P.NonTerm{..} = nonTerm
    addID i (Just is) = Just (i:is)
    addID _ Nothing   = Nothing


-- | Convert the parsed tree into an auxiliary TAG tree.
mkAuxTree :: P.Tree -> Maybe AuxTree
mkAuxTree t = LT.AuxTree (mkTree t) <$> findFoot t


-- | Make a regular or an auxiliary tree, depending on the input.
mkSomeTree :: P.Tree -> SomeTree
mkSomeTree t = case findFoot t of
    Just is -> Right $ LT.AuxTree (mkTree t) is
    Nothing -> Left $ mkTree t


-- | Print some tree.
showSomeTree :: SomeTree -> String
showSomeTree (Left t) = LT.showTree' t
showSomeTree (Right aux) = LT.showTree' (LT.auxTree aux)


-------------------------------------------------
-- Testing
-------------------------------------------------


-- | A TAG flat rule.
-- * non-terminal   = symbol
-- * terminal       = symbol
-- * identifier     = internal LTAG identifier
-- * attribute      = either attribute or input variable name
-- * value          = value
type Rule = LR.Rule P.Sym P.Sym


-- | Rule generation monad.
type RM m = LR.RM P.Sym P.Sym m


-- | Duplication-removal monad.
-- type DupT n t m = E.StateT (DupS n t) m
type DupM m = LE.DupT P.Sym P.Sym m


---------------------------------------------------------------
-- Generic function responsible for obtaining the set of trees.
-- After all, there can be some duplicates in the source set.
---------------------------------------------------------------


-- | Get the set of TAG trees.
getTrees :: FilePath -> IO (S.Set SomeTree)
getTrees path = do
    ts <- P.parseGrammar <$> L.readFile path
    flip E.execStateT S.empty $ forM_ ts $ \tree -> do
        let tree' = mkSomeTree tree
        -- Rather stupid trick, but works.  Otherwise the tree is
        -- not constructed at this precise momend.  Find a better
        -- solution.
        length (showSomeTree tree') `seq`
            E.modify (S.insert tree')


-------------------------------------------------
-- Baseline
-------------------------------------------------


-- | Make a Vanilla TAG grammar given a set of trees.
-- Common subtrees are *not* merged.
baseLineMkTAG :: Monad m => [SomeTree] -> RM m ()
baseLineMkTAG ts = 
    sequence_ $ map getRules ts
  where
    getRules (Left t) = LR.treeRules True t
    getRules (Right a) = LR.auxRules True a


-- | Print rules in the factorized grammar.
baseLineRules :: FilePath -> IO ()
baseLineRules path = do
    ts <- S.toList <$> getTrees path
    LR.runRM $ Pipes.runEffect $
        for (baseLineMkTAG ts) $ \rule -> do
            liftIO $ LR.printRule rule >> putStrLn ""


-- | Calculate the number of rules in the factorized grammar.
baseLineRuleNum :: FilePath -> IO ()
baseLineRuleNum path = do
    ts <- S.toList <$> getTrees path
    n <- LR.runRM $ Pipes.length $ baseLineMkTAG ts
    print n


-------------------------------------------------
-- Substructure Sharing
-------------------------------------------------


-- | Make a Vanilla TAG grammar given a set of trees.
-- Common subtrees are merged.
shareMkTAG :: Monad m => [SomeTree] -> RM (DupM m) ()
shareMkTAG ts = hoist (hoist (hoist generalize))
    (   hoist (hoist lift) (baseLineMkTAG ts)
    >-> hoist lift LE.rmDups )


-- | Calculate the number of rules in the factorized grammar:
-- removing duplicates.
shareRuleNum :: FilePath -> IO ()
shareRuleNum path = do
    ts <- S.toList <$> getTrees path
    n <- LE.runDupT $ LR.runRM $ Pipes.length $ shareMkTAG ts
    print n


-------------------------------------------------
-- Backup
-------------------------------------------------


-- -- | Parse the stand-alone French TAG xml file.
-- -- readGrammar :: FilePath -> IO (S.Set Rule)
-- -- readGrammar :: FilePath -> IO [Rule]
-- readGrammar path = do
--     ts <- liftIO $ P.parseGrammar <$> L.readFile path
--     mkTAG ts
-- 
-- 
-- printGrammar :: FilePath -> IO ()
-- printGrammar path = do
--     let printRule x = LR.printRule x >> L.putStrLn ""
--     gram <- fmap snd $ LE.runDupT $ LR.runRM $ for
--         (readGrammar path)
--         -- (liftIO . printRule)
--         (liftIO . Pipes.discard)
--     void $ LE.earley gram ["jean", "dort"]
-- --     return ()
