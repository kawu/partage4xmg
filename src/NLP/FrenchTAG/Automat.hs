{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- A provisional module responsible for vanilla TAG grammar creation
-- and converions to the automaton.


module NLP.FrenchTAG.Automat where


import           Control.Applicative ((<$>))
import           Control.Monad (msum)
import qualified Control.Monad.State.Strict as E
-- import           Control.Monad.Trans.Class (lift)
-- import           Control.Monad.IO.Class (liftIO)
-- import           Control.Monad.Morph (generalize)

-- import           Data.Maybe (fromJust)
import qualified Data.Tree           as R
-- import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import qualified Data.Text.Lazy.IO   as L
-- import qualified Pipes               as Pipes
-- import qualified Pipes.Prelude       as Pipes
-- import           Pipes               (for, (>->), hoist)
-- import qualified Control.Monad.Atom  as Atom
import qualified Data.DAWG.Ord      as DAWG


import qualified NLP.Partage.Tree as LT
import qualified NLP.Partage.Rule as LR
import qualified NLP.Partage.SubtreeSharing as LS
import qualified NLP.Partage.Auto.DAWG as LA

import qualified NLP.FrenchTAG.Parse as P


-------------------------------------------------
-- Base types
-------------------------------------------------


-- type Tree    = T.Tree String String
-- type AuxTree = T.AuxTree String String
-- type Gram    = S.Set Rl


-- | An vanilla TAG tree.
type Tree = LT.Tree P.Sym P.Sym


-- | An LTAG auxiliary tree.
type AuxTree = LT.AuxTree P.Sym P.Sym


-- | Some TAG tree.
type SomeTree = Either Tree AuxTree


-- | A label.
type Lab = LR.Lab P.Sym P.Sym


-- | Flat production rule.
type Rule     = LR.Rule P.Sym P.Sym


-- | Compiled TAG grammar.
type Gram     = S.Set Rule


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


-- -- | A TAG flat rule.
-- -- * non-terminal   = symbol
-- -- * terminal       = symbol
-- -- * identifier     = internal LTAG identifier
-- -- * attribute      = either attribute or input variable name
-- -- * value          = value
-- type Rule = LR.Rule P.Sym P.Sym
--
--
-- -- | Rule generation monad.
-- type RM m = LR.RM P.Sym P.Sym m
--
--
-- -- | Duplication-removal monad.
-- -- type DupT n t m = E.StateT (DupS n t) m
-- type DupM m = LS.DupT P.Sym P.Sym m


---------------------------------------------------------------
-- Generic function responsible for obtaining the set of trees.
-- After all, there can be some duplicates in the source set.
---------------------------------------------------------------


-- | Get the set of TAG trees.
getTrees :: FilePath -> IO (S.Set SomeTree)
getTrees path = do
    ts <- P.parseGrammar <$> L.readFile path
    flip E.execStateT S.empty $ E.forM_ ts $ \tree -> do
        let tree' = mkSomeTree tree
        -- Rather nasty trick, but works.  Otherwise the tree is
        -- not constructed at this precise momend.  Find a better
        -- solution.
        length (showSomeTree tree') `seq`
            E.modify (S.insert tree')


-------------------------------------------------
-- Baseline
-------------------------------------------------


-- -- | Make a Vanilla TAG grammar given a set of trees.
-- -- Common subtrees are *not* merged.
-- baseLineMkTAG :: Monad m => [SomeTree] -> RM m ()
-- baseLineMkTAG ts =
--     sequence_ $ map getRules ts
--   where
--     getRules (Left t) = LR.treeRules True t
--     getRules (Right a) = LR.auxRules True a


-- | Print rules in the factorized grammar.
baseLineRules :: FilePath -> IO ()
baseLineRules path = do
    ruleSet <- baseLineRuleSet path
    E.forM_ (S.toList ruleSet) $ \rule -> do
        LR.printRule rule >> putStrLn ""


-- | Set of rules (baseline).
baseLineRuleSet :: FilePath -> IO (S.Set Rule)
baseLineRuleSet path = do
    treeSet <- getTrees path
    LR.compile (S.toList treeSet)


-- | Print rules in the factorized grammar.
baseLineEdges :: FilePath -> IO ()
baseLineEdges path = do
    ruleSet <- baseLineRuleSet path
--     let auto = LA.buildAuto ruleSet
    putStr "\nTotal number of 'edges': "
--     print . length $ LA.edges auto
    print . sum . map (length.mkWord) $ S.toList ruleSet
    putStr "Total number of 'states': "
    print . (+2) . sum . map ((\n->n-1) . length.mkWord) $ S.toList ruleSet


-------------------------------------------------
-- Substructure Sharing
-------------------------------------------------


-- | Set of rules (baseline).
shareRuleSet :: FilePath -> IO (S.Set Rule)
shareRuleSet path = do
    treeSet <- getTrees path
    LS.compile (S.toList treeSet)


-- -- | Make a Vanilla TAG grammar given a set of trees.
-- -- Common subtrees are merged.
-- shareMkTAG :: Monad m => [SomeTree] -> RM (DupM m) ()
-- shareMkTAG ts = hoist (hoist (hoist generalize))
--     (   hoist (hoist lift) (baseLineMkTAG ts)
--     >-> hoist lift LS.rmDups )
--
--
-- -- | Print rules in the factorized grammar.
-- shareRuleSet :: FilePath -> IO (S.Set Rule)
-- shareRuleSet path = do
--     ts <- S.toList <$> getTrees path
--     fmap snd $ LS.runDupT $ LR.runRM $ Pipes.runEffect $
--         for (shareMkTAG ts) (const $ return ())



-- | Print rules in the factorized grammar.
shareRules :: FilePath -> IO ()
shareRules path = do
    ruleSet <- shareRuleSet path
    E.forM_ (S.toList ruleSet) $ \rule -> do
        LR.printRule rule >> putStrLn ""


-- | Print rules in the factorized grammar.
shareEdges :: FilePath -> IO ()
shareEdges path = do
    ruleSet <- shareRuleSet path
    putStr "\nTotal number of 'edges': "
    print . sum . map (length.mkWord) $ S.toList ruleSet
    putStr "Total number of 'states': "
    print . (+2) . sum . map ((\n->n-1) . length.mkWord) $ S.toList ruleSet
    return $ error "share this code piece"


-------------------------------------------------
-- Baseline + Automaton
-------------------------------------------------


-- | Build the automaton from the rules.
baseAutomatRules :: FilePath -> IO ()
baseAutomatRules path = do
    ruleSet <- baseLineRuleSet path
    let auto = LA.buildAuto ruleSet
--     let (ruleSet, labMap) = convGram ruleSet0
--         dawg = DAWG.fromLang (S.toList ruleSet)
--     traverse labMap dawg
    mapM_ print $ LA.edges auto
    putStrLn ""
    putStr "Number of states: " >> print (DAWG.numStates auto)
    putStr "Number of edges: "  >> print (DAWG.numEdges auto)


-------------------------------------------------
-- Substructure Sharing + Automaton
-------------------------------------------------


-- -- | Reverse the map assuming that each key gets a unique int.
-- revMap :: Ord a => M.Map a Int -> M.Map Int a
-- revMap m = M.fromList [(v, k) | (k, v) <- M.toList m]
--
--
-- -- | Convert the set of rules to a set of rules with ints
-- -- representing individual labels.
-- convGram :: S.Set Rule -> (S.Set [Int], M.Map Int Lab)
-- convGram ruleSet =
--     ( ruleSet'
--     , revMap (Atom.mapping tab) )
--   where
--     (ruleSet', tab) = flip Atom.runAtom Atom.empty $ do
--         rules <- forM (S.toList ruleSet) $ \rule -> do
--             let xs = mkWord rule
--             mapM Atom.toAtom xs
--         return $ S.fromList rules


-- | Build the automaton from the rules.
automatRules :: FilePath -> IO ()
automatRules path = do
--     ruleSet0 <- shareRuleSet path
--     let (ruleSet, labMap) = convGram ruleSet0
--         dawg = DAWG.fromLang (S.toList ruleSet)
--     traverse labMap dawg
--     putStrLn ""
--     putStr "Number of states: " >> print (DAWG.numStates dawg)
--     putStr "Number of edges: "  >> print (DAWG.numEdges dawg)
    ruleSet <- shareRuleSet path
    let auto = LA.buildAuto ruleSet
    mapM_ print $ LA.edges auto
    putStrLn ""
    putStr "Number of states: " >> print (DAWG.numStates auto)
    putStr "Number of edges: "  >> print (DAWG.numEdges auto)


-------------------------------------------------
-- Automaton Traversal
-------------------------------------------------


-- -- | Traverse and print the automaton.
-- traverse :: M.Map Int Lab => DAWG.DAWG Int () () -> IO ()
-- traverse labMap dawg =
--     flip E.evalStateT S.empty $ doit (getID dawg)
--   where
--     getID = DAWG.rootID
--     doit i = do
--         b <- E.gets $ S.member i
--         when (not b) $ do
--             lift . putStrLn $ "[Node " ++ show i ++ "]"
--             E.modify $ S.insert i
--             let dg = fromJust $ DAWG.byID i dawg
--                 edges = DAWG.edges dg
--             forM_ edges $ \(x, end) -> do
--                 lift . putStrLn $
--                     "  " ++ LR.viewLab (labMap M.! x) ++
--                     " => " ++ show (getID end)
--             forM_ edges $ \(_, end) ->
--                 doit (getID end)


-------------------------------------------------
-- Utils
-------------------------------------------------


-- | Build a sequence from a rule.
mkWord :: Rule -> [Lab]
mkWord LR.Rule{..} = bodyR ++ [headR]
