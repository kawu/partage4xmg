{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


-- A provisional module responsible for LTAG grammar creation.


module NLP.Partage4Xmg.Gram where


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

import qualified NLP.FeatureStructure.Tree as FT

import qualified NLP.LTAG.Tree2 as LT
import qualified NLP.LTAG.Rule as LR
import qualified NLP.LTAG.Earley5 as LE

import qualified NLP.Partage4Xmg.Parse as P


-- | An LTAG tree.
type Tree = LT.Tree P.Sym P.Sym P.Var P.Attr P.Val


-- | An LTAG auxiliary tree.
type AuxTree = LT.AuxTree P.Sym P.Sym P.Var P.Attr P.Val


-- | A feature structure.
type FS = FT.FN P.Var P.Attr P.Val


-- | Convert the parsed tree into an LTAG tree.
mkTree :: P.Tree -> Tree
mkTree (R.Node nonTerm xs) = case typ of
    P.Lex   -> LT.LNode sym
    _       -> LT.INode
        { LT.labelI = sym
        , LT.topFS  = maybe FT.empty mkFS top
        , LT.botFS  = maybe FT.empty mkFS bot
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
mkSomeTree :: P.Tree -> Either Tree AuxTree
mkSomeTree t = case findFoot t of
    Just is -> Right $ LT.AuxTree (mkTree t) is
    Nothing -> Left $ mkTree t


-- | Construct a feature structure.
mkFS :: P.AVM -> FS
mkFS m = FT.FN Nothing $ FT.Subs $ M.fromList
    [ (attr, mkValVar valVar)
    | (attr, valVar) <- M.toList m ]
  where
    mkValVar (Left val) = FT.FN Nothing $ FT.Atom val
    mkValVar (Right vr) = FT.FN (Just vr) $ FT.Subs M.empty


-------------------------------------------------
-- Testing
-------------------------------------------------


-- | An LTAG flat rule.
-- * non-terminal   = symbol
-- * terminal       = symbol
-- * identifier     = internal LTAG identifier
-- * attribute      = either attribute or input variable name
-- * value          = value
type Rule = LE.Rule P.Sym P.Sym LE.ID (LR.Feat P.Attr) P.Val


-- | Make an LTAG grammar given a set of trees.
-- TODO: We would like the function to work on an input set but
-- a tree doesn't provide an Ord instance!
-- mkLTAG :: [P.Tree] -> S.Set Rule
-- mkLTAG :: Monad m => [P.Tree] -> Pipes.Producer Rule (StateT Int (StateT DupS)) m

-- mkLTAG ts =
--     ruleProd >-> Pipes.map LE.compile
mkLTAG ts = hoist (hoist (hoist generalize))
    (   hoist (hoist lift) ruleProd
    >-> Pipes.map LE.compile
    >-> hoist lift LE.rmDups )
  where
    ruleProd = sequence_ $ map (getRules . mkSomeTree) ts
    getRules (Left t) = LR.treeRules True t
    getRules (Right a) = LR.auxRules True a


-- | Parse the stand-alone French TAG xml file.
-- readGrammar :: FilePath -> IO (S.Set Rule)
-- readGrammar :: FilePath -> IO [Rule]
readGrammar path = do
    ts <- liftIO $ P.parseGrammar <$> L.readFile path
    mkLTAG ts


printGrammar :: FilePath -> IO ()
printGrammar path = do
    let printRule x = LE.printRuleFS x >> L.putStrLn ""
    gram <- fmap snd $ LE.runDupT $ LR.runRM $ for
        (readGrammar path)
        -- (liftIO . printRule)
        (liftIO . Pipes.discard)
    void $ LE.earley gram ["jean", "dort"]
--     return ()
