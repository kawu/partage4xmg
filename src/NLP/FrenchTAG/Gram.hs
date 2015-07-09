{-# LANGUAGE RecordWildCards #-}


-- A provisional module responsible for LTAG grammar creation.


module NLP.FrenchTAG.Gram where


import qualified Data.Tree           as R
import qualified Data.Map.Strict     as M

import qualified NLP.FeatureStructure.Tree as FT
import qualified NLP.LTAG.Tree2 as LT

import qualified NLP.FrenchTAG.Parse as P


-- | An LTAG tree.
type Tree = LT.Tree P.Sym P.Sym P.Var P.Attr P.Val


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


-- | Construct a feature structure.
mkFS :: P.AVM -> FS
mkFS m = FT.FN Nothing $ FT.Subs $ M.fromList
    [ (attr, mkValVar valVar)
    | (attr, valVar) <- M.toList m ]
  where
    mkValVar (Left val) = FT.FN Nothing $ FT.Atom val
    mkValVar (Right vr) = FT.FN (Just vr) $ FT.Subs M.empty
