{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Parsing TAG grammars consistent with the XMG format.


module NLP.Partage4Xmg.Grammar
(
-- * Types
  Family
, Tree
, TreeID
, NonTerm (..)
, Type (..)
, SubType
, AVM
, XAVM (..)
-- , topOnly
-- , botOnly
, Sym
, Attr
-- , Feat
, Val
, Var

-- * Functions
, parseGrammar
, readGrammar
, printGrammar

-- * Utils
, attrValQ
, -- onEps
) where


import Debug.Trace (trace)

import qualified Control.Monad.State.Strict as E
import qualified Control.Arrow       as Arr
import           Control.Applicative ((*>), (<$>), (<*>),
                        optional, (<|>))
import           Control.Monad ((<=<))

import           Data.Maybe          (mapMaybe)
import qualified Data.Foldable       as F
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as L
import qualified Data.Text.Lazy.IO   as L
import qualified Data.Tree           as R
import qualified Data.Set            as S
import qualified Data.Map.Strict     as M

import qualified Text.HTML.TagSoup   as TagSoup
import           Text.XML.PolySoup   hiding (P, Q)
import qualified Text.XML.PolySoup   as PolySoup

-- TODO: just to have Tree Ord instance?
import qualified NLP.Partage.DAG as DAG
-- import           NLP.TAG.Vanilla.Core    (View(..))

import           NLP.Partage.FSTree2 (Loc(..))


-- import           NLP.Partage4Xmg.Tree


-------------------------------------------------
-- Data types
-------------------------------------------------


-- | Parsing predicates.
type P a = PolySoup.P (XmlTree L.Text) a
type Q a = PolySoup.Q (XmlTree L.Text) a


-- instance View L.Text where
--     view = L.unpack


-- | Syntagmatic symbol.
type Sym = T.Text


-- | Attribute.
type Attr = T.Text


-- -- | Feature (simple, top or bottom)
-- type Feat = Loc Attr


-- | Attribute value.
type Val = S.Set T.Text


-- | Variable.
type Var = T.Text


-- | Attribute-value matrix. We are using a list and not e.g. a map on purpose,
-- so that it is possible to define a single feature with different values and
-- variables (something which might happen in practice).
type AVM = [(Attr, Either Val Var)]


-- | Non-terminal/node type.
data Type
    = Std
    | Foot
    | Anchor
    | Lex
    | NAdj
      -- ^ Null adjoining constraint
    | Other SubType
    deriving (Show, Eq, Ord)


-- | Node subtype (e.g. subst).  Not handled in any special way.
type SubType = T.Text


-- | Non-terminal or terminal.
-- TODO: change the name.
data NonTerm = NonTerm
    { typ :: Type
    , sym :: Sym
    , avm :: XAVM }
    deriving (Show, Eq, Ord)


-- | Mixed AVM.
data XAVM = XAVM
  { reg :: AVM
    -- ^ Regular features
  , top :: Maybe AVM
    -- ^ The top AVM
  , bot :: Maybe AVM
    -- ^ The bottom AVM
  } deriving (Show, Eq, Ord)


-- -- | Create a complex AVM with the top part only.
-- topOnly :: AVM -> AVM2
-- topOnly avm = AVM2 {top = Just avm, bot = Nothing}
--
--
-- -- | Create a complex AVM with the bottom part only.
-- botOnly :: AVM -> AVM2
-- botOnly avm = AVM2 {top = Nothing, bot = Just avm}


-- | Partage4Xmg tree.
type Tree = R.Tree NonTerm


-- | Name of a tree family.
type Family = T.Text


-- | Tree identifier.
type TreeID = T.Text


-------------------------------------------------
-- AVM
-------------------------------------------------


-- join v1 v2 = case (v2, v2) of
--   (Left x, Left y) -> S.union x y
--   (Right )
--
--
-- -- | Join two AVMs.
-- joinAVM :: AVM -> AVM ->
-- joinAVM avm1 avm2 =


-------------------------------------------------
-- Phon
-------------------------------------------------


-- -- | Remove phonologically empty nodes (and the corresponding subtrees)
-- rmPhonEps :: Tree -> Maybe (Tree)
-- rmPhonEps t
--   | phonEps (R.rootLabel t) = Nothing
--   | null (R.subForest t) = Just t
--   | otherwise = case mapMaybe rmPhonEps (R.subForest t) of
--       [] -> Nothing
--       xs -> Just $ t {R.subForest = xs}


-------------------------------------------------
-- Parsing
-------------------------------------------------


-- | Grammar parser (as a parser).
grammarP :: P [(Family, TreeID, Tree)]
grammarP = concat <$> every' grammarQ


-- | Grammar parser.
grammarQ :: Q [(Family, TreeID, Tree)]
grammarQ = concat <$> (true //> entryQ)


-- | Entry parser (family + one or more trees).
entryQ :: Q [(Family, TreeID, Tree)]
entryQ = named "entry" `joinR` do
    famName <- first familyQ
    trees <- every' treeQ
    return [(famName, treeID, tree) | (treeID, tree) <- trees]


-- | Tree parser.
familyQ :: Q Family
familyQ = fmap L.toStrict $ named "family" `joinR` first (node name)


-- | Tree parser.
treeQ :: Q (TreeID, Tree)
treeQ = (named "tree" *> attr "id") `join`
  (\tid -> (L.toStrict tid,) <$> first nodeQ)


-- | Node parser.
nodeQ :: Q Tree
nodeQ = (named "node" *> attr "type") `join` ( \typTxt -> R.Node
        <$> first (nonTermQ typTxt)
        <*> every' nodeQ )


-- | Non-terminal parser.
nonTermQ :: L.Text -> Q NonTerm
nonTermQ typ' = joinR (named "narg") $
  first $ joinR (named "fs") $ do
    sym' <- first symQ
    avm' <- avmP
    return $ NonTerm (parseTyp typ') sym' avm'


-- | Syntagmatic value parser.
symQ :: Q Sym
symQ = joinR (named "f" *> hasAttrVal "name" "cat") $
    first $ node (named "sym" *> (L.toStrict <$> attr "value"))


-- | AVM parser.
avmQ :: L.Text -> Q [(Attr, Either Val Var)]
avmQ name' =
  (named "f" *> hasAttrVal "name" name')
  `joinR`
  first (named "fs" `joinR` _avmP)


-- | Simple AVM parser.
_avmP :: P [(Attr, Either Val Var)]
_avmP = every attrValQ


-- | AVM parser.
avmP :: P XAVM
avmP = do
  top' <- optional $ first $ avmQ "top"
  bot' <- optional $ first $ avmQ "bot"
  reg' <- _avmP
  return $ XAVM
    { reg = reg'
    , top = top'
    , bot = bot' }
--   return $
--     [ (Top x, v) | (x, v) <- maybe [] id top' ] ++
--     [ (Bot x, v) | (x, v) <- maybe [] id bot' ] ++
--     concat [ (,v) <$> [Top x, Bot x]
--            | (x, v) <- sim' ]


-- | An attribute/value parser.
attrValQ :: Q (Attr, Either Val Var)
attrValQ = join (named "f" *> attr "name") $ \atr -> do
    valVar <- first $ (Left <$> valQ)
                  <|> (Right <$> varQ)
    return (L.toStrict atr, valVar)


-- | Attribute value parser.
valQ :: Q Val
valQ = simpleValQ <|> altValQ


-- | Attribute simple-value parser.
simpleValQ :: Q Val
simpleValQ = node $ named "sym" *> (S.singleton . L.toStrict <$> attr "value")


-- | Attribute alt-value parser.
altValQ :: Q Val
altValQ = joinR (named "vAlt") $
  S.unions <$> every' simpleValQ


-- | Attribute variable parser.
varQ :: Q Var
varQ = node $ named "sym" *> (L.toStrict <$> attr "varname")


-- | Type parser.
parseTyp :: L.Text -> Type
parseTyp x = case x of
    "std"       -> Std
    "lex"       -> Lex
    "anchor"    -> Anchor
    "foot"      -> Foot
    "nadj"      -> NAdj
    _           -> Other (L.toStrict x)


-- -- | Parse textual contents of the French TAG XML file.
-- parseGrammar :: L.Text -> [(Family, Tree)]
-- parseGrammar =
--     F.concat . evalP grammarP . parseForest . TagSoup.parseTags


-- | Parse textual contents of the French TAG XML file.
parseGrammar
  :: L.Text
  -> M.Map Family (M.Map Tree TreeID)
parseGrammar txt =
  flip E.execState M.empty $
  E.forM_ ts $
  \(family, treeID, tree) ->
    do length (show tree) + length (show treeID) `seq`
         E.modify' (M.insertWith M.union
                                 family
                                 (M.singleton tree treeID))
  where ts = F.concat . evalP grammarP . parseForest . TagSoup.parseTags $ txt


-- | Parse the stand-alone French TAG xml file.
-- readGrammar :: FilePath -> IO [(Family, Tree)]
readGrammar
  :: FilePath
  -> IO (M.Map Family (M.Map Tree TreeID))
readGrammar path = parseGrammar <$> L.readFile path


printGrammar
  :: FilePath
  -> IO ()
printGrammar =
  let printTree (famName, ts) = do
        putStrLn $ "### " ++ show famName ++ " ###"
        E.forM_ (M.toList ts) $ \(tree, treeID) -> do
          putStrLn $ "@@@ " ++ show treeID ++ " @@@"
          putStrLn . R.drawTree . fmap show $ tree
  in  mapM_ printTree . M.toList <=< readGrammar


-------------------------------------------------
-- Utils
-------------------------------------------------


takeLeft :: Either a b -> Maybe a
takeLeft (Left x) = Just x
takeLeft _ = Nothing
