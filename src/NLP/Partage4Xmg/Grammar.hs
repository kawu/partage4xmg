{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Parsing TAG grammars consistent with the XMG format.


module NLP.Partage4Xmg.Grammar
(
-- * Types
  Family
, Tree
, NonTerm (..)
, Type (..)
, SubType
, AVM
, AVM2 (..)
, topOnly
, botOnly
, Sym
, Attr
, Val
, Var

-- * Functions
, parseGrammar
, readGrammar
, printGrammar
-- ** Parsing AVMs
, avmP1
, avmP2

-- * Utils
, attrValQ
, -- onEps
) where


import Debug.Trace (trace)

import qualified Control.Monad.State.Strict as E
import           Control.Arrow       (second)
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


-- | Attribute value.
type Val = S.Set T.Text


-- | Variable.
type Var = T.Text


-- | Attribute-value matrix.
type AVM = M.Map Attr (Either Val Var)


-- | Non-terminal/node type.
data Type
    = Std
    | Foot
    | Anchor
    | Lex
    | Other SubType
    deriving (Show, Eq, Ord)


-- | Node subtype (e.g. subst, nadj, whatever they mean...)
type SubType = T.Text


-- | Non-terminal or terminal.
-- TODO: change the name.
data NonTerm avm = NonTerm
    { typ   :: Type
    , sym   :: Sym
    , avm   :: avm }
--     , phonEps :: Bool
--       -- ^ Phonetically empty?
--     , avm   :: AVM }
--     , top   :: Maybe AVM
--     , bot   :: Maybe AVM }
    deriving (Show, Eq, Ord)


data AVM2 = AVM2
  { top :: Maybe AVM
    -- ^ The top AVM
  , bot :: Maybe AVM
    -- ^ The bottom AVM
  } deriving (Show, Eq, Ord)


-- | Create a complex AVM with the top part only.
topOnly :: AVM -> AVM2
topOnly avm = AVM2 {top = Just avm, bot = Nothing}


-- | Create a complex AVM with the bottom part only.
botOnly :: AVM -> AVM2
botOnly avm = AVM2 {top = Nothing, bot = Just avm}


-- | Partage4Xmg tree.
type Tree avm = R.Tree (NonTerm avm)


-- | Name of a tree family.
type Family = T.Text


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
grammarP :: P avm -> P [(Family, Tree avm)]
grammarP avmP = concat <$> every' (grammarQ avmP)


-- | Grammar parser.
grammarQ :: P avm -> Q [(Family, Tree avm)]
grammarQ avmP = concat <$> (true //> entryQ avmP)


-- | Entry parser (family + one or more trees).
entryQ :: P avm -> Q [(Family, Tree avm)]
entryQ avmP = named "entry" `joinR` do
    famName <- first familyQ
    trees <- every' (treeQ avmP)
    return [(famName, t) | t <- trees]


-- | Tree parser.
familyQ :: Q Family
familyQ = fmap L.toStrict $ named "family" `joinR` first (node name)


-- | Tree parser.
treeQ :: P avm -> Q (Tree avm)
treeQ avmP = named "tree" `joinR` first (nodeQ avmP)


-- | Node parser.
nodeQ :: P avm -> Q (Tree avm)
nodeQ avmP = (named "node" *> attr "type") `join` ( \typTxt -> R.Node
        <$> first (nonTermQ avmP typTxt)
        <*> every' (nodeQ avmP) )


-- | Non-terminal parser.
nonTermQ :: P avm -> L.Text -> Q (NonTerm avm)
nonTermQ avmP typ' = joinR (named "narg") $
  first $ joinR (named "fs") $ do
    sym' <- first symQ
    avm' <- avmP
    return $ NonTerm (parseTyp typ') sym' avm'
--     top' <- optional $ first $ avmQ avmP "top"
--     bot' <- optional $ first $ avmQ avmP "bot"
--     return $ NonTerm (parseTyp typ') sym' $ AVM2
--       { top = top'
--       , bot = bot' }


-- | Syntagmatic value parser.
symQ :: Q Sym
symQ = joinR (named "f" *> hasAttrVal "name" "cat") $
    first $ node (named "sym" *> (L.toStrict <$> attr "value"))


-- | AVM parser.
avmQ :: L.Text -> Q AVM
avmQ name' =
  (named "f" *> hasAttrVal "name" name')
  `joinR`
  first (named "fs" `joinR` avmP1)


-- | AVM parser.
avmP1 :: P AVM
avmP1 = M.fromList <$> every attrValQ


-- | AVM2 parser.
avmP2 :: P AVM2
avmP2 = do
  top' <- optional $ first $ avmQ "top"
  bot' <- optional $ first $ avmQ "bot"
  return $ AVM2
    { top = top'
    , bot = bot' }


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
    _           -> Other (L.toStrict x)


-- -- | Parse textual contents of the French TAG XML file.
-- parseGrammar :: L.Text -> [(Family, Tree)]
-- parseGrammar =
--     F.concat . evalP grammarP . parseForest . TagSoup.parseTags


-- | Parse textual contents of the French TAG XML file.
parseGrammar
  :: (Ord avm, Show avm)
  => P avm
  -> L.Text
  -> M.Map Family (S.Set (Tree avm))
parseGrammar avmP txt =
  flip E.execState M.empty $ E.forM_ ts $ \(family, tree) -> do
    length (show tree) `seq`
      E.modify' (M.insertWith S.union family
                 (S.singleton tree))
  where
    ts = F.concat . evalP (grammarP avmP) . parseForest . TagSoup.parseTags $ txt


-- | Parse the stand-alone French TAG xml file.
-- readGrammar :: FilePath -> IO [(Family, Tree)]
readGrammar
  :: (Ord avm, Show avm)
  => P avm
  -> FilePath
  -> IO (M.Map Family (S.Set (Tree avm)))
readGrammar avmP path = parseGrammar avmP <$> L.readFile path


printGrammar
  :: (Ord avm, Show avm)
  => P avm
  -> FilePath
  -> IO ()
printGrammar avmP =
  let printTree (famName, ts) = do
        putStrLn $ "### " ++ show famName ++ " ###"
        E.forM_ ts $ putStrLn . R.drawTree . fmap show
  in  mapM_ printTree . M.toList <=< readGrammar avmP


-------------------------------------------------
-- Utils
-------------------------------------------------


takeLeft :: Either a b -> Maybe a
takeLeft (Left x) = Just x
takeLeft _ = Nothing
