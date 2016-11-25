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
, Sym
, Attr
, Val
, Var

-- * Functions
, parseGrammar
, readGrammar
, printGrammar

-- * Utils
, attrValQ
) where


import           Control.Applicative ((*>), (<$>), (<*>),
                        optional, (<|>))
import           Control.Monad ((<=<))

import qualified Data.Foldable       as F
-- import qualified Data.Text           as T
import qualified Data.Text.Lazy      as L
import qualified Data.Text.Lazy.IO   as L
import qualified Data.Tree           as R
import qualified Data.Set            as S
import qualified Data.Map.Strict     as M

import qualified Text.HTML.TagSoup   as TagSoup
import           Text.XML.PolySoup   hiding (P, Q)
import qualified Text.XML.PolySoup   as PolySoup

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
type Sym = L.Text


-- | Attribute.
type Attr = L.Text


-- | Attribute value.
type Val = S.Set L.Text


-- | Variable.
type Var = L.Text


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
type SubType = L.Text


-- | Non-terminal.
data NonTerm = NonTerm
    { typ   :: Type
    , sym   :: Sym
    , avm   :: AVM }
--     , top   :: Maybe AVM
--     , bot   :: Maybe AVM }
    deriving (Show, Eq, Ord)


-- | Partage4Xmg tree.
type Tree = R.Tree NonTerm


-- | Name of a tree family.
type Family = L.Text


-------------------------------------------------
-- Parsing
-------------------------------------------------


-- | Grammar parser (as a parser).
grammarP :: P [(Family, Tree)]
grammarP = concat <$> every' grammarQ


-- | Grammar parser.
grammarQ :: Q [(Family, Tree)]
grammarQ = concat <$> (true //> entryQ)


-- | Entry parser (family + one or more trees).
entryQ :: Q [(Family, Tree)]
entryQ = named "entry" `joinR` do
    famName <- first familyQ
    trees <- every' treeQ
    return [(famName, t) | t <- trees]


-- | Tree parser.
familyQ :: Q Family
familyQ = named "family" `joinR` first (node name)


-- | Tree parser.
treeQ :: Q Tree
treeQ = named "tree" `joinR` first nodeQ


-- | Node parser.
nodeQ :: Q Tree
nodeQ = (named "node" *> attr "type") `join` ( \typTxt -> R.Node
        <$> first (nonTermQ typTxt)
        <*> every' nodeQ )


-- | Non-terminal parser.
nonTermQ :: L.Text -> Q NonTerm
nonTermQ typ' = joinR (named "narg") $
  first $ joinR (named "fs") $ do
    -- sym' <- first symQ
    avm' <- avmP
    -- top' <- optional $ first $ avmQ "top"
    -- bot' <- optional $ first $ avmQ "bot"
    -- return $ NonTerm (parseTyp typ') sym' avm'
    let sym' = pick (takeLeft =<< M.lookup "cat" avm')
    return $ NonTerm (parseTyp typ') sym' avm'
  where
    pick (Just s) = case S.toList s of
      [x] -> x
      _ -> error "nonTermQ: impossible happened"
    pick Nothing = error "nonTermQ: impossible happened 2"


-- | Syntagmatic value parser.
symQ :: Q Sym
symQ = joinR (named "f" *> hasAttrVal "name" "cat") $
    first $ node (named "sym" *> attr "value")


-- -- | AVM parser.
-- avmQ :: L.Text -> Q AVM
-- avmQ name' = joinR (named "f" *> hasAttrVal "name" name') $
--     first $ joinR (named "fs") $
--         M.fromList <$> every attrValQ


-- | AVM parser.
avmP :: P AVM
avmP = M.fromList <$> every attrValQ


-- | An attribute/value parser.
attrValQ :: Q (Attr, Either Val Var)
attrValQ = join (named "f" *> attr "name") $ \atr -> do
    valVar <- first $ (Left <$> valQ)
                  <|> (Right <$> varQ)
    return (atr, valVar)


-- | Attribute value parser.
valQ :: Q Val
valQ = node $ named "sym" *> (S.singleton <$> attr "value")


-- | Attribute variable parser.
varQ :: Q Var
varQ = node $ named "sym" *> attr "varname"


-- | Type parser.
parseTyp :: L.Text -> Type
parseTyp x = case x of
    "std"       -> Std
    "lex"       -> Lex
    "anchor"    -> Anchor
    "foot"      -> Foot
    _           -> Other x


-- | Parse textual contents of the French TAG XML file.
parseGrammar :: L.Text -> [(Family, Tree)]
parseGrammar =
    F.concat . evalP grammarP . parseForest . TagSoup.parseTags


-- | Parse the stand-alone French TAG xml file.
readGrammar :: FilePath -> IO [(Family, Tree)]
readGrammar path = parseGrammar <$> L.readFile path


printGrammar :: FilePath -> IO ()
printGrammar =
  let printTree (famName, t) = do
        putStrLn $ "### " ++ show famName ++ " ###"
        putStrLn . R.drawTree . fmap show $ t
  in  mapM_ printTree <=< readGrammar


-------------------------------------------------
-- Utils
-------------------------------------------------


takeLeft :: Either a b -> Maybe a
takeLeft (Left x) = Just x
takeLeft _ = Nothing
