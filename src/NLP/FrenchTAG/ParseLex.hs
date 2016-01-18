{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Lexicon parsers.


module NLP.FrenchTAG.ParseLex where


import           Control.Applicative ((*>), (<$>), (<*>),
                        optional, (<|>))
import           Control.Monad ((<=<))

import qualified Data.Foldable       as F
-- import qualified Data.Text           as T
import qualified Data.Text.Lazy      as L
import qualified Data.Text.Lazy.IO   as L
import qualified Data.Tree           as R
import qualified Data.Map.Strict     as M

import qualified Text.HTML.TagSoup   as TagSoup
import           Text.XML.PolySoup   hiding (P, Q)
import qualified Text.XML.PolySoup   as PolySoup

-- import           NLP.TAG.Vanilla.Core    (View(..))


-- import           NLP.FrenchTAG.Tree


-------------------------------------------------
-- Data types
-------------------------------------------------


-- | Parsing predicates.
type P a = PolySoup.P (XmlTree L.Text) a
type Q a = PolySoup.Q (XmlTree L.Text) a


-- | Lexicon entry.
data Lemma = Lemma
    { name :: L.Text
    -- ^ Name of the lemma (i.e. the lemma itself)
    , cat :: L.Text
    -- ^ Lemma category (e.g. "subst")
    , treeFams :: [L.Text]
    -- ^ Families of which the lemma can be an anchor
    } deriving (Show)


-------------------------------------------------
-- Parsing
-------------------------------------------------


-- | Lexicon parser.
lexiconP :: P [Lemma]
lexiconP = concat <$> every' lexiconQ


-- | Lexicon parser.
lexiconQ :: Q [Lemma]
lexiconQ = true //> lemmaQ


-- | Entry parser (family + one or more trees).
lemmaQ :: Q Lemma
lemmaQ = (named "lemma" *> nameCat) `join` \(nam, cat') -> do
    Lemma nam cat' <$>
        every' (node famNameQ)
  where
    nameCat = (,) <$> attr "name" <*> attr "cat"
    famNameQ = getFamName <$> (named "anchor" *> attr "tree_id")


-- | Extract the family name from ID.
getFamName :: L.Text -> L.Text
getFamName x = case L.stripPrefix "family[@name=" x of
    Nothing -> error "getFamName: wrong prefix"
    Just y  -> case L.stripSuffix "]" y of
        Nothing -> error "getFamName: wrong suffix"
        Just z  -> z


-- | Parse textual contents of the French TAG XML file.
parseLexicon :: L.Text -> [Lemma]
parseLexicon =
    F.concat . evalP lexiconP . parseForest . TagSoup.parseTags


-- | Parse the stand-alone French TAG xml file.
readLexicon :: FilePath -> IO [Lemma]
readLexicon path = parseLexicon <$> L.readFile path


printLexicon :: FilePath -> IO ()
printLexicon =
    mapM_ print <=< readLexicon
