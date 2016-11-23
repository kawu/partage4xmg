{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Morphology parsers.


module NLP.Partage4Xmg.Morph
( Morph (..)
, LemmaRef (..)
, parseMorph
, readMorph
, printMorph
) where


import           Control.Monad       ((<=<))

import qualified Data.Set            as S
import qualified Data.Text.Lazy      as L
import qualified Data.Text.Lazy.IO   as L
import qualified Data.Foldable       as F

import qualified Text.HTML.TagSoup   as TagSoup
import           Text.XML.PolySoup   hiding (P, Q)
import qualified Text.XML.PolySoup   as PolySoup

import qualified NLP.Partage4Xmg.Lexicon as Lex


-------------------------------------------------
-- Data types
-------------------------------------------------


-- | Parsing predicates.
type P a = PolySoup.P (XmlTree L.Text) a
type Q a = PolySoup.Q (XmlTree L.Text) a
type TagQ a = PolySoup.Q (TagSoup.Tag L.Text) a


-- | Morphology entry.
data Morph = Morph
    { wordform :: L.Text
      -- ^ Lexical form of a word
    , analyzes :: S.Set LemmaRef
      -- ^ Possible analyses of the word
    } deriving (Show, Eq, Ord)


-- | A potential analysis of the given wordform.
data LemmaRef = LemmaRef
  { name :: L.Text
    -- ^ Corresponds to `Lex.name`
  , cat :: L.Text
    -- ^ Corresponds to `Lex.cat`
  } deriving (Show, Eq, Ord)


-------------------------------------------------
-- Parsing
-------------------------------------------------


-- | Morph parser.
allP :: P [Morph]
allP = concat <$> every' allQ


-- | Morph parser.
allQ :: Q [Morph]
allQ = true //> morphQ


-- | Entry parser.
morphQ :: Q Morph
morphQ = (named "morph" *> lex) `join` \form -> do
  Morph form . S.fromList <$>
    every' (node lemmaRefQ)
  where
    lex = attr "lex"


lemmaRefQ :: TagQ LemmaRef
lemmaRefQ =
  uncurry LemmaRef <$> (named "lemmaref" *> nameCat)
  where
    nameCat = (,) <$> attr "name" <*> attr "cat"


-- | Parse textual contents of the French TAG XML file.
parseMorph :: L.Text -> [Morph]
parseMorph = F.concat . evalP allP . parseForest . TagSoup.parseTags


-- | Parse the stand-alone French TAG xml file.
readMorph :: FilePath -> IO [Morph]
readMorph path = parseMorph <$> L.readFile path


printMorph :: FilePath -> IO ()
printMorph =
    mapM_ print <=< readMorph
