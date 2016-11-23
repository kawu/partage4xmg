{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Morphology.


module NLP.Partage4Xmg.Morph
( Morph (..)
, Ana (..)
, lemma
, parseMorph
, readMorph
, printMorph
) where


import           Control.Applicative ((<|>))
import           Control.Monad       ((<=<))

import qualified Data.Set            as S
import qualified Data.Map.Strict     as M
import qualified Data.Text.Lazy      as L
import qualified Data.Text.Lazy.IO   as L
import qualified Data.Foldable       as F

import qualified Text.HTML.TagSoup   as TagSoup
import           Text.XML.PolySoup   hiding (P, Q, name)
import qualified Text.XML.PolySoup   as PolySoup

import qualified NLP.Partage4Xmg.Lexicon as Lex
import qualified NLP.Partage4Xmg.Grammar as G


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
      -- ^ Surface form of a word
    , analyzes :: S.Set Ana
      -- ^ Possible analyzes of the `wordform`
    } deriving (Show, Eq, Ord)


-- | A potential analysis of the given wordform.
data Ana = Ana
  { word :: Lex.Word
    -- ^ The corresponding word
  , avm :: G.AVM
    -- ^ AVM with no variables corresponding to the given analysis of the wordform
  } deriving (Show, Eq, Ord)


-- | Get the lemma of the analysis.
lemma :: Ana -> L.Text
lemma = Lex.lemma . word


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
    every' lemmaRefQ
  where
    lex = attr "lex"


lemmaRefQ :: Q Ana
lemmaRefQ = do
  (named "lemmaref" *> nameCatQ) `join` \(name', cat') -> do
    avm' <- first avmQ
    let word' = Lex.Word
          { Lex.lemma = name'
          , Lex.cat = cat' }
    return $ Ana
      { word = word'
      , avm = avm' }
  where
    nameCatQ = (,) <$> attr "name" <*> attr "cat"


-- | AVM parser.
avmQ :: Q G.AVM
avmQ = M.fromList <$> joinR (named "fs") (every' attrValQ)


-- | An attribute/value parser.
attrValQ :: Q (G.Attr, Either G.Val G.Var)
attrValQ = join (named "f" *> attr "name") $ \atr -> do
  valVar <- first $ (Left <$> valQ)
                <|> (Right <$> varQ)
  return (atr, valVar)
  where
    valQ = node $ named "sym" *> attr "value"
    varQ = node $ named "sym" *> attr "varname"


-- | Parse textual contents of the French TAG XML file.
parseMorph :: L.Text -> [Morph]
parseMorph = F.concat . evalP allP . parseForest . TagSoup.parseTags


-- | Parse the stand-alone French TAG xml file.
readMorph :: FilePath -> IO [Morph]
readMorph path = parseMorph <$> L.readFile path


printMorph :: FilePath -> IO ()
printMorph =
    mapM_ print <=< readMorph
