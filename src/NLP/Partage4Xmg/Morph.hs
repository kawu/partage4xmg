{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Morphology.


module NLP.Partage4Xmg.Morph
( Morph (..)
, Ana (..)
, lemma
-- * Parsing
-- ** From XML
, parseMorph
, readMorph
, printMorph
-- ** From .mph
, parseMorphMph
, readMorphMph
, printMorphMph
) where


import Debug.Trace (trace)

import           Control.Applicative ((<|>), many, optional)
import           Control.Monad       ((<=<))

import           Data.Function       (on)
import           Data.List           (groupBy)
import qualified Data.Set            as S
import qualified Data.Map.Strict     as M
-- import qualified Data.Char           as C
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as L
import qualified Data.Text.Lazy.IO   as L
import qualified Data.Foldable       as F
import qualified Data.Attoparsec.Text.Lazy as A

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
    { wordform :: T.Text
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
lemma :: Ana -> T.Text
lemma = Lex.lemma . word


-------------------------------------------------
-- Parsing XML
-------------------------------------------------


-- | Morph parser.
allP :: P [Morph]
allP = concat <$> every' allQ


-- | Morph parser.
allQ :: Q [Morph]
allQ = true //> morphQ


-- | Entry parser.
morphQ :: Q Morph
morphQ = (named "morph" *> attr "lex") `join` \form -> do
  Morph (L.toStrict form) . S.fromList <$>
    every' lemmaRefQ


lemmaRefQ :: Q Ana
lemmaRefQ = do
  (named "lemmaref" *> nameCatQ) `join` \(name', cat') -> do
    avm' <- first avmQ
    let word' = Lex.Word
          { Lex.lemma = L.toStrict name'
          , Lex.cat = L.toStrict cat' }
    return $ Ana
      { word = word'
      , avm = avm' }
  where
    nameCatQ = (,) <$> attr "name" <*> attr "cat"


-- | AVM parser.
avmQ :: Q G.AVM
avmQ = M.fromList <$> joinR (named "fs") (every' G.attrValQ)


-- -- | An attribute/value parser.
-- attrValQ :: Q (G.Attr, Either G.Val G.Var)
-- attrValQ = join (named "f" *> attr "name") $ \atr -> do
--   valVar <- first $ (Left <$> valQ)
--                 <|> (Right <$> varQ)
--   return (atr, valVar)
--   where
--     valQ = node $ named "sym" *> attr "value"
--     varQ = node $ named "sym" *> attr "varname"


-- | Parse textual contents of the French TAG XML file.
parseMorph :: L.Text -> [Morph]
parseMorph = F.concat . evalP allP . parseForest . TagSoup.parseTags


-- | Parse the stand-alone French TAG xml file.
readMorph :: FilePath -> IO [Morph]
readMorph path = parseMorph <$> L.readFile path


printMorph :: FilePath -> IO ()
printMorph =
    mapM_ print <=< readMorph


-------------------------------------------------
-- Parsing .mph
-------------------------------------------------


printMorphMph :: FilePath -> IO ()
printMorphMph =
    mapM_ print <=< readMorphMph


-- | Parse the stand-alone French TAG xml file.
readMorphMph :: FilePath -> IO [Morph]
readMorphMph path = parseMorphMph <$> L.readFile path


-- | Parse the stand-alone French TAG xml file.
parseMorphMph :: L.Text -> [Morph]
parseMorphMph
  = map fromGroup
  . groupBy ((==) `on` wordform)
  . map parseMph
  . filter (not . useless)
  . map L.strip
  . L.lines
  where
    fromGroup xs@(x:_) = Morph (wordform x) (S.unions $ map analyzes xs)
    fromGroup [] = error "parseMorphMph: impossible happened"
    useless line = case L.uncons line of
      Just (x, _) -> x == '%'
      Nothing -> True


-- | Parse .mph line.
parseMph :: L.Text -> Morph
parseMph line =
  check . A.maybeResult . A.parse mphA $ line
  where
    check Nothing  = trace (show line) $ error "parseMph: no parse"
    check (Just x) = x
    mphA = do
      form <- wordA <* many_ A.space
      base <- wordA <* many_ A.space
      pos  <- wordA <* many_ A.space
      avmTxt <- L.strip <$> A.takeLazyText
      let ana = Ana
            { word = Lex.Word
              { Lex.lemma = L.toStrict base
              , Lex.cat = L.toStrict pos }
            , avm = avm0 }
          avm0 = parseAvm avmTxt
      return $ Morph
        { wordform = L.toStrict form
        , analyzes = S.singleton ana }


-- | Parse AVM.
parseAvm :: L.Text -> G.AVM
parseAvm x =
  check . A.maybeResult . A.parse avmA $ x
  where
    check Nothing  = trace (show x) $ error "parseAvm: no parse"
    check (Just x) = x


-- | AVM attoparsec.
avmA :: A.Parser G.AVM
avmA = do
  xs <- between "[" "]" $ many (attrValA <* optional (A.char ';'))
  return $ M.fromList xs
  where
    between p q x = p *> x <* q


attrValA :: A.Parser (T.Text, Either G.Val G.Var)
attrValA = do
  x <- many_ A.space *> idenA
  many_ A.space *> A.char '='
  y <- valA
  return (L.toStrict x, Left y)


valA :: A.Parser G.Val
valA = do
  xs <- (many_ A.space *> idenA <* many_ A.space) `A.sepBy1'` (A.char '|')
  return . S.fromList . map L.toStrict $ xs


-- | A single identifier or value.
idenA :: A.Parser L.Text
idenA = L.pack <$> many (A.letter <|> A.digit <|> A.char '+' <|> A.char '-')


-- | A word.
wordA :: A.Parser L.Text
wordA = L.pack <$> many (A.letter <|> A.digit <|> A.char '\'')
