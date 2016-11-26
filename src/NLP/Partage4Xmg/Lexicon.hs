{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Lexicon parsers.


module NLP.Partage4Xmg.Lexicon
( Word (..)
, Entry

-- * Parsing
-- ** XML
, parseLexicon
, readLexicon
, printLexicon
-- ** Lex
, parseLexiconLex
, readLexiconLex
, printLexiconLex
) where


import           Prelude hiding (Word)
import           Control.Applicative ((*>), (<$>), (<*>),
                        optional, (<|>))
import           Control.Monad ((<=<))

import           Data.Maybe          (mapMaybe)
import           Data.List           (groupBy)
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
import qualified Data.Attoparsec.Text.Lazy as A

import           NLP.Partage4Xmg.Grammar (Family)

-- import           NLP.TAG.Vanilla.Core    (View(..))


-- import           NLP.Partage4Xmg.Tree


-------------------------------------------------
-- Data types
-------------------------------------------------


-- | Parsing predicates.
type P a = PolySoup.P (XmlTree L.Text) a
type Q a = PolySoup.Q (XmlTree L.Text) a


-- | Word.
data Word = Word
    { lemma :: T.Text
      -- ^ Lemma -- the base form of the word
    , cat :: T.Text
      -- ^ Category of the word (e.g. "subst")
    } deriving (Show, Eq, Ord)


-- | Lexicon entry.
type Entry = (Word, S.Set Family)


-- -- | Lexicon entry.
-- data Lemma = Lemma
--     { name :: L.Text
--     -- ^ Name of the lemma (i.e. the lemma itself)
--     , cat :: L.Text
--     -- ^ Lemma category (e.g. "subst")
--     , treeFams :: S.Set Family
--     -- ^ Families of which the lemma can be an anchor
--     } deriving (Show)


-------------------------------------------------
-- Parsing
-------------------------------------------------


-- | Lexicon parser.
lexiconP :: P [Entry]
lexiconP = concat <$> every' lexiconQ


-- | Lexicon parser.
lexiconQ :: Q [Entry]
lexiconQ = true //> lemmaQ


-- | Entry parser (family + one or more trees).
lemmaQ :: Q Entry
lemmaQ = (named "lemma" *> nameCat) `join` \(nam, cat') -> do
  let word = Word
        { lemma = L.toStrict nam
        , cat = L.toStrict cat' }
  famSet <- S.fromList <$>
    every' (node famNameQ)
  return (word, famSet)
  where
    nameCat = (,) <$> attr "name" <*> attr "cat"
    famNameQ = getFamName <$> (named "anchor" *> attr "tree_id")


-- | Extract the family name from ID.
getFamName :: L.Text -> Family
getFamName x = case L.stripPrefix "family[@name=" x of
    Nothing -> error "getFamName: wrong prefix"
    Just y  -> case L.stripSuffix "]" y of
        Nothing -> error "getFamName: wrong suffix"
        Just z  -> L.toStrict z


-- | Parse textual contents of the French TAG XML file.
parseLexicon :: L.Text -> [Entry]
parseLexicon =
    F.concat . evalP lexiconP . parseForest . TagSoup.parseTags


-- | Parse the stand-alone French TAG xml file.
readLexicon :: FilePath -> IO [Entry]
readLexicon path = parseLexicon <$> L.readFile path


printLexicon :: FilePath -> IO ()
printLexicon =
    mapM_ print <=< readLexicon


-------------------------------------------------
-- Parsing Lex
-------------------------------------------------


printLexiconLex :: FilePath -> IO ()
printLexiconLex =
    mapM_ print <=< readLexiconLex


-- | Parse the stand-alone French TAG xml file.
readLexiconLex :: FilePath -> IO [Entry]
readLexiconLex path = parseLexiconLex <$> L.readFile path


-- | Parse textual contents of the French TAG XML file.
parseLexiconLex :: L.Text -> [Entry]
parseLexiconLex
  = mapMaybe parseEntry
  . map L.strip
  . L.splitOn "\n\n"
--   where
--     fromGroup xs@(x:_) = Morph (wordform x) (S.unions $ map analyzes xs)
--     fromGroup [] = error "parseMorphMph: impossible happened"
--     comment line = case L.uncons line of
--       Just (x, _) -> x == '%'
--       _ -> False
--     useless line = case L.uncons line of
--       Just (x, _) -> x == '%'
--       Nothing -> True


-- | Return `Nothing` if a comment.
parseEntry :: L.Text -> Maybe Entry
parseEntry
  = fmap analyze
  . sections
  . filter (not . useless . L.strip)
  . L.lines
  where
    sections [] = Nothing
    sections xs = Just (parseSections xs)
    analyze m =
      let word = Word
            { lemma = look "ENTRY" m
            , cat = look "CAT" m }
          famSet = S.fromList . T.words $ look "FAM" m
      in  (word, famSet)
    look x m = case M.lookup x m of
      Nothing -> error $ "parseEntry: " ++ T.unpack x ++ " not found"
      Just y  -> y
    useless line = case L.uncons line of
      Just (x, _) -> x == '%'
      Nothing -> True


-- | Parse the individual lines of an entry and return a map of its sections.
parseSections :: [L.Text] -> M.Map T.Text T.Text
parseSections
  = M.fromList
  . map parseGroup
  . groupBy (\_ y -> not $ "*" `L.isPrefixOf` y)
  where
    parseGroup (x : xs) =
      let (name, val1) = L.breakOn ":" (L.drop 1 x)
       in ( L.toStrict name
          , L.toStrict . L.strip . L.unlines $ L.drop 1 val1 : xs)
