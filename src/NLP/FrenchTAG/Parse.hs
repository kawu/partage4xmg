{-# LANGUAGE OverloadedStrings #-}


-- Parsing French TAG generated from an FrenchTAG XMG metagrammar.


module NLP.FrenchTAG.Parse where


-- import           Control.Applicative       ((*>), (<$), (<$>), (<*), (<|>))
-- import           Control.Monad             (guard)
--
-- import qualified Data.Attoparsec.Text.Lazy as A
-- import qualified Data.Char                 as C
-- import qualified Data.Either               as E
-- import qualified Data.Map                  as M
-- import           Data.Maybe                (catMaybes)
-- import qualified Data.Text.Lazy            as L
-- import qualified Data.Text.Lazy.IO         as L



import           Control.Applicative ((*>), (<$>), (<*>))
import           Control.Monad ((<=<))

import qualified Data.Foldable       as F
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as L
import qualified Data.Text.Lazy.IO   as L
import qualified Data.Tree           as R

import qualified Text.HTML.TagSoup   as TagSoup
import           Text.XML.PolySoup   hiding (P, Q)
import qualified Text.XML.PolySoup   as PolySoup


-- import           NLP.FrenchTAG.Tree


-------------------------------------------------
-- Data types
-------------------------------------------------


-- | Parsing predicates.
type P a = PolySoup.P (XmlTree L.Text) a
type Q a = PolySoup.Q (XmlTree L.Text) a


-- | Syntagmatic symbol.
type Sym = L.Text


-- | FrenchTAG tree.
type Tree = R.Tree Sym


-------------------------------------------------
-- Parsing
-------------------------------------------------


-- | Grammar parser (as a parser).
grammarP :: P [Tree]
grammarP = concat <$> every' grammarQ


-- | Grammar parser.
grammarQ :: Q [Tree]
grammarQ = true //> treeQ


-- | Tree parser.
treeQ :: Q Tree
treeQ = named "tree" `joinR` first nodeQ


-- | Node parser.
nodeQ :: Q Tree
nodeQ = named "node" `joinR` ( R.Node
        <$> first symQ
        <*> every' nodeQ )


-- | Syntagmatic value finder.
symQ :: Q Sym
symQ = joinR (named "narg") $ first $
       joinR (named "fs") $ first $
       joinR (named "f" *> hasAttrVal "name" "cat") $ first $
       node (named "sym" *> attr "value")


-- | Parse textual contents of the French TAG XML file.
parseGrammar :: L.Text -> [Tree]
parseGrammar =
    F.concat . evalP grammarP . parseForest . TagSoup.parseTags


-- | Parse the stand-alone French TAG xml file.
readGrammar :: FilePath -> IO [Tree]
readGrammar path = parseGrammar <$> L.readFile path


printGrammar :: FilePath -> IO ()
printGrammar =
  let printTree = putStrLn . R.drawTree . fmap L.unpack
  in mapM_ printTree <=< readGrammar
