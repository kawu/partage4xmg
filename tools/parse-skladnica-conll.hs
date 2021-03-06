{-# LANGUAGE OverloadedStrings #-}


-- | Parse Składnica in the CONLL format and output lemma sentences
-- which can be potentially parsed by the plTAG grammar.
--
-- (The CONLL parser used below is borrowed from the Grzegorz
-- Chrupala's `colada` library)


import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.List.Split
import qualified Data.Vector as V

import Control.Monad (forM_)
import System.Environment (getArgs)



-- | @Token@ is a representation of a word, which consists of a number of fields.
type Token = V.Vector L.Text

-- | @Field@ is a part of a word token, such as word form, lemma or POS tag
type Field = L.Text

-- | @Sentence@ is a vector of tokens.
type Sentence = V.Vector Token

-- | @parse text@ returns a lazy list of sentences.
parse :: L.Text -> [Sentence]
parse =
      map V.fromList
    . splitWhen V.null
    . map (V.fromList . L.words)
    . L.lines


-------------------------------------------------
-- Conversion
-------------------------------------------------


-- | Dummy terminal type.
data Term x = Term x deriving (Show)


-- | Process the list (custom scanning function).
process :: ([a] -> Maybe (b, [a])) -> [a] -> [b]
process f xs = case f xs of
  Nothing -> []
  Just (y, xs') -> y : process f xs'


-- | Convert lemma to a grammar-consistent form .
--
-- TODO:
--
--   * ... -> "dots"
convertWord :: [L.Text] -> Maybe (L.Text, [L.Text])
convertWord l = case l of
  [] -> Nothing
  "." : "." : "." : t -> Just ("dots", t)
  "." : t -> Just ("fullstop", t)
  "," : t -> Just ("comma", t)
  "?" : t -> Just ("qmark", t)
  "!" : t -> Just ("excl", t)
  ";" : t -> Just ("semicol", t)
  "-" : t -> Just ("hyph", t)
  x   : t -> Just (x, t)

-- convertWord :: L.Text -> L.Text
-- convertWord "," = "comma"
-- convertWord "." = "fullstop"
-- convertWord "?" = "qmark"
-- convertWord "!" = "excl"
-- convertWord ";" = "semicol"
-- convertWord "-" = "hyph"
-- convertWord x   = x


-- | Convert the sentence to a form appropriate for parsing.
convert :: Sentence -> [Term L.Text]
convert
    -- = map (\x -> Term (convertWord x))
    = map Term . process convertWord
    . map (V.! 2)
    . V.toList

-------------------------------------------------
-- Main
-------------------------------------------------

main :: IO ()
main = do
    [path] <- getArgs
    conts <- L.readFile path
    forM_ (parse conts) $ \sent -> do
         print $ convert sent
