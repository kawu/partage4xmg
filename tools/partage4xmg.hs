{-# LANGUAGE RecordWildCards #-}


-- import           Data.Monoid (mempty)
import           Options.Applicative
import qualified Data.Char as C


-- import qualified NLP.Partage4Xmg.Automat as A
import qualified NLP.Partage4Xmg.Build as B
import qualified NLP.Partage4Xmg.Parse as P
import qualified NLP.Partage4Xmg.ParseLex as L
-- import qualified NLP.Partage4Xmg.Gen as G
-- import qualified NLP.Partage4Xmg.GenLex as GL
import qualified NLP.Partage4Xmg.Stats as S
-- import qualified NLP.Partage4Xmg.Select as S


--------------------------------------------------
-- Global Options
--------------------------------------------------


data Command
    = Build B.BuildData
    -- ^ Build an automaton
    -- | Parse -- ParseOptions
    | Parse FilePath
    -- ^ Only parse and show the input grammar
    | Trees B.BuildData
    -- ^ Only parse and show elementary grammar trees, with no FSs
    | Stats B.BuildData S.StatCfg
    -- ^ Parse sentences from stdin (one sentence per line)
    | Lexicon FilePath
    -- ^ Parse and print the lexicon
    | Print B.BuildData
    -- ^ Print trees (lexicon allowed, FSs removed)
    | Rules B.BuildData
    -- ^ Experimental mode


-- parseCompression :: Monad m => String -> m B.Compress
-- parseCompression s = return $ case map C.toLower s of
--     'a':_       -> B.Auto    -- Automaton
--     't':_       -> B.Trie    -- Trie
--     'l':_       -> B.List    -- List
--     's':_       -> B.SetAuto -- Set of automata
--     'x':_       -> B.SetTrie -- Set of tries
--     _           -> B.Auto


buildOptions :: Parser B.BuildData
buildOptions = B.BuildData
  <$> strOption
        ( long "grammar"
       <> short 'g'
       <> metavar "FILE"
       <> help "Grammar .xml file" )
  <*> strOption
        ( long "lexicon"
       <> short 'l'
       <> metavar "FILE"
       <> help "Lexicon .xml file" )
  <*> ( optional . strOption )
        ( long "auxiliary"
       <> short 'a'
       <> metavar "FILE"
       <> help "Auxiliary grammar/lexicon .xml file" )


--------------------------------------------------
-- Parse options
--------------------------------------------------


parseOptions :: Parser Command
parseOptions = Parse
  <$> strOption
     ( long "grammar"
    <> short 'g'
    <> metavar "FILE"
    <> help "Grammar .xml file" )


--------------------------------------------------
-- Lexicon options
--------------------------------------------------


lexicOptions :: Parser Command
lexicOptions = Lexicon
  <$> strOption
     ( long "lexicon"
    <> short 'l'
    <> metavar "FILE"
    <> help "Lexicon .xml file" )


--------------------------------------------------
-- Stats options
--------------------------------------------------


statsOptions :: Parser Command
statsOptions = Stats
    <$> buildOptions
    <*> (S.StatCfg
          <$> option
                  ( Just <$> auto )
                  ( metavar "MAX-SIZE"
                 <> value Nothing
                 <> long "max-size"
                 <> short 'm' )
          <*> strOption
                ( metavar "START-SYM"
               <> long "start-sym"
               <> short 's' )
          <*> option
            auto
             ( metavar "PRINT-PARSED"
               <> value 0
               <> long "print-parsed"
               <> short 'p' ) )


--------------------------------------------------
-- Global options
--------------------------------------------------


opts :: Parser Command
opts = subparser
        ( command "build"
            (info (helper <*> (Build <$> buildOptions))
                (progDesc "Build automaton from the grammar")
                )
        <> command "parse"
            (info (helper <*> parseOptions)
                (progDesc "Parse the input grammar file")
                )
        <> command "trees"
            (info (helper <*> (Trees <$> buildOptions))
                (progDesc "Show elementary trees, no FSs")
                )
        <> command "stats"
            (info (helper <*> statsOptions)
                (progDesc "Parse sentences from stdin")
                )
        <> command "lexicon"
            (info (helper <*> lexicOptions)
                (progDesc "Parse and print the lexicon")
                )
        <> command "print"
            (info (helper <*> (Print <$> buildOptions))
                (progDesc "Parse and print the lexicon")
                )
        <> command "rules"
            (info (helper <*> (Rules <$> buildOptions))
                (progDesc "Print standard rules; experimental mode")
                )
        )


-- | Run program depending on the cmdline arguments.
run :: Command -> IO ()
run cmd = case cmd of
         Build buildData ->
            B.printAuto buildData
         Trees buildData ->
            B.printTrees buildData
         Parse grammarPath ->
            P.printGrammar grammarPath
         Print buildData ->
            B.printTrees buildData
         Stats buildData cfg ->
            S.statsOn cfg buildData
         Lexicon lexPath ->
            L.printLexicon lexPath
         Rules buildData ->
            B.printRules buildData


main :: IO ()
main =
    execParser optsExt >>= run
  where
    optsExt = info (helper <*> opts)
       ( fullDesc
      <> progDesc "Parsing with XMG grammars"
      <> header "partage4xmg" )
