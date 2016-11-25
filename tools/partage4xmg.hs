{-# LANGUAGE RecordWildCards #-}


-- import           Data.Monoid (mempty)
import           Options.Applicative
import qualified Data.Char as C


-- import qualified NLP.Partage4Xmg.Automat as A
import qualified NLP.Partage4Xmg.Build as B
import qualified NLP.Partage4Xmg.Grammar as G
import qualified NLP.Partage4Xmg.Lexicon as L
import qualified NLP.Partage4Xmg.Morph as Morph
import qualified NLP.Partage4Xmg.Ensemble as E
import qualified NLP.Partage4Xmg.Parse as P
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
    | Grammar FilePath
    -- ^ Parse and show the input grammar
    | Lexicon FilePath
    -- ^ Parse and print lexicon
    | Morph FilePath Bool
    -- ^ Parse and print morphology
    | Trees B.BuildData
    -- ^ Only parse and show elementary grammar trees, with no FSs
    | Stats B.BuildData S.StatCfg
    -- ^ Parse sentences from stdin (one sentence per line)
    | Print B.BuildData
    -- ^ Print trees (lexicon allowed, FSs removed)
    | Rules B.BuildData
    -- ^ Experimental mode
    | Parse E.GramCfg P.ParseCfg
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


gramCfgOptions :: Parser E.GramCfg
gramCfgOptions = E.GramCfg
  <$> strOption
        ( long "morph"
       <> short 'm'
       <> metavar "FILE"
       <> help "Morphology .xml file" )
  <*> strOption
        ( long "lexicon"
       <> short 'l'
       <> metavar "FILE"
       <> help "Lexicon .xml file" )
  <*> strOption
        ( long "grammar"
       <> short 'g'
       <> metavar "FILE"
       <> help "Grammar .xml file" )


parseCfgOptions :: Parser P.ParseCfg
parseCfgOptions = P.ParseCfg
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
  <*> option auto
     ( metavar "PRINT-PARSED"
       <> value 10
       <> long "print-parsed"
       <> short 'p' )
  <*> switch
     ( -- metavar "USE-FEATURES"
          long "use-features"
       -- <> value True
       <> help "Use FS unification"
       <> short 'u' )
  <*> switch
     (    long "derivations"
       <> short 'd'
       <> help "Print derivations instead of derived trees" )


parseOptions :: Parser Command
parseOptions = Parse <$> gramCfgOptions <*> parseCfgOptions


--------------------------------------------------
-- Grammar options
--------------------------------------------------


grammarOptions :: Parser Command
grammarOptions = Grammar
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
-- Morph options
--------------------------------------------------


morphOptions :: Parser Command
morphOptions = Morph
  <$> strOption
     ( long "morph"
    <> short 'm'
    <> metavar "FILE"
    <> help "Morphology .xml file" )
  <*> switch
     ( long "mph"
    <> short 'h'
    <> help "Use the alternative .mph format" )


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
        <> command "grammar"
            (info (helper <*> grammarOptions)
                (progDesc "Parse the input grammar file")
                )
        <> command "lexicon"
            (info (helper <*> lexicOptions)
                (progDesc "Parse and print the lexicon")
                )
        <> command "morph"
            (info (helper <*> morphOptions)
                (progDesc "Parse and print the morphology file")
                )
        <> command "trees"
            (info (helper <*> (Trees <$> buildOptions))
                (progDesc "Show elementary trees, no FSs")
                )
        <> command "stats"
            (info (helper <*> statsOptions)
                (progDesc "Parse sentences from stdin")
                )
        <> command "print"
            (info (helper <*> (Print <$> buildOptions))
                (progDesc "Parse and print the lexicon")
                )
        <> command "rules"
            (info (helper <*> (Rules <$> buildOptions))
                (progDesc "Print standard rules; experimental mode")
                )
        <> command "parse"
            (info (helper <*> parseOptions)
                (progDesc "Parse stdin")
                )
        )


-- | Run program depending on the cmdline arguments.
run :: Command -> IO ()
run cmd = case cmd of
         Build buildData ->
            B.printAuto buildData
         Trees buildData ->
            B.printTrees buildData
         Grammar grammarPath ->
            G.printGrammar grammarPath
         Lexicon lexPath ->
            L.printLexicon lexPath
         Morph path useMph ->
           if useMph
           then Morph.printMorphMph path
           else Morph.printMorph path
         Print buildData ->
            B.printTrees buildData
         Stats buildData cfg ->
            S.statsOn cfg buildData
         Rules buildData ->
            B.printRules buildData
         Parse gramCfg parseCfg ->
            P.parseAll parseCfg gramCfg


main :: IO ()
main =
    execParser optsExt >>= run
  where
    optsExt = info (helper <*> opts)
       ( fullDesc
      <> progDesc "Parsing with XMG grammars"
      <> header "partage4xmg" )
