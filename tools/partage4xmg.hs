{-# LANGUAGE RecordWildCards #-}


-- import           Data.Monoid (mempty)
import           Options.Applicative
import qualified Data.Char as C


-- import qualified NLP.Partage4Xmg.Automat as A
import qualified NLP.Partage4Xmg.Build as B
import qualified NLP.Partage4Xmg.Parse as P
import qualified NLP.Partage4Xmg.ParseLex as L
import qualified NLP.Partage4Xmg.Gen as G
-- import qualified NLP.Partage4Xmg.GenLex as GL
import qualified NLP.Partage4Xmg.Stats as S
import qualified NLP.Partage4Xmg.Select as S


--------------------------------------------------
-- Global Options
--------------------------------------------------


data Command
    = Build B.BuildData B.BuildCfg
    -- ^ Build an automaton
    -- | Parse -- ParseOptions
    | Parse FilePath
    -- ^ Only parse and show the input grammar
    | Gen B.BuildData Int
    -- ^ Generate size-bounded derived trees
    | GenRand B.BuildData G.GenConf
    -- ^ Randomly generate derived sentences
    | Stats B.BuildData S.StatCfg
    -- ^ Parse sentences from stdin (one sentence per line)
    | Select S.SelectCfg
    -- ^ Randomly select sentences from stdin (given number
    -- per sentence length)
    | Lexicon FilePath
    -- ^ Parse and print the lexicon
    | Print B.BuildData
    -- ^ Print trees (lexicon allowed, FSs removed)
    | Rules B.BuildData
    -- ^ Experimental mode
    | Weights B.BuildData
    -- ^ Print weighted rules
    | Tmp B.BuildData String
    -- ^ Experimental mode


-- --------------------------------------------------
-- -- Build options
-- --------------------------------------------------
--
--
-- data BuildOptions
--     = Automat
--     | Trie
--     | List
--     deriving (Show, Read)
--
--
-- -- parseBuildOptions :: Monad m => String -> m BuildOptions
-- parseBuildOptions :: Monad m => String -> m BuildOptions
-- parseBuildOptions s = return $ case map C.toLower s of
--     'a':_       -> Automat
--     't':_       -> Trie
--     'l':_       -> List
--     _           -> Automat
--
--
-- buildOptions :: Parser Command
-- buildOptions = Build
--     <$> argument
--             -- auto
--             ( str >>= parseBuildOptions )
--             ( metavar "BUILD-TYPE"
--            <> value Automat
--            <> help "Possible values: automat(on), trie" )


parseCompression :: Monad m => String -> m B.Compress
parseCompression s = return $ case map C.toLower s of
    'a':_       -> B.Auto    -- Automaton
    't':_       -> B.Trie    -- Trie
    'l':_       -> B.List    -- List
    's':_       -> B.SetAuto -- Set of automata
    'x':_       -> B.SetTrie -- Set of tries
    _           -> B.Auto


-- dataParser :: Parser (Maybe FilePath)
dataParser :: Parser B.BuildData
dataParser = B.BuildData
  <$> strOption
        ( long "grammar"
       <> short 'g'
       <> metavar "FILE"
       <> help "Grammar .xml file" )
  <*> ( optional . strOption )
        ( long "lexicon"
       <> short 'l'
       <> metavar "FILE"
       <> help "Lexicon .xml file" )
  <*> ( optional . strOption )
        ( long "auxiliary"
       <> short 'a'
       <> metavar "FILE"
       <> help "Auxiliary grammar/lexicon .xml file" )


buildOptions :: Parser (B.BuildData, B.BuildCfg)
buildOptions = (,)
    <$> dataParser
    <*> ( B.BuildCfg
        <$> option
                ( str >>= parseCompression )
                ( metavar "COMPRESSION-METHOD"
               <> value B.Auto
               <> long "compression-method"
               <> short 'c' )
        <*> (not <$> switch
                ( long "no-subtree-sharing"
               <> short 'n' )) )


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
-- Generation options
--------------------------------------------------


genOptions :: Parser Command
genOptions = Gen
    <$> dataParser
    <*> option
            auto
            ( metavar "MAX-SIZE"
           <> value 5
           <> long "max-size"
           <> short 'm' )


--------------------------------------------------
-- Generation/parsing options
--------------------------------------------------


genRandOptions :: Parser Command
genRandOptions = GenRand
    <$> dataParser
    <*> (G.GenConf
        <$> option
                auto
                ( metavar "MAX-SIZE"
               <> value 5
               <> long "max-size"
               <> short 'm' )
        <*> option
                auto
                ( metavar "ADJOIN-PROB"
               <> value 0.1
               <> long "adjoin-prob"
               <> short 'a' )
        <*> option
                auto
                ( metavar "TREE-NUM"
               <> value 10
               <> long "tree-num"
               <> short 'n' ))


--------------------------------------------------
-- Stats options
--------------------------------------------------


statsOptions :: Parser Command
statsOptions = Stats
    <$> dataParser
    <*> (S.StatCfg
          <$> option
                  ( Just <$> auto )
                  ( metavar "MAX-SIZE"
                 <> value Nothing
                 <> long "max-size"
                 <> short 'm' )
          <*> (snd <$> buildOptions)
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
--          <*> switch
--                ( long "print-parsed-trees"
--               <> short 'p' ) )


--------------------------------------------------
-- Selection options
--------------------------------------------------


selectOptions :: Parser Command
selectOptions = fmap Select $ S.SelectCfg
    <$> option
            ( Just <$> auto )
            ( metavar "MAX-SIZE"
           <> value Nothing
           <> long "max-size"
           <> short 'm' )
    <*> option
            auto
            ( metavar "SELECT-NUM"
           <> value 100
           <> long "select-num"
           <> short 'n' )
    <*> option
            auto
            ( metavar "MERGE-NUM"
           <> value 1
           <> long "merge-num"
           <> short 'k' )


--------------------------------------------------
-- Tmp options
--------------------------------------------------


tmpOptions :: Parser Command
tmpOptions = Tmp
    <$> dataParser
    <*> strOption
        ( long "start-sym"
       <> short 's'
       <> metavar "START-SYM"
       <> help "Start parsing from symbol" )


--------------------------------------------------
-- Global options
--------------------------------------------------


opts :: Parser Command
opts = subparser
        ( command "build"
            (info (helper <*> (uncurry Build <$> buildOptions))
                (progDesc "Build automaton from the grammar")
                )
        <> command "parse"
            (info (helper <*> parseOptions)
                (progDesc "Parse the input grammar file")
                )
        <> command "gen"
            (info (helper <*> genOptions)
                (progDesc "Generate trees based on input grammar file")
                )
        <> command "gen-rand"
            (info (helper <*> genRandOptions)
                (progDesc "Generate and parse trees")
                )
        <> command "stats"
            (info (helper <*> statsOptions)
                (progDesc "Parse sentences from stdin")
                )
        <> command "select"
            (info (helper <*> selectOptions)
                (progDesc "Select sentences from stdin")
                )
        <> command "lexicon"
            (info (helper <*> lexicOptions)
                (progDesc "Parse and print the lexicon")
                )
        <> command "print"
            (info (helper <*> (Print <$> dataParser))
                (progDesc "Parse and print the lexicon")
                )
        <> command "rules"
            (info (helper <*> (Rules <$> dataParser))
                (progDesc "Print standard rules; experimental mode")
                )
        <> command "weights"
            (info (helper <*> (Weights <$> dataParser))
                (progDesc "Print weighted rules; experimental mode")
                )
        <> command "tmp"
            (info (helper <*> tmpOptions)
                (progDesc "Parse with weights; experimental mode")
                )
        )


-- | Run program depending on the cmdline arguments.
run :: Command -> IO ()
run cmd = case cmd of
         Build buildData cfg ->
            B.printAuto cfg buildData
         Parse grammarPath ->
            P.printGrammar grammarPath
         Print B.BuildData{..} ->
            G.printTrees gramPath mayLexPath
         Gen B.BuildData{..} sizeMax ->
            G.generateFrom gramPath mayLexPath sizeMax
         GenRand B.BuildData{..} cfg ->
            G.genRandFrom cfg gramPath mayLexPath
         Stats buildData cfg ->
            S.statsOn cfg buildData
         Select cfg ->
            S.select cfg
         Lexicon lexPath ->
            L.printLexicon lexPath
         Rules buildData ->
            B.printRules buildData
         Weights buildData ->
            -- B.printWRules input lexicon
            B.printWeiAuto buildData
         Tmp buildData begSym ->
            S.parseWei buildData begSym


main :: IO ()
main =
    execParser optsExt >>= run
  where
    optsExt = info (helper <*> opts)
       ( fullDesc
      <> progDesc "Parsing with XMG grammars"
      <> header "partage4xmg" )
