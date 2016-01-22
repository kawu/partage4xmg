{-# LANGUAGE RecordWildCards #-}


-- import           Data.Monoid (mempty)
import           Options.Applicative
import qualified Data.Char as C


-- import qualified NLP.FrenchTAG.Automat as A
import qualified NLP.FrenchTAG.Build as B
import qualified NLP.FrenchTAG.Parse as P
import qualified NLP.FrenchTAG.ParseLex as L
import qualified NLP.FrenchTAG.Gen as G
import qualified NLP.FrenchTAG.GenLex as GL
import qualified NLP.FrenchTAG.Stats as S
import qualified NLP.FrenchTAG.Select as S


--------------------------------------------------
-- Global Options
--------------------------------------------------


data Options = Options {
    input :: FilePath
  , cmd   :: Command
  }


data Command
    = Build (Maybe FilePath) B.BuildCfg
    -- ^ Build an automaton
    | Parse -- ParseOptions
    -- ^ Only parse and show the input grammar
    | Gen Int
    -- ^ Generate size-bounded derived trees
    | GenRand G.GenConf
    -- ^ Randomly generate derived sentences
    | Stats S.StatCfg
    -- ^ Parse sentences from stdin (one sentence per line)
    | Select S.SelectCfg
    -- ^ Randomly select sentences from stdin (given number
    -- per sentence length)
    | Lexicon
    -- ^ Parse and print the lexicon
    | Experiment FilePath
    -- ^ Experimental mode (with additional path to the lexicon)


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


buildOptions :: Parser (Maybe FilePath, B.BuildCfg)
buildOptions = (,)
    <$> optional (strOption
        ( long "lexicon"
       <> short 'l'
       <> metavar "FILE"
       <> help "Lexicon .xml file" ))
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
-- Generation options
--------------------------------------------------


genOptions :: Parser Command
genOptions = Gen
    <$> option
            auto
            ( metavar "MAX-SIZE"
           <> value 5
           <> long "max-size"
           <> short 'm' )


--------------------------------------------------
-- Generation/parsing options
--------------------------------------------------


genRandOptions :: Parser Command
genRandOptions = fmap GenRand $ G.GenConf
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
           <> short 'n' )


--------------------------------------------------
-- Stats options
--------------------------------------------------


statsOptions :: Parser Command
statsOptions = fmap Stats $ S.StatCfg
    <$> option
            ( Just <$> auto )
            ( metavar "MAX-SIZE"
           <> value Nothing
           <> long "max-size"
           <> short 'm' )
    <*> (snd <$> buildOptions)


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
-- Global options
--------------------------------------------------


opts :: Parser Options
opts = Options
    <$> strOption
       ( long "input"
      <> short 'i'
      <> metavar "FILE"
      <> help "Input .xml (e.g. valuation.xml) file" )
    <*> subparser
        ( command "build"
            (info (uncurry Build <$> buildOptions)
                (progDesc "Build automaton from the grammar")
                )
        <> command "parse"
            (info (pure Parse)
                (progDesc "Parse the input grammar file")
                )
        <> command "gen"
            (info genOptions
                (progDesc "Generate trees based on input grammar file")
                )
        <> command "gen-rand"
            (info genRandOptions
                (progDesc "Generate and parse trees")
                )
        <> command "stats"
            (info statsOptions
                (progDesc "Parse sentences from stdin")
                )
        <> command "select"
            (info selectOptions
                (progDesc "Select sentences from stdin")
                )
        <> command "lexicon"
            (info (pure Lexicon)
                (progDesc "Parse and print the lexicon")
                )
        <> command "experiment"
            (info (Experiment <$> strOption
                    ( long "lexicon"
                  <> short 'l'
                  <> metavar "FILE"
                  <> help "Lexicon .xml file" ))
                (progDesc "Parse and print the lexicon")
                )
        )


-- | Run program depending on the cmdline arguments.
run :: Options -> IO ()
run Options{..} =
    case cmd of
         Build Nothing cfg ->
            B.printAuto cfg input
         Build (Just lex) cfg ->
            B.printLexAuto cfg input lex
         Parse ->
            P.printGrammar input
         Gen sizeMax ->
            G.generateFrom input sizeMax
         GenRand cfg ->
            G.genRandFrom cfg input
         Stats cfg ->
            S.statsOn cfg input
         Select cfg ->
            S.select cfg
         Lexicon ->
            L.printLexicon input
         Experiment lexicon ->
            GL.printTrees input lexicon


main :: IO ()
main =
    execParser (info opts desc) >>= run
  where
    desc = progDesc "Manipulating FrenchTAG"
