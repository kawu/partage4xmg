{-# LANGUAGE RecordWildCards #-}


-- import           Data.Monoid (mempty)
import           Options.Applicative
import qualified Data.Char as C


import qualified NLP.FrenchTAG.Automat as A
import qualified NLP.FrenchTAG.Build as B
import qualified NLP.FrenchTAG.Parse as P
import qualified NLP.FrenchTAG.Gen as G
import qualified NLP.FrenchTAG.Stats as S


--------------------------------------------------
-- Global Options
--------------------------------------------------


data Options = Options {
    input :: FilePath
  , cmd   :: Command
  }


data Command
    = Build BuildOptions
    -- ^ Build an automaton
    | Parse -- ParseOptions
    -- ^ Only parse and show the input grammar
    | Gen Int
    -- ^ Generate size-bounded derived trees
    | GenParse G.GenConf
    -- ^ Generate and parse size-bounded derived trees
    | Stats S.StatCfg
    -- ^ Parse sentences from stdin (one sentence per line)


--------------------------------------------------
-- Build options
--------------------------------------------------


data BuildOptions
    = Automat
    | Trie
    deriving (Show, Read)


-- parseBuildOptions :: Monad m => String -> m BuildOptions
parseBuildOptions :: Monad m => String -> m BuildOptions
parseBuildOptions s = return $ case map C.toLower s of
    'a':_       -> Automat
    't':_       -> Trie
    _           -> Automat


buildOptions :: Parser Command
buildOptions = Build
    <$> argument
            -- auto
            ( str >>= parseBuildOptions )
            ( metavar "BUILD-TYPE"
           <> value Automat
           <> help "Possible values: automat(on), trie" )


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


genParseOptions :: Parser Command
genParseOptions = fmap GenParse $ G.GenConf
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


parseCompression :: Monad m => String -> m S.Compression
parseCompression s = return $ case map C.toLower s of
    'a':_       -> S.Auto    -- Automaton
    't':_       -> S.Trie    -- Trie
    _           -> S.Auto


statsOptions :: Parser Command
statsOptions = fmap Stats $ S.StatCfg
    <$> option
            auto
            ( metavar "MAX-SIZE"
           <> value Nothing
           <> long "max-size"
           <> short 'm' )
    <*> (not <$> switch
            ( long "no-subtree-sharing"
           <> short 'n' ))
    <*> option
            ( str >>= parseCompression )
            ( metavar "COMPRESSION-METHOD"
           <> value S.Auto
           <> long "compression-method"
           <> short 'c' )


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
            (info buildOptions
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
        <> command "gen-parse"
            (info genParseOptions
                (progDesc "Generate and parse trees")
                )
        <> command "stats"
            (info statsOptions
                (progDesc "Parse sentences from stdin")
                )
        )


-- | Run program depending on the cmdline arguments.
run :: Options -> IO ()
run Options{..} =
    case cmd of
--          Build Base ->
--             A.baseLineRules input
--          Build Share ->
--             A.shareRules input
--          Build AutoBase ->
--             A.baseAutomatRules input
--          Build AutoShare ->
--             A.automatRules input
         Build Automat ->
            B.buildAuto input
         Build Trie ->
            B.buildTrie input
         Parse ->
            P.printGrammar input
         Gen sizeMax ->
            G.generateFrom input sizeMax
         GenParse cfg ->
            G.genAndParseFrom cfg input
         Stats cfg ->
            S.statsOn cfg input


main :: IO ()
main =
    execParser (info opts desc) >>= run
  where
    desc = progDesc "Manipulating FrenchTAG"
