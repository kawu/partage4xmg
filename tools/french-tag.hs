{-# LANGUAGE RecordWildCards #-}


-- import           Data.Monoid (mempty)
import           Options.Applicative
import qualified Data.Char as C


import qualified NLP.FrenchTAG.Automat as A
import qualified NLP.FrenchTAG.Parse as P


--------------------------------------------------
-- Global Options
--------------------------------------------------


data Options = Options {
    input :: FilePath
  , cmd   :: Command
  }


data Command
    = Build BuildOptions
    | Parse -- ParseOptions


--------------------------------------------------
-- Build options
--------------------------------------------------


data BuildOptions
    = Base
    | Share
    | AutoBase
    | AutoShare
    deriving (Show, Read)


-- parseBuildOptions :: Monad m => String -> m BuildOptions
parseBuildOptions :: Monad m => String -> m BuildOptions
parseBuildOptions s = return $ case (map C.toLower) s of
    'b':_       -> Base
    's':_       -> Share
    'a':'u':'t':'o':'b':_
                -> AutoBase
    _           -> AutoShare


buildOptions :: Parser Command
buildOptions = Build
    <$> option
            -- auto
            ( str >>= parseBuildOptions )
            ( metavar "BUILD-TYPE"
           <> value AutoShare
           <> help "Possible values: base, share, autob(ase), auto(share)"
           <> long "build-type"
           <> short 'b' )


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
                (progDesc "Build different automaton versions"))
        <> command "parse"
            (info (pure Parse)
                (progDesc "Parse the input grammar file"))
        )


run :: Options -> IO ()
run Options{..} = do
    case cmd of
         Build Base ->
            A.baseLineRules input
         Build Share ->
            A.shareRules input
         Build AutoBase ->
            A.baseAutomatRules input
         Build AutoShare ->
            A.automatRules input
         Parse ->
            P.printGrammar input


main :: IO ()
main =
    execParser (info opts desc) >>= run
  where
    desc = progDesc "Manipulating FrenchTAG"
