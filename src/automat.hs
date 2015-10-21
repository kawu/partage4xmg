import System.Environment
import NLP.FrenchTAG.Automat


main :: IO ()
main = do
    [path] <- getArgs
    automatRules path
