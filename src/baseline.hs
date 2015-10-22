import System.Environment
import NLP.FrenchTAG.Automat


main :: IO ()
main = do
    [path] <- getArgs
    baseLineRules path
    baseLineEdges path
--     putStrLn "Rules: " >> baseLineRules path
--     putStrLn ""
--     putStr "Number of rules: " >> baseLineRuleNum path
