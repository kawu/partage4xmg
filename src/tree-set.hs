import           Control.Monad (forM_)
import           System.Environment
import qualified Data.Set as S

import           NLP.FrenchTAG.Automat


main :: IO ()
main = do
    [path]  <- getArgs
    treeSet <- getTrees path
    forM_ (S.toList treeSet) $ \tree -> do
        putStrLn $ showSomeTree tree
    putStr "The number of trees in the grammar: "
    print $ S.size treeSet
