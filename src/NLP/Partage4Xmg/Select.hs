{-# LANGUAGE RecordWildCards #-}


-- Extract selected sentences.


module NLP.Partage4Xmg.Select
( select
, SelectCfg(..)
) where


import           Control.Applicative ((<$>))
import           Control.Monad (unless, forM_)
import qualified Control.Monad.State.Strict   as E

import           System.Random (randomRIO)

import           Data.List (transpose)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Pipes.Prelude as Pipes
import           Pipes

import qualified NLP.Partage4Xmg.Gen as G


--------------------------------------------------
-- Configuration
--------------------------------------------------


-- | Configuration.
data SelectCfg = SelectCfg
    { maxSize       :: Maybe Int
    -- ^ Optional limit on the sentence size
    , selNum        :: Int
    -- ^ The number of sentences to select per sent. length
    , mergeNum      :: Int
    -- ^ Each sentence in the output will be consists of several
    -- (`mergeNum` determines how many) derived sentences merged
    -- into one.
    } deriving (Show, Read, Eq, Ord)


-- | If the list longer than the given length?
longerThan :: [a] -> Maybe Int -> Bool
longerThan _ Nothing   = False
longerThan xs (Just n) = length xs > n


--------------------------------------------------
-- Std input
--------------------------------------------------


-- | Produce sentence in the given file.
sentPipe :: Producer [G.Term] IO ()
sentPipe = Pipes.stdinLn >-> Pipes.map read


--------------------------------------------------
-- Selection
--------------------------------------------------


-- | Read sentences to parse from stdin and select the given number
-- of sentences per sentence length.
select :: SelectCfg -> IO ()
select SelectCfg{..} = do
    let thePipe = hoist lift sentPipe
    sentMap <- flip E.execStateT M.empty . runEffect . for thePipe $
        \sent -> unless (sent `longerThan` maxSize) $ do
            let n = length sent
            -- liftIO $ print sent
            set <- M.findWithDefault S.empty n <$> E.get
            let newSet = S.insert sent set
            -- length (show newSet) `seq` E.modify $
            newSet `seq` E.modify $
            -- E.modify $
                M.insert n newSet
--     return ()
--             length (show sentSet) `seq`
--                 E.modify $ M.insertWith setUnion
--                     (length sent) sentSet
    print "############"
    forM_ (M.toList sentMap) $ \(sentLen, sentSet) ->
        putStr (show sentLen) >> putStr " -> " >> print (S.size sentSet)
    print "############"
    forM_ (M.elems sentMap) $ \sentSet0 -> do
        sentMultiSet <- multiSubset mergeNum selNum sentSet0
        forM_ (mergeSets sentMultiSet) print
  where
--     setUnion x y =
--         let z = S.union x y
--         in  S.size z `seq` z
    mergeSets = nub . map merge . S.toList
    merge = map nub . transpose . S.toList


-- | Take a random multisubset from the given set (unless empty) of
-- the given size (at max).
-- Each element of the resulting set will be a set of sentences (of
-- the specific size, at max) as well.
multiSubset
    :: Ord a
    => Int          -- ^ Target size of a result element
    -> Int          -- ^ Target size of the result
    -> S.Set a
    -> IO (S.Set (S.Set a))
multiSubset k n s
    | n > 0 = do
        x  <- subset k s
        xs <- multiSubset k (n - 1) s
        return $ S.insert x xs
    | otherwise = return S.empty


-- | Take a random subset, of the given size, from the given set.
subset :: Ord a => Int -> S.Set a -> IO (S.Set a)
subset n s
    | n > 0 && not (S.null s) = do
        x <- draw s
        r <- subset (n - 1) (S.delete x s)
        return $ S.insert x r
    | otherwise = return S.empty


-- | Draw a random element from the given set.
draw :: Ord a => S.Set a -> IO a
draw s = if S.null s
    then error "cannot draw from empty set"
    else do
        i <- randomRIO (0, S.size s - 1)
        return $ S.toList s !! i


-- | Remove duplicates.
nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList


--------------------------------------------------
-- Conversion (obsolete)
--------------------------------------------------


-- inpPipe :: Producer [[G.Term]] IO ()
-- inpPipe = Pipes.stdinLn >-> Pipes.map read
--
-- outPipe :: Consumer [G.Term] IO ()
-- outPipe = Pipes.map show >-> Pipes.stdoutLn
--
-- tmpProg = runEffect $ inpPipe >-> Pipes.map (map head) >-> outPipe
