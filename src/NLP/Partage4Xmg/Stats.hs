{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


-- Parsing sentences from input and computing stats.


module NLP.Partage4Xmg.Stats
( StatCfg (..)
, statsOn

-- * Tmp
, parseWei
) where


import           Control.Monad              (forM_, unless, when)
import qualified Control.Monad.State.Strict as E
import           Control.Monad.Trans.Maybe

import qualified Data.Map.Strict            as M
import qualified Data.MemoCombinators       as Memo
import qualified Data.Set                   as S
import qualified Data.Text.Lazy             as L
import qualified Data.Tree                  as R
import Data.IORef
import           Pipes
import qualified Pipes.Prelude              as Pipes

import qualified NLP.Partage.AStar          as AStar
import qualified NLP.Partage.DAG            as D
import qualified NLP.Partage.Earley         as Earley
import qualified NLP.Partage.Tree.Other     as T

import qualified NLP.Partage4Xmg.Build      as B
import qualified NLP.Partage4Xmg.Gen        as G


--------------------------------------------------
-- Configuration
--------------------------------------------------


-- | Configuration.
data StatCfg = StatCfg
    { maxSize     :: Maybe Int
    -- ^ Optional limit on the sentence size
    , buildCfg    :: B.BuildCfg
    -- ^ Grammar construction configuration
    , startSym    :: String
    -- ^ The starting symbol of trees
    , printParsed :: Int
    -- ^ Print the set of parsed trees?
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
-- Stats entry
--------------------------------------------------


-- | Stats for a given sentence length.
data Stat = Stat
    { nodeNum :: Int
    , edgeNum :: Int
    , statNum :: Int
    , parsNum :: Int
    } deriving (Show, Eq, Ord)


-- | Create new `Stat` based on (number of nodes, number of edges).
newStat :: Maybe (Int, Int) -> Stat
newStat (Just (n, m)) = Stat
    { nodeNum   = n
    , edgeNum   = m
    , statNum   = 1
    , parsNum   = 1 }
newStat Nothing = Stat
    { nodeNum   = 0
    , edgeNum   = 0
    , statNum   = 1
    , parsNum   = 0 }


-- | Add to `Stat`s.
addStat :: Stat -> Stat -> Stat
addStat x y = Stat
    { nodeNum   = nodeNum x + nodeNum y
    , edgeNum   = edgeNum x + edgeNum y
    , statNum   = statNum x + statNum y
    , parsNum   = parsNum x + parsNum y }


--------------------------------------------------
-- Stats computation
--------------------------------------------------


-- | Read the grammar from the input file, sentences to parse from
-- std input, and perform the experiment.
statsOn
    :: StatCfg
    -> B.BuildData
    -- -> FilePath         -- ^ Grammar
    -- -> Maybe FilePath   -- ^ Lexicon (if present)
    -> IO ()
statsOn StatCfg{..} buildData = do
    -- extract the grammar and build the automaton
    (dag, gramAuto) <- B.buildAuto buildCfg buildData
    let auto = Earley.mkAuto dag gramAuto
    -- read sentences from input
    let thePipe = hoist lift sentPipe
    statMap <- flip E.execStateT M.empty . runEffect . for thePipe $
        \sent -> unless (sent `longerThan` maxSize) $ do
            stat <- liftIO $ do
                (stat, parseSet) <- parseEarley auto sent
                putStr "### "
                putStr (show sent)
                putStr " => " >> print stat
                putStrLn ""
                forM_ (take printParsed $ parseSet) $ \t -> do
                    putStrLn . R.drawTree . fmap show . T.encode . Left $ t
                return stat
            E.modify $ M.insertWith addStat
                (length sent) (newStat stat)
    liftIO $ do
        putStrLn ""
        putStrLn "length,sentences,parsed,nodes,edges"
        forM_ (M.toList statMap) $ \(n, stat) -> do
            putStr (show n ++ ",")
            printStat stat
            putStrLn ""
    liftIO $ do
        putStrLn ""
        putStrLn " === TOTAL === "
        putStrLn ""
        putStrLn "sentences,parsed,nodes,edges"
        printStat $ foldl1 addStat (M.elems statMap)
        putStrLn ""


  where

    -- | Parse with Earley version.
    parseEarley auto sent = do
        let input = Earley.fromSets $ map S.singleton sent
        hype <- Earley.earleyAuto auto input
        let treeSet = Earley.parsedTrees hype
                        (L.pack startSym) (length sent)
        stat <- if not (null treeSet) then do
                    return $ Just
                        ( Earley.hyperNodesNum hype
                        , Earley.hyperEdgesNum hype )
                 else do return Nothing
        return (stat, treeSet)

    printStat Stat{..} = do
        putStr (show statNum ++ ",")
        putStr (show parsNum ++ ",")
        putStr (show (nodeNum `divide` statNum) ++ ",")
        putStr (show (edgeNum `divide` statNum))


--------------------------------------------------
-- Temp section
--------------------------------------------------


-- | Read the grammar from the input files, sentences to parse from
-- std input, and perform the experiment.
parseWei
    :: B.BuildData
    -> String           -- ^ Start symbol
    -> Bool             -- ^ Show trees?
    -> IO ()
parseWei buildData begSym showTrees = do
    putStrLn "<<ELEMENTARY TREES>>"
    _ts <- S.toList <$> B.getTrees buildData
    mapM_  (putStrLn . R.drawTree . fmap show) _ts
    -- extract the grammar and build the automaton
    gram <- D.mkGram
           . map (,1)
           . S.toList
         <$> B.getTrees buildData
    putStrLn "<<DAG>>"
    let dag = D.dagGram gram
        toTree i = case D.toTree i dag of
          Nothing -> error "CARABANDA"
          Just t  -> t
        printDID i = do
          putStr "## " >> putStr (show i) >> putStrLn "##"
          putStrLn . R.drawTree . fmap show . toTree $ i
    mapM_ printDID (S.toList $ D.rootSet dag)
    putStrLn "<<PARSING>>"
    let auto = AStar.mkAuto termMemo gram
    -- read sentences from input
    runEffect . for sentPipe
        $ liftIO
        . parseAStar dag auto

  where

    -- | Parse with Prob.AutoAP (A*) version.
    -- parseAStar :: AStar.Auto G.NonTerm G.Term -> [G.Term] -> IO ()
    parseAStar dag auto sent = do
        let input = AStar.fromList sent
            pipe = AStar.earleyAutoP auto input
            sentLen = length sent
            final p = AStar._spanP p == AStar.Span 0 sentLen Nothing
                   && AStar._dagID p == Left (L.pack begSym)
        -- b <- AStar.recognizeFromAuto
        --         auto (L.pack begSym) input
        -- putStrLn  $ ">>> " ++ show b
        contRef <- newIORef True
        hype <- runEffect . for pipe $ \(item, hype) -> void . runMaybeT $ do
          E.guard =<< liftIO (readIORef contRef)
          AStar.ItemP p <- return item
          E.guard (final p)
          liftIO $ do
            putStrLn "<<CHECKPOINT>>" >> printStats hype >> putStrLn ""
            when showTrees $ mapM_
              (putStrLn . R.drawTree . fmap show . T.encode . Left)
              (nubOrd $ AStar.fromPassive p hype)
          liftIO $ writeIORef contRef False
        putStrLn "<<FINISH>>" >> printStats hype
        let ts = AStar.parsedTrees hype (L.pack begSym) sentLen
        putStr "deriv num: " >> print (length ts)
        putStr "tree num: "  >> print (length $ nubOrd ts)  >> putStrLn ""
        when showTrees $ mapM_
          (putStrLn . R.drawTree . fmap show . T.encode . Left)
          (nubOrd ts)
        putStrLn "<<DERIVATIONS>>"
        when showTrees $ do
          ds <- AStar.derivTrees hype (L.pack begSym) input
          let ds' = map (AStar.expandDeriv dag . AStar.deriv2tree) (S.toList ds)
          mapM_ (putStrLn . R.drawTree . fmap show) ds'
    termMemo = Memo.wrap read show $ Memo.list Memo.char
    printStats hype = do
      putStr "done nodes: " >> print (AStar.doneNodesNum hype)
      putStr "done edges: " >> print (AStar.doneEdgesNum hype)
      putStr "waiting nodes: " >> print (AStar.waitingNodesNum hype)
      putStr "waiting edges: " >> print (AStar.waitingEdgesNum hype)


--------------------------------------------------
-- Misc
--------------------------------------------------


divide :: (Integral a, Integral b) => a -> b -> Double
divide x y = (fromIntegral x :: Double) / fromIntegral y


nubOrd :: (Ord a) => [a] -> [a]
nubOrd = S.toList . S.fromList
