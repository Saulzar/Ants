{-# LANGUAGE PatternGuards #-}

module Ant.Scheduler where

import Ant.Point
import Ant.IO
import Ant.Graph
import Ant.RegionBuilder
import Ant.Map
import Ant.RegionStats

import qualified Data.IntSet as S
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict


type AntSet = M.Map Point Task

data Task  = Unassigned | Goto !RegionIndex | Gather !Point | Guard

 

data Context = Context 
    { cWorld :: Map
    , cStats :: RegionStats
    , cGraph :: Graph
    }

type Scheduler = StateT AntSet (Reader Context)

reserveAnt :: Point -> Task -> Scheduler ()
reserveAnt point task = get >>=  put . (M.insert point task)
{-# INLINE reserveAnt #-}    

reserveAnts :: [Point] -> Task -> Scheduler ()
reserveAnts points task = mapM_ (flip reserveAnt task) points
{-# INLINE reserveAnts #-}  

freeAnt :: Point -> Scheduler Bool
freeAnt point = do
    ants <- get 
    return (maybe False (== Unassigned) (M.lookup point ants))
{-# INLINE freeAnt #-}  

freeAnts :: [Point] -> Scheduler [Point]
freeAnts = filterM freeAnt
{-# INLINE freeAnts #-}  

data SearchData = SearchData
    { sOpen    :: [RegionIndex]
    , sVisited :: S.IntSet 
    }

stepBFS :: Graph -> SearchData -> SearchData
stepBFS graph (SearchData visted open) = SearchData open' visited' where
    open'     = filter (not . S.member visited)
    visited'  = foldr S.insert visited open'
    neighbors = concatMap (flip grEdgeIndices graph) open

finishedBFS :: SearchData -> Bool
finishedBFS (SearchData _ []) = True
finishedBFS _                 = False

initBFS :: RegionIndex -> SearchData
initBFS r = SearchData [r] (S.singleton r)

findFreeAnts :: RegionIndex -> Int -> Int -> Scheduler [Point]
findFreeAnts region maxDistance numAnts = 

    find' 0 s = returnAnts 
