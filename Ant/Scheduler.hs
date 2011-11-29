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

data Task  = Unassigned | Goto !RegionIndex | Gather !Point | Guard deriving Eq

 

data Context = Context 
    { cWorld :: Map
    , cStats :: GameStats
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


data Search = Search
    { sOpen    :: [(RegionIndex, Distance)]
    , sVisited :: S.IntSet 
    }

nubRegions :: [(RegionIndex, Distance)] -> [(RegionIndex, Distance)]
nubRegions = M.toList . M.fromListWith min 


edgeDistances :: Graph -> (RegionIndex, Distance) -> [(RegionIndex, Distance)]
edgeDistances graph (r, d) = map toDistancePair (grEdges r graph)
    where
        toDistancePair (r', e) = (r', edgeDistance e + d)
{-# INLINE edgeDistances #-}


stepSearch :: Graph -> Search -> Search
stepSearch graph (SearchData open visited) = SearchData open' visited' where
    open'     = nubRegions (filter (not . (flip S.member visited) . fst) neighbors)
    visited'  = foldr (S.insert . fst) visited open'
    neighbors = concatMap (edgeDistances graph) open

closedSearch :: Search -> Bool
closedSearch = not . null . sOpen 


initSearch :: RegionIndex -> Search
initSearch r = Search [(r, 0)] (S.singleton r)


visitedRegions :: Search -> [RegionStats]
visitedRegions = map fst . sVisited

countAnts :: GameStats -> SearchData -> Int
countAnts stats  = sum . map (fst . rsAntCount . indexV regionStats) . visitedRegions
    where regionStats gsRegions stats

getAnts :: GameState -> [RegionStats] -> [Point]
getAnts stats = concatMap (rcOurAnts . rsContent . indexV regionStats) . visitedRegions

findAnts :: GameStats -> Graph -> RegionIndex -> Int -> Int -> [Point]
findAnts stats graph region maxDistance numAnts = getAnts stats regions
    where
        search = zip [0..] $ iterate (stepSearch graph) (initSearch region)
        regions = visitedRegions . head . (dropWhile satisfied) $ search

        satisfied (n, s) = (countAnts stats s < numAnts) 
                        && openSearch s 
                        && n < maxDistance


findFreeAnts :: RegionIndex -> Int -> Int -> Scheduler [Point]
findFreeAnts region maxDistance numAnts = do
    
    search <- findFree' (initSearch region) 0 0 

    where
    
        findFree' search n d = do
            


            search
                             




    