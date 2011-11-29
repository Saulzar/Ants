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

data SearchNode = SearchNode 
    { snRegion :: !RegionIndex
    , snPred :: !RegionIndex
    , snDistance :: !Distance
    , snPredDistance :: !Distance 
    }

data Search = Search
    { sOpen    :: [SearchNode]
    , sVisited :: S.IntSet 
    }

nubRegions :: [SearchNode] -> [SearchNode]
nubRegions = map snd . M.toList . M.fromListWith minDist . map (\sn -> (snRegion sn, sn))
    where minDist sn sn' = if (snDistance sn) < (snDistance sn') then sn else sn'


edgeDistances :: Graph -> SearchNode -> [SearchNode]
edgeDistances graph (SearchNode r _ d ) = map toNode (grEdges r graph)
    where
        toNode (r', e) = SearchNode r' r (edgeDistance e + d)
{-# INLINE edgeDistances #-}


stepSearch :: Search -> Int -> Graph -> Search
stepSearch (Search open visited) maxDistance graph = Search open' visited' where
    open'     = nubRegions (filter (not . (flip S.member visited) . snRegion) neighbors)
    visited'  = foldr (S.insert . snRegion) visited open'
    neighbors = filter ( (< maxDistance) . snDistance) $ concatMap (edgeDistances graph) open

openSearch :: Search -> Bool
openSearch = not . null . sOpen 


initSearch :: RegionIndex -> Search
initSearch r = Search [SearchNode r r 0 0] (S.singleton r)

getAnts :: [SearchNode] -> GameStats -> [(Point, RegionIndex)]
getAnts regions stats = concatMap () regions
     antDistances (SearchNode r r' _) = map (p -> ((p, r'),   (allAnts r)
     
     allAnts = rcOurAnts . rsContent . indexV (gsRegions stats)

findFreeAnts :: RegionIndex -> Int -> Int -> Scheduler [Point]
findFreeAnts region maxDistance numAnts =  do 
    ants <- findFree' (initSearch region) [] 
    --return $ (take numAnts . sortBy (compare `on` 
    return ants
    
    where    
    
        findFree' search found | finished = return found
                               | otherwise = do
                                   
            let open = map snRegion (sOpen search)
        
            ants <- asks $ (getAnts open) . cStats
            found' <- liftM (++ found) (freeAnts ants)
         
            search' <- asks $ (stepSearch search maxDistance) . cGraph
            findFree' search' found'
                 
            where 
                finished = length found < numAnts 
                        && openSearch search
                    

    