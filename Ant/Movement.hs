module Movement where

import Data.List
import Data.Maybe
import Data.Function

import Debug.Trace

import Control.Monad.RWS.Strict

import Ant.Map
import Ant.Point
import Ant.Search
import Ant.Vector
import Ant.Graph
import Ant.Scheduler
import Ant.IO

import qualified Data.IntSet as S

type SquareSet = S.IntSet 
    
type Move = RWS Context [Order] SquareSet
type PathNode = SearchNode Point

data Context = Context 
    { cWorld :: Map
    , cStats :: GameStats
    , cGraph :: Graph
    , cRegions :: RegionMap
    }
    
moveAnts :: RegionMap -> Map -> GameStats -> Graph -> AntSet -> [Order]
moveAnts regions world stats graph ants = orders 
    where 
        ctx = (Context world stats graph regions)
        (_, orders) = evalRWS ctx S.empty (moveAnts' ants)



succ ::  Map -> SquareSet -> SearchNode -> [SearchNode]
succ world occupied sn@(SearchNode i d prev) = toNode . filter valid $ neighbors
    where
        neighbors = neighborIndices (mapSize world) i
        valid i' = isLand (world `atIndex` i')           -- Land and not water (and we've seen it before)
               && (d > 0 || S.notMember i' occupied)     -- Immediate neighbor is taken (avoid collisions)
               
        toNode i' = SearchNode i' (d + 1) sn
        
-- For A-star
metric :: Size -> Point -> SearchNode -> Float
metric size p = metric' where
    i = size `wrapIndex` p
    metric' (SearchNode i' d _) = (fromIntegral d) + distanceIndex size i i'
        
        
inRegion :: RegionIndex -> Size -> RegionMap -> Int -> Bool
inRegion r size regionMap p = r == (regionMap `indexU` index)

searchLimit :: Int
searchLimit = 100    
    
pathFind :: Point -> RegionIndex -> Move (Maybe SearchNode)
pathFind p r = do
    dest <- asks (regionCentre . (`grIndex` r) . cGraph)
    world <- asks cWord
    regionMap <- asks cRegions
    occupied <- get    

    let searchMetric = metric (mapSize world) dest 
    let result = search (succ world occupied) searchMetric (mapSize world `wrapIndex` p)
    
    return $ find (reachedRegion . snKey) (take searchLimit result)
    
    
moveAnts' :: AntSet -> Move ()
moveAnts' ants = do
    return ()