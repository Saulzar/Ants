module Ant.Movement 
	( moveAnts
	
	)
	where

import Data.List
import Data.Maybe
import Data.Function

import Debug.Trace

import Control.Monad.RWS.Strict

import Ant.Map
import Ant.Square
import Ant.Point
import Ant.Vector
import Ant.Graph
import Ant.Scheduler
import Ant.IO
import Ant.RegionBuilder
import Ant.RegionStats
import Ant.Search

import Debug.Trace

import qualified Data.IntSet as S

type SquareSet = S.IntSet   
type Move = RWS Context [Order] SquareSet

data Context = Context 
    { cWorld :: Map
    , cStats :: GameStats
    , cGraph :: Graph
    , cRegions :: RegionMap
    }
    
moveAnts :: RegionMap -> Map -> GameStats -> Graph -> [AntTask] -> [Order]
moveAnts regionMap world stats graph ants = orders 
    where 
        ctx = (Context world stats graph regionMap)
        (_, orders) = evalRWS (moveAnts' ants) ctx S.empty 



successor ::  Map -> SquareSet -> SearchNode -> [SearchNode]
successor world occupied sn@(SearchNode p d prev) = map toNode . filter valid $ neighbors
    where
        neighbors = neighborIndices (mapSize world) p
        valid p' = isLand (world `atIndex` p')           -- Land and not water (and we've seen it before)
               && (d > 0 || S.notMember p' occupied)     -- Immediate neighbor is taken (avoid collisions)
               
        toNode p' = SearchNode p' (d + 1) (Just sn)
{-# INLINE successor #-}
        
-- For A-star
metric :: Size -> Point -> SearchNode -> Float
metric size p = metric' where
    index = size `wrapIndex` p
    metric' (SearchNode index' d _) = (fromIntegral d) + distanceIndex size index index'
{-# INLINE metric #-}        
        
nodeRegion :: RegionMap -> SearchNode -> RegionIndex
nodeRegion regionMap = fst . (regionMap `indexU`) . snKey
{-# INLINE nodeRegion #-}

   


isFree :: Point -> Move Bool
isFree p = do
    size <- asks (mapSize . cWorld)

    notTaken <- gets (S.notMember (size `wrapIndex`  p))
    land 	 <- asks (isLand . (`at` p) .  cWorld)
    
    return (notTaken && land) 
{-# INLINE isFree #-}

occupy :: Point -> Move ()
occupy p =  do
    size <- asks (mapSize . cWorld)
    modify (S.insert (size `wrapIndex`  p))
{-# INLINE occupy #-}


    
allMoves :: Point -> Move [(Point, Maybe Direction)]
allMoves (Point x y) = do
    size <- asks (mapSize . cWorld)
    
    let order p dir = (wrapPoint size p, dir)
    return $ 	[ order (Point (x - 1) y) (Just West)
                            , order (Point (x + 1) y) (Just East)
                            , order (Point x (y + 1)) (Just South)
                            , order (Point x (y - 1)) (Just North)
                            , order (Point x y) Nothing
                            ]

            
order :: Point -> Maybe Direction -> Move ()
order p Nothing         = occupy p                      -- Mark square as occupied but don't send a move
order (Point x y) (Just dir)      = occupy p' >> tell [(Point x y, dir)]   -- Mark as occupied and send move
    where 
        p' = case dir of 
            East -> Point (x + 1) y
            West -> Point (x - 1) y
            North -> Point x (y - 1)
            South -> Point x (y + 1)
    
    
anyMove ::  Point -> Move ()
anyMove p = allMoves p >>= move' 
    where 
            move' [] = return ()  -- Ouch, no valid moves
            move' ((p', dir) : ms) = do
                    free <- isFree p'
                    if free then order p dir else move' ms

moveTo :: Point -> Point -> Move ()
moveTo p p' =  do
    moves <- allMoves p
    case lookup p' moves of 
            (Just dir) -> order p dir
            Nothing    -> error "Move square not adjacent to source"
                    
makeMove :: Point -> Maybe SearchNode -> Move ()
makeMove p Nothing   = anyMove p
makeMove p (Just sn) = makeMove' (searchPath sn)
    where 
            makeMove' (_ : next : _) = do
                    size <- asks (mapSize . cWorld)
                    let p' = fromIndex size next
                    
                    moveTo p p'
                    
            makeMove' _ = anyMove p  -- We're at the destination already, do something
    
    
searchLimit :: Int
searchLimit = 100

findDest :: (SearchNode -> Bool) -> [SearchNode] -> Maybe SearchNode
findDest f = find f . take searchLimit 

pathToPoint :: Point -> Point -> Move (Maybe SearchNode)
pathToPoint source dest = do
	nodes <- pathFind source dest 
	size <- asks (mapSize . cWorld)
	
	return $ findDest ((== dest) . fromIndex size . snKey) $ nodes
	
pathToRegion ::  Point -> RegionIndex -> Move (Maybe SearchNode)
pathToRegion source region = do
	regionMap <- asks cRegions
	dest <- asks (regionCentre . (`grIndex` region) . cGraph)
	nodes <- pathFind source dest 
	
	return $ findDest ((== region) . nodeRegion regionMap)  $ nodes
	
pathFind :: Point -> Point -> Move [SearchNode]
pathFind source dest = do
    world <- asks cWorld
    occupied <- get    
    return $ search (successor world occupied) (metric (mapSize world) dest)  (mapSize world `wrapIndex` source)
    
	
moveAnt :: Point -> Task -> Move ()
moveAnt p (Goto r) 		= pathToRegion p r >>= makeMove p
moveAnt p (Gather p')   = pathToPoint p p' >>= makeMove p
moveAnt p _ = anyMove p

    
moveAnts' :: [AntTask] -> Move ()
moveAnts' ants = forM_ ants (uncurry moveAnt)
	
    