module Ant.Movement 
	( moveAnts
	, antPaths
	
	)
	where

import Data.List
import Data.Maybe
import Data.Function

import Debug.Trace

import Control.Monad.State.Strict
import Ant.Scheduler
import Ant.Game

import Debug.Trace

import qualified Data.IntSet as S

type SquareSet = S.IntSet   

data MoveState = MoveState 
    {  mOrders   :: [Order]
    ,  mReserved :: SquareSet
    }

type Move a = StateT MoveState (StateT GameState IO) a

moveAnts :: [AntTask] -> Game [Order]
moveAnts ants = runMove $ (moveAnts' ants) >> gets mOrders
        
runMove :: Move a -> Game a
runMove move = evalStateT move moveState
    where 
        moveState = MoveState [] S.empty

getGame :: (GameState -> a) -> Move a
getGame f = lift (gets f)        

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
    size <- getGame (mapSize . gameMap)

    notTaken <- gets (S.notMember (size `wrapIndex`  p) . mReserved)
    land 	 <- getGame (isLand . (`at` p) .  gameMap)
    
    return (notTaken && land) 
{-# INLINE isFree #-}

occupy :: Point -> Move ()
occupy p =  do
    size <- getGame (mapSize . gameMap)
    let reserve state = state { mReserved = S.insert (size `wrapIndex`  p) (mReserved state) }
    
    modify reserve
{-# INLINE occupy #-}

makeOrder :: Point -> Direction -> Move ()
makeOrder p d = modify addOrder
    where addOrder state = state { mOrders = (p, d) : (mOrders state) }
{-# INLINE makeOrder #-}   
    
allMoves :: Point -> Move [(Point, Maybe Direction)]
allMoves (Point x y) = do
    size <- getGame (mapSize . gameMap)
    
    let order p dir = (wrapPoint size p, dir)
    return $ 	[ order (Point (x - 1) y) (Just West)
                            , order (Point (x + 1) y) (Just East)
                            , order (Point x (y + 1)) (Just South)
                            , order (Point x (y - 1)) (Just North)
                            , order (Point x y) Nothing
                            ]

            
order :: Point -> Maybe Direction -> Move ()
order p Nothing         = occupy p                      -- Mark square as occupied but don't send a move
order (Point x y) (Just dir)      = occupy p' >> makeOrder (Point x y) dir   -- Mark as occupied and send move
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
                    size <- getGame (mapSize . gameMap)
                    let p' = fromIndex size next
                    
                    moveTo p p'
                    
            makeMove' _ = anyMove p  -- We're at the destination already, do something
    
    
searchLimit :: Int
searchLimit = 400

findDest :: (SearchNode -> Bool) -> [SearchNode] -> Maybe SearchNode
findDest f = find f . take searchLimit 

pathToPoint :: Point -> Point -> Move (Maybe SearchNode)
pathToPoint source dest = do
	nodes <- pathFind source dest 
	size <- getGame (mapSize . gameMap)
	
	return $ findDest ((== dest) . fromIndex size . snKey) $ nodes
	
pathToFood :: Point -> Point -> Move (Maybe SearchNode)
pathToFood source dest = do
        nodes <- pathFind source dest 
        world <- getGame gameMap
        
        return $ findDest (hasFood . (world `atIndex`) . snKey) $ nodes	
	
pathToRegion ::  Point -> RegionIndex -> Move (Maybe SearchNode)
pathToRegion source region = do
	regionMap <- getGame (regionMap . gameBuilder)
	dest <- getGame (regionCentre . (`grIndex` region) . gameGraph)
	nodes <- pathFind source dest 
	
	return $ findDest ((== region) . nodeRegion regionMap)  $ nodes
	
pathFind :: Point -> Point -> Move [SearchNode]
pathFind source dest = do
    world <- getGame gameMap
    occupied <- gets mReserved    
    return $ search (successor world occupied) (metric (mapSize world) dest)  (mapSize world `wrapIndex` source)

    
        
moveAnt :: Point -> Task -> Move ()
moveAnt p (Goto r)      = pathToRegion p r >>= makeMove p
moveAnt p (Gather p')   = pathToFood p p' >>= makeMove p
moveAnt p _ = anyMove p

    
moveAnts' :: [AntTask] -> Move ()
moveAnts' ants = forM_ ants (uncurry moveAnt)
	
-- Debug versions to check returned paths	
antPaths :: [AntTask] -> Move [[Point]]
antPaths ants = forM ants (uncurry antPath)	

antPath :: Point -> Task -> Move [Point]
antPath p (Goto r)      = pathToRegion p r >>= makeMove' p
antPath p (Gather p')   = pathToFood p p' >>= makeMove' p
antPath p _ = anyMove p >> return []          
            
makeMove' p Nothing   = makeMove p Nothing >> return []          
makeMove' p (Just sn) = do   
    size <- getGame (mapSize . gameMap)
    makeMove p (Just sn)
    return $ map (fromIndex size) $ (searchPath sn) 
