module Ant.Movement 
    ( moveAnts
    , antPaths
    , runMove
    
    )
    where

import Data.List
import Data.Maybe
import Data.Function

import Debug.Trace

import Control.Monad.State.Strict
import Ant.Scheduler
import Ant.Game

import Text.Printf
import System.IO

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
{-# INLINE getGame #-}

successors :: Map -> (SearchNode -> PointIndex -> Maybe SearchNode) -> SearchNode -> [SearchNode]
successors world gen sn = catMaybes . map (gen sn) $ neighbors
    where neighbors = neighborIndices (mapSize world) (snKey sn)
{-# INLINE successors #-}    

validLand :: Map -> SquareSet -> PointIndex -> Bool
validLand world occupied p = isLand (world `atIndex` p)              -- Land and not water (and we've seen it before)

    
successor ::  Map -> SquareSet -> SearchNode -> PointIndex -> Maybe SearchNode
successor world occupied sn p' | valid     = Just $ SearchNode p' (snDistance sn + 1) (Just sn) 
                               | otherwise = Nothing
    where valid = validLand world  occupied p'  
{-# INLINE successor #-}
        
 
        
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
            makeMove' (next : _) = do
                    size <- getGame (mapSize . gameMap)
                    let p' = fromIndex size next
                    
                    moveTo p p'
                    
            makeMove' _ = anyMove p  -- We're at the destination already, do something
    
    
searchLimit :: Int
searchLimit = 400

findDest :: (SearchNode -> Bool) -> [SearchNode] -> Maybe SearchNode
findDest f = find f . take searchLimit 


makeSuccessors :: Move (SearchNode -> PointIndex -> Maybe SearchNode)
makeSuccessors = do
    world <- getGame gameMap
    occupied <- gets mReserved  
    return $ (successor world occupied)

pathToPoint :: Point -> Point -> Move (Maybe SearchNode)
pathToPoint source dest = do
    succ <- makeSuccessors   
    metric <- distanceMetric dest
    
    nodes <- pathFind succ metric source 

    size <- getGame (mapSize . gameMap)
    return $ findDest ((== dest) . fromIndex size . snKey) $ nodes
	
pathToFood :: Point -> Point -> Move (Maybe SearchNode)
pathToFood source dest = do
        succ <- makeSuccessors
        metric <- distanceMetric dest
        
        nodes <- pathFind succ metric source 
        
        world <- getGame gameMap
        return $ findDest (hasFood . (world `atIndex`) . snKey) $ nodes	

   
        
pathToRegion ::  Point -> RegionIndex -> Move (Maybe SearchNode)
pathToRegion sourcePoint destRegion = do
    regionMap <- getGame (regionMap . gameBuilder)
    destPoint <- getGame (regionCentre . (`grIndex` destRegion) . gameGraph)
    world <- getGame gameMap
    occupied <- gets mReserved   

    let (sourceRegion, _) = regionMap `indexU` (mapSize world `wrapIndex` sourcePoint)     
    let successor sn p | valid     = Just $ SearchNode p (snDistance sn + 1) (Just sn) 
                       | otherwise = Nothing where
            valid  = validLand world occupied p
                  && (region == sourceRegion || region == destRegion)
            region = fst (regionMap `indexU` p)

    metric <- distanceMetric destPoint
            
    nodes <- pathFind successor metric sourcePoint 
    return $ findDest ((== destRegion) . nodeRegion regionMap)  $ nodes
    
    
pathToFight :: Point -> Float -> Move (Maybe SearchNode)
pathToFight source minDistance = do
    
    distanceMap <- getGame (gsEnemyDistances . gameStats) 
    occupied <- gets mReserved   
    world <- getGame gameMap
    
    let successor sn p | valid      = Just $ SearchNode p d' (Just sn)
                       | otherwise  = Nothing where
            valid  = validLand world occupied p
                  && (fst (distanceMap `indexU` p) >= minDistance)
                  && d < 6
                
            d = snDistance sn    
            d' | d > 0 && isAnt (world `atIndex` p) = d + 2
               | otherwise = d + 1
              
    let heuristic (SearchNode p d _) = enemyDistance -- + fromIntegral d * 0.2 - proximity * 0.4
            where 
                enemyDistance = fst (distanceMap `indexU` p) - minDistance
                neighbors = neighborIndices (mapSize world) p
                proximity = fromIntegral . length . filter (flip S.member occupied) $ neighbors

    nodes <- pathFind successor snDistance source
    case nodes of 
        [] -> traceShow "Not found!" $ return Nothing
        _  -> do 
            let best = minimumBy (compare `on` heuristic) $ nodes 
            let dist = fst . (distanceMap `indexU`) . snKey $ best 
            
            liftIO $ print (best, dist, minDistance)
            return . Just $ best
        

-- For A-star
distanceMetric :: Point -> Move (SearchNode -> Float)
distanceMetric dest =  do
    size <- getGame (mapSize . gameMap)
    
    let index = size `wrapIndex` dest   
    return $ \(SearchNode index' d _) -> (fromIntegral d) + distanceIndex size index index'
    
        
pathFind :: Ord m => (SearchNode -> PointIndex -> Maybe SearchNode) -> (SearchNode -> m) -> Point -> Move [SearchNode]
pathFind succ metric source = do
    world <- getGame gameMap
 
    let p = mapSize world `wrapIndex` source
    let seeds = p : neighborIndices (mapSize world) p 
 
    return $ searchN (successors world succ) metric seeds

    
        
moveAnt :: Point -> Task -> Move ()
moveAnt p (Goto r)      = pathToRegion p r >>= makeMove p
moveAnt p (Gather p')   = pathToFood p p' >>= makeMove p
moveAnt p (Guard _)       = do
    --radius <- getGame (fromIntegral . attackRadius2 . gameSettings)
    pathToFight p 5 >>= makeMove p
    
moveAnt p t = anyMove p

    
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
