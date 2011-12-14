{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

module Ant.Scheduler 
    ( scheduleAnts
    , runScheduler
    , initialSet
    , testSearch
    , diffuseAnts
    
    , flowDensity
    
    , AntSet
    , Task (..)
    , AntTask
    
    )
where

import Ant.Game

import Data.List
import Data.Function
import Data.Maybe

import Debug.Trace

import qualified Data.IntSet as S
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

type AntSet = M.Map Point Task
type AntTask = (Point, Task)

data Task  = Unassigned | Goto !RegionIndex | Gather !Point | Guard | Retreat deriving (Eq, Show)

scheduleAnts :: [Point] -> Game [AntTask]
scheduleAnts ants = runScheduler ants $ gatherFood >> diffuseAnts >> gets (M.toList)
    
runScheduler :: [Point] -> Scheduler a -> Game a
runScheduler ants schedule = evalStateT schedule antSet
    where antSet = initialSet ants	
	
    
initialSet :: [Point] -> AntSet 
initialSet ants = M.fromList (zip ants (repeat Unassigned))          
          
getGame :: (GameState -> a) -> Scheduler a
getGame f = lift (gets f)
{-# INLINE getGame #-}
  
type Scheduler a = StateT AntSet (StateT GameState IO) a

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



predNode :: SearchNode -> SearchNode
predNode node | (Just pred) <- snPred node = pred
              | Nothing   <- snPred node = node
{-# INLINE predNode #-}                

succRegion :: Graph -> GameStats -> SearchNode -> [SearchNode]
succRegion graph stats = grSucc graph distance where
    distance r r' e  = Just (fromIntegral (edgeDistance e))
        {-
        where    
            (our, enemy) = (gsRegionInfluence stats) `indexU`  r'
                        
            -- Modify distance by number of enemies, if there are a lot it is likely to be a logjam
            slow = 1.0 + 0.3 * enemy
            d    = slow * fromIntegral (edgeDistance e) 
          -}           
{-# INLINE succRegion #-}            

assignedAnts :: Scheduler [Point]
assignedAnts = gets (map fst . M.toList . M.filter (/= Unassigned))

testSearch :: Scheduler [Point]
testSearch = do
   
   {- numRegions <- getGame  (grSize . gameGraph)
    
    traceShow "Testsearch" $ return ()
    
    forM_ [1..1000] $ \r -> do
        let third (_, _, x) = x
        
        ants <- getAntPaths (r `mod` numRegions)  170 50
        traceShow (maximum $ [0] ++ map adDistance ants) $ return () 
        
        -}
    
    --gatherFoodAt 70
    --gatherFoodAt 78
    
    gatherFood
    ants <- assignedAnts
    traceShow (length ants) $ return ()
    
    return ants
    
   
    
freeAntsRegion :: RegionIndex -> Scheduler [Point]
freeAntsRegion region = do
    regionAnts <- getGame  (rcAnts  .  (`gsRegion` region) . gameStats)                                             
    freeAnts regionAnts
{-# INLINE freeAntsRegion #-}



    
findAnts :: forall a. (SearchNode -> Point ->  Maybe a) -> RegionIndex -> Int -> Distance -> Scheduler [a]
findAnts f region minRequired maxDistance = do     
    stats <- getGame gameStats
    graph <- getGame gameGraph
    
    let xs   = search (succRegion graph stats) snDistance region
    getAnts' xs 0 [] 
        
    where
        
        getAnts' :: [SearchNode] -> Int -> [a] -> Scheduler [a]
        getAnts' []          _ accum = return accum            
        getAnts' (sn : rest) n accum | snDistance sn > maxDistance = return accum
                                     | otherwise = do
                ants <- freeAntsRegion (snKey sn) 
                let ants' = catMaybes . map (f sn) $ ants 
                                              
                if (n + length ants < minRequired) 
                    then getAnts' rest (n + length ants) (ants' ++ accum)
                    else return (ants' ++ accum)

data AntDirection = AntDirection 
         { adPoint      :: !Point
         , adNextRegion :: !RegionIndex
         , adDistance   :: !Distance
         }
                    
onPath :: Size -> Graph -> GameStats -> SearchNode -> Point -> AntDirection
onPath size graph stats sn p = AntDirection p prevR distance where
    (SearchNode prevR prevD _) = predNode sn
    
    prevCentre = regionCentre (graph `grIndex` prevR)
    distance = prevD + manhatten size p prevCentre
{-# INLINE onPath #-}

getAnts :: RegionIndex -> Int -> Distance -> Scheduler [Point]
getAnts = findAnts (\_ p -> Just p) 
                
getAntPaths :: RegionIndex -> Int -> Distance -> Scheduler [AntDirection]
getAntPaths region numRequired maxDistance = do
    
    toPath <- liftM3 onPath (getGame (mapSize . gameMap)) (getGame gameGraph) (getGame gameStats)
    ants   <- findAnts (\sn p -> Just (toPath sn p)) region numRequired maxDistance
    
    return $ take numRequired . sortBy (compare `on` adDistance) $ ants
    
                                       
foodDistance :: Distance
foodDistance = 8

assignFood :: [Point] -> Point -> Scheduler ()
assignFood ants  p = do
    size <- getGame (mapSize . gameMap)
    ants' <- freeAnts ants

    when (not . null $ ants') $ do
        let ant = minimumBy (compare `on` manhatten size p) ants'
        
        when (manhatten size p ant < foodDistance) $
            reserveAnt ant (Gather p)
    
    --traceShow ant $ return ()

gatherFoodAt :: RegionIndex -> Scheduler ()
gatherFoodAt region = do
    food <- getGame (rcFood . (`gsRegion` region) . gameStats)           
           
    when (length food > 0) $ do          
        -- Find some nearby ants        
        ants <- getAnts region (length food) foodDistance                
        --ants <- freeAntsRegion region 
        forM_ food (assignFood ants)
            
        
gatherFood :: Scheduler ()
gatherFood = do
    regions <- getGame (grNodes . gameGraph)
    mapM_ gatherFoodAt regions
    

allFreeAnts ::Scheduler [Point]
allFreeAnts = do 
    (ants, _) <- getGame (gsAnts . gameStats)                                               
    freeAnts ants
{-# INLINE allFreeAnts #-}    
    
    
sumInfluence :: Float -> Int -> RegionMap -> U.Vector Int -> U.Vector Float
sumInfluence scale numRegions regionMap influence = U.create $ do                 
    v <- UM.replicate numRegions 0
    forRegion regionMap $ \i region -> do
        let inf = influence `indexU` i 
        inf' <- readU v region
        
        writeU v region (inf' + scale * fromIntegral inf)
    return v    
    
antDensity :: Scheduler (U.Vector Float)
antDensity = do
    regionMap   <- getGame (regionMap . gameBuilder)
    size        <- getGame (mapSize . gameMap) 
    numRegions  <- getGame (grSize . gameGraph)
    
    (ants, _) <- getGame (gsAnts . gameStats)  
        
    return (antDensity' size numRegions regionMap ants)
    
antDensity' :: Size -> Int -> RegionMap -> [Point] -> U.Vector Float    
antDensity' size numRegions regionMap ants = sumInfluence influenceScale numRegions regionMap squareInfluence  
    where
        squareInfluence = influenceCount size distSq ants

        distSq = 16
        influenceScale = 1.0 / fromIntegral (length (circlePoints distSq)) 

makeFlowGraph :: Scheduler FlowGraph
makeFlowGraph = do        
    regions <- getGame (grNodes . gameGraph)
                
    passable <- mapM diffusableRegion regions
    let passableVec = U.fromList passable
    
    graph <- getGame gameGraph
    return (flowGraph graph passableVec) 
    
           
            
flowDensity :: Scheduler (U.Vector Float, FlowGraph)
flowDensity = do

    rDensity   <- regionDensity
   -- aDensity   <- antDensity
        
    flowGraph         <- makeFlowGraph 
    
    let diffuseRegions = diffuse 1.0 flowGraph rDensity  !! 30 
    --let diffuseAnts    = U.map (* 0.2) $ diffuse 4.0 flowGraph aDensity  !! 8
    
    return (diffuseRegions, flowGraph) --(U.zipWith (+) diffuseRegions diffuseAnts, flowGraph)
        
diffuseAnts :: Scheduler ()
diffuseAnts =  do
    regions <- getGame (grNodes . gameGraph)            
    (density, flowGraph)  <- flowDensity 
    
    mapM_ (diffuseRegion flowGraph density) regions
    
    
diffuseRegion :: FlowGraph -> U.Vector Float -> RegionIndex -> Scheduler ()
diffuseRegion flowGraph density region = do
    ants <- freeAntsRegion region   
    size <- getGame (mapSize . gameMap)
    
    let antDests = flowParticles size flowGraph density region ants
    forM_ antDests $ \(ant, dest) ->  reserveAnt ant (Goto dest)
    
    
diffusableRegion :: RegionIndex -> Scheduler Bool
diffusableRegion region = return True {- do 
    stats <- getGame gameStats 

    let enemyInfluence = snd . (`indexU` region) .  gsRegionInfluence $ stats
        return (enemyInfluence < 2) -} 
                
regionDensity :: Scheduler (U.Vector Float)    
regionDensity = do
    regions <- getGame (U.fromList . grNodes . gameGraph) 
    U.forM regions $ \r -> regionDensity' r
                
        
regionDensity' ::  RegionIndex -> Scheduler Float
regionDensity' region = do  
    stats <- getGame gameStats 
    graph <- getGame gameGraph
            
    let lastVisible = gsVisited stats `indexU` region 
    let visibleMod = max (-0.1 * fromIntegral (max (lastVisible - 10) 0)) (-1.0)

    frontier <- getGame (regionFrontier . (`grIndex` region) . gameGraph)
    let frontierMod = if frontier then -1.0 else 0
    
    let rs = stats `gsRegion` region    
    let foodMod = negate . fromIntegral . length . rcFood  $ rs
  
  
    return $  4.0 * frontierMod + visibleMod 

{-
getAnts :: [SearchNode] -> GameStats -> [(Point, Distance)]
getAnts regions stats = concatMap () regions
     antDistances node = map (p -> ((p, r'),   (allAnts r)
     
     allAnts = rcOurAnts . rsContent . indexV (gsRegions stats)
-}
 
    