{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

module Ant.Scheduler 
    ( runScheduler
    , initialSet
    , testSearch
    )
where

import Ant.Point
import Ant.IO
import Ant.Graph
import Ant.RegionBuilder
import Ant.Map
import Ant.RegionStats
import Ant.Search

import Data.List
import Data.Function
import Data.Maybe

import Debug.Trace

import qualified Data.IntSet as S
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict


type AntSet = M.Map Point Task

data Task  = Unassigned | Goto !RegionIndex | Gather !Point | Guard deriving Eq

runScheduler :: Map -> GameStats -> Graph -> AntSet -> Scheduler a -> a
runScheduler world stats graph ants action = runReader (evalStateT action ants) ctx
    where ctx = (Context world stats graph)

    
initialSet :: [Point] -> AntSet 
initialSet ants = M.fromList (zip ants (repeat Unassigned))          
          
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



predNode :: SearchNode -> SearchNode
predNode node | (Just pred) <- snPred node = pred
              | Nothing   <- snPred node = node
{-# INLINE predNode #-}                

succRegion :: Graph -> GameStats -> SearchNode -> [SearchNode]
succRegion graph stats = grSucc graph distance where
    distance r r' e  = Just (round d)
        where    
            (our, enemy) = (gsRegionInfluence stats) `indexU`  r'
                        
            -- Modify distance by number of enemies, if there are a lot it is likely to be a logjam
            slow = 1.0 + 0.3 * (fromIntegral enemy / area)
            d    = slow * fromIntegral (edgeDistance e) 
            area = fromIntegral (gsInfluenceArea stats)
                    
{-# INLINE succRegion #-}            

assignedAnts :: Scheduler [Point]
assignedAnts = gets (map fst . M.toList . M.filter (/= Unassigned))

testSearch :: Scheduler [Point]
testSearch = do
   
   {- numRegions <- asks  (grSize . cGraph)
    
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
    stats <- asks cStats                                               
    let rs = stats `gsRegion` region
        
    freeAnts (map fst . rcAnts . rsContent $ rs)
{-# INLINE freeAntsRegion #-}
    
findAnts :: forall a. (SearchNode -> Point ->  Maybe a) -> RegionIndex -> Int -> Distance -> Scheduler [a]
findAnts f region minRequired maxDistance = do     
    stats <- asks cStats
    graph <- asks cGraph
    
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
    rs = stats `gsRegion` (snKey sn)
    prevCentre = regionCentre (graph `grIndex` prevR)
    distance = prevD + manhatten size p prevCentre
{-# INLINE onPath #-}

getAnts :: RegionIndex -> Int -> Distance -> Scheduler [Point]
getAnts = findAnts (\_ p -> Just p) 
                
getAntPaths :: RegionIndex -> Int -> Distance -> Scheduler [AntDirection]
getAntPaths region numRequired maxDistance = do
    
    toPath <- liftM3 onPath (asks (mapSize . cWorld)) (asks cGraph) (asks cStats)
    ants   <- findAnts (\sn p -> Just (toPath sn p)) region numRequired maxDistance
    
    return $ take numRequired . sortBy (compare `on` adDistance) $ ants
    
                                       
foodDistance :: Distance
foodDistance = 40

assignFood :: [Point] -> Point -> Scheduler ()
assignFood []    _  = traceShow "None" $ return ()
assignFood ants  p = do
    size <- asks (mapSize . cWorld)
    ants' <- freeAnts ants
    
    let ant = minimumBy (compare `on` manhatten size p) ants'
    reserveAnt ant (Gather p)
    
    --traceShow ant $ return ()

gatherFoodAt :: RegionIndex -> Scheduler ()
gatherFoodAt region = do
    food <- asks (rcFood . rsContent . (`gsRegion` region) . cStats)           
    
       
    when (length food > 0) $ do          
        -- Find some nearby ants 
        traceShow (region, food) $ return ()
        
        ants <- getAnts region (length food + 2) foodDistance                
        forM_ food (assignFood ants)
            
        
gatherFood :: Scheduler ()
gatherFood = do
    regions <- asks (grNodes . cGraph)
    mapM_ gatherFoodAt regions
    
    
diffuseAnts :: Scheduler ()
diffuseAnts =  do
    regions <- asks (grNodes . cGraph)
    mapM regionDensity regions
    
regionDensity :: RegionIndex -> Scheduler (Maybe Double)
regionDensity region = do 
    stats <- asks cStats 

    let enemyInfluence = snd . (`indexU` region) .  gsRegionInfluence $ stats
    
    if enemyInfluence > 2 
        then return Nothing 
        else density' stats
        
    where
    
        density' stats = do
            
            let lastVisible = rsLastVisible . (`gsRegion` region) $ stats
            let visibleMod = -0.1 * fromIntegral lastVisible

            frontier <- asks (regionFrontier . (`grIndex` region) . cGraph)
            let frontierMod = if frontier then -0.5 else 0
            
            ants <- freeAntsRegion region       
            return (fromIntegral (length ants) + visibleMod + frontierMod)

{-
getAnts :: [SearchNode] -> GameStats -> [(Point, Distance)]
getAnts regions stats = concatMap () regions
     antDistances node = map (p -> ((p, r'),   (allAnts r)
     
     allAnts = rcOurAnts . rsContent . indexV (gsRegions stats)
-}
 
    