{-# LANGUAGE PatternGuards #-}

module Ant.Scheduler 

where

import Ant.Point
import Ant.IO
import Ant.Graph
import Ant.RegionBuilder
import Ant.Map
import Ant.RegionStats
-- import Ant.Search

import Data.List
import Data.Function

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

{-

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
            slow = 1.0 + 0.3 * (fromIntegral enemy / gsInfluenceArea stats)
            d    = slow * fromIntegral (edgeDistance e) 
                    
{-# INLINE succRegion #-}            

type AntDirection = (Point, RegionIndex, Distance)

testSearch :: Scheduler [Point]
testSearch = do
    ants <- getAnts 0 17 200
    
    numRegions <- asks  (grSize . cGraph)
    
    traceShow "Testsearch" $ return ()
    
    forM_ [1..1000] $ \r -> do
        let third (_, _, x) = x
        
        ants <- getAnts (r `mod` numRegions)  17 200
        traceShow (sum $ map third ants) $ return () 
    
    return (map (\(p, _, _) -> p) ants)
                
getAnts :: Int -> Int -> Int -> Scheduler [AntDirection]
getAnts region numRequired maxDistance = do
    
    stats <- asks cStats
    graph <- asks cGraph
    
    let xs   = search (succRegion graph stats) snDistance region
    ants <- getAnts' xs 0 [] 
    
    return $ take numRequired . sortBy (compare `on` third) $ ants
    
    where
        third (_, _, x) = x
        
        getAnts' :: [SearchNode] -> Int -> [AntDirection] -> Scheduler [AntDirection]
        getAnts' []          _ accum = return accum            
        getAnts' (sn : rest) n accum | snDistance sn > maxDistance = return accum
                                     | otherwise = do
                graph <- asks cGraph
                size <- asks (mapSize . cWorld)   
                stats <- asks cStats                      
                                         
                let (SearchNode prevR prevD _) = predNode sn
                let rs = stats `gsRegion` (snKey sn)
                
                let prevCentre = regionCentre (graph `grIndex` prevR)             
                let withDist p = (p, prevR, prevD + manhatten size p prevCentre)
                
                ants <- freeAnts $ (map fst . rcAnts . rsContent $ rs)
                let ants' = map withDist ants ++ accum
                
                if (n + length ants < numRequired + 5) 
                    then getAnts' rest (n + length ants) ants'
                    else return ants'
                                    
-}
                    

{-
getAnts :: [SearchNode] -> GameStats -> [(Point, Distance)]
getAnts regions stats = concatMap () regions
     antDistances node = map (p -> ((p, r'),   (allAnts r)
     
     allAnts = rcOurAnts . rsContent . indexV (gsRegions stats)
-}
 
    