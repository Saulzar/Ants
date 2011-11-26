{-# LANGUAGE PatternGuards, NoMonomorphismRestriction #-}

module Ant.RegionStats 
	( RegionContent (..)
	, RegionStats (..)
    , GameStats (..)
	, regionVisibility
	, hillDistances
    , initialStats
    , updateStats
	)
	where

import Data.List

import Ant.Map
import Ant.Point
import Ant.RegionBuilder
import Ant.IO
import Ant.Square

import Ant.Graph
import Ant.AntTask

import qualified Data.Set as S
import qualified Data.IntMap as M
import qualified Data.Sequence as Seq

import Control.Monad.ST
import Control.Monad

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import qualified Data.Vector.Generic as G
import qualified Data.PriorityQueue.FingerTree as Q

import Debug.Trace

indexU :: (U.Unbox a) => U.Vector a -> Int -> a
indexU = (U.!)
{-# INLINE indexU #-}

indexV :: V.Vector a -> Int -> a
indexV = (V.!)
{-# INLINE indexV #-}

readV = VM.read
{-# INLINE readV #-}

writeV = VM.write
{-# INLINE writeV #-}

readU = UM.read
{-# INLINE readU #-}

writeU = UM.write
{-# INLINE writeU #-}

data RegionContent = RegionContent
    { rcAnts       	:: AntList
    , rcNearAnts   	:: AntList
    , rcFood   		:: [Point]
    , rcHills  		:: [(Point, Player)]
	, rcDeadAnts	:: [(Point, Player)]
    } deriving Show
	

	
data RegionStats = RegionStats
	{ rsLastVisible 	:: !Int
	, rsHillDistance 	:: !Int
	, rsContent			:: !RegionContent

	, rsDead		:: !(Int, Int)
	, rsFightRecord :: !(Int, Int)

	, rsEnemyPlayer :: Maybe Player  -- The main opponent here
	} deriving Show

data GameStats = GameStats
	{ gsRegions     :: V.Vector RegionStats
	, gsFightRecord :: U.Vector (Int, Int)
	, gsFreeAnts	:: !Int
	, gsShortage 	:: !Int
	
	, gsHills		:: S.Set (Point, Player)	
	} deriving Show


maxPlayers :: Int
maxPlayers = 20
	
initialRegionStats = RegionStats	
	{	rsLastVisible 	= 0
	, 	rsHillDistance 	= 1000
	, 	rsContent 		= emptyContent
    ,   rsDead          = (0, 0)
	,   rsFightRecord 	= (0, 0)
	, 	rsEnemyPlayer   = Nothing 
	}
	
initialStats = GameStats
	{ gsRegions = V.empty
	, gsFightRecord = U.replicate maxPlayers (0, 0)
	, gsFreeAnts 	= 0
	, gsShortage 	= 0
	, gsHills		= S.empty
	}
	
hills :: Map -> [SquareContent] -> S.Set (Point, Player) -> S.Set (Point, Player)
hills world content hills = S.filter hillExists $ foldr S.insert hills visibleHills
	where
		visibleHills = map (\(p, Hill n) -> (p, n)) . filter (containsHill . snd) $ content
		hillExists (p, _) = isHill (world `at` p)  
	
growRegions :: Int -> V.Vector RegionStats -> V.Vector RegionStats
growRegions numRegions v = growRegions' newRegions	
	where
		newRegions = numRegions -  (V.length v)

		growRegions' 0 = v
		growRegions' n = v V.++ V.replicate newRegions initialRegionStats
	

updateFightRecords :: V.Vector RegionStats -> U.Vector (Int, Int) -> U.Vector (Int, Int)
updateFightRecords regionStats frVec = frVec--U.modify update frVec where
{-    update v = do	
    	V.forM_ regionStats $ \stats -> case (rsEnemyPlayer stats) of
    		Nothing 		-> return ()
    		(Just player)	-> do
    			fr <- readU v player
    			writeU v player (addFightRecord fr (rsDead stats))
-}
		 

updateStats :: Map -> Graph -> RegionMap -> U.Vector Bool -> [SquareContent] -> GameStats -> GameStats
updateStats world graph regionMap vis content stats = stats
	{ gsRegions = regionStats'
	, gsFightRecord = updateFightRecords regionStats' (gsFightRecord stats)
	, gsHills = hills'
	}
	where

		regionVisibility' = regionVisibility (grSize graph) regionMap vis  
		hills'    = hills world content (gsHills stats)
		
		getRegion      = regionAt regionMap (mapSize world)
		regionDistances = hillDistances graph (map getRegion . map fst . S.toList $ hills')
		
		regionContent'  = regionContent (mapSize world) graph regionMap content

		regions' = growRegions (grSize graph) (gsRegions stats) 
		regionStats' = regionStats regionVisibility' regionDistances regionContent' graph regions'
		

	
	
addContent :: RegionContent -> SquareContent -> RegionContent
addContent rc (p, Ant n)    = rc { rcAnts    = (p, n) : rcAnts rc }
addContent rc (p, DeadAnt n)    = rc { rcDeadAnts  = (p, n) : rcDeadAnts rc }
addContent rc (p, Hill n)   = rc { rcHills = (p, n) : rcHills rc }
addContent rc (p, Food)     = rc { rcFood  = p : rcFood rc }
addContent rc _             = rc
     
emptyContent = RegionContent
		{ rcAnts 	 = []
		, rcDeadAnts = []
		, rcFood 	 = []
		, rcHills 	 = []
        , rcNearAnts = []
		}
		

regionVisibility :: Int -> RegionMap -> U.Vector Bool -> U.Vector Int
regionVisibility numRegions regionMap visible = runST countVisible
	where
        countVisible :: ST s (U.Vector Int)
        countVisible = do
            v <- UM.replicate numRegions 0
{-            forM_ [0.. U.length regionMap - 1] $ \i -> when (visible `indexU` i) $ do
                let (region, _) = regionMap `indexU` i 
                n <- readU v region
                writeU v i (n + 1)
  -}              
            U.unsafeFreeze v

	
type Queue = Q.PQueue Distance RegionIndex 	
	
edgeDistances :: RegionIndex -> Graph -> [(Distance, RegionIndex)]
edgeDistances i graph = map toDistancePair (grEdges i graph)
    where
        toDistancePair (r, e) = (edgeDistance e, r)
{-# INLINE edgeDistances #-}

hillDistances :: Graph -> [RegionIndex] -> U.Vector Int
hillDistances graph hills = runST searchHills where 	
    initialQueue = Q.fromList (zip [0..] hills)
        
    searchHills :: ST s (U.Vector Int)
    searchHills = do
        v <- UM.replicate (grSize graph) 1000  
        forM_ hills $ \r -> UM.unsafeWrite v r 0
        
--        searchHills' v initialQueue
        U.unsafeFreeze v
    
    searchHills' v queue | Nothing 				  <- view = return ()	
                         | Just ((d, r), queue') <- view = do
            
            successors <- filterM (isSuccessor d) (edgeDistances r graph)
            forM_ successors $ \(d', r') -> writeU v r' (d + d')
            searchHills' v (foldr (uncurry Q.insert) queue successors)
                                     
        where
            view = Q.minViewWithKey queue  
            
            isSuccessor d (d', r') = do
                d'' <- readU v r'
                return (d + d' < d'')
               
nearRadius :: Int                
nearRadius = 16

nearbyAnts :: Size -> Point -> [RegionContent] -> AntList
nearbyAnts worldSize p rcs = filter ( (< nearRadius)  . distance)  allAnts 
	where
		allAnts 	 = concatMap rcAnts rcs
		distance (p', _) = manhatten worldSize p p' 



regionContent :: Size -> Graph -> RegionMap -> [SquareContent] -> V.Vector RegionContent
regionContent worldSize graph regionMap content = addNearby worldSize graph rc 
	where rc = regionContent' worldSize (grSize graph) regionMap content

addNearby :: Size -> Graph -> V.Vector RegionContent -> V.Vector RegionContent
addNearby worldSize graph content = V.imap (\i c -> c { rcNearAnts = nearby i } ) content
	where nearby i = nearbyAnts worldSize centre cs
		where 
			centre  = regionCentre (graph `grIndex` i)
			cs = map (indexV content)  (grEdgeIndices i graph)

regionContent' :: Size -> Int -> RegionMap -> [SquareContent] ->  V.Vector RegionContent
regionContent' worldSize numRegions regionMap content = runST regionStats'
    where 
        regionStats' :: ST s (V.Vector RegionContent)
        regionStats' = do
            v <- VM.replicate numRegions emptyContent
                
            forM_ content $ \(p, c) -> do
                let i = regionAt regionMap worldSize p

                rc <- traceShow i $ readV v i 
                writeV v i (rc `addContent` (p, c))
                
            V.unsafeFreeze v
	

countPlayers :: AntList -> [(Player, Int)]
countPlayers = map (\x -> (head x, length x)) . group . map snd 
		

maxPlayer :: AntList -> Maybe Player
maxPlayer [] 	= Nothing
maxPlayer ants 	= Just (fst $ head (countPlayers ants)) 


addFightRecord :: (Int, Int) -> (Int, Int) -> (Int, Int)
addFightRecord (e, p) (e', p') = (e + e', p + p')


regionStats :: U.Vector Int -> U.Vector Int -> V.Vector RegionContent -> Graph -> V.Vector RegionStats -> V.Vector RegionStats
regionStats visVec distVec contVec graph statsVec = V.generate (V.length statsVec) fromIndex where

    fromIndex i = regionStats' (visVec `indexU` i) (distVec `indexU` i) (contVec `indexV` i) (graph `grIndex` i) (statsVec `indexV` i)

    regionStats' numVisible hillDistance content region stats = stats
		{ rsLastVisible  = lastVisible
		, rsHillDistance = hillDistance
		, rsContent 	 = content

		, rsDead		 = dead
		, rsFightRecord	 = addFightRecord dead (rsFightRecord stats)

		, rsEnemyPlayer  = maxPlayer (enemyDead ++ enemyAnts)
		
		}
		where 
            lastVisible | regionSize region > 0 && isVisible  = rsLastVisible stats + 1
               	        | otherwise = 0
			
            isVisible = (numVisible `div` regionSize region * 100) > 80  

            splitEnemy = partition ( (== 0) . snd )

            (ourDead, enemyDead) = splitEnemy (rcDeadAnts content)
            (ourAnts, enemyAnts) = splitEnemy (rcAnts content ++ rcNearAnts content)

            dead = (length enemyDead, length ourDead)

