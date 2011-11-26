{-# LANGUAGE PatternGuards #-}

module Ant.RegionStats 
	( RegionContent (..)
	, RegionStats (..)
	, regionVisibility
	, hillDistances
	, regionStats
	)
	where

import Data.List

import Ant.Map
import Ant.Point
import Ant.GraphBuilder
import Ant.IO

import Ant.Graph

import qualified Data.Set as S
import qualified Data.IntMap as M

import Control.Monad.ST
import Control.Monad

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Data.Vector.Generic as G
import qualified Data.PriorityQueue.FingerTree as Q

data RegionContent = RegionContent
    { rcAnts       	:: AntList
    , rcNearAnts   	:: AntList
    , rcFood   		:: [Point]
    , rcHills  		:: [(Point, Player)]
	, rcDeadAnts	:: [(Point, Player)]
    }

	
addContent :: RegionContent -> SquareContent -> RegionContent
addContent rc (p, Ant n)    = rc { rcAnts    = (p, n) : rcAnts rc }
addContent rc (p, DeadAnt n)    = rc { rcDeadAnts  = (p, n) : rcDeadAnts rc }
addContent rc (p, Hill n)   = rc { rcHills = (p, n) : rcHills rc }
addContent rc (p, Food)     = rc { rcFood  = p : rcFood rc }
addContent rc _             = rc
     
emptyContent = RegionContent
		{ rcAnts 	= []
		, rcDeadAnts = []
		, rcFood 	= []
		, rcHills 	= []
        , rcNearAnts = []
		}
		

regionVisibility :: Int -> RegionMap -> U.Vector Bool -> U.Vector Int
regionVisibility numRegions regionMap visible = runST countVisible
	where
        countVisible :: ST s (U.Vector Int)
        countVisible = do
            v <- UM.replicate numRegions 0
            forM_ [0.. U.length regionMap - 1] $ \i -> when (visible `U.unsafeIndex` i) $ do
                let (region, _) = regionMap `U.unsafeIndex` i 
                n <- UM.unsafeRead v region
                UM.unsafeWrite v i (n + 1)
                
            U.unsafeFreeze v

	
type Queue = Q.PQueue Distance RegionIndex 	
	
hillDistances :: Graph -> [RegionIndex] -> U.Vector Int
hillDistances contents hills = runST searchHills where 	
    initialQueue = Q.fromList (zip [0..] hills)
        
    searchHills :: ST s (U.Vector Int)
    searchHills = do
        v <- UM.replicate (V.length contents) 1000  
        forM_ hills $ \r -> UM.unsafeWrite v r 0
        
        searchHills' v initialQueue
        U.unsafeFreeze v
    
    searchHills' v queue | Nothing 				  <- view = return ()	
                         | Just ((d, r), queue') <- view = do
            
            successors <- filterM (isSuccessor d) (neighborDistances r contents)
            forM_ successors $ \(d', r') -> UM.unsafeWrite v r' (d + d')
            searchHills' v (foldr (uncurry Q.insert) queue successors)
                                     
        where
            view = Q.minViewWithKey queue  
            
            isSuccessor d (d', r') = do
                d'' <- UM.unsafeRead v r'
                return (d + d' < d'')
                


regionContents :: Int -> [SquareContent] -> GraphBuilder -> V.Vector RegionContent
regionContents numRegions content builder = runST regionStats'
    where 
        regionStats' :: ST s (V.Vector RegionContents)
        regionStats' = do
            v <- VM.replicate (M.size regionGraph) emptyContent
                
            forM_ content $ \(p, c) -> do
                let i = builder `regionAt` p

                rc <- VM.unsafeRead v i 
                VM.unsafeWrite v i (rc `addContent` (p, c))
                
            V.unsafeFreeze v
	

data RegionStats = RegionStats
	{ rsLastVisible 	:: !Int
	, rsHillDistance 	:: !Int
	, rsContent			:: !RegionContents

	, rsOurDead		:: !Int
	, rsEnemyDead	:: !Int
	}
	

regionStats :: U.Vector Int -> U.Vector Int -> V.Vector RegionContent -> GaphVector -> V.Vector RegionStats -> V.Vector RegionStats
regionStats = G.zipWith5 regionStats' where
	
	regionStats' visibility hillDistance content region stats = stats
		{ rsLastVisible  = lastVisible stats + visible
		, rsHillDistance =  hillDistance
		, rsContent 	 = content

		, rsOurDead		 = rsOurDead stats + length ourDead
		, rsEnemyDead  	 = rsEnemyDead stats + length enemyDead
		
		}
		where visible | (regionSize region * 100 `div` numVisible) > 80 = 1
					  | otherwise 										= 0
			  
			  (ourDead, enemyDead) = partition ( (== 0) . snd ) (rcDeadAnts content)

