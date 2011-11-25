{-# LANGUAGE PatternGuards #-}

module Ant.RegionStats where

import Data.List

import Ant.Map
import Ant.Point
import Ant.GraphBuilder
import Ant.IO

import qualified Data.Set as S
import qualified Data.IntMap as M

import Control.Monad.ST
import Control.Monad

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import qualified Data.PriorityQueue.FingerTree as Q

type AntSet = S.Set AntTask
type AntList = [(Point, Player)]

type ContentGraph = V.Vector RegionContents

data RegionContents = RegionContents
    { rcAnts       :: AntList
    , rcNearAnts   :: AntList
    , rcFood   :: [Point]
    , rcHills  :: [(Point, Player)]
    
    , rcHillDistance :: Int
    , rcRegion :: Region
    }
	
addContent :: RegionContents -> SquareContent -> RegionContents
addContent rc (p, Ant n)    = rc { rcAnts    = (p, n) : rcAnts rc }
addContent rc (p, Hill n)   = rc { rcHills = (p, n) : rcHills rc }
addContent rc (p, Food)     = rc { rcFood  = p : rcFood rc }
addContent rc _             = rc
     
emptyContents region = RegionContents
		{ rcAnts 	= []
		, rcFood 	= []
		, rcHills 	= []
        , rcNearAnts = []
        , rcHillDistance = 1000
		, rcRegion	 = region
		}
		
data Task  = Unassigned | Goto !RegionIndex | Gather !Point | Guard
        
data AntTask = AntTask
    { antTask  :: !Task
    , antPos   :: !Point
    }
	
instance Eq AntTask where
    (==) a b = (antPos a == antPos b)
    
instance Ord AntTask where
    compare a b = antPos a `compare` antPos b
	
makeAnt :: SquareContent -> AntTask
makeAnt (p, _) = AntTask 
	{ antTask = Unassigned
	, antPos   = p
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

updateRegionVisible :: Int -> Region -> Region
updateRegionVisible numVisible region | visible   = region { regionLastSeen = 0 }
									  | otherwise = region { regionLastSeen = (regionLastSeen region) + 1 } 
	where visible = (regionSize region * 100 `div` numVisible) > 80

antSet :: [SquareContent] -> AntSet
antSet = S.fromList . map makeAnt . filter (playerAnt 0 . snd) 
	
    

contentNeighbors :: RegionContents -> ContentGraph -> [(RegionContents, Edge)]
contentNeighbors rc graph = map fromIndex (contentNeighbors' rc)
    where fromIndex (i, e) = (graph `V.unsafeIndex` i, e)
    
contentNeighbors' :: RegionContents -> [(RegionIndex, Edge)]
contentNeighbors' = M.toList . regionNeighbors . rcRegion
    
neighborDistances :: RegionIndex -> V.Vector RegionContents -> [(Distance, RegionIndex)]
neighborDistances i graph = (map toDistancePair . contentNeighbors') rc
    where
        rc = graph `V.unsafeIndex` i
        toDistancePair (r, e) = (edgeDistance e, r)

	
type Queue = Q.PQueue Distance RegionIndex 	
	
hillDistances :: ContentGraph -> [RegionIndex] -> U.Vector Int
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
                
			

regionStats :: U.Vector Int -> RegionGraph -> [SquareContent] -> GraphBuilder -> ContentGraph
regionStats visibility regionGraph content builder = runST regionStats'
    where 
        regionStats' :: ST s (V.Vector RegionContents)
        regionStats' = do
            v <- VM.new (M.size regionGraph)

            forM_ (M.toList regionGraph) $ \(i, r) -> do
                let r' = updateRegionVisible (visibility `U.unsafeIndex` i) r 
                VM.unsafeWrite v i (emptyContents r')
                
            forM_ content $ \(p, c) -> do
                let i = builder `regionAt` p

                rc <- VM.unsafeRead v i 
                VM.unsafeWrite v i (rc `addContent` (p, c))
                
            V.unsafeFreeze v
			
