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


type AntSet = S.Set AntTask
type AntList = [(Point, Player)]

data RegionContents = RegionContents
    { contentAnts       :: AntList
    , contentNearAnts   :: AntList
    , contentFood   :: [Point]
    , contentHills  :: [(Point, Player)]
    
    , hillDistance :: Int
    , contentRegion :: Region
    }
	
addContent :: RegionContents -> SquareContents -> RegionContents
addContent rc (p, Ant n)    = rc { contentAnts    = (p, n) : contentAnts rc }
addContent rc (p, Hill n)   = rc { contentHills = p : contentHills rc }
addContent rc (p, Food)     = rc { contentFood  = p : contentFood rc }
addContent rc _             = rc
     
emptyContents region = RegionContents
		{ rcAnts 	= []
		, rcFood 	= []
		, rcHills 	= []
        , rcNearAnts = []
        , rcDistance = 0
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


antSet :: [SquareContent] -> AntSet
antSet = S.fromList . map makeAnt . filter (playerAnt 0 . snd) 
	
    

contentNeighbors :: RegionContents -> ContentGraph -> [(RegionContents, Edge)]
contentNeighbors rc graph = map fromIndex edges
    where 
        edges = (M.toList . regionNeighbors . contentRegion) rc
        fromIndex (i, e) = (graph `V.unsafeIndex` i, e)
    

regionStats :: U.Vector Int -> RegionGraph -> [SquareContent] -> GraphBuilder -> V.Vector RegionContents
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
			
