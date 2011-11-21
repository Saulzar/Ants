module RegionStats where

import Data.List

import Ant.Game
import Ant.Renderer

import qualified Data.IntMap as M


import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as UV



type AntIndex = Int
type AntSet = M.IntMap Ant

data RegionContents
    { friendlyAnts  :: ![(Point, AntIndex)]
    , enemyAnts  	:: ![(Point, Player)]
    , foodSquares   :: ![Point]
    , friendlyHill :: !Maybe Point
    , enemyHill    :: !Maybe Point
	
	, numFriendlies :: !Int
	, numEnemies	:: !Int
    }
     
emptyContents = RegionContents
		{ friendlyAnts 	= []
		, enemyAnts 	= []
		, foodSquares 	= []
		, friendlyHill	= Nothing
		, enemyHill	 	= Nothing
		, numFriendlies = 0
		, numEnemies 	= 0
		}
		


regionVisibility :: Int -> RegionMap -> U.Vector Bool -> U.Vector Int
regionVisibility numRegions regionMap visible = runST countVisible
	where
		countVisible = do
			v <- UM.replicate numRegions 0
			forM_ [0.. UM.length regionMap - 1] $ \i -> when (visible `unsafeIndex` i) $ do
				let (region, _) = regionMap `unsafeIndex` i 
				n <- unsafeRead v region
				unsafeWrite v (n + 1)
	
regionStats :: Int -> [SquareContent] -> RegionMap -> (AntSet, V.Vector RegionContents)
regionStats numRegions content regionMap = runST regionStats'
	
	where 
		(ourAnts, content') = partition (playerAnt 0 . snd) content
		antSet = M.fromList (zip [0.. ] ourAnts
		
		regionStats' = do
			