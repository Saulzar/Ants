module RegionStats where

import Data.List

import Ant.Game
import Ant.Renderer

import qualified Data.Set as S
import Control.Monad.ST
import Control.Monad

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as UV

type AntSet = S.Set AntOrders

data RegionContents = RegionContents
    { friendlyAnts  :: ![Point]
    , enemyAnts  	:: ![(Point, Player)]
    , foodSquares   :: ![Point]
    , friendlyHill :: Maybe Point
    , enemyHill    :: Maybe Point
	
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
		
data Assignment  = Unassigned | Goto !RegionIndex | Gather !Point | Guard
        
data AntOrders = AntOrders
    { antAssign :: !Assignment
    ,  antPos    :: !Point
    }
instance Eq AntOrders where
    (==) a b = (antPos a == antPos b)
    
instance Ord AntOrders where
    compare a b = antPos a `compare` antPos b

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
	{-
regionStats :: Int -> [SquareContent] -> RegionMap -> (AntSet, V.Vector RegionContents)
regionStats numRegions content regionMap = runST regionStats'
	
	where 
		(ourAnts, content') = partition (playerAnt 0 . snd) content
		antSet = S.fromList (map makeAnt ourAnts)
		
		regionStats' = do
			-}