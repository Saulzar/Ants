{-# LANGUAGE BangPatterns #-}

module Ant.Passibility 
    ( Passibility
    , newlyVisible
    , updatePassibility
    , emptyPassibility
    , squareCost  
    , indexCost
    , maxCost  
    
    , Pattern
    , pattern2
    )
    
where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Control.Monad
import Control.Arrow
import Control.Monad.ST

import Ant.Map
import Ant.Point
import Ant.Square
import Ant.IO

type Pattern = U.Vector (Int, Int, Int)

data Passibility = Passibility
    { squareCosts  :: U.Vector Int
    , costPattern :: Pattern
    , passSize    :: !Size
    , maxCost     :: !Int
    }
    
squareCost :: Passibility -> Point  -> Int
squareCost pass p = (squareCosts pass) `U.unsafeIndex` i where 
        i = (passSize pass) `wrapIndex` p
{-# INLINE squareCost #-}
        
indexCost :: Passibility -> Int -> Int
indexCost pass i = (squareCosts pass) `U.unsafeIndex` i       
{-# INLINE indexCost #-}
        
newlyVisible :: U.Vector Bool -> Map -> U.Vector Int
newlyVisible vis world =  U.map snd . U.filter fst . U.imap newlyVisible $ vis 
    where
        newlyVisible i True  = (not . wasSeen $ world `atIndex` i, i)
        newlyVisible i False = (False, i)

        
updatePassibility  :: U.Vector Int -> Map -> Passibility -> Passibility
updatePassibility newSquares world pass = pass { squareCosts = U.modify addSquares (squareCosts pass) } where
    
    addPattern :: UM.MVector s Int -> Int -> ST s ()
    addPattern v i = do
        let (Point x y) = fromIndex (mapSize world) i
        
        U.forM_ (costPattern pass) $ \ (dx, dy, weight) -> do
            let i' = wrapIndex (mapSize world) (Point (x + dx) (y + dy)) 
    
            c <- UM.unsafeRead v i'
            UM.unsafeWrite v i' (c - weight) 
    
    -- addSquares :: ST s  (U.Vector Int)
    addSquares v = do
       -- v <- U.unsafeThaw (squareCosts pass)
        
        U.forM_ newSquares $ \p -> do
            let sq = world `atIndex` p
            when (not (isWater sq)) $ addPattern v p
                
      --  U.unsafeFreeze v
        
emptyPassibility :: Size -> Pattern -> Passibility
emptyPassibility size pattern = Passibility 
    { squareCosts = U.replicate (area size) (sumPattern pattern)
    , costPattern = pattern
    , passSize    = size
    , maxCost     = sumPattern pattern
    }    


pattern2 :: Pattern
pattern2 = U.fromList (zipWith (\(x, y) w -> (x, y, w)) coords weights) 
    where 
    
    coords = [(x, y) | x <- [-2..2], y <- [-2..2]]
    weights = [ 0, 1, 1, 1, 0
              , 1, 2, 3, 2, 1
              , 1, 3, 6, 3, 1
              , 1, 2, 3, 2, 1
              , 0, 1, 1, 1, 0 
              ]
    


sumPattern :: U.Vector (Int, Int, Int) -> Int
sumPattern = U.sum . U.map third where
    third (_, _, x) = x
    
    
