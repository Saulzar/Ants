{-# LANGUAGE BangPatterns #-}

module Ant.Map 
    ( Passibility
    
    )
    
where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Ant.Map
import Ant.Point
import Ant.Square
import Ant.IO



type Passibility = 
    { squareCost  :: U.Vector Int
    , costPattern :: U.Vector (Int, Int, Int)
    }

newlyVisible :: U.Vector Bool -> Map -> U.Vector Int
newlyVisible vis (Map squares size) =  U.map fst . U.filter snd . U.imap (newlyVisible &&& id) $ squares 
    where
        newlyVisible i True  = not . wasSeen (squares `U.unsafeIndex` index )
        newlyVisible _ False = False

        
updatePassibility  :: U.Vector Int -> Map -> Passibility -> Passibility
updatePassibility newSquares world (Passibility cost pattern) = runST update where
    
    addPattern :: UM.Vector Int -> Int -> ST s ()
    addPattern v p = 
    
    addSquares :: ST s  (U.Vector Int)
    addSquares = do
        v <- unsafeThaw cost
        
        U.forM_ newSquares $ \p -> do
            let sq = world `atIndex` p
        
            if (isWater p)
                then unsafeWrite v p 0
                else addPattern v p
                
        
emptyPassiblity :: Size -> Int -> Passibility
emptyPassibility size localSize = Passibility 
    { squareCost = U.replicate (area size) 0
    , costPattern = pattern localSize
    }    

pattern :: Int -> U.Vector (Int, Int, Int)
pattern size = [(x, y, weight x y) | x <- [-size.. size], y <- [-size .. size]]
    weight 0 0 = 0
    weight x y = 1 + min (size - abs x) (size - abs y) 

