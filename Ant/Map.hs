{-# LANGUAGE BangPatterns #-}

module Ant.Map where

import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Control.Monad.ST

import Data.List
import Data.List.Split


import Ant.Point
import Ant.Square
import Ant.IO

data Map = Map { mapSquares :: S.Vector Square 
               , mapSize    :: !Size
               }     

instance Show Map where
    show (Map squares (Size width height)) = concat . intersperse "\n" $ dim : lines
        where
            dim     = show (width, height)
            lines   = splitEvery width . map tileChar . S.toList $ squares  

               
          
emptyMap :: Size -> Map
emptyMap size = Map (S.replicate (area size) unknownSquare) size

noVisibility :: Size -> U.Vector Bool
noVisibility size = (U.replicate (area size) False)

visibleSet :: Size -> Int -> [Point] -> U.Vector Bool
visibleSet size radius ants = runST $ do
        v <- UM.replicate (area size) False
        
        flip mapM_ ants  $ \ant ->
           flip mapM_ offsets $ \offset -> do
               UM.unsafeWrite v (wrapIndex size (ant `addSize` offset)) True
         
        U.unsafeFreeze v
    where
        offsets = [Size x y | x <- [-radius..radius], y <- [-radius..radius],  x * x + y * y <= radiusSq]
        radiusSq = radius * radius
 

updateVisibility :: U.Vector Bool -> Map -> Map
updateVisibility vis (Map squares size) = Map squares' size 
    where
        squares' = S.imap (\index -> setVisibility (vis `U.unsafeIndex` index)) squares         
 
updateContent :: [SquareContent] -> Map -> Map
updateContent content (Map squares size) =  Map (S.modify updateSquares squares) size 
    where
        updateSquares v = flip mapM_ content  $ \(point, c) -> do
            let index = size `indexMap` point
            SM.unsafeWrite v index (setContent c $ squares `S.unsafeIndex` index) 
               
    
wrapIndex :: Size -> Point -> Int
wrapIndex (Size width height) (Point x y) = (y `mod` height) * width + x `mod` width
{-# INLINE wrapIndex #-}

indexMap :: Size -> Point -> Int
indexMap (Size width height) (Point x y) = y * width + x
{-# INLINE indexMap #-}

at :: Map -> Point -> Square
at (Map squares size) point =  squares `S.unsafeIndex` (size `wrapIndex` point)
{-# INLINE at #-}



