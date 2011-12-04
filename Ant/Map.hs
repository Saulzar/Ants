{-# LANGUAGE BangPatterns, PatternGuards #-}

module Ant.Map 
    ( Map
    
    , mapSize
    
    , fromSquares
    , emptyMap
    
    , noVisibility
    , visibleSet
    , influenceCount
    , circlePoints
    
    , updateVisibility
    , updateContent
    
    , fromIndex
    --, toIndex
    
    , at
    , wrapIndex
    , fromFunction
    
    , atIndex   
    , neighbors 
    , neighborIndices  
    
    , findSquareBy
    , rectIndices
    
    , manhatten
    , manhattenIndex
    
    , difference
    , distanceSqIndex
    , distanceSq
    
    , tileMap
    )
    
where

import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Control.Monad

import Control.Monad.ST
import Control.Arrow 

import Data.List
import Data.List.Split


import Ant.Point
import Ant.Square
import Ant.IO
import Ant.Vector

data Map = Map { mapSquares :: S.Vector Square 
               , mapSize    :: !Size
               }     

instance Show Map where
    show (Map squares (Size width height)) = concat . intersperse "\n" $ dim : lines
        where
            dim     = show (width, height)
            lines   = splitEvery width . map squareChar . S.toList $ squares  


tileMap :: Map -> Size -> Map
tileMap world = flip fromFunction (world `at`)

            
fromSquares :: Size -> [Square] -> Map
fromSquares size squares 
    | length squares == area size = Map (S.fromList squares) size 
    | otherwise                   = error $ "Mismatched squares creating map " ++ show (length squares)           
          
emptyMap :: Square -> Size -> Map
emptyMap square size = Map (S.replicate (area size) square) size



fromFunction :: Size -> (Point -> Square) -> Map
fromFunction size@(Size x y) f = Map (S.generate (area size) (\i -> let (sx, sy) = i `divMod` x in f (Point sx sy))) size

noVisibility :: Size -> U.Vector Bool
noVisibility size = (U.replicate (area size) False)


influenceCount :: Size -> Int -> [Point] -> U.Vector Int
influenceCount size radiusSq ants = U.create $ do
    v <- UM.replicate (area size) 0
    
    mapOffsets radiusSq ants $ \ant offset -> do
       let i = wrapIndex size (ant `addSize` offset)
        
       n <- UM.unsafeRead v i
       UM.unsafeWrite v i (n + 1)
     
    return v
                
circlePoints :: Int -> [Size]
circlePoints radiusSq =  [Size x y | x <- [-radius..radius], y <- [-radius..radius],  x * x + y * y <= radiusSq]
    where radius = ceiling (sqrt (fromIntegral radiusSq))       
        
        
mapOffsets :: (Monad m) => Int -> [a] -> (a -> Size -> m ()) -> m ()
mapOffsets radiusSq xs action = do
    forM_ xs $ \x -> do
        forM_ offsets $ \offset -> action x offset         
    where
        offsets = circlePoints radiusSq  
    
       
visibleSet :: Size -> Int -> [Point] -> U.Vector Bool
visibleSet size radiusSq ants = U.create $ do
    v <- UM.replicate (area size) False
    
    mapOffsets radiusSq ants $ \ant offset -> do
       UM.unsafeWrite v (wrapIndex size (ant `addSize` offset)) True
     
    return v
      
    
updateVisibility :: U.Vector Bool -> Map -> Map
updateVisibility vis (Map squares size) = Map squares' size 
    where
        squares' = S.imap (\index -> setVisibility (vis `U.unsafeIndex` index)) squares         
 
updateContent :: [SquareContent] -> Map -> Map
updateContent content (Map squares size) =  Map (S.modify updateSquares squares) size 
    where
        updateSquares v = flip mapM_ content  $ \(point, c) -> do
            let index = size `toIndex` point
            SM.unsafeWrite v index (setContent c $ squares `S.unsafeIndex` index) 
               

rectIndices :: Size -> Point -> Size -> [Int]
rectIndices (Size width height) (Point px py) (Size rh rw) = do
    x <- [xu `mod` width  | xu <- [px .. px + rh - 1]]
    y <- [yu `mod` height | yu <- [py .. py + rh - 1]]    
    
    return (y * width + x) 
        
                              
findSquareBy :: Map -> (Square -> Bool) -> Point -> Size -> Maybe Point
findSquareBy world f origin size =  fmap (fromIndex (mapSize world)) (find f' indices)
    where
        f' i = f (world `atIndex` i)
        indices = rectIndices (mapSize world) origin size

    
wrapIndex :: Size -> Point -> Int
wrapIndex (Size width height) (Point x y) = (y `mod` height) * width + x `mod` width
{-# INLINE wrapIndex #-}

        
neighbors :: Point -> [Point]
neighbors (Point x y) = 
    [ Point (x - 1) y
    , Point (x + 1) y
    , Point x (y - 1)
    , Point x (y + 1)
    ]

    
difference :: Size -> Point -> Point -> Size
difference (Size width height) (Point x1 y1) (Point x2 y2) = Size dx dy
     where
        dx = min ((x1 - x2) `mod` width) ((x2 - x1) `mod` width) 
        dy = min ((y1 - y2) `mod` height) ((y2 - y1) `mod` height)    
{-# INLINE difference #-}       
        
manhatten :: Size -> Point -> Point -> Int
manhatten size p1 p2 | (Size dx dy) <- difference size p1 p2 = dx  + dy        
{-# INLINE manhatten #-}       
    
manhattenIndex :: Size -> Int -> Int -> Int
manhattenIndex size p1 p2 = manhatten size (fromIndex size p1) (fromIndex size p2)  
{-# INLINE manhattenIndex #-}    


distanceSq :: Size -> Point -> Point -> Int
distanceSq size p1 p2 | (Size dx dy) <- difference size p1 p2 = dx * dx  + dy * dy
{-# INLINE distanceSq #-}      
    
distanceSqIndex :: Size -> Int -> Int -> Int
distanceSqIndex size p1 p2 = distanceSq size (fromIndex size p1) (fromIndex size p2)  
{-# INLINE distanceSqIndex #-}  

    
neighborIndices :: Size -> Int -> [Int]
neighborIndices size index = map (wrapIndex size) (neighbors point) where
    point = fromIndex size index
        
toIndex :: Size -> Point -> Int
toIndex (Size width height) (Point x y) = y * width + x
{-# INLINE toIndex #-}

fromIndex :: Size -> Int -> Point
fromIndex (Size width _) index = Point x y where
    (y, x) = index `divMod` width
{-# INLINE fromIndex #-}

at :: Map -> Point -> Square
at (Map squares size) point =  squares `S.unsafeIndex` (size `wrapIndex` point)
{-# INLINE at #-}


atIndex :: Map -> Int -> Square
atIndex (Map squares _) index =  squares `S.unsafeIndex` index
{-# INLINE atIndex #-}
