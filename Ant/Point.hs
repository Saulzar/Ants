{-# LANGUAGE BangPatterns #-}

module Ant.Point where

data Point = Point 
    { pointX ::  {-# UNPACK #-} !Int
    , pointY ::  {-# UNPACK #-} !Int 
    } 
      deriving (Show, Ord, Eq)


data Size = Size    {-# UNPACK #-}   !Int
                    {-# UNPACK #-}   !Int  deriving (Show, Ord, Eq)             
                 
modDistance :: Int -> Int -> Int -> Int
modDistance n x y = min ((x - y) `mod` n) ((y - x) `mod` n)


wrap :: Size -> Point -> Point
wrap (Size width height) (Point x y) = Point (x `mod` width) (y `mod` height)

addSize :: Point -> Size -> Point
addSize (Point x y) (Size dx dy) = Point (x + dx) (y + dy)

addSizes :: Size -> Size -> Size
addSizes (Size x y) (Size x' y') = Size (x + x') (y + y')

subSize :: Point -> Size -> Point
subSize (Point x y) (Size dx dy) = Point (x - dx) (y - dy)

subPoint :: Point -> Point -> Size
subPoint (Point x1 y1) (Point x2 y2) = Size (x1 - x2) (y1 - y2)


area :: Size -> Int
area (Size width height) = width * height

manhattan :: Size -> Point -> Point -> Int
manhattan (Size width height) (Point x1 y1) (Point x2 y2) = dx + dy
  where
      dx = modDistance width x1 x2
      dy = modDistance height y1 y2

      
distSquared :: Size  -> Point -> Point -> Int
distSquared (Size width height) (Point x1 y1) (Point x2 y2) = dx * dx + dy * dy
  where
      dx = modDistance width x1 x2
      dy = modDistance height y1 y2
