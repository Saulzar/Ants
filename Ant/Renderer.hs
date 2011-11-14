{-# LANGUAGE BangPatterns, PatternGuards #-}

module Ant.Renderer where

import Control.Monad
import Graphics.Rendering.Cairo

import qualified Data.Vector as V

import Ant.Game

  
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV

import Data.Colour


  
setColour :: Colour Double -> Render ()
setColour c =  setSourceRGB r g b where
    (RGB r g b) = toSRGB c
{-# INLINE setColour #-}
    
squareColour :: Square -> Colour Double
squareColour square | not (wasSeen square) = black
                    | isVisible square     = squareColour'
                    | otherwise            = blend 0.5 black squareColour'
    where 
        squareColour' | isWater square        = blue
                      | otherwise             = saddlebrown
{-# INLINE squareColour #-}

rectangle' :: Int -> Int -> Int -> Int -> Render ()
rectangle' !x !y !w !h = rectangle (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)   
{-# INLINE rectangle' #-}
  
renderMap :: (Point -> Colour Double) -> Point -> Point -> Render ()
renderMap f (Point sx sy) (Point ex ey) = do

   forM_ [sx.. ex] $ \x -> do
     forM_ [sy.. ey] $ \y -> do
       setColour $ f (Point x y)
       rectangle' x y 1 1
       fill
   

worldColour :: Map -> Point -> Colour Double
worldColour world p = squareColour (world `at` p)
{-# INLINE worldColour #-}


regionColour ::  Int -> Colour Double
regionColour i = makeRGB (fromIntegral (i * 103 `mod` 360))  
  where
      makeRGB hue = uncurryRGB sRGB (hsv hue 1 1)

      
circle :: Point -> Double -> Render ()
circle (Point x y) r = arc (fromIntegral x) (fromIntegral y) r 0 (pi * 2.0)        
{-# INLINE circle #-}

renderGraph :: Graph -> Render()
renderGraph graph = do      

    setColour darkseagreen

    V.forM_ (regions graph) $ \r -> do 
        let p = fromIndex  (graphSize graph) (regionCentre r)
        circle p 1.0 
        fill
      
      
graphColours :: Map -> Graph -> Point -> Colour Double
graphColours world graph p   | isWater square   = blue
                             | region < 0       = white
                             | otherwise        = blend scale burlywood baseColour  
     where                   

        square = world `at` p
        (region, distance) = graph `graphSquare` p 
        
        scale = fromIntegral distance / fromIntegral (regionDistance graph)
        baseColour = regionColour region 
    
{-# INLINE graphColours #-}