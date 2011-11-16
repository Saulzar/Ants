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
import Data.Function
import Data.List
import Data.Maybe

import Debug.Trace
import qualified Data.IntMap as M

  
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
   setAntialias AntialiasNone

   forM_ [sx.. ex] $ \x -> do
     forM_ [sy.. ey] $ \y -> do
       setColour $ f (Point x y)
       rectangle (fromIntegral x - 0.5) (fromIntegral y - 0.5) 1 1
       fill
   
   setAntialias AntialiasDefault
   

worldColour :: Map -> Point -> Colour Double
worldColour world p = squareColour (world `at` p)
{-# INLINE worldColour #-}


regionColour ::  Int -> Colour Double
regionColour i = makeRGB (fromIntegral (i * 117 `mod` 360))  
  where
      makeRGB hue = uncurryRGB sRGB (hsv hue 1 1)

      
drawCircle :: Point -> Double -> Render ()
drawCircle (Point x y) r = arc (fromIntegral x) (fromIntegral y) r 0 (pi * 2.0)        
{-# INLINE drawCircle #-}

drawLine :: Point -> Point -> Render ()
drawLine (Point x y) (Point x' y') = do 
    moveTo (fromIntegral x) (fromIntegral y)
    lineTo (fromIntegral x') (fromIntegral y')

drawLine' :: Point -> Size -> Render ()
drawLine' (Point x y) (Size dx dy) = do 
    moveTo (fromIntegral x) (fromIntegral y)
    lineTo (fromIntegral (x + dx)) (fromIntegral (y + dy))
    
    
wrapDiff :: Size -> Point -> Point -> Size
wrapDiff (Size width height) (Point x1 y1) (Point x2 y2) = Size (wrap x1 x2 width) (wrap y1 y2 height)
    where
        wrap d1 d2 dim = minimumBy (compare `on` abs) [d2 - d1, d2 - d1 + dim, d2 - d1 - dim]
    
            
renderGraph :: Graph -> Render()
renderGraph graph = do      

    setLineWidth 0.3
    
    setSourceRGBA 1 1 0 0.4    
    forM_ (M.toList (regions graph)) $ \(i, r) -> do 
        drawCircle (regionCentre r) 0.6 
        fill
          
        traceShow (regionNeighbors r) $ return ()
  
        forM_ (M.toList (regionNeighbors r)) $ \(j, n) -> do
            
            let r' = fromJust (j `M.lookup` (regions graph))
            let d = wrapDiff (graphSize graph)  (regionCentre r)  (regionCentre r')
          
            drawLine' (regionCentre r) d
            stroke
                               
      
mapColours :: Map -> Colour Double -> Point -> Colour Double
mapColours world c p   | isWater square         = lightblue
                       | otherwise              = c  
     where                   
        square = world `at` p
{-# INLINE mapColours #-}



graphColours :: Map -> Graph -> Point -> Colour Double
graphColours world graph p = mapColours world colour p  
     where                   
        (region, distance) = graph `graphSquare` p 
        
        scale = fromIntegral distance / fromIntegral (regionDistance graph)
        colour | region >= 0 = blend scale (regionColour region) black
               | otherwise   = red
{-# INLINE graphColours #-}



passColours :: Map -> Passibility -> Point -> Colour Double
passColours world pass p =  mapColours world colour p  
     where                   
        cost = pass `squareCost` p 
        
        scale = fromIntegral cost / fromIntegral (maxCost pass)
        colour = blend scale green white
    
{-# INLINE passColours #-}



