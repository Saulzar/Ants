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

import qualified Data.Vector as V

import Debug.Trace
import qualified Data.IntMap as M

  
setColour :: Colour Float -> Render ()
setColour c =  setSourceRGB (realToFrac r) (realToFrac g) (realToFrac b) where
    (RGB r g b) = toSRGB c
{-# INLINE setColour #-}
    
squareColour :: Square -> Colour Float
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
  
renderMap :: (Point -> Colour Float) -> Point -> Point -> Render ()
renderMap f (Point sx sy) (Point ex ey) = do
   setAntialias AntialiasNone

   forM_ [sx.. ex] $ \x -> do
     forM_ [sy.. ey] $ \y -> do
       setColour $ f (Point x y)
       rectangle (fromIntegral x - 0.5) (fromIntegral y - 0.5) 1 1
       fill
   
   setAntialias AntialiasDefault

drawHill :: Player -> Point -> Render ()
drawHill player p = do
    setColour black
    drawCircle p 0.8 >> fillPreserve 
    
    setLineWidth 0.1
    setColour (antColour player) >> stroke
    drawTextAt p "H"

    
drawAnt :: Player -> Point -> Render ()
drawAnt player p = do
    setColour (antColour player)
    drawCircle p 0.5
    fill
    
drawFood :: Point -> Render ()
drawFood p = do
    setFontSize 2.0
    setColour magenta
    drawTextAt p "F"
   

whenMaybe :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenMaybe (Just v) f  = f v
whenMaybe (Nothing) _ = return () 
 
renderContent :: Map -> Point -> Point -> Render ()
renderContent world (Point sx sy) (Point ex ey) = do
   setFontSize 1.0

   forM_ [sx.. ex] $ \x -> do
     forM_ [sy.. ey] $ \y -> do
       let p = (Point x y)
       renderSquare (world `at` p) p

renderPoints :: [Point] -> Render ()
renderPoints ps = do
   setColour magenta
   
   forM_ ps $ \p -> do
        drawCircle p 0.5
        fill  
 
renderSquare :: Square -> Point  -> Render ()
renderSquare sq p = do
    
    whenMaybe (squareHill sq) $ \player ->
        drawHill player p
        
    whenMaybe (squareAnt sq) $ \player ->
        drawAnt player p
    
    when (hasFood sq) $ drawFood p

    
worldColour :: Map -> Point -> Colour Float
worldColour world p = squareColour (world `at` p)
{-# INLINE worldColour #-}

regionColourSet :: V.Vector (Colour Float)
regionColourSet = V.fromList [lightsalmon, lightseagreen, cornflowerblue, brown, pink, cadetblue, olive, brown, moccasin, darkkhaki, cornsilk, lightsteelblue, darkgoldenrod, azure]

regionColour ::  Int -> Colour Float
regionColour i = regionColourSet `V.unsafeIndex` (i `mod` V.length regionColourSet)

antColourSet :: V.Vector (Colour Float)
antColourSet = V.fromList [white, lightgreen, orange, darkturquoise, red, blue, lightsalmon, mediumpurple]


antColour ::  Int -> Colour Float
antColour i = antColourSet `V.unsafeIndex` (i `mod` V.length antColourSet)


{-makeRGB (fromIntegral (i * 117 `mod` 360))  
  where
      makeRGB hue = uncurryRGB sRGB (hsv hue 1 1)
-}
      
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
    
moveTo' :: Point -> Render ()
moveTo' (Point x y) = moveTo (fromIntegral x) (fromIntegral y)
            

drawTextAt :: Point -> String -> Render ()
drawTextAt (Point x y) str = do
    ext <- textExtents str
    
    moveTo (fromIntegral x + 0.4 - textExtentsWidth ext * 0.5) (fromIntegral y  + textExtentsHeight ext * 0.5)
    showText str
            
renderGraph :: Size -> Graph -> Render()
renderGraph worldSize graph = do      

    setLineWidth 0.3
    
    setSourceRGBA 1 1 0 0.4    
    forM_ (grRegions graph) $ \r -> do  
          
        forM_ (grNeighbors graph r) $ \(r', e) -> do
            
            let d = wrapDiff worldSize  (regionCentre r)  (regionCentre r')
          
            drawLine' (regionCentre r) d
            stroke
            
    setFontSize 1.0
   
    
    forM_ (grRegions graph) $ \r -> do 
        setSourceRGB 0 0 0
        drawCircle (regionCentre r) 0.6 
        fill 

        setSourceRGB 1 1 0        
        drawTextAt (regionCentre r) (show (regionId r))
        

    
mapColours :: Map -> Colour Float -> Point -> Colour Float
mapColours world c p   | isWater square         = black
                       | otherwise              = c  
     where                   
        square = world `at` p
{-# INLINE mapColours #-}



regionColours :: Map -> RegionMap -> Point -> Colour Float
regionColours world regionMap p = mapColours world colour p  
     where                    
        region = regionAt regionMap (mapSize world) p
        
        colour | region >= 0 = (regionColour region)
               | otherwise   = red
{-# INLINE regionColours #-}



regionColours' :: (RegionIndex -> Float) -> Map -> RegionMap -> Point -> Colour Float
regionColours' lookupColor world regionMap p = mapColours world colour p  
     where                    
        region = regionAt regionMap (mapSize world) p
        intensity = lookupColor region

        colour | region >= 0 = (sRGB 0 intensity 0)
               | otherwise   = red
{-# INLINE regionColours' #-}




passColours :: Map -> Passibility -> Point -> Colour Float
passColours world pass p =  mapColours world colour p  
     where                   
        cost = pass `squareCost` p 
        
        scale = fromIntegral cost / fromIntegral (maxCost pass)
        colour = blend scale white darkgreen
    
{-# INLINE passColours #-}



