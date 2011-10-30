{-# LANGUAGE NoMonomorphismRestriction, BangPatterns #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import Data.Colour.SRGB         ( sRGB       )
import Data.Colour.RGBSpace     ( uncurryRGB )
import Data.Colour.RGBSpace.HSV ( hsv        )
import Data.Colour

markerColor :: Int -> Int -> Colour Double
markerColor i n = uncurryRGB sRGB (hsv (360* fromIntegral i / fromIntegral n) 1 1)

main = do
    let format = PNG (400, 400)
    fst (renderDia Cairo (CairoOptions "test.png" format) (testMarkers `mappend` squares) )
    
    return ()
    
testMarkers = markers [((fromIntegral x, fromIntegral y), markerColor (x + y) 10) |  x <- [1,7..30], y <- [1,4..30]]
    
markers :: [((Double, Double), Colour Double)] -> Diagram Cairo R2
markers p = mconcat (map markerShape p)
    where
        markerShape (p, c) = translate p  (circle 0.5 # fc c) 
    
    
squares  ::  Diagram Cairo R2   
squares = position [(P (fromIntegral x, fromIntegral y), squareAt x y) | x <- [0..80], y <- [0..80]] 
   where 
       squareAt !x !y  = square 1 
           # fc (if (x + y) `mod` 2 > 0 then white else black)
       