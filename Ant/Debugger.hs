module Main where


import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Control.Monad

import Control.Concurrent

import Ant.Game
import Ant.Renderer

import qualified Data.Map as M

{-
createMap :: Map
createMap = fromFunction (Size 40 40) f where
    f (Point x y) | x == 1 || x == 38 = waterSquare
                  | y == 1 || y == 38 = waterSquare
                  | otherwiscenario <- readScenario "maps/maze/maze_02p_01.map"se = landSquare

   -}

testState :: IO GameState
testState = do
    --scenario <- readScenario "maps/random_walk/random_walk_08p_01.map"
    scenario <- readScenario "maps/maze/maze_02p_01.map"
    
    let graph  = emptyGraph (mapSize scenario) 16
    let (graph', r) = addRegion scenario graph (toIndex (mapSize scenario) (Point 24 11))
    let (graph'', r) = addRegion scenario graph' (toIndex (mapSize scenario) (Point 66 41))
    
    
    --print (map (\(p, d) -> fromIndex (mapSize scenario) p)  (M.toList (regionSquares r) ))
    
    print (isWater (scenario `at` (Point 16 11)))
    print (mapSize scenario)
    
    --print (regionSquares r)
    
    let graph3 = updateGraph scenario graph''
    let graph4 = updateGraph scenario graph3
    let graph5 = updateGraph scenario graph4
    let graph6 = updateGraph scenario graph5
    let graph7 = updateGraph scenario graph6
    
    let graph8 = iterate (updateGraph scenario) graph4
        
    return GameState
        { gameSettings = defaultSettings
        , gameMap      = scenario
        , gameGraph    = graph8 !! 20
        }
        
        
        

main :: IO ()
main = do
     initGUI
     window <- windowNew
     set window [windowTitle := "Hello Cairo",
                 windowDefaultWidth := 300, windowDefaultHeight := 200,
                 containerBorderWidth := 30 ]

     frame <- frameNew
     containerAdd window frame
     canvas <- drawingAreaNew
     containerAdd frame canvas
     widgetModifyBg canvas StateNormal (Color 65535 65535 65535)     
     widgetShowAll window 
     
     stateVar <- testState >>= newMVar
     canvas `on` exposeEvent $ updateCanvas stateVar
         
     onDestroy window mainQuit
     mainGUI

updateCanvas :: MVar GameState -> EventM EExpose Bool
updateCanvas stateVar = do
  win <- eventWindow
  
  liftIO $ do 
      state <- readMVar stateVar
      renderInWindow win state 
  return True
  
withTransform :: Render () -> Render ()
withTransform r = do
    m <- getMatrix 
    r
    setMatrix m
    
renderInWindow :: DrawableClass drawable => drawable -> GameState -> IO ()
renderInWindow win state = do 
    (width, height) <- drawableGetSize win
    
    let s = fromIntegral $ round $ max 1.0 (0.9 * squareSize width height world)

    let vw = ceiling (fromIntegral width / s)
    let vh = ceiling (fromIntegral height / s) 
    
    let (Size w h) = mapSize world
    
    let dw = (vw - w) `div` 2
    let dh = (vh - h) `div` 2
    
    renderWithDrawable win $ withTransform $ do 
        setAntialias AntialiasNone
        
        translate (fromIntegral dw * s) (fromIntegral dh * s)
        scale s s

        --renderMap (worldColour world) (Point (-dw) (-dh)) (Point (w + dw) (h + dh))
        renderMap (graphColours world graph) (Point (-dw) (-dh)) (Point (w + dw) (h + dh))
        
        setSourceRGB 0 0 0 
        setLineWidth (2.0 / s)
        rectangle' 0 0 w h
        stroke
        
    where
        world = gameMap state
        graph = gameGraph state
        

               
        
        
squareSize :: Int -> Int -> Map -> Double
squareSize width height world = min aspectW aspectH
    where
        (Size width' height') = mapSize world
        aspectW = aspect width width'
        aspectH = aspect height height'
 
        aspect x y = fromIntegral x / fromIntegral y        