module Main where


import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Control.Monad

import Control.Monad.State.Strict
import Control.Concurrent


import Data.List
import Data.Maybe

import Ant.Game
import Ant.Renderer
import Ant.Scheduler
import Ant.Diffusion
import Ant.GameLoop
import Ant.Movement


import Debug.Trace

import qualified Data.IntMap as M
import qualified Data.Vector.Unboxed as U


smallMap :: Map
smallMap = fromFunction (Size 40 40) f where
    f  = const landSquare   

     
        {-
main :: IO () 
main = do
    print "Starting..."   
    time $ testState        

    print "Done."
        -}

main :: IO ()
main = do

    
    initGUI
    window <- windowNew

    set window [windowTitle := "Hello Cairo",
             windowDefaultWidth := 800, windowDefaultHeight := 600,
             containerBorderWidth := 10 ]


             
    frame <- frameNew
    containerAdd window frame
    canvas <- drawingAreaNew
    containerAdd frame canvas
    widgetModifyBg canvas StateNormal (Color 65535 65535 65535)     
    widgetShowAll window 
    
    settings <- readSettings
    print settings
    
    state <- runGame (initialState settings) 
    
        
    stateVar <- newMVar state
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
    
normalizeIntensity :: U.Vector Float -> (Int -> Float)
normalizeIntensity v = intensity where
    v' = U.toList v
    (low, high) = (minimum v', maximum v')
    
    scale = 1.0 / (high - low)
    intensity i = scale * (v `indexU` i - low)   
    
renderInWindow :: DrawableClass drawable => drawable -> GameState -> IO ()
renderInWindow win state = do 
    (width, height) <- drawableGetSize win
    
    let s = fromIntegral $ round $ max 1.0 (0.9 * squareSize width height world)

    let vw = ceiling (fromIntegral width / s)
    let vh = ceiling (fromIntegral height / s) 
    
    let (Size w h) = mapSize world
    
    let dw = (vw - w) `div` 2
    let dh = (vh - h) `div` 2
    
    (tasks, paths, density) <- flip evalStateT state $ do
        
        ants  <- gets (fst . gsAnts . gameStats)
        tasks <- scheduleAnts ants 
        paths <- runMove (antPaths tasks)

        (density, _) <- runScheduler ants flowDensity 
        return (tasks, paths, density)
    
    renderWithDrawable win $ withTransform $ do 
        --setAntialias AntialiasNone
        
        translate (fromIntegral dw * s) (fromIntegral dh * s)
        scale s s

        let (start, end) = (Point (-dw) (-dh), Point (w + dw) (h + dh))
        
        --renderMap (worldColour world) start end
        --renderMap (regionColours world (regionMap builder)) start end    
        
        let f = normalizeIntensity density 
        
        renderMap (regionColours' f world (regionMap builder)) start end    
        
        --renderMap (passColours world (gamePass state)) start end
        
        renderPoints (concat paths)
        renderContent world start end 
        
        renderTasks (mapSize world) graph tasks

        
        setSourceRGBA 0.0 0.0 0.0 0.4 
        setLineWidth (2.0 / s)
        rectangle (-0.5) (-0.5) (fromIntegral w) (fromIntegral h)
        stroke
        
        
    where
        world = gameMap state
        graph = gameGraph state
        builder = gameBuilder state
        stats    = gameStats state

        ants = fst . gsAnts $ stats
        
        -- diffGr = runScheduler world stats graph antSet diffuseAnts     
            
        --diffGr = diffusionGraph graph density       
        --diffused = (diffuse 1.2 diffGr) !! 200
        
        
        
squareSize :: Int -> Int -> Map -> Float
squareSize width height world = min aspectW aspectH
    where
        (Size width' height') = mapSize world
        aspectW = aspect width width'
        aspectH = aspect height height'
 
        aspect x y = fromIntegral x / fromIntegral y        