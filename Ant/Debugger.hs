module Main where


import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Control.Monad

import Control.Concurrent
import Data.List

import Ant.Game
import Ant.Renderer

import System.CPUTime
import Text.Printf

import qualified Data.IntMap as M
import qualified Data.Vector.Unboxed as U

createMap :: Map
createMap = fromFunction (Size 40 40) f where
    f (Point x y) | x > 4 && x < 36 && y > 4 && y < 36 = landSquare
                  | otherwise = waterSquare

smallMap :: Map
smallMap = fromFunction (Size 40 40) f where
    f  = const landSquare   

testState :: IO GameState
testState = do
    --scenario <- readScenario "maps/random_walk/random_walk_02p_01.map"
    scenario <- readScenario "maps/maze/maze_08p_01.map"
    --let scenario = smallMap
    
    --let scenario = tileMap scenario1 (Size 300 300)
    
    let graph  = emptyGraph (mapSize scenario) 100
    
    let (Size w h) = (mapSize scenario)
    
    let points = [Point x y | x <- [5, 10 .. w - 1], y <- [5, 10 .. h - 1]]
    let graph' = foldr addRegion graph points
  
    
--    let (graph', r) = addRegion scenario graph (toIndex (mapSize scenario) (Point 24 11))
--    let (graph'', r) = addRegion scenario graph' (toIndex (mapSize scenario) (Point 66 41))
    
    
    --print (map (\(p, d) -> fromIndex (mapSize scenario) p)  (M.toList (regionSquares r) ))

    let allSquares = U.fromList [0.. w * h - 1]         
    let pass = updatePassibility allSquares scenario (emptyPassibility (Size w h) pattern2)
        
    print (maxCost pass)
        
    print (mapSize scenario)
    
    --print (regionSquares r)
           
    let graphs = iterate (updateGraph pass scenario ) graph'
    let final = graphs !! 10
      
    print (M.size (regions final))
    print $ neighborsValid final
        
    return GameState
        { gameSettings = defaultSettings
        , gameMap      = scenario
        , gameGraph    = final 
        , gamePass     = pass
        }

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v        
        {-
main :: IO () 
main = do
    print "Starting..."   
    time $ testState        

    print "Done."
        -}

main :: IO ()
main = do
    settings <- readSettings
    print settings
    
    state <- runGame (initialState settings) 

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
        --setAntialias AntialiasNone
        
        translate (fromIntegral dw * s) (fromIntegral dh * s)
        scale s s

        let (start, end) = (Point (-dw) (-dh), Point (w + dw) (h + dh))
        
        --renderMap (worldColour world) start end
        renderMap (graphColours world graph) start end    
        renderGraph graph
        
        
        {-let (Just r) = M.lookup 8 (regions graph)
    
        let visited = searchRegion (gamePass state) world (regionMap graph) r   
        let neighbors = neighborSquares (mapSize world) visited
        let (sqs, rs) = findChanges (regionMap graph) visited 
        
        setSourceRGBA 0 0 0.4 1 
                
        forM_ (M.toList visited) $ \(i, d) ->  do
            let p = fromIndex (mapSize world) i
            drawCircle p 0.2
            fill
        -}
        
        --renderMap (passColours world (gamePass state)) start end
        --renderContent world start end 
        
        setSourceRGBA 0.0 0.0 0.0 0.4 
        setLineWidth (2.0 / s)
        rectangle (-0.5) (-0.5) (fromIntegral w) (fromIntegral h)
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