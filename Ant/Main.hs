module Main where

import Control.Monad
import Control.Monad.State.Strict

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import qualified Data.Set as S

import Ant.Point
import Ant.IO

import System.Random

type Game a = StateT GameState IO a


data GameState = GameState
    { gameSettings      :: !GameSettings
    
    , gameMap               :: Map
    , mapSize              :: Size 
    }


 
initialState :: GameSettings -> GameState
initialState settings = GameState 
    { gameSettings    = settings
    , gameMap         = emptyMap (mapSize settings) 
    }
   
    where
       mapArea = area (mapSize settings) 
    
main ::  IO ()
main = do
    settings <- readSettings
      
    execStateT (gameLoop processTurn) (initialState settings) 
    return ()
            
            
isAnt :: Int -> Content -> Bool
isAnt p (Ant _ p')  = p == p' 
isAnt _ _           = False
          
processTurn :: Int -> [Content] -> Game [Order]
processTurn n content = do
    
    let ants = filter (isAnt 0) content
    orders <- flip mapM ants $ \(Ant point p) -> do
        dir <- liftIO $ fmap toEnum $ randomRIO (0, 3)
        return (point, dir)
        
    return orders