module Main where

import Control.Monad
import Control.Monad.State.Strict

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import qualified Data.Set as S

import Ant.Point
import Ant.IO
import Ant.Map

import System.Random

type Game a = StateT GameState IO a


data GameState = GameState
    { gameSettings      :: !GameSettings
    
    , gameMap               :: Map
    }


 
initialState :: GameSettings -> GameState
initialState settings = GameState 
    { gameSettings    = settings
    , gameMap         = emptyMap (mapDimensions settings) 
    }
 

    
main ::  IO ()
main = do
    settings <- readSettings
      
    execStateT (gameLoop processTurn) (initialState settings) 
    return ()
            
            
isAnt :: Int -> Content -> Bool
isAnt p (Ant p')  = p == p' 
isAnt _ _           = False
          
processTurn :: Int -> [SquareContent] -> Game [Order]
processTurn n content = do
    
    let ants = filter (isAnt 0 . snd) content
    
    orders <- liftM (map toEnum . randomRs (0, 3)) (liftIO getStdGen)
    return $ zip (map fst ants) orders
