module Ant.Game 
     ( GameState(..)
     , Game
     , runGame
     , initialState
     
     , module Ant.Point
     , module Ant.Square
     , module Ant.IO
     , module Ant.Map
     , module Ant.Scenario
     , module Ant.Graph
     , module Ant.Passibility
     
     )
    where

import Control.Monad
import Control.Monad.State.Strict

import qualified Data.Set as S

import Ant.Point
import Ant.IO
import Ant.Map
import Ant.Scenario
import Ant.Square
import Ant.Graph
import Ant.Passibility

import System.Random

type Game a = StateT GameState IO a


data GameState = GameState
    { gameSettings      :: !GameSettings
    , gameMap           :: Map
    , gameGraph         :: Graph
    , gamePass          :: Passibility
    }


 
initialState :: GameSettings -> GameState
initialState settings = GameState 
    { gameSettings    = settings
    , gameMap         = emptyMap unknownSquare (mapDimensions settings) 
    , gameGraph       = emptyGraph (mapDimensions settings) 32
    , gamePass = emptyPassibility (mapDimensions settings) pattern2
    }
 
 
            
playerAnt :: Int -> Content -> Bool
playerAnt p (Ant p')  = p == p' 
playerAnt _ _           = False
          
processTurn :: Int -> [SquareContent] -> Game [Order]
processTurn n content = do
    
    let ants = filter (playerAnt 0 . snd) content
    
    orders <- liftM (map toEnum . randomRs (0, 3)) (liftIO getStdGen)
    return $ zip (map fst ants) orders


runGame :: GameState -> IO GameState
runGame = execStateT (gameLoop processTurn)