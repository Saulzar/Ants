module Main where

import Ant.IO
import Ant.Game

import Ant.Scheduler
import Ant.Movement

import Control.Monad.State.Strict


processTurn :: Int -> [SquareContent] -> Game [Order]
processTurn n content = do

    updateState content
    
    tasks <- gets (fst . gsAnts . gameStats) >>= scheduleAnts 
    orders <- moveAnts tasks

    return orders
    
    --orders <- liftM (map toEnum . randomRs (0, 3)) (liftIO getStdGen)
    --return $ zip (map fst ants) orders


runGame :: GameState -> IO GameState
runGame = execStateT (gameLoop processTurn)

    
main ::  IO ()
main = do
    settings <- readSettings
      
    runGame (initialState settings) 
    return ()
            

