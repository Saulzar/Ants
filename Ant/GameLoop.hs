module Ant.GameLoop
    ( processTurn
    , runGame
    
    )
    where

import Ant.IO
import Ant.Game

import Ant.Scheduler
import Ant.Movement

import Control.Monad.State.Strict
import System.IO


processTurn :: Int -> [SquareContent] -> Game [Order]
processTurn n content = do

    updateState content
    
    tasks <- gets (fst . gsAnts . gameStats) >>= scheduleAnts 
    --liftIO $ hPrint stderr tasks 
    
    orders <- moveAnts tasks

    return orders
    
    --orders <- liftM (map toEnum . randomRs (0, 3)) (liftIO getStdGen)
    --return $ zip (map fst ants) orders


runGame :: GameState -> IO GameState
runGame = execStateT (gameLoop processTurn)
