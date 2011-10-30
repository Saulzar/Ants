{-# LANGUAGE BangPatterns #-}

module Ant.Scenario where

import Data.Maybe
import Control.Monad
import Data.List

import System.IO

import Ant.Point
import Ant.Square
import Ant.Util
import Ant.Map


data ScenarioSettings = ScenarioSettings 
    {   scenarioPlayers         :: Int
    ,   scenarioDimensions      :: Size
    }

    
mapLine :: String -> Maybe [Square]
mapLine ('m' : ' ' : rest) = Just (map charSquare rest)
mapLine _                  = Nothing    

readSquares :: Size -> Handle -> IO Map
readSquares size handle = do
    squares <- liftM concat $ readLines handle mapLine
    return $ fromSquares size squares
    
    
readScenario :: FilePath -> IO Map
readScenario filePath = do
    
    handle <- openFile filePath ReadMode
    settings <- liftM makeSettings (readLines handle readSetting)

    case settings of 
         Nothing -> error "Error reading scenario settings"
         Just (size, players) -> readSquares size handle
               
makeSettings :: [Setting] -> Maybe (Size, Int)
makeSettings settings = do
    width       <- find "rows"
    height      <- find "cols"
    players     <- find "players"
    
    return (Size width height, players)
    
    where    
        find     = (`lookup` settings)
    
   