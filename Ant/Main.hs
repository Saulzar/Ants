module Main where

import Ant.IO
import Ant.Game

    
main ::  IO ()
main = do
    settings <- readSettings
      
    runGame (initialState settings) 
    return ()
            

