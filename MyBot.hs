module Main where

import System.Cmd

main :: IO ()
main = do 
    rawSystem "./Ants" []
    return ()