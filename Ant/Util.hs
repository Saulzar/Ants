module Ant.Util 
    ( readLines
    , maybeRead
    
    , Setting
    , readSetting
    )
    
where


import Data.Maybe
import Control.Monad.State
import System.IO

maybeRead :: Read a => String -> Maybe a 
maybeRead = fmap fst . listToMaybe . reads

readLines :: Handle -> (String -> Maybe a) -> IO [a]
readLines handle reader = readLines' [] 
    where 
        readLines' xs = do
            eof <- hIsEOF handle 
            
            if eof 
               then return xs
               else do 
                    pos  <- hGetPosn handle
                    line <- hGetLine handle
                    
                    case reader line of 
                         Nothing -> hSetPosn pos >> return xs 
                         Just x  -> readLines' (x : xs)
               

type Setting = (String, Int)
                              
readSetting :: String -> Maybe Setting
readSetting str | [key, value] <- (words str) = liftM ((,) key) (maybeRead value) 
                | otherwise                   = Nothing
                