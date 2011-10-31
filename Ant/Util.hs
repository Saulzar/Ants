{-# LANGUAGE PatternGuards #-}

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
        readLines' xs = checkEOF handle xs $ do
            line <- hGetLine handle
            
            case reader line of 
                 Nothing -> return xs 
                 Just x  -> readLines' (x : xs)

checkEOF :: Handle -> a -> IO a -> IO a
checkEOF handle x io = do
    eof <- hIsEOF handle
    
    if eof 
       then return x
       else io 
       
readLinesN :: Int -> Handle -> (String -> Maybe a) -> IO [a]
readLinesN n handle reader = do
    lines <- replicateM n (hGetLine handle)
    return $ catMaybes $ map reader $ lines
     

type Setting = (String, Int)
                              
readSetting :: String -> Maybe Setting
readSetting str | [key, value] <- (words str) = liftM ((,) key) (maybeRead value) 
                | otherwise                   = Nothing
                