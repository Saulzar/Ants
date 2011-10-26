{-# LANGUAGE PatternGuards #-}

module AntIO 
    ( GameSettings (..)
    , Content (..)
    , Direction (..)
    , Order 
    , Player
    , readSettings
    , gameLoop
    )
    
where
 
import Point 
 
import Data.Maybe
import Control.Monad.State
import System.IO


data Direction = North | East | South | West deriving (Show, Eq, Enum)
type Player = Int

type Order   = (Point, Direction)
data Content = Water    !Point
             | Food     !Point
             | Hill     !Point !Player
             | Ant      !Point !Player deriving (Show, Eq)


data GameSettings = GameSettings
     { loadTime :: !Int
     , turnTime :: !Int
     , mapSize      :: !Size
     , maxTurns     :: !Int
     , viewRadius2  :: !Int
     , attackRadius2 :: !Int
     , spawnRadius2 :: !Int
     , playerSeed   :: !Int
     }

type Setting = (String, Int)

makeSettings :: [Setting] -> GameSettings
makeSettings settings = GameSettings 
    { loadTime          = setting 3000 "loadtime"
    , turnTime          = setting 1000 "turntime"
    , mapSize           = fromMaybe (Size 60 90) (liftM2 Size (find "cols") (find "rows"))
    , maxTurns          = setting 1000 "turns"
    , viewRadius2       = setting 55 "viewradius2"
    , attackRadius2     = setting 5 "attackradius2"
    , spawnRadius2      = setting 1 "spawnradius2"
    , playerSeed        = setting 0 "player_seed"
    }
    
    where
       find     = (`lookup` settings)
       setting d = (fromMaybe d) . find               
             
             
orderString :: Order -> String
orderString (Point x y, dir) = "o " ++ (show x) ++ " " ++ (show y) ++ " " ++ (dirString dir)
    where
        dirString North = "N"
        dirString East  = "E"
        dirString South = "S"
        dirString West  = "W"
             
maybeRead :: Read a => String -> Maybe a 
maybeRead = fmap fst . listToMaybe . reads


readSetting :: String -> Maybe Setting
readSetting str | [key, value] <- (words str) = liftM ((,) key) (maybeRead value) 
                | otherwise                   = Nothing
                
readSettings :: IO GameSettings
readSettings = do
    n <- liftM readTurn getLine
    settings <- readLines "ready" readSetting
    
    return (makeSettings settings)
    

readTurn :: String -> Maybe Int
readTurn str | ["turn", value] <- (words str) =  maybeRead value 
         | otherwise                   = Nothing


readContent :: String -> Maybe Content
readContent str | (c : args) <- (words str) = (mapM maybeRead args) >>= content' c
                | otherwise                 = Nothing
    where 
       content' "a" [x, y, p] = Just $ Ant   (Point x y) p    
       content' "h" [x, y, p] = Just $ Hill  (Point x y) p
       content' "w" [x, y]    = Just $ Water (Point x y)
       content' "f" [x, y]    = Just $ Food  (Point x y)     
       content' _   _         = Nothing


readLines :: String -> (String -> Maybe a) -> IO [a]
readLines end reader = liftM catMaybes (readLines' []) 
    where 
        readLines' xs = do
            line <- getLine
            
            if line == end 
               then return xs
               else readLines' $ (reader line) : xs
               

beginTurn :: IO ()
beginTurn = do
    putStrLn "go"
    hFlush stdout
    

gameLoop :: (MonadIO m) => (Int -> [Content] -> m [Order]) ->  m ()
gameLoop turn = do
    
    liftIO beginTurn
    n <- liftIO $ liftM readTurn getLine
    
    when (isJust n) $ do
        
        content <- liftIO $ readLines "go" readContent    
        orders <- turn (fromJust n) content
        
        liftIO $ mapM_ (putStrLn . orderString)  orders
        gameLoop turn
        
        