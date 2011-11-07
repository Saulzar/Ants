{-# LANGUAGE PatternGuards #-}

module Ant.IO 
    ( GameSettings (..)
    , Content (..)
    , SquareContent
    , Direction (..)
    , Order 
    , Player
    , readSettings
    , gameLoop
    , defaultSettings
    )
    
where
 
import Ant.Point 
 
import Data.Maybe
import Control.Monad.State
import System.IO
import Ant.Util

data Direction = North | East | South | West deriving (Show, Eq, Enum)
type Player = Int

type Order   = (Point, Direction)
data Content = Water   
             | Food   
             | Hill  !Player
             | Ant   !Player 
             | DeadAnt !Player  
      deriving (Show, Eq)

type SquareContent = (Point, Content)             

data GameSettings = GameSettings
     { loadTime :: !Int
     , turnTime :: !Int
     , mapDimensions :: !Size
     , maxTurns      :: !Int
     , viewRadius2   :: !Int
     , attackRadius2 :: !Int
     , spawnRadius2  :: !Int
     , playerSeed    :: !Int
     }
     

defaultSettings :: GameSettings
defaultSettings = makeSettings []

makeSettings :: [Setting] -> GameSettings
makeSettings settings = GameSettings 
    { loadTime          = setting 3000 "loadtime"
    , turnTime          = setting 1000 "turntime"
    , mapDimensions     = fromMaybe (Size 60 90) (liftM2 Size (find "cols") (find "rows"))
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
             

expectLine :: String -> IO ()
expectLine  str = do
    line <- getLine 
    if (line /= str)
       then hPutStrLn stderr ("Expected: " ++ str ++ " got: " ++ str)
       else return ()

                
readSettings :: IO GameSettings
readSettings = do
    n <- liftM readTurn getLine
    settings <- readLines stdin readSetting
    
    expectLine "ready"
    return (makeSettings settings)
    

readTurn :: String -> Maybe Int
readTurn str | ["turn", value] <- (words str) =  maybeRead value 
         | otherwise                   = Nothing


readContent :: String -> Maybe SquareContent
readContent str | (c : args) <- (words str) = (mapM maybeRead args) >>= content' c
                | otherwise                 = Nothing
    where 
       content' "a" [x, y, p] = at x y (Ant p)    
       content' "h" [x, y, p] = at x y (Hill p)
       content' "w" [x, y]    = at x y Water
       content' "f" [x, y]    = at x y Food
       content' "d" [x, y, p] = at x y (DeadAnt p)
            
       content' _   _         = Nothing
       
       at x y c = Just (Point x y, c)



               

beginTurn :: IO ()
beginTurn = do
    putStrLn "go"
    hFlush stdout
    

gameLoop :: (MonadIO m) => (Int -> [SquareContent] -> m [Order]) ->  m ()
gameLoop turn = do
    
    liftIO beginTurn
    n <- liftIO $ liftM readTurn getLine
    
    when (isJust n) $ do
        
        content <- liftIO $ readLines stdin readContent
        liftIO $ expectLine "go"
            
        orders <- turn (fromJust n) content
        
        liftIO $ mapM_ (putStrLn . orderString)  orders
        gameLoop turn
        
        