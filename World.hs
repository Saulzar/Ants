{-# LANGUAGE BangPatterns #-}


module World where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Control.Monad.ST

import Data.List
import Data.Char

import Point


type Player = Int
type Ant = (Point, Player)

data Tile = Water
          | Land !Object !Bool
          | Unknown deriving (Show, Eq)
 
data Object = Ant !Player
            | Hill !Player
            | DeadAnt !Player
            | Food 
            | Empty deriving (Show, Eq)
                    
data World = World { worldTiles       :: V.Vector Tile
                   , worldSize        :: {-# UNPACK #-}   !Size
                   }

worldWidth :: World -> Int
worldWidth world = let (Size width _) = worldSize world in width                        
        
worldHeight :: World -> Int
worldHeight world = let (Size height _) = worldSize world in height                      
        
        
tileChar :: Tile -> Char
tileChar Unknown           = '?'
tileChar Water             = '~' 
tileChar (Land (Ant p) vis)  = if vis then toUpper char else char where
    char = chr(ord 'a' + p)
tileChar (Land (Hill p) vis)      = chr(ord '0' + p)
tileChar (Land (DeadAnt p) True)  = 'x'
tileChar (Land Empty  vis)        = if vis then '.' else ','       
   
   
buildRow :: World -> Int -> String
buildRow world row = V.toList $ V.generate (worldWidth world) (\col -> tileChar $ world `at` (Point row col))
    
buildMap :: World -> [String]
buildMap world = map (buildRow world) [0..(worldHeight world) - 1] 

instance Show World where
    show world = concat (intersperse "\n" (show (worldSize world) : buildMap world)) 

          
emptyWorld :: Size -> World
emptyWorld size = World  
    { worldTiles        = V.replicate (area size) Unknown
    , worldSize         = size
    }

noVisibility :: Size -> U.Vector Bool
noVisibility size = (U.replicate (area size) False)


visibleSet :: Size -> Int -> [Point] -> U.Vector Bool
visibleSet size radius ants = runST $ do
        v <- M.replicate (area size) False
        
        flip mapM_ ants  $ \ant ->
           flip V.mapM_ offsets $ \offset -> do
               M.unsafeWrite v (wrapIndex size (ant `addSize` offset)) True
         
        U.unsafeFreeze v
    where
        offsets = V.fromList [Size x y | x <- [-radius..radius], y <- [-radius..radius],  x * x + y * y <= radiusSq]
        radiusSq = radius * radius

        
resetTiles :: V.Vector Tile -> U.Vector Bool -> V.Vector Tile
resetTiles tiles visible = V.imap (\i tile -> updateTile tile (visible `U.unsafeIndex` i)) tiles
    where
        updateTile tile    False = tile
        updateTile Water   vis   = Water
        updateTile _       True  = Land Empty True
 

updateTiles :: [(Point, Tile)] -> Size -> V.Vector Tile -> V.Vector Tile
updateTiles newTiles size tiles = runST $ do
    tiles' <- V.unsafeThaw tiles
    
    flip mapM_ newTiles  $ \(point, tile) -> 
        MV.unsafeWrite tiles' (size `index` point) tile
        
    V.unsafeFreeze tiles' 


  
updateWorld :: Int -> [(Point, Object)] -> [Point] -> World -> World
updateWorld radius objects water world = world { worldTiles = updateTiles newTiles (worldSize world) tiles' } 
    where
        tiles'  = resetTiles (worldTiles world) visible
        visible = visibleSet (worldSize world) radius (map fst ourAnts)

        newTiles = map (\(point, object) -> (point, Land object True)) objects ++ map (\point -> (point, Water)) water
        
        ourAnts = filter (isAnt 0) objects
        isAnt n = (== (Ant n)) . snd 
        
       
wrapIndex :: Size -> Point -> Int
wrapIndex (Size width height) (Point x y) = (y `mod` height) * width + x `mod` width
{-# INLINE wrapIndex #-}

index :: Size -> Point -> Int
index (Size width height) (Point x y) = y * width + x
{-# INLINE index #-}

at :: World -> Point -> Tile
at world point = (worldTiles world) `V.unsafeIndex` ((worldSize world) `wrapIndex` point)
{-# INLINE at #-}
         



