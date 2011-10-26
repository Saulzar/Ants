{-# LANGUAGE BangPatterns #-}


module Ant.Map where

import qualified Data.Vector.Storable as S
import Control.Monad.ST

import Data.List
import Data.List.Split
import Data.Char

import Foreign
import Foreign.C.Types

import Data.Bits
import Data.Word

import Ant.Point
import Ant.IO


type Flag = Word8

visibleFlag, seenFlag, waterFlag :: Flag 
visibleFlag = 1
seenFlag    = 2
waterFlag   = 4
{-# INLINE waterFlag #-} 
{-# INLINE visibleFlag #-} 
{-# INLINE seenFlag #-}


data Square = Square 
    { sAnt         :: {-# UNPACK #-} !Word8  
    , sHill        :: {-# UNPACK #-} !Word8
    , sFlags       :: {-# UNPACK #-} !Flag
    , sEmpty       :: {-# UNPACK #-} !Word8
    }

    
type Map = S.Vector Square            
        
noPlayer :: Word8 
noPlayer = -1
{-# INLINE noPlayer #-}        
        
maybePlayer :: Word8 -> Maybe Player
maybePlayer n | n == noPlayer = Just (fromIntegral n)
         | otherwise     = Nothing
{-# INLINE maybePlayer #-}
         
isPlayer :: Word8 -> Bool
isPlayer n = n /= noPlayer
{-# INLINE isPlayer #-}         
         
squareHill, squareAnt :: Square -> Maybe Player
squareHill = maybePlayer . sHill 
squareAnt  = maybePlayer . sAnt  
{-# INLINE squareHill #-}
{-# INLINE squareAnt #-}

testFlag :: Flag -> Flag -> Bool
testFlag flag1 = \flag2 -> (flag1 .&. flag2) > 0
{-# INLINE testFlag #-}         

isHill, isAnt, isVisible, wasSeen, isWater :: Square -> Bool
isHill    = isPlayer . sHill  
isAnt     = isPlayer . sAnt   
isWater   = (testFlag waterFlag) . sFlags       
isVisible = (testFlag visibleFlag) . sFlags     
wasSeen   = (testFlag seenFlag) . sFlags        
{-# INLINE isHill #-}
{-# INLINE isWater #-} 
{-# INLINE isAnt #-}
{-# INLINE isVisible #-}
{-# INLINE wasSeen #-}
 
        
tileChar :: Square -> Char
tileChar square  | not (wasSeen square)  = '?'
                 | isWater square        = '~'
                 | (Just hill) <- squareAnt square = chr(ord 'a' + hill) 
                 | (Just ant) <- squareAnt square  = chr(ord '0' + ant)
                 | isVisible square  = '.'
                 | otherwise         = 'o' 
   
   
instance Storable Square where
  sizeOf _ = sizeOf (undefined :: Word8) * 4
  alignment _ = alignment (undefined :: CInt)
 
  {-# INLINE peek #-}
  peek p = do
     ant   <- peekElemOff q 0
     hill  <- peekElemOff q 1
     flags <- peekElemOff q 2
     return (Square ant hill flags 0)
    where
      q = castPtr p
  {-# INLINE poke #-}
  poke p (Square ant hill flags _) = do
     pokeElemOff q 0 ant
     pokeElemOff q 1 hill
     pokeElemOff q 2 flags
    where
      q = castPtr p

         
    {-
buildMap :: World -> [String]
buildMap (Map world = map (buildRow world) [0..(worldHeight world) - 1] 

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
         
   -}


