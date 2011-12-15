module Ant.Influence 
    ( countInfluences
    , lookupInfluence
    
    , makeInfluenceTable
    , makeDistanceTable
    
    , moveIndex
    , DistanceTable
    , InfluenceTable
    
    )
    where


import Ant.Map
import Ant.Point
import Ant.RegionBuilder
import Ant.IO
import Ant.Square
import Ant.Vector

import Data.Bits
import Control.Monad

import qualified Data.Set as S
import qualified Data.Map as M

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

offsetTable :: [(Int, Size)]
offsetTable =   [ (1, Size (-1) 0) 
                , (2, Size 1 0) 
                , (4, Size 0 (-1)) 
                , (8, Size 0 1) 
                ]
                
type Point' = (Int, Int)
                
type InfluenceTable = V.Vector (U.Vector Point')
type DistanceTable = V.Vector (U.Vector (Point', Float))

getPoints :: (Size -> Size -> a) -> Int -> Int -> [a]
getPoints f radiusSq bits = [ f p o | p <- circlePoints radiusSq, o <- Size 0 0 : offsets]
    where offsets = map snd . filter ((> 0) . (bits .&.) . fst) $ offsetTable

makeDistanceTable :: Int -> DistanceTable
makeDistanceTable radiusSq = V.generate 16 influenceMap 
    where
        influenceMap i = U.fromList $ M.toList . M.fromListWith min $ points where
            points = getPoints makePair radiusSq i
            makePair (Size x y) (Size x' y') = ((x + x', y + y'), sqrt (fromIntegral (x * x + y * y))  )
        
makeInfluenceTable :: Int -> InfluenceTable
makeInfluenceTable radiusSq = V.generate 16 influenceMap 
    where
        influenceMap i = U.fromList $ S.toList . S.fromList $ points where
            points = getPoints makePair radiusSq i
            makePair (Size x y) (Size x' y') = (x + x', y + y')


moveIndex :: Map -> Point -> Int
moveIndex world p = foldr (.|.) 0 offsets where
    offsets = map fst . filter (isLand . (world `at`) . (p `addSize`) . snd) $ offsetTable
{-# INLINE moveIndex #-}    
    
lookupInfluence :: Map -> Point -> InfluenceTable -> U.Vector (Int, Int)
lookupInfluence world p table = table `indexV` moveIndex world p

    
{-# INLINE lookupInfluence #-}    
    
countInfluences :: [Point] -> Map -> InfluenceTable -> U.Vector Int
countInfluences ants world table = U.create $ do
    v <- UM.replicate (area (mapSize world)) 0
   
    forM_ ants $ \ant -> do
        let offsets = lookupInfluence world ant table
        U.forM_ offsets $ \o -> do
            let i = mapSize world `wrapIndex` (ant `addOffset` o)
            n <- UM.unsafeRead v i
            UM.unsafeWrite v i (n + 1)
     
    return v      
        where addOffset (Point x y) (x', y') = Point (x + x') (y + y')
    