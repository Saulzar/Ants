{-# LANGUAGE PatternGuards, NoMonomorphismRestriction #-}

module Ant.RegionStats 
    ( RegionContent (..)
    , GameStats (..)
    , gsRegion

    , regionVisibility
    , hillDistances
    , initialStats
    , updateStats

    , forRegion   
    , sumRegionInfluence
    
    )
    where

import Data.List

import Ant.Map
import Ant.Point
import Ant.RegionBuilder
import Ant.IO
import Ant.Square
import Ant.Vector

import Ant.Graph

import qualified Data.Set as S
import qualified Data.IntMap as M
import qualified Data.Sequence as Seq

import Control.Monad.ST
import Control.Monad

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import qualified Data.Vector.Generic as G
import qualified Data.PriorityQueue.FingerTree as Q

import Debug.Trace



type AntList = [(Point, Player)]

data RegionContent = RegionContent
    { rcAnts            :: [Point]
    , rcEnemies         :: AntList
    , rcFood            :: [Point]
    , rcHills           :: AntList
    , rcDeadAnts        :: AntList
    } deriving Show
      

data GameStats = GameStats
    { gsRegions     :: V.Vector RegionContent
    
    , gsHills           :: S.Set (Point, Player)
    , gsHillDistances   :: U.Vector Int
    
    , gsAnts            :: ([Point], AntList)
    , gsContent         :: [SquareContent]     
    
    , gsVisited         :: U.Vector Int
    , gsEnemyDistances  :: U.Vector Float
    
    
    } deriving Show


    
maxPlayers :: Int
maxPlayers = 20
        
    
initialStats = GameStats
    { gsRegions = V.empty
    , gsHills           = S.empty
    , gsHillDistances   = U.empty
    , gsAnts            = ([], [])
    , gsVisited         = U.empty
    , gsContent             = []
    , gsEnemyDistances      = U.empty
    }
    
    
updateHillSet :: Map -> [SquareContent] -> S.Set (Point, Player) -> S.Set (Point, Player)
updateHillSet world content hills = S.filter hillExists $ foldr S.insert hills visibleHills
        where
                visibleHills = map (\(p, Hill n) -> (p, n)) . filter (containsHill . snd) $ content
                hillExists (p, _) = isHill (world `at` p)  
				
gsRegion :: GameStats -> Int -> RegionContent
gsRegion gs i = (gsRegions gs) `indexV` i
{-# INLINE gsRegion #-}
        
        
potentialEnemies :: Map -> [Point] -> S.Set Point
potentialEnemies world ants = foldr addValid S.empty ants where
    addValid p s = foldr S.insert s (valid p)
    valid p = filter (isLand . (world `at`)) .  map (wrapPoint (mapSize world)) $ (p : neighbors p)
        
enemyDistanceMap :: Map -> Int -> [Point] -> U.Vector Float
enemyDistanceMap world distance ants = minDistanceMap (mapSize world) distance ants'
    where ants' = S.toList $ potentialEnemies world ants 
        
minDistanceMap :: Size -> Int -> [Point] -> U.Vector Float
minDistanceMap size distance ants = U.create $ do
    v <- UM.replicate (area size) 1000
    
    forM_ ants $ \ant -> do
        forM_ offsets $ \(d, offset) ->  do
            let i = wrapIndex size (ant `addSize` offset)

            d' <- UM.unsafeRead v i
            UM.unsafeWrite v i (min d d')
     
    return v
    
    where offsets = [distPair x y | x <- [-distance..distance], y <- [-distance..distance]]
          distPair x y = (sqrt (fromIntegral (x * x + y * y)), Size x y)        
          
{-
updateFightRecords :: V.Vector RegionStats -> U.Vector (Int, Int) -> U.Vector (Int, Int)
updateFightRecords regionStats frVec = U.modify update frVec where
    update v = do
        V.forM_ regionStats $ \stats -> case (rsEnemyPlayer stats) of
                Nothing 		-> return ()
                (Just player)	-> do
                        fr <- readU v player
                        writeU v player (fr `addFightRecord` rsDead stats)	 
-}

filterAnts :: [SquareContent] -> AntList
filterAnts content = map fromAnt $ filter (containsAnt . snd) content
    where fromAnt (p, (Ant n)) = (p, n)
                            
                        
splitEnemy :: AntList -> (AntList, AntList)                        
splitEnemy = partition ( (== 0) . snd )
                        

updateStats :: Map -> Graph -> RegionMap -> U.Vector Bool -> [SquareContent] -> GameStats -> GameStats
updateStats world graph regionMap vis content stats = gsEnemyDistances `seq` stats
        { gsRegions     = regionContent'
        , gsHills       = hills
        , gsHillDistances = regionDistances
        , gsAnts        = (map fst ourAnts, enemyAnts)
        , gsVisited     = updateVisited regionContent' (gsVisited stats)
        , gsContent     = content'
        , gsEnemyDistances = enemyDistances
        }
        where

            hills    = updateHillSet world content (gsHills stats)           
            getRegion      = regionAt regionMap (mapSize world)
           
            ourHills = map fst . filter ((==0) .  snd) . S.toList $ hills
            hillRegions     = filter ( /= invalidRegion) . map getRegion $ ourHills
            regionDistances = hillDistances graph hillRegions
            
            -- Remember content we can't see
            hidden   = filter (not . (vis `indexU`) . wrapIndex (mapSize world) .  fst) (gsContent stats)

            content' = hidden ++ content
            (ourAnts, enemyAnts) = splitEnemy . filterAnts $ content'            
            
            regionContent'  = regionContent (mapSize world) graph regionMap content'
            
            enemyDistances = enemyDistanceMap world 5 (map fst enemyAnts)
        
addContent :: RegionContent -> SquareContent -> RegionContent
addContent rc (p, Ant 0)     = rc { rcAnts    = p : rcAnts rc }
addContent rc (p, Ant n)     = rc { rcEnemies = (p, n) : rcEnemies rc }
addContent rc (p, DeadAnt n) = rc { rcDeadAnts  = (p, n) : rcDeadAnts rc }
addContent rc (p, Hill n)    = rc { rcHills = (p, n) : rcHills rc }
addContent rc (p, Food)      = rc { rcFood  = p : rcFood rc }
addContent rc _              = rc
     
emptyContent = RegionContent
    { rcAnts     = []
    , rcEnemies  = []
    , rcDeadAnts = []
    , rcFood     = []
    , rcHills    = []
    }
                

forRegion :: (Monad m) => RegionMap -> (Int -> RegionIndex -> m ()) -> m ()
forRegion regionMap action = do
    forM_ [0.. U.length regionMap - 1] $ \i -> do
            let (region, _) = regionMap `indexU` i 
            when (region /= invalidRegion) $ action i region       
                
regionVisibility :: Int -> RegionMap -> U.Vector Bool -> U.Vector Int
regionVisibility numRegions regionMap visible = U.create $ do                 
        v <- UM.replicate numRegions 0
        forRegion regionMap $ \i region -> when (visible `indexU` i) $ do
            n <- readU v region
            writeU v region (n + 1)   
        return v


sumRegionInfluence :: Float -> Int -> RegionMap -> U.Vector (Int, Int) -> U.Vector (Float, Float)
sumRegionInfluence scale numRegions regionMap influence = U.create $ do                 
        v <- UM.replicate numRegions (0, 0)
        forRegion regionMap $ \i region -> do
            let (our', enemy') = influence `indexU` i 
            (our, enemy) <- readU v region
            
            writeU v region (our + scale * fromIntegral our', enemy + scale * fromIntegral enemy')   
        return v
            
        
type Queue = Q.PQueue Distance RegionIndex 	
        
edgeDistances :: Int -> RegionIndex -> Graph -> [(Distance, RegionIndex)]
edgeDistances d i graph = map toDistancePair (grEdges i graph)
    where
        toDistancePair (r, e) = (edgeDistance e + d, r)
{-# INLINE edgeDistances #-}

hillDistances :: Graph -> [RegionIndex] -> U.Vector Int
hillDistances graph hills = runST searchHills where 	
    initialQueue = Q.fromList (zip (repeat 0) hills)
        
    searchHills :: ST s (U.Vector Int)
    searchHills = do
        v <- UM.replicate (grSize graph) 1000  
        forM_ hills $ \r -> UM.unsafeWrite v r 0
        
        searchHills' v initialQueue
        U.unsafeFreeze v
     
    searchHills' v queue | Nothing                <- view = return ()	
                         | Just ((d, r), queue')  <- view = do
            
            successors <- filterM (isSuccessor d) (edgeDistances d r graph)
            forM_ successors $ \(d', r') -> writeU v r' d'
            searchHills' v (foldr (uncurry Q.insert) queue' successors)
                                     
        where 
            view = Q.minViewWithKey queue  
            
            isSuccessor d (d', r') = do
                d'' <- readU v r'
                return (d' < d'')
               


regionContent :: Size -> Graph -> RegionMap -> [SquareContent] -> V.Vector RegionContent
regionContent worldSize graph regionMap content = runST allContent
    where 
        allContent :: ST s (V.Vector RegionContent)
        allContent = do
            v <- VM.replicate (grSize graph) emptyContent
                
            forM_ content $ \(p, c) -> do
                let i = regionAt regionMap worldSize p
                when (i /= invalidRegion) $ do
                    rc <- readV v i 
                    writeV v i (rc `addContent` (p, c))
                
            V.unsafeFreeze v
	

countPlayers :: AntList -> [(Player, Int)]
countPlayers = map (\x -> (head x, length x)) . group . map snd 
		

maxPlayer :: AntList -> Maybe Player
maxPlayer [] 	= Nothing
maxPlayer ants 	= Just (fst $ head (countPlayers ants)) 


addFightRecord :: (Int, Int) -> (Int, Int) -> (Int, Int)
addFightRecord (e, p) (e', p') = (e + e', p + p')


updateVisited :: V.Vector RegionContent -> U.Vector Int -> U.Vector Int
updateVisited content lastVisited =  U.generate (V.length content) update' 
    where
        update' i | isVisited i = 0
                  | otherwise   = inc i

        inc i | i < (U.length lastVisited) = 1 + lastVisited `indexU` i
              | otherwise                  = 1
                    
        isVisited i = not . null . rcAnts $ content `indexV` i

