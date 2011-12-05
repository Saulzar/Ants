{-# LANGUAGE PatternGuards, NoMonomorphismRestriction #-}

module Ant.RegionStats 
    ( RegionContent (..)
    , rcOurAnts
    , RegionStats (..)
    , GameStats (..)
    , gsRegion

    , regionVisibility
    , hillDistances
    , initialStats
    , updateStats

    , indexV
    , indexU
    
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
    { rcAnts            :: AntList
    , rcFood            :: [Point]
    , rcHills           :: AntList
    , rcDeadAnts        :: AntList
    } deriving Show

rcOurAnts :: RegionContent -> [Point]
rcOurAnts = map fst . filter ((== 0) . snd) . rcAnts
        
data RegionStats = RegionStats
    { rsLastVisible     :: !Int
    , rsHillDistance    :: !Int
    , rsContent         :: !RegionContent
    , rsDead            :: !(Int, Int)
    , rsFightRecord     :: !(Int, Int)
    , rsAntCount        :: !(Int, Int)
    
    , rsEnemyPlayer :: Maybe Player  -- The main opponent here
    } deriving Show

data GameStats = GameStats
    { gsRegions     :: V.Vector RegionStats
    , gsFightRecord :: U.Vector (Int, Int)
    , gsFreeAnts        :: !Int
    , gsShortage        :: !Int
    , gsHills           :: S.Set (Point, Player)
    , gsAnts            :: (AntList, AntList)
    , gsInfluenceMap    :: U.Vector (Int, Int)
    , gsRegionInfluence :: U.Vector (Int, Int)
    , gsInfluenceArea       :: !Int
    
    } deriving Show


gsRegion :: GameStats -> RegionIndex -> RegionStats
gsRegion = indexV . gsRegions 
    
maxPlayers :: Int
maxPlayers = 20
        
initialRegionStats = RegionStats
    { rsLastVisible     = 0
    , rsHillDistance    = 1000
    , rsContent         = emptyContent
    , rsDead            = (0, 0)
    , rsFightRecord     = (0, 0)
    , rsAntCount        = (0, 0)
    , rsEnemyPlayer     = Nothing 
    }
    
initialStats = GameStats
    { gsRegions = V.empty
    , gsFightRecord = U.replicate maxPlayers (0, 0)
    , gsFreeAnts        = 0
    , gsShortage        = 0
    , gsHills           = S.empty
    , gsAnts            = ([], [])
    , gsInfluenceMap    = U.empty
    , gsRegionInfluence = U.empty
    , gsInfluenceArea       = 0
    
    }
    
    
hills :: Map -> [SquareContent] -> S.Set (Point, Player) -> S.Set (Point, Player)
hills world content hills = S.filter hillExists $ foldr S.insert hills visibleHills
        where
                visibleHills = map (\(p, Hill n) -> (p, n)) . filter (containsHill . snd) $ content
                hillExists (p, _) = isHill (world `at` p)  
        
growRegions :: Int -> V.Vector RegionStats -> V.Vector RegionStats
growRegions numRegions v = growRegions' newRegions	
        where
                newRegions = numRegions -  (V.length v)

                growRegions' 0 = v
                growRegions' n = v V.++ V.replicate newRegions initialRegionStats
        

updateFightRecords :: V.Vector RegionStats -> U.Vector (Int, Int) -> U.Vector (Int, Int)
updateFightRecords regionStats frVec = U.modify update frVec where
    update v = do
        V.forM_ regionStats $ \stats -> case (rsEnemyPlayer stats) of
                Nothing 		-> return ()
                (Just player)	-> do
                        fr <- readU v player
                        writeU v player (fr `addFightRecord` rsDead stats)	 
                        

filterAnts :: [SquareContent] -> AntList
filterAnts content = map fromAnt $ filter (containsAnt . snd) content
    where fromAnt (p, (Ant n)) = (p, n)
                            
                        
splitEnemy :: AntList -> (AntList, AntList)                        
splitEnemy = partition ( (== 0) . snd )
                        

updateStats :: Map -> Graph -> RegionMap -> U.Vector Bool -> [SquareContent] -> GameStats -> GameStats
updateStats world graph regionMap vis content stats = traceShow "updateStats" $ stats
        { gsRegions = regionStats'
        , gsFightRecord = updateFightRecords regionStats' (gsFightRecord stats)
        , gsHills = hills'
        , gsAnts  = (ourAnts, enemyAnts)
        , gsInfluenceMap    = influence
        , gsRegionInfluence = regionInfl
        , gsInfluenceArea   = length (circlePoints distSq)
        }
        where

            regionVisibility' = regionVisibility numRegions regionMap vis  
            hills'    = hills world content (gsHills stats)
            
            getRegion      = regionAt regionMap size
            
            hillRegions     = filter ( /= invalidRegion) . map getRegion . map fst . S.toList $ hills'
            regionDistances = hillDistances graph hillRegions
            
            regionContent'  = regionContent (mapSize world) graph regionMap content

            regions' = growRegions numRegions (gsRegions stats) 
            regionStats' = regionStats regionVisibility' regionDistances regionContent' graph regions'
                
            (ourAnts, enemyAnts) = splitEnemy . filterAnts $ content
            influence = U.zip (infl ourAnts) (infl enemyAnts)
            regionInfl = countInfluence numRegions regionMap influence
            
            infl ants = influenceCount size distSq (map fst ants)

            size = mapSize world
            distSq = 15
            
            numRegions = grSize graph

        
        
addContent :: RegionContent -> SquareContent -> RegionContent
addContent rc (p, Ant n)     = rc { rcAnts    = (p, n) : rcAnts rc }
addContent rc (p, DeadAnt n) = rc { rcDeadAnts  = (p, n) : rcDeadAnts rc }
addContent rc (p, Hill n)    = rc { rcHills = (p, n) : rcHills rc }
addContent rc (p, Food)      = rc { rcFood  = p : rcFood rc }
addContent rc _              = rc
     
emptyContent = RegionContent
    { rcAnts     = []
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


countInfluence :: Int -> RegionMap -> U.Vector (Int, Int) -> U.Vector (Int, Int)
countInfluence numRegions regionMap influence = U.create $ do                 
        v <- UM.replicate numRegions (0, 0)
        forRegion regionMap $ \i region -> do
            let (our, enemy) = influence `indexU` i 
            (our', enemy') <- readU v region
            
            writeU v region (our + our', enemy + enemy')   
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
regionContent worldSize graph regionMap content = regionContent' worldSize (grSize graph) regionMap content

regionContent' :: Size -> Int -> RegionMap -> [SquareContent] ->  V.Vector RegionContent
regionContent' worldSize numRegions regionMap content = runST regionStats'
    where 
        regionStats' :: ST s (V.Vector RegionContent)
        regionStats' = do
            v <- VM.replicate numRegions emptyContent
                
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


regionStats :: U.Vector Int -> U.Vector Int -> V.Vector RegionContent -> Graph -> V.Vector RegionStats -> V.Vector RegionStats 
regionStats visVec distVec contVec graph statsVec = V.generate (V.length statsVec) fromIndex where

    fromIndex i = regionStats' (visVec `indexU` i) (distVec `indexU` i) (contVec `indexV` i) (graph `grIndex` i) (statsVec `indexV` i)

    regionStats' numVisible hillDistance content region stats = stats
            { rsLastVisible  = lastVisible
            , rsHillDistance = hillDistance
            , rsContent      = content

            , rsDead         = dead
            , rsFightRecord  = addFightRecord dead (rsFightRecord stats)

            , rsAntCount     = (length ourAnts, length enemyAnts)
            , rsEnemyPlayer  = maxPlayer (enemyDead ++ enemyAnts)
            
            }
            where 
                lastVisible | regionSize region > 0 && isVisible  = rsLastVisible stats + 1
                            | otherwise = 0
                            
                isVisible = (numVisible `div` regionSize region * 100) > 80  

                (ourDead, enemyDead) = splitEnemy (rcDeadAnts content)
                (ourAnts, enemyAnts) = splitEnemy (rcAnts content)

                dead = (length enemyDead, length ourDead)

