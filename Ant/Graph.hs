module Ant.Graph
    ( Graph
    , Region (..)

    , RegionIndex
    , Distance
        
    , emptyGraph
    , addRegion
    , updateGraph
    
    , graphSquare
    , regionDistance

    
            
    )        
    where


import qualified Data.Vector as V
    
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import qualified Data.IntMap as M
import qualified Data.Sequence as S

import Data.List

import Control.Monad

import Ant.Square
import Ant.Map
import Ant.Point

import Debug.Trace

-- Some type snyonyms to make the intentions clearer
type PointIndex = Int
type RegionIndex = Int 
type Distance = Int
type Sq = (PointIndex, Distance)

-- Data structures for doing bredth first search
type DistanceMap = U.Vector (RegionIndex, Distance)
type Set = M.IntMap Distance  
type Queue = S.Seq Sq 

data Graph = Graph
    { regionMap  :: DistanceMap
    , regions    :: V.Vector Region
    , graphSize  :: !Size
    , regionDistance :: Int
    }
    
    
data Region = Region 
     {  regionSquares :: Set
     ,  openSquares   :: [Sq]
     ,  regionId      :: RegionIndex
     } deriving Show
     
     
emptyGraph :: Size -> Int -> Graph
emptyGraph size distance = Graph 
    { regionMap = (U.replicate (area size) (-1, 1000 )) 
    , regions   = V.empty     
    , graphSize = size
    , regionDistance = distance
    }
     

--addSplits :: Map -> Graph -> Graph
--addSplits world graph =      
updateGraph :: Map -> Graph -> Graph
updateGraph world graph = graph''
    where
        graph' = expandRegions world graph
    
        seeds = findSeeds world graph'
        graph'' = traceShow seeds $ foldl ((fst .) . addRegion world) graph' seeds
        
        
     
expandRegions :: Map -> Graph -> Graph
expandRegions world graph = graph 
      { regionMap = writeToMap regions' (regionMap graph) 
      , regions   = updateBy (regions graph) regions' regionId
      } 
      where        
          openRegions = filter isOpen (V.toList (regions graph))         
          regions' = map (expandRegion (regionDistance graph) world) openRegions 
     
isOpen :: Region -> Bool
isOpen = not . null . openSquares   
{-# INLINE isOpen #-}
     
addRegion ::  Map -> Graph -> PointIndex -> (Graph, Region)
addRegion world graph centre = (graph', region)
    where 
        graph' = graph { regions = V.cons region (regions graph) }
    
        regionIndex = V.length (regions graph)
        region  = Region 
            { openSquares   = [(centre, 0)]  
            , regionId      = V.length (regions graph)
            , regionSquares = M.empty
            }
             
findSeeds :: Map -> Graph -> [PointIndex]
findSeeds world graph =  separate (manhattenIndex (mapSize world)) (regionDistance graph) valid
    where
        valid = map fst (U.toList (U.filter validSquare squares)) 
        validSquare (p, d) = d == (regionDistance graph)
        
        squares = U.imap (\p (r, d) -> (p, d)) (regionMap graph)

separate :: (a -> a -> Int) -> Int -> [a] -> [a]        
separate metric distance = foldl' add []
     where
        add seeds p | all (apart p) seeds = p : seeds 
                    | otherwise            = seeds

        apart p1 p2 = distance < metric p1 p2
{-# INLINE separate #-}       

            
graphSquare :: Graph -> Point ->  (RegionIndex, Distance)   
graphSquare graph p = (regionMap graph) `U.unsafeIndex` (wrapIndex (graphSize graph) p)  
{-# INLINE graphSquare #-}       
     
updateBy :: V.Vector a -> [a] -> (a -> Int) -> V.Vector a
updateBy v updates f =  v V.// map (\x -> (f x, x)) updates     
     
writeToMap :: [Region] -> DistanceMap -> DistanceMap
writeToMap regions distanceMap =  U.modify updateSq distanceMap
    where
        updateSq v = forM_ regions $ \region -> do
            forM_ (M.toList (regionSquares region)) $ \(p, d) -> do
                (_, d') <- UM.unsafeRead v p
                when (d < d') $ UM.unsafeWrite v p (regionId region, d)

                
type GetSuccessors = Set -> Sq -> ([Sq], [Sq])                        
        
bfs :: GetSuccessors -> Queue -> Set -> Set -> (Set, Set)
bfs successors queue unseen set = bfs' (S.viewl queue) unseen set   
    where        
        bfs' S.EmptyL            unseen set  =   (set, unseen)
        bfs' ((p, d) S.:< queue) unseen set  =   bfs' (S.viewl queue') unseen' set'
                                 
            where
                (next, hidden)   = successors set (p, d)
                
                queue' = foldl (S.|>) queue next
                set'   = foldr (uncurry M.insert) set (hidden ++ next)  
                unseen'  = foldr (uncurry M.insert) unseen hidden
     
  
     
expandRegion :: Int -> Map -> Region -> Region
expandRegion distance world region = region 
    { regionSquares = squares'
    ,  openSquares   = M.toList open'
    }   
        
    where 
        
        (squares', open') = bfs successors queue unseen (regionSquares region)
    
        successors :: GetSuccessors              
        successors set (p, d) | d < distance    = (next, unseen) 
                              | otherwise       = ([], [])
            where
                neighbors = neighborIndices (mapSize world) p 
                incDistance p = (p, d + 1)
                
                next =  (map incDistance . filter isSuccessor)  neighbors
                unseen = (map incDistance . filter isUnknown)  neighbors
                                
                isUnknown = not . wasSeen . (world `atIndex`)
                isSuccessor p = isLand (world `atIndex` p)        -- Land and not water (and we've seen it before)
                              && (M.notMember p set)              -- Not already visited
                              
        -- Starting condition
        filterOpen f = filter (\(p, _) -> f (world `atIndex` p)) (openSquares region) 
 
        queue   = S.fromList (filterOpen isLand) 
        unseen  = M.fromList (filterOpen (not . wasSeen))

        