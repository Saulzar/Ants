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

import qualified Data.Map as M
import qualified Data.Sequence as S

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
type Set = M.Map PointIndex Distance  
type Queue = S.Seq Sq 

data Graph = Graph
    { regionMap  :: DistanceMap
    , regions    :: V.Vector Region
    , graphSize  :: !Size
    , regionDistance :: Int
    }
    
    
data Region = Region 
     {  openSquares   :: Set
     ,  regionId      :: RegionIndex
     } deriving Show
     
     
emptyGraph :: Size -> Int -> Graph
emptyGraph size distance = Graph 
    { regionMap = (U.replicate (area size) (-1, distance)) 
    , regions   = V.empty     
    , graphSize = size
    , regionDistance = distance
    }
     

--addSplits :: Map -> Graph -> Graph
--addSplits world graph =      
updateGraph :: Map -> Graph -> Graph
updateGraph world graph = graph'
    where
        (graph', updates) = expandRegions world graph
     
expandRegions :: Map -> Graph -> (Graph, SquareUpdates)
expandRegions world graph = (graph', squareUpdates) 

    where
          graph' = graph 
              { regionMap = writeToMap squareUpdates (regionMap graph) 
              , regions   = updateBy (regions graph) regions' regionId
              } 
        
          (regions', squareUpdates) = (unzip . map updateOpen) (V.toList (regions graph))
          updateOpen region = (region', (regionId region, squareSet)) 
              where
                  region' = region { openSquares = openSquares' }
                  (openSquares', squareSet) = expandSquares graph world (openSquares region) 
     
type SquareUpdates = [(RegionIndex, Set)]    
     
findSplits :: Map -> Graph -> SquareUpdates -> [PointIndex]
findSplits world graph updates = []
    where
        
        candidates = filter isSplitPoint  (concatMap (M.toList . snd) updates)
        isSplitPoint (p, d) = d == (regionDistance graph - 1)     
     
     
addRegion ::  Map -> Graph -> PointIndex -> (Graph, Region)
addRegion world graph centre = (graph', region)
    where 
        graph' = graph { regions = V.cons region (regions graph) }
    
        regionIndex = V.length (regions graph)
        region  = Region 
            { openSquares   = M.singleton centre 0  
            , regionId = V.length (regions graph)
            }
             

graphSquare :: Graph -> Point ->  (RegionIndex, Distance)   
graphSquare graph p = (regionMap graph) `U.unsafeIndex` (wrapIndex (graphSize graph) p)  
     
     
updateBy :: V.Vector a -> [a] -> (a -> Int) -> V.Vector a
updateBy v updates f =  v V.// map (\x -> (f x, x)) updates     
     
writeToMap :: SquareUpdates -> DistanceMap -> DistanceMap
writeToMap regions' distanceMap =  U.modify updateSq distanceMap
    where
        updateSq v = forM_ regions' $ \(region, set) -> do
            forM_ (M.toList set) $ \(p, d) -> do
                (region', d') <- UM.unsafeRead v p
                when (d < d') $ UM.unsafeWrite v p (region, d)
        
        
bfs :: (Set -> Sq -> ([Sq], [Sq])) -> Queue -> Set -> Set -> (Set, Set)
bfs successors queue open set = bfs' (S.viewl queue) open set   
    where        
        bfs' S.EmptyL            open set  = (open, set)
        bfs' ((p, d) S.:< queue) open set  =  bfs' (S.viewl queue') open' set'
                                 
            where
                (next, unseen)   = successors set (p, d)
                
                queue' = foldl (S.|>) queue next
                set'   = foldr (uncurry M.insert) set (unseen ++ next)  
                open'  = foldr (uncurry M.insert) open unseen
     

     
     
--expandRegion :: Int -> World -> Region -> Region
--expandRegion distance world region =      
     
expandSquares :: Graph -> Map -> Set -> (Set, Set)
expandSquares graph world open = bfs successors queue open' set
        
    where 
    
        successors :: Set -> Sq -> ([Sq], [Sq])               
        successors set (p, d) | d < (regionDistance graph)    = (next, unseen) 
                              | otherwise                     = ([], [])
            where
                neighbors = neighborIndices size p 
                incDistance p = (p, d + 1)
                
                next =  (map incDistance . filter isSuccessor)  neighbors
                unseen = (map incDistance . filter isUnknown)  neighbors
                
                (r, existing) = (regionMap graph) `U.unsafeIndex` p
                
                isUnknown = not . wasSeen . (world `atIndex`)
                isSuccessor p = isLand (world `atIndex` p)        -- Land and not water (and we've seen it before)
                              && (M.notMember p set)              -- Not already visited
                              && (d < existing) 
                              
        -- Starting condition
        filterAt f = M.filterWithKey (\p d -> f (world `atIndex` p))
        
        openList = filterAt isLand open
        
        queue = S.fromList (M.toList openList) 
        open'  = filterAt (not . wasSeen) open
        set   = M.empty 
        
        size   = mapSize world
        