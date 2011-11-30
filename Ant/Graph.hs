{-# LANGUAGE PatternGuards #-}

module Ant.Graph 
	( Graph

	, grCreate
	, grNeighbors
	, grEdges
	, grEdgeIndices
    , grIndex
    , grSize
    , grRegions
    , grEmpty
    
    , SearchNode(..)
    , grBFS
    , grAStar
    , grSearch
    , grPath
	)
where

import Ant.Point
import Ant.RegionBuilder
import Ant.Map

import qualified Data.IntMap as M
import qualified Data.Vector as V

import qualified Data.IntSet as S

import Debug.Trace
import Data.Maybe

import Data.PSQueue ( Binding(..) )
import qualified Data.PSQueue as Q

newtype Graph = Graph { unGr :: V.Vector Region }

grEmpty :: Graph 
grEmpty = Graph (V.empty)

grCreate :: RegionGraph -> Graph
grCreate = Graph . V.fromList . map snd . M.toList    

grNeighbors ::  Graph -> Region -> [(Region, Edge)]
grNeighbors graph region = map fromIndex (neighbors' region)
    where fromIndex (i, e) = (graph `grIndex` i, e)
{-# INLINE grNeighbors #-}    
	
	
neighbors' :: Region -> [(RegionIndex, Edge)]
neighbors' = M.toList . regionNeighbors
{-# INLINE neighbors' #-}

grEdges :: RegionIndex -> Graph -> [(RegionIndex, Edge)]
grEdges i graph = neighbors' region
    where
        region = graph `grIndex` i
{-# INLINE grEdges #-}

grEdgeIndices :: RegionIndex -> Graph -> [RegionIndex]
grEdgeIndices i graph = map fst (grEdges i graph)
{-# INLINE grEdgeIndices #-}


grIndex :: Graph -> RegionIndex -> Region
grIndex (Graph v) r = v `V.unsafeIndex` r
{-# INLINE grIndex #-}
  
 
grSize :: Graph -> Int
grSize (Graph v) = V.length v
{-# INLINE grSize #-}

grRegions :: Graph -> [Region]
grRegions (Graph v) = V.toList v


type Queue = Q.PSQ SearchNode Distance

data SearchNode = SearchNode 
    { snRegion   :: !RegionIndex
    , snDistance :: !Distance
    , snPred     :: Maybe SearchNode
    }
    

instance Show SearchNode where
    show (SearchNode r d _) = show r ++ " dist: " ++ show d

    
instance Eq SearchNode where
    (==) sn sn' = snRegion sn == snRegion sn'
    
instance Ord SearchNode where
    compare sn sn' = snRegion sn `compare` snRegion sn'
    
    
edgeDistances :: Graph -> SearchNode -> [SearchNode]
edgeDistances graph node@(SearchNode r d _) = map toNode (grEdges r graph)
    where
        toNode (r', e) = SearchNode r' (edgeDistance e + d) (Just node)
{-# INLINE edgeDistances #-}

grPath :: SearchNode -> [RegionIndex]
grPath (SearchNode r _ Nothing)  = [r]
grPath (SearchNode r _ (Just p)) = r : grPath p


grBFS :: Graph -> RegionIndex -> [SearchNode]
grBFS = grSearch snDistance

grAStar :: Size -> RegionIndex -> Graph -> RegionIndex -> Maybe SearchNode
grAStar worldSize dest graph r = listToMaybe $ dropWhile ( (/= dest) . snRegion ) nodes
    where 
        nodes  = grSearch metric graph r
        metric (SearchNode r d _) = (round . (*100) . sqrt . fromIntegral) $ distanceSq worldSize destCentre (centre r) + d
            
        centre r = regionCentre (graph `grIndex` r)
        destCentre  = centre dest
        
    
grSearch :: (SearchNode -> Distance) -> Graph -> RegionIndex -> [SearchNode]
grSearch metric graph r = search (S.singleton r) (Q.singleton node0 (metric node0)) where
    node0 = SearchNode r 0 Nothing

    succ node@(SearchNode r d _) = map toDistancePair (grEdges r graph)
        where toDistancePair (r', e) = SearchNode r' (edgeDistance e + d) (Just node)
    {-# INLINE succ #-}
          
    search seen queue  | Nothing                      <- view = []
                       | Just ((node :-> _), queue')  <- view = next node seen queue'                    
        where view = Q.minView queue  
        
    next node seen queue = node : (search seen' queue')
        where
            queue'  = foldr insert queue successors
            seen'   = S.insert (snRegion node) seen
            
            successors = filter (flip S.notMember seen . snRegion) (succ node)
            insert node = Q.insert node (metric node)
    {-# INLINE next #-}
            
            
grTest :: Graph
grTest = Graph regions where
    size = 10
    
    regions = V.fromList (map mkRegion [0.. size * size - 1])
    mkRegion n = Region
        { regionCentre = Point x y
        , regionId     = y * size + x
        , regionNeighbors = M.fromList neighbors
        , regionSize      = 0
        , regionFrontier  = False
        }
        where
            (y, x) = n `divMod` size
            inBounds (x, y) = x >= 0 && y >=  0 && x < size && y < size
            toEdge  (x, y)  = (y * size + x, Edge 1 1)
            neighbors       = map toEdge $ filter inBounds [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
            
            