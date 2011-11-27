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
	)
where

import Ant.Point
import Ant.RegionBuilder

import qualified Data.IntMap as M
import qualified Data.Vector as V

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
grIndex (Graph v) r = v V.! r --v `V.unsafeIndex` r
{-# INLINE grIndex #-}
  

grSize :: Graph -> Int
grSize (Graph v) = V.length v
{-# INLINE grSize #-}

grRegions :: Graph -> [Region]
grRegions (Graph v) = V.toList v