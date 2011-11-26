{-# LANGUAGE PatternGuards #-}

module Ant.Graph 
	( Graph
	, createGraph
	, neighborList
	,  neighborList'
	, neighborDistances
	)
where

import Ant.Map
import Ant.Point
import Ant.GraphBuilder

import qualified Data.IntMap as M
import qualified Data.Vector as V

type Graph = V.Vector Region

createGraph :: RegionGraph -> Graph
createGraph = V.fromList . map snd . M.toList    

neighborList :: RegionVector -> ContentGraph -> [(Region, Edge)]
neighborList rc graph = map fromIndex (contentNeighbors' rc)
    where fromIndex (i, e) = (graph `V.unsafeIndex` i, e)
{-# INLINE graphNeighbors #-}    
	
	
neighborList' :: Region -> [(RegionIndex, Edge)]
neighborList' = M.toList . regionNeighbors
{-# INLINE graphNeighbors' #-}
    
neighborDistances :: RegionIndex -> RegionVector -> [(Distance, RegionIndex)]
neighborDistances i graph = (map toDistancePair . contentNeighbors') rc
    where
        rc = graph `V.unsafeIndex` i
        toDistancePair (r, e) = (edgeDistance e, r)
{-# INLINE neighborDistances #-}