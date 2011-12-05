{-# LANGUAGE PatternGuards #-}

module Ant.Graph 
    ( Graph
    
    , grCreate
    , grNeighbors
    , grEdge
    , grEdges
    , grEdgeIndices
    , grIndex
    , grSize
    , grRegions
    , grEmpty
    
{-
    , grBFS
    , grAStar
    , grPath
    
    , grSucc
  -}  
    , grTest
	)
where

import Ant.Point
import Ant.RegionBuilder
import Ant.Map
 -- import Ant.Search
import Ant.Vector

import qualified Data.IntMap as M
import qualified Data.Vector as V

import qualified Data.IntSet as S

import Debug.Trace
import Data.Maybe


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

grEdge :: RegionIndex -> RegionIndex -> Graph -> Maybe Edge
grEdge r r' graph = M.lookup r' (regionNeighbors region)
    where region = graph `grIndex` r
{-# INLINE grEdge #-}        
        
grEdges :: RegionIndex -> Graph -> [(RegionIndex, Edge)]
grEdges r graph = neighbors' region
    where  region = graph `grIndex` r
{-# INLINE grEdges #-}

grEdgeIndices :: RegionIndex -> Graph -> [RegionIndex]
grEdgeIndices i graph = map fst (grEdges i graph)
{-# INLINE grEdgeIndices #-}


grIndex :: Graph -> RegionIndex -> Region
grIndex (Graph v) r = v `indexV` r
{-# INLINE grIndex #-}
  
 
grSize :: Graph -> Int
grSize (Graph v) = V.length v
{-# INLINE grSize #-}

grRegions :: Graph -> [Region]
grRegions (Graph v) = V.toList v

{-    
    
edgeDistances :: Graph -> SearchNode -> [SearchNode]
edgeDistances graph node@(SearchNode r d _) = map toNode (grEdges r graph)
    where toNode (r', e) = SearchNode r' (edgeDistance e + d) (Just node)
{-# INLINE edgeDistances #-}

grPath :: SearchNode -> [RegionIndex]
grPath sn = path sn [] where
    path (SearchNode r _ Nothing)  xs = r : xs
    path (SearchNode r _ (Just p)) xs = path p (r : xs)


grBFS :: Graph -> RegionIndex -> [SearchNode]
grBFS graph = search (grSuccDistance graph) snDistance

grAStar :: Size -> Graph -> RegionIndex -> RegionIndex -> Maybe SearchNode
grAStar worldSize graph source dest = listToMaybe $ dropWhile ( (/= dest) . snKey ) nodes
    where 
        nodes  = search (grSuccDistance graph) metric source
        
        metric :: SearchNode -> Float
        metric (SearchNode r d _) = fromIntegral d + (sqrt . fromIntegral $ distanceSq worldSize destCentre (centre r))
        {-# INLINE metric #-}
            
        centre r = regionCentre (graph `grIndex` r)      
        destCentre  = centre dest
        
grSucc :: Graph -> (RegionIndex -> RegionIndex -> Edge -> Maybe Distance) -> SearchNode -> [SearchNode]
grSucc graph distance node@(SearchNode r d _) = catMaybes . map  succ $ (grEdges r graph)
    where succ (r', e) =  distance r r' e >>= \d' -> return $ SearchNode r' (d + d') (Just node)
{-# INLINE grSucc #-}     
    
grSuccDistance :: Graph -> SearchNode -> [SearchNode]
grSuccDistance graph = grSucc graph distance 
    where distance _ _ e = Just (edgeDistance e)
{-# INLINE grSuccDistance #-}            
        
            -}
            
grTest :: Size -> Graph
grTest (Size w h) =  Graph regions where
    
    regions = V.fromList (map mkRegion [0.. w * h - 1])
    mkRegion n = Region
        { regionCentre = Point x y
        , regionId     = y * w + x
        , regionNeighbors = M.fromList neighbors
        , regionSize      = 0
        , regionFrontier  = False
        }
        where
            (y, x) = n `divMod` w
            inBounds (x, y) = x >= 0 && y >=  0 && x < w && y < h
            toEdge  (x, y)  = (y * w + x, Edge 1 1)
            neighbors       = map toEdge $ filter inBounds [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
            
            