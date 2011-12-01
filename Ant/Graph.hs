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
    
    , grTest
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


type Queue a = Q.PSQ SearchNode a

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
grPath sn = path sn [] where
    path (SearchNode r _ Nothing)  xs = r : xs
    path (SearchNode r _ (Just p)) xs = path p (r : xs)


grBFS :: Graph -> RegionIndex -> [SearchNode]
grBFS = grSearch snDistance

grAStar :: Size -> Graph -> RegionIndex -> RegionIndex -> Maybe SearchNode
grAStar worldSize graph source dest = listToMaybe $ dropWhile ( (/= dest) . snRegion ) nodes
    where 
        nodes  = grSearch metric graph source
        
        metric :: SearchNode -> Float
        metric (SearchNode r d _) = fromIntegral d + (sqrt . fromIntegral $ distanceSq worldSize destCentre (centre r))
        {-# INLINE metric #-}
            
        centre r = regionCentre (graph `grIndex` r)      
        destCentre  = centre dest
        
        
insertMin :: Ord a => SearchNode -> a -> Queue a -> Queue a
insertMin node p = Q.alter alter node where
    alter (Just p') = Just (min p p')
    alter Nothing    = Just p
{-# INLINE insertMin #-}
    
grSearch :: Ord a => (SearchNode -> a) -> Graph -> RegionIndex -> [SearchNode]
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
            insert node = insertMin node (metric node)
            
            
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
            
            