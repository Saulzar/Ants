{-# LANGUAGE PatternGuards #-}

module Ant.Graph
    ( Graph
    , regions
    , graphSize
    
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
import qualified Data.IntSet as IS 
import qualified Data.Sequence as S

import Control.Monad.ST

import Data.List
import Data.Maybe

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
type RegionMap = U.Vector (RegionIndex, Distance)
type Set = M.IntMap Distance  
type Queue = S.Seq Sq 

type RegionSet = IS.IntSet

data Graph = Graph
    { regionMap  :: RegionMap
    , regions    :: V.Vector Region
    
    , openRegions    :: RegionSet
    , graphSize         :: !Size
    , regionDistance    :: Int
    }
    
    
data Region = Region 
     {  regionCentre  :: PointIndex
     ,  regionId      :: RegionIndex
     ,  regionNeighbors :: M.IntMap [PointIndex]
     } deriving Show
     
     
invalidRegion :: RegionIndex
invalidRegion = -1
     
emptyGraph :: Size -> Int -> Graph
emptyGraph size distance = Graph 
    { regionMap = (U.replicate (area size) (invalidRegion, 1000 )) 
    , regions   = V.empty     
    , openRegions = IS.empty
    , graphSize = size
    , regionDistance = distance
    }
     
  
updateGraph :: Map -> Graph -> Graph
updateGraph world graph = traceShow open $ foldl' (expandRegion world) graph open
    where
        open = map (regions graph V.!) $ IS.toList (openRegions graph)
        
     
addRegion ::  Map -> Graph -> PointIndex -> (Graph, Region)
addRegion world graph centre = (graph', region)
    where 
        graph' = graph 
            { regions = V.cons region (regions graph)
            , openRegions = IS.insert regionIndex (openRegions graph) 
            }
    
        regionIndex = V.length (regions graph)
        region  = Region 
            { regionId      = V.length (regions graph)
            , regionCentre  = centre
            }
             

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
     


                
data SearchState = SearchState
    { visitedSquares :: Set
    , unseenSquares  :: !Bool
    , searchQueue    :: Queue
    , neighbourSquares :: [(RegionIndex, PointIndex)] 
    }    
                
search :: (SearchState -> Sq -> SearchState)  -- Add successors for one square
    -> (SearchState -> (Maybe Sq, SearchState)) -- Take one square from the queue
    -> SearchState -> SearchState
search genSuccessors takeOne state = search' (takeOne state)   
    where        
        search' (Nothing, state)  =   state
        search' (Just sq, state)  =   search' (takeOne (genSuccessors state sq))

        
takeOne :: SearchState -> (Maybe Sq, SearchState)
takeOne state | S.EmptyL         <- S.viewl (searchQueue state) = (Nothing, state)
              | (sq S.:< queue') <- S.viewl (searchQueue state) = (Just sq, state { searchQueue = queue' })
        
                 
searchFrom :: PointIndex -> SearchState
searchFrom p = SearchState 
    {   visitedSquares   = M.singleton  p 0
    ,   searchQueue      = S.singleton (p, 0)
    ,   unseenSquares    = False   
    ,   neighbourSquares = []
    }


changes ::  RegionMap -> Set -> ([Sq], [RegionIndex])
changes regions = unzip . catMaybes . map lessSq . M.toList where
        lessSq (p, d) | d < d'          = Just ((p, d), r')
                      | otherwise       = Nothing
            where
                (r', d') = regions `U.unsafeIndex` p
 

expandRegion :: Map -> Graph -> Region -> Graph
expandRegion world graph region = graph 
    { regionMap  = writeSquares (regionId region) changedSquares (regionMap graph)
    , openRegions = IS.delete (regionId region) open'
    , regions     = modify (\v -> write v (regionIndex region) region') (regions graph)
    }
    
    }
    where
       search = searchRegion (regionDistance graph) world (regionMap graph) region
       (changedSquares, changedRegions) = changes (regionMap graph) (visitedSquares search)        
       open' = foldr IS.insert (openRegions graph) (filter (/= invalidRegion) changedRegions)
       
    region' = region { regionNeighbors = foldlr insert M.empty (neighbourSquares search) }
    insert (k, v) m = M.insertWith (++) k [v]    
    
writeSquares :: RegionIndex -> [Sq] -> RegionMap ->  RegionMap
writeSquares r squares regionMap = runST update where
 
    update :: ST s RegionMap
    update = do
        v <- U.unsafeThaw regionMap
        forM_ squares $ \(p, d) -> UM.unsafeWrite v p (r, d)
        U.unsafeFreeze v

                                                   
searchRegion :: Int -> Map -> RegionMap -> Region -> SearchState
searchRegion distance world regions region = search successors takeOne (searchFrom (regionCentre region)) where

    successors :: SearchState -> Sq -> SearchState               
    successors  state (p, d) | d < distance = state'
                             | otherwise  = state
    
        where
            
            state' = state 
                     { visitedSquares = foldr (uncurry M.insert) (visitedSquares state) next
                     , unseenSquares  = unseenSquares state || any isUnknown neighbors
                     , searchQueue    = foldl' (S.|>) (searchQueue state) next'
                     , neighbourSquares =  borders' ++ (neighborSquares state)
                     }
            
            neighbors = neighborIndices (mapSize world) p 
            (next, borders) = (partition isBorder . filter isSuccessor)  neighbors
            	
            next' =  map (\p -> (p, d + 1)) next
            borders' = map ((fst . getRegion) &&& id)  borders
            
            isUnknown = not . wasSeen . (world `atIndex`)
                      
            isSuccessor p = isLand (world `atIndex` p)                -- Land and not water (and we've seen it before)
                          && (M.notMember p (visitedSquares state))   -- Not already visited
            
            getRegion p = regions `U.unsafeIndex` p
                          
            isBorder p = (regionId region) /= r' && d < d'
                 where
                     (r', d') = getRegion p
                     
                     