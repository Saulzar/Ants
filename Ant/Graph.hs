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
    , setAllOpen
    
            
    )        
    where


import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
    
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import qualified Data.IntMap as M
import qualified Data.IntSet as S 

import qualified Data.PriorityQueue.FingerTree as Q

import Control.Monad.ST

import Data.List
import Data.Maybe

import Control.Monad
import Control.Arrow

import Ant.Square
import Ant.Map
import Ant.Point
import Ant.Passibility

import Debug.Trace

-- Some type snyonyms to make the intentions clearer
type PointIndex = Int
type RegionIndex = Int 
type Distance = Int
type Sq = (PointIndex, Distance)

-- Data structures for doing bredth first search
type RegionMap = U.Vector (RegionIndex, Distance)
type Set = M.IntMap Distance  
type Queue = Q.PQueue Distance PointIndex 

type RegionSet = S.IntSet
type PointSet = S.IntSet
type RegionCount = M.IntMap Int

data Graph = Graph
    { regionMap  :: !RegionMap
    , regions    :: M.IntMap Region
    , openRegions    :: !RegionSet
    , graphSize         :: !Size
    , regionDistance    :: !Int
    }
    
    
data Region = Region 
     {  regionCentre  :: !Point
     ,  regionId      :: !RegionIndex
     ,  regionNeighbors :: !RegionCount
     } deriving Show

     
indexSq = U.unsafeIndex
{-# INLINE indexSq #-}     
     
invalidRegion :: RegionIndex
invalidRegion = -1
     
emptyGraph :: Size -> Int -> Graph
emptyGraph size distance = Graph 
    { regionMap = (U.replicate (area size) (invalidRegion, 1000 )) 
    , regions   = M.empty    
    , openRegions = S.empty
    , graphSize = size
    , regionDistance = distance
    }
     

updateRegion :: Passibility -> Map -> RegionIndex -> Graph -> Graph
updateRegion pass world i graph = (expandRegion pass world graph region) 
    where (Just region) = M.lookup i (regions graph) 

setAllOpen :: Graph -> Graph 
setAllOpen graph = graph { openRegions = S.fromList [0.. M.size (regions graph)] }
           
updateGraph :: Passibility -> Map -> Graph -> Graph
updateGraph pass world graph =  foldl' (expandRegion pass world) graph open
    where
        open = catMaybes . map (flip M.lookup (regions graph)) $ rs
        rs = S.toList (openRegions graph)
     
addRegion ::  Map -> Graph -> Point -> (Graph, Region)
addRegion world graph centre =  (graph', region)
    where 
        graph' = graph 
            { regions = M.insert (regionId region) region (regions graph)
            , openRegions = S.insert (regionId region) (openRegions graph) 
            }
    
        region  = Region 
            { regionId      = M.size (regions graph)
            , regionCentre  = centre
            , regionNeighbors = M.empty
            }
             

separate :: (a -> a -> Int) -> Int -> [a] -> [a]        
separate metric distance = foldl' add []
     where
        add seeds p | all (apart p) seeds = p : seeds 
                    | otherwise            = seeds

        apart p1 p2 = distance < metric p1 p2
{-# INLINE separate #-}       

            
graphSquare :: Graph -> Point ->  (RegionIndex, Distance)   
graphSquare graph p = (regionMap graph) `indexSq` (wrapIndex (graphSize graph) p)  
{-# INLINE graphSquare #-}       
     
updateBy :: V.Vector a -> [a] -> (a -> Int) -> V.Vector a
updateBy v updates f =  v V.// map (\x -> (f x, x)) updates     
     


                
data SearchState = SearchState
    { visitedSquares :: Set
    , searchQueue    :: Queue
    }    
                
search :: (SearchState -> Sq -> SearchState)  -- Add successors for one square
    -> (SearchState -> (Maybe Sq, SearchState)) -- Take one square from the queue
    -> SearchState -> SearchState
search genSuccessors takeOne state = search' (takeOne state)   
    where        
        search' (Nothing, state)  =   state
        search' (Just sq, state)  =   search' (takeOne (genSuccessors state sq))

        
takeOne :: SearchState -> (Maybe Sq, SearchState)
takeOne state | Nothing            <- view  = (Nothing, state)
              | Just ((d, p), queue')  <- view  = (Just (p, d), state { searchQueue = queue' })
        where
            view = Q.minViewWithKey (searchQueue state)
                 
searchFrom :: PointIndex -> SearchState
searchFrom p = SearchState 
    {   visitedSquares   = M.singleton  p 0
    ,   searchQueue      = Q.singleton 0 p
    }


changes ::  RegionMap -> Set -> ([Sq], [RegionIndex])
changes regions = unzip . catMaybes . map lessSq . M.toList where
        lessSq (p, d) | d < d'          = Just ((p, d), r')
                      | otherwise       = Nothing
            where
                (r', d') =  regions `indexSq` p

 
neighborSquares :: Size -> Set -> PointSet
neighborSquares size visited  = (S.fromList . concatMap neighbors) (M.toList visited) 
    where 
        neighbors (p, _) = filter (\p' -> M.notMember p' visited) $ neighborIndices size p 


countAssocs :: [(Int, Int)] -> M.IntMap Int
countAssocs = foldr insert M.empty
    where insert = uncurry (M.insertWith (+))

        
neighborRegions :: RegionMap -> PointSet -> M.IntMap Int
neighborRegions regions neighbors =  countAssocs regions' where
    
    regions' = map regionSquare (S.toList neighbors)    
    regionSquare p = (r, 1)
        where (r, d) = regions `indexSq` p 
              
              
updateNeighbor :: RegionIndex -> M.IntMap Int -> Region -> Region              
updateNeighbor index revNeighbors region | (Just n) <- entry = region  { regionNeighbors = withEntry n }  
                                          | Nothing  <- entry = region { regionNeighbors = withoutEntry }
        where
            entry = M.lookup (regionId region) revNeighbors 
            
            withoutEntry    = M.delete index (regionNeighbors region)
            withEntry n     = M.insert index n withoutEntry
        
expandRegion :: Passibility -> Map -> Graph -> Region -> Graph
expandRegion pass world graph region =  graph 
    { regionMap    = writeSquares (regionId region) changedSquares (regionMap graph)
    , openRegions  = open'
    , regions = M.map updateNeighbor' regions'
    }
    
    where
    
       search = searchRegion pass world (regionMap graph) region
       (changedSquares, changedRegions) = changes (regionMap graph) (visitedSquares search)        

       open' | unseen    = set
             | otherwise = S.delete (regionId region) set
           where set = foldr S.insert (openRegions graph) (filter (/= invalidRegion) changedRegions)
       

       neighbors = neighborSquares (mapSize world) (visitedSquares search)
       regionNeighbors' = M.delete invalidRegion (neighborRegions (regionMap graph) neighbors) 
       
       region' = region { regionNeighbors = regionNeighbors' }
       regions' = M.insert (regionId region) region' (regions graph)
       
       updateNeighbor' = updateNeighbor (regionId region) regionNeighbors'

       unseen    = any (isUnseen) (S.toList neighbors) where
           isUnseen p = (not . wasSeen) (world `atIndex` p)
    
writeSquares :: RegionIndex -> [Sq] -> RegionMap ->  RegionMap
writeSquares r squares regionMap = runST update where
 
    update :: ST s RegionMap
    update = do
        v <- U.unsafeThaw regionMap
        forM_ squares $ \(p, d) -> UM.unsafeWrite v p (r, d)
        U.unsafeFreeze v

        
maxDistance :: Passibility -> Int
-- maxDistance pass = (maxCost pass) * 32
maxDistance = const 200   
                                               
searchRegion :: Passibility -> Map -> RegionMap -> Region -> SearchState
searchRegion pass world regions region = search successors takeOne (searchFrom centre) where
    centre = (mapSize world) `wrapIndex`  (regionCentre region)

    successors :: SearchState -> Sq -> SearchState               
    successors  state (p, d) | d < (maxDistance pass) = state'
                             | otherwise              = state
    
        where
            
            state' = state 
                     { visitedSquares = foldr (uncurry M.insert) (visitedSquares state) next
                     , searchQueue    = foldr (\(p, d) -> Q.insert d p) (searchQueue state) next
                     }
            
            neighbors = neighborIndices (mapSize world) p 
            next = (map withDistance . filter isSuccessor)  neighbors
            withDistance p' = (p', d + 1)         

            getCost p'= pass `indexCost` p'

                                  
            isSuccessor p' = isLand (world `atIndex` p')                -- Land and not water (and we've seen it before)
                          && (M.notMember p' (visitedSquares state))   -- Not already visited
                          && lessDistance p'
                     
            lessDistance p' = d < d' 
                where (r, d') = regions `indexSq` p'                          
                     