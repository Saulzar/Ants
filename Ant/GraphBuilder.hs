{-# LANGUAGE PatternGuards #-}

module Ant.GraphBuilder
    ( GraphBuilder
    , RegionMap
    , Region (..)
    , RegionIndex
	, RegionGraph
	
	, EdgeMap
	, Edge
    
    , regions
    , regionMap
    , graphSize
    
    , numRegions
    , regionAt
    
    , Distance
    , invalidRegion
        
    , emptyGraph
    , addRegion
    , updateGraph
    
    , graphSquare
    , regionDistance
    , setAllOpen
    
    , searchRegion
    , neighborSquares
    , neighborRegions
    
    , findChanges
    , neighborsValid
    
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
import Data.Function

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

type Edge = Int
type EdgeMap = M.IntMap Edge

type RegionGraph = M.IntMap Region

data GraphBuilder = GraphBuilder
    { regionMap  :: !RegionMap
    , regions    :: RegionGraph
    , openRegions    :: !RegionSet
    , graphSize         :: !Size
    , regionDistance    :: !Int
    , candidates        :: !PointSet
    }
    
    
data Region = Region 
     {  regionCentre  :: !Point
     ,  regionId      :: !RegionIndex
     ,  regionNeighbors :: !EdgeMap
     ,  regionSize      :: !Int
	 ,  regionLastSeen  :: !Int
     } deriving Show

     
indexSq = U.unsafeIndex
{-# INLINE indexSq #-}     
     
invalidRegion :: RegionIndex
invalidRegion = -1
     
emptyGraph :: Size -> Int -> GraphBuilder
emptyGraph size distance = GraphBuilder 
    { regionMap = (U.replicate (area size) (invalidRegion, 1000 )) 
    , regions   = M.empty    
    , openRegions = S.empty
    , graphSize = size
    , regionDistance = distance
    , candidates = S.empty
    }
     

regionAt :: GraphBuilder -> Point -> RegionIndex
regionAt graph p = fst $ regionMap graph `indexSq` index where
   index = graphSize graph `wrapIndex` p 
{-# INLINE regionAt #-}      
     
numRegions :: GraphBuilder -> Int
numRegions graph = M.size (regions graph)     
     
updateRegion :: Passibility -> Map -> RegionIndex -> GraphBuilder -> GraphBuilder
updateRegion pass world i graph = (expandRegion pass world graph region) 
    where (Just region) = M.lookup i (regions graph) 

setAllOpen :: GraphBuilder -> GraphBuilder 
setAllOpen graph = graph { openRegions = S.fromList [0.. M.size (regions graph)] }
       
updateGraph :: Passibility -> Map -> GraphBuilder -> GraphBuilder
updateGraph pass world graph =  (seedRegions pass . expandGraph pass world) graph 


expandGraph :: Passibility -> Map -> GraphBuilder -> GraphBuilder
expandGraph pass world graph =  foldr (flip (expandRegion pass world)) graph open
    where
        open = catMaybes . map (flip M.lookup (regions graph)) $ rs
        rs = S.toList (openRegions graph)
     
     
seedRegions :: Passibility -> GraphBuilder -> GraphBuilder
seedRegions pass graph = graph' { candidates = S.empty }
    where 
        seeds = findSeeds pass graph
        graph' = foldr addRegion graph (map (fromIndex (graphSize graph)) seeds)
     
seedDistance :: Int
seedDistance = 8
     
findSeeds :: Passibility -> GraphBuilder -> [PointIndex]
findSeeds pass graph = separate (manhattenIndex (graphSize graph)) maxDistance prioritised
    where
        candidates' = (filter validSeed . S.toList) (candidates graph)
        prioritised = sortBy (compare `on` (indexCost pass)) candidates'
        distance = snd . (regionMap graph `indexSq`)
        
        validSeed p = distance p > seedDistance && pass `indexCost` p < 8
        
        
addRegion :: Point -> GraphBuilder -> GraphBuilder
addRegion centre graph = graph 
            { regions = M.insert (regionId region) region (regions graph)
            , openRegions = S.insert (regionId region) (openRegions graph) 
            }
        where
            region  = Region 
                { regionId      = numRegions graph
                , regionCentre  = centre
                , regionNeighbors = M.empty
                , regionSize      = 0
				, regionLastSeen  = 0
                }
             

separate :: (a -> a -> Int) -> Int -> [a] -> [a]        
separate metric distance = foldl' add []
     where
        add seeds p | all (apart p) seeds = p : seeds 
                    | otherwise            = seeds

        apart p1 p2 = distance < metric p1 p2
{-# INLINE separate #-}       

            
graphSquare :: GraphBuilder -> Point ->  (RegionIndex, Distance)   
graphSquare graph p = (regionMap graph) `indexSq` (wrapIndex (graphSize graph) p)  
{-# INLINE graphSquare #-}       
     
updateBy :: V.Vector a -> [a] -> (a -> Int) -> V.Vector a
updateBy v updates f =  v V.// map (\x -> (f x, x)) updates     
     

                 

findChanges ::  RegionMap -> Set -> ([Sq], [RegionIndex])
findChanges regions = unzip . catMaybes . map lessSq . M.toList where
        lessSq (p, d) | d < d'          = Just ((p, d), r')
                      | otherwise       = Nothing
            where
                (r', d') =  regions `indexSq` p

 
neighborSquares :: Size -> Set -> PointSet
neighborSquares size visited  = (S.fromList . concatMap neighbors) (M.toList visited) 
    where 
        neighbors (p, _) = filter (\p' -> M.notMember p' visited) $ neighborIndices size p 


countAssocs :: [(Int, Int)] -> EdgeMap
countAssocs = foldr insert M.empty
    where insert = uncurry (M.insertWith (+))

        
neighborRegions :: RegionMap -> PointSet -> EdgeMap
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

updateNeighbors :: RegionIndex -> [RegionIndex] -> M.IntMap Int -> M.IntMap Region -> M.IntMap Region
updateNeighbors index changed neighbors rs = foldr update rs changed
    where update = M.update (Just . updateNeighbor index neighbors)
 

hasNeighbor :: GraphBuilder -> Int -> Int -> Bool
hasNeighbor graph source dest = isJust $ do 
    s <- source `M.lookup` (regions graph)
    dest `M.lookup` (regionNeighbors s)
        
neighborsValid:: GraphBuilder -> Bool
neighborsValid graph = all (uncurry (hasNeighbor graph)) pairs
    where 
        neighbourPairs r = map (\(i, _) -> (i, regionId r)) $ M.toList (regionNeighbors r)
        pairs = concatMap neighbourPairs (map snd $ M.toList (regions graph))

        
expandRegion :: Passibility -> Map -> GraphBuilder -> Region -> GraphBuilder
expandRegion pass world graph region = open' `seq` graph 
    { regionMap    = regionMap'
    , openRegions  = open'
    --, regions = M.map updateNeighbor' regions' 
    , regions      = updateNeighbors (regionId region) changedNeighbors regionNeighbors' regions'
    , candidates   = foldr S.insert  (candidates graph) (map fst changedSquares)
    }
    
    where
        regionMap' = writeSquares (regionId region) changedSquares (regionMap graph)
    
        visited = searchRegion pass world (regionMap graph) region
        (changedSquares, changedRegions) =  findChanges (regionMap graph) visited      

        neighbors = neighborSquares (mapSize world) visited
        regionNeighbors' = M.delete invalidRegion (neighborRegions (regionMap graph) neighbors) 

        unseen    = any (isUnseen) (S.toList neighbors) where
            isUnseen p = (not . wasSeen) (world `atIndex` p)

        open' | unseen    = set
              | otherwise = S.delete (regionId region) set
           where set = foldr S.insert (openRegions graph) (filter (/= invalidRegion) changedRegions)

        region' = region { regionNeighbors = regionNeighbors', regionSize = M.size visited }
        regions' = M.insert (regionId region) region' (regions graph)

        --updateNeighbor' = updateNeighbor (regionId region) regionNeighbors'
        changedNeighbors = map fst $ M.toList (M.union (regionNeighbors region) regionNeighbors') 

{-        
writeSquares :: RegionIndex -> [Sq] -> RegionMap ->  RegionMap
writeSquares r squares regionMap = U.modify update regionMap 
    where update v = forM_ squares $ \(p, d) -> UM.unsafeWrite v p (r, d)
-}
    
writeSquares :: RegionIndex -> [Sq] -> RegionMap ->  RegionMap
writeSquares r squares regionMap = runST update where
 
    update :: ST s RegionMap
    update = do
        v <- U.unsafeThaw regionMap
        forM_ squares $ \(p, d) -> UM.unsafeWrite v p (r, d)
        U.unsafeFreeze v
    
    
data SearchState = SearchState
    { visitedSquares :: !Set
    , searchQueue    :: !Queue
    }        


search :: (SearchState -> Sq -> SearchState)  -- Add successors for one square
        -> Int -> SearchState -> SearchState
search genSuccessors n state = search' n (takeOne state)   
    where    
        search' 0 (_,       state)  =   state
        search' _ (Nothing, state)  =   state
        search' n (Just sq, state)  =   search' (n - 1) (takeOne (genSuccessors state sq))

        
takeOne :: SearchState -> (Maybe Sq, SearchState)
takeOne state | Nothing            <- view  = (Nothing, state)
              | Just ((d, p), queue')  <- view  = (Just (p, d), state { searchQueue = queue' })
        where
            view = Q.minViewWithKey (searchQueue state)
            
searchFrom :: PointIndex -> SearchState
searchFrom p = SearchState 
    {   visitedSquares   = M.singleton p 0
    ,   searchQueue      = Q.singleton 0 p
    }
            
maxDistance ::  Int
maxDistance = 12
                                              
searchRegion :: Passibility -> Map -> RegionMap -> Region -> Set
searchRegion pass world regions region = visitedSquares $ search successors maxSquares (searchFrom centre) where
    centre = (mapSize world) `wrapIndex`  (regionCentre region)
    maxSquares = (maxDistance * maxDistance)

    successors :: SearchState -> Sq -> SearchState               
    successors  state (p, d) = state 
                     { visitedSquares = foldr (uncurry M.insert) (visitedSquares state) next
                     , searchQueue    = foldr (\(p, d) -> Q.insert d p) (searchQueue state) next
                     }       
        where

            neighbors = neighborIndices (mapSize world) p 
            next = (filter isSuccessor . map withDistance)  neighbors
            withDistance p' = (p', d + getCost p')         

            getCost p'= 1 + pass `indexCost` p' `div` 8

                                  
            isSuccessor (p', d') = isLand (world `atIndex` p')                -- Land and not water (and we've seen it before)
                          && (M.notMember p' (visitedSquares state))   -- Not already visited
                          && lessDistance p' d'
                     
            lessDistance p' d' = d' < d'' || r' == (regionId region)
                where (r', d'') = regions `indexSq` p'                          
                     