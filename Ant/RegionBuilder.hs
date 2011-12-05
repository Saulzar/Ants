{-# LANGUAGE PatternGuards #-}

module Ant.RegionBuilder
    ( RegionBuilder
    , RegionMap
    , Region (..)
    , RegionIndex
	, RegionGraph
	
	, EdgeMap
	, Edge (..)
    
    , regions
    , regionMap
    , builderDim
    
    , numRegions
    , regionAt
    
    , Distance
    , invalidRegion
        
    , emptyBuilder
    , addRegion
    , updateBuilder
    
    , regionSquare
    , regionDistance
    , setAllOpen
    
    , searchRegion
    , neighborSquares
    
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

data Edge = Edge 
    { edgeDistance :: !Int
    , edgeConnectivity :: !Int
    } deriving Show
    
type EdgeMap = M.IntMap Edge

type RegionGraph = M.IntMap Region

data RegionBuilder = RegionBuilder
    { regionMap  		:: !RegionMap
    , regions    		:: RegionGraph
    , openRegions       :: !RegionSet
    , builderDim        :: !Size
    , regionDistance    :: !Int
    , candidates        :: !PointSet
    }
    
    
data Region = Region 
     {  regionCentre  	:: !Point
     ,  regionId      	:: !RegionIndex
     ,  regionNeighbors :: !EdgeMap
     ,  regionSize      :: !Int
	 ,  regionFrontier  :: !Bool
     } deriving Show

     
indexSq = U.unsafeIndex
{-# INLINE indexSq #-}     
     
invalidRegion :: RegionIndex
invalidRegion = -1
     
emptyBuilder :: Size -> Int -> RegionBuilder
emptyBuilder size distance = RegionBuilder 
    { regionMap = (U.replicate (area size) (invalidRegion, 1000 )) 
    , regions   = M.empty    
    , openRegions = S.empty
    , builderDim = size
    , regionDistance = distance
    , candidates = S.empty
    }
     

regionAt :: RegionMap -> Size -> Point -> RegionIndex
regionAt regionMap worldSize p = fst $ regionMap `indexSq` index where
   index = worldSize `wrapIndex` p 
{-# INLINE regionAt #-}      
     
numRegions :: RegionBuilder -> Int
numRegions builder = M.size (regions builder)     
     
updateRegion :: Passibility -> Map -> RegionIndex -> RegionBuilder -> RegionBuilder
updateRegion pass world i builder = (expandRegion pass world builder region) 
    where (Just region) = M.lookup i (regions builder) 

setAllOpen :: RegionBuilder -> RegionBuilder 
setAllOpen builder = builder { openRegions = S.fromList [0.. M.size (regions builder)] }
       
updateBuilder :: Passibility -> Map -> RegionBuilder -> RegionBuilder
updateBuilder pass world builder =  (seedRegions pass . expandGraph pass world) builder 


expandGraph :: Passibility -> Map -> RegionBuilder -> RegionBuilder
expandGraph pass world builder =  foldr (flip (expandRegion pass world)) builder open
    where
        open = catMaybes . map (flip M.lookup (regions builder)) $ rs
        rs = S.toList (openRegions builder)
     
     
seedRegions :: Passibility -> RegionBuilder -> RegionBuilder
seedRegions pass builder = builder' { candidates = S.empty }
    where 
        seeds = findSeeds pass builder
        builder' = foldr addRegion builder (map (fromIndex (builderDim builder)) seeds)
     
seedDistance :: Int
seedDistance = 6
     
findSeeds :: Passibility -> RegionBuilder -> [PointIndex]
findSeeds pass builder = separate (manhattenIndex (builderDim builder)) maxDistance prioritised
    where
        candidates' = (filter validSeed . S.toList) (candidates builder)
        prioritised = sortBy (compare `on` (indexCost pass)) candidates'
        distance = snd . (regionMap builder `indexSq`)
        
        validSeed p = distance p > seedDistance && pass `indexCost` p < 6
        
        
addRegion :: Point -> RegionBuilder -> RegionBuilder
addRegion centre builder = builder 
        { regions = M.insert (regionId region) region (regions builder)
        , openRegions = S.insert (regionId region) (openRegions builder) 
        }
    where
        region  = Region 
            { regionId      = numRegions builder
            , regionCentre  = centre
            , regionNeighbors = M.empty
            , regionSize      = 0
            , regionFrontier  = True
            }
             

separate :: (a -> a -> Int) -> Int -> [a] -> [a]        
separate metric distance = foldl' add []
     where
        add seeds p | all (apart p) seeds = p : seeds 
                    | otherwise            = seeds

        apart p1 p2 = distance < metric p1 p2
{-# INLINE separate #-}       

            
regionSquare :: RegionBuilder -> Point ->  (RegionIndex, Distance)   
regionSquare builder p = (regionMap builder) `indexSq` (wrapIndex (builderDim builder) p)  
{-# INLINE regionSquare #-}       
     
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
        neighbors (p, _) = filterNeighbors size visited p


filterNeighbors :: Size -> Set -> PointIndex -> [PointIndex]
filterNeighbors size visited  = filter (\p' -> M.notMember p' visited) . neighborIndices size 

{-
neighborSquares :: Size -> Set -> PointSet
neighborSquares size visited  = neighborSets !! 20  where

    neighborSets = iterate neighbors' (edgeSquares size visited)
    neighbors' =  S.fromList . concatMap (filterNeighbors size visited) . S.toList
-}

makeEdges :: Size -> Region -> M.IntMap Int -> RegionGraph -> EdgeMap
makeEdges size region conn regions =  M.mapWithKey edge conn 
    where edge i c = Edge 
            { edgeDistance = (ceiling . sqrt) (fromIntegral sq)
            , edgeConnectivity = c
            }
            where
                region' = fromJust (M.lookup i regions)
                sq      = distanceSq size (regionCentre region) (regionCentre region')
        
connectivity ::  RegionMap -> PointSet -> M.IntMap Int
connectivity regions neighbors =  countAssocs regions' where
    
    regions' = map regionSquare (S.toList neighbors)    
    regionSquare p = (r, 1)
        where (r, d) = regions `indexSq` p 
              
countAssocs :: [(Int, Int)] -> M.IntMap Int
countAssocs = foldr insert M.empty
    where insert = uncurry (M.insertWith (+))              
              
updateNeighbor :: RegionIndex -> EdgeMap -> Region -> Region              
updateNeighbor index revNeighbors region | (Just n) <- entry = region  { regionNeighbors = withEntry n }  
                                         | Nothing  <- entry = region { regionNeighbors = withoutEntry }
        where
            entry = M.lookup (regionId region) revNeighbors 
            
            withoutEntry    = M.delete index (regionNeighbors region)
            withEntry n     = M.insert index n withoutEntry

updateNeighbors :: RegionIndex -> [RegionIndex] -> EdgeMap -> RegionGraph -> RegionGraph
updateNeighbors index changed neighbors rs = foldr update rs changed
    where update = M.update (Just . updateNeighbor index neighbors)
 

hasNeighbor :: RegionBuilder -> Int -> Int -> Bool
hasNeighbor builder source dest = isJust $ do 
    s <- source `M.lookup` (regions builder)
    dest `M.lookup` (regionNeighbors s)
        
neighborsValid :: RegionBuilder -> Bool
neighborsValid builder = all (uncurry (hasNeighbor builder)) pairs
    where 
        neighbourPairs r = map (\(i, _) -> (i, regionId r)) $ M.toList (regionNeighbors r)
        pairs = concatMap neighbourPairs (map snd $ M.toList (regions builder))

        
expandRegion :: Passibility -> Map -> RegionBuilder -> Region -> RegionBuilder
expandRegion pass world builder region = open' `seq` builder 
    { regionMap    = regionMap'
    , openRegions  = open'
    --, regions = M.map updateNeighbor' regions' 
    , regions      = updateNeighbors (regionId region) changedNeighbors regionNeighbors' regions'
    , candidates   = foldr S.insert  (candidates builder) (map fst changedSquares)
    }
    
    where
        regionMap' = writeSquares (regionId region) changedSquares (regionMap builder)
    
        visited = searchRegion pass world (regionMap builder) region
        (changedSquares, changedRegions) =  findChanges (regionMap builder) visited      

        neighbors = neighborSquares (mapSize world) visited
        
        conn = M.delete invalidRegion $ connectivity regionMap' neighbors
        regionNeighbors' = makeEdges (builderDim builder) region conn (regions builder)
        
        unseen    = any (isUnseen) (S.toList neighbors) where
            isUnseen p = (not . wasSeen) (world `atIndex` p)

        open' | unseen    = set
              | otherwise = S.delete (regionId region) set
           where set = foldr S.insert (openRegions builder) (filter (/= invalidRegion) changedRegions)

        region' = region 
			{ regionNeighbors 	= regionNeighbors'
			, regionSize 		= M.size visited 
			, regionFrontier 	= unseen
			}
			
        regions' = M.insert (regionId region) region' (regions builder)

        --updateNeighbor' = updateNeighbor (regionId region) regionNeighbors'
        changedNeighbors = map fst $ M.toList (M.union (regionNeighbors region) regionNeighbors') 

   
writeSquares :: RegionIndex -> [Sq] -> RegionMap ->  RegionMap
writeSquares r squares regionMap = U.modify update regionMap 
    where update v = forM_ squares $ \(p, d) -> UM.unsafeWrite v p (r, d)

{-    
writeSquares :: RegionIndex -> [Sq] -> RegionMap ->  RegionMap
writeSquares r squares regionMap = runST update where
 
    update :: ST s RegionMap
    update = do
        v <- U.unsafeThaw regionMap
        forM_ squares $ \(p, d) -> UM.unsafeWrite v p (r, d)
        U.unsafeFreeze v
  -}  
    
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
maxDistance = 8
                                              
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
                     