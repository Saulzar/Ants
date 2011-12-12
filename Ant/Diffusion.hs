{-# LANGUAGE PatternGuards #-}

module Ant.Diffusion 
    ( FlowGraph
    , flowGraph
    , diffuse
    , flowParticles
    )   
    where

import Data.List
import Data.Maybe
import Data.Function

import Debug.Trace

import Ant.Map
import Ant.Point
import Ant.RegionBuilder
import Ant.IO
import Ant.Vector

import Ant.Graph


import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import qualified Data.Vector as V

type FlowGraph = V.Vector Node
type Connection = (RegionIndex, Float, Float)

cRegion :: Connection -> RegionIndex 
cRegion (r, _, _) = r

cDistance :: Connection -> Float
cDistance (_, _, d) = d

cConnectivity :: Connection -> Float
cConnectivity (_, c, _) = c

data Node = Node 
    { nConnections :: U.Vector Connection
    , nCentre      :: !Point
    , nTotal       :: !Float
    }

flowGraph :: Graph -> U.Vector Bool -> FlowGraph
flowGraph graph passable = V.fromList $ map toNode [0.. grSize graph - 1] where
    toNode r | not (isPassable r) = Node U.empty centre 0.0  
             | otherwise        = Node nConnections centre (fromIntegral total)
    
        where   
            isPassable = (passable `indexU`)
            
            centre = regionCentre (graph `grIndex` r)
            
            edges = filter (isPassable . fst) $ grEdges r graph
            total = sum . map (edgeConnectivity . snd) $ edges

            toConnection (r, e)  = (r, fromIntegral (edgeConnectivity e), fromIntegral (edgeDistance e))
            nConnections   = U.fromList (map toConnection edges)
        
nodeOutflows :: Int -> U.Vector Float -> RegionIndex -> Node -> [(Connection, Float)]
nodeOutflows n densities region (Node connections _ _) = map scaleFlow flows
    where    
        density     = densities `indexU` region

        difference c  = (c, density - (densities `indexU` cRegion c))
        flows = filter ( (> 0) . snd) $ map difference (U.toList connections)
        
        total = sum (map snd flows)
        scale = (fromIntegral n + 1) / total     

        scaleFlow (c, f) = (c, scale * f)
        
flowParticles :: Size -> FlowGraph ->  U.Vector Float -> RegionIndex -> [Point] -> [(Point, RegionIndex)]
flowParticles worldSize flowGraph densities region particles = flowParticles' outflows particles [] where 
    outflows = nodeOutflows (length particles) densities region node
    node = (flowGraph `indexV` region) 

    -- Normalized distance, distance of particle divided by length of edge * flow rate
    
    heuristic :: Point -> (Connection, Float) -> ((Point, RegionIndex), Float)
    heuristic p (c, f) = ((p, r), f * capacity / normDistance)
        where
            node' = flowGraph `indexV` r
            capacity = cConnectivity c / nTotal node 
            
            r = cRegion c
            normDistance = distance worldSize p (nCentre node') / cDistance c
        
    decFlows flows r | f > 1       =  (c, f - 1) : flows'
                     | otherwise   =  flows'
        where
            (Just (c, f)) = find isRegion flows
            flows' = filter (not . isRegion) flows
        
            isRegion = (== r) . cRegion . fst
             
    flowParticles' :: [(Connection, Float)] -> [Point] -> [(Point, RegionIndex)] -> [(Point, RegionIndex)]
    flowParticles' []     _       assigns = assigns
    flowParticles' _     []       assigns = assigns
    flowParticles' flows ps assigns = flowParticles' flows' ps' ((best, r) : assigns) where
        
        ((best, r), _) = maximumBy (compare `on` snd) [heuristic p flow | flow <- flows, p <- ps]
        flows'      = decFlows flows r
        ps'         = filter (/= best) ps
        
        
        
    
-- Gauss Sidel diffusion, approximately from
-- http://www.dgp.toronto.edu/people/stam/reality/Research/pdf/GDC03.pdf
diffuse :: Float -> FlowGraph ->  U.Vector Float  -> [U.Vector Float]
diffuse rate flow density0 = iterate diffuse' density0
    where
        diffuse' density' = U.imap (diffuseNode density') density0
    
        diffuseNode density' i d = (d + rate * weighted) / (1.0 + rate * total) where       
            weight (r, w, _) = (density' `indexU` r) * w
            weighted = (U.sum . U.map weight) conn
            (Node conn _ total) = flow `indexV` i
            
            

            