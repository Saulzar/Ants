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

data Node = Node 
    { nConnections :: U.Vector (RegionIndex, Float, Float)
    , nTotal       :: !Float
    }

flowGraph :: Graph -> U.Vector Bool -> FlowGraph
flowGraph graph passable = V.fromList $ map toNode [0.. grSize graph - 1] where
    toNode r | not (isPassable r) = Node U.empty 0.0  
             | otherwise        = Node nConnections (fromIntegral total)
    
        where   
            isPassable = (passable `indexU`)
            
            edges = filter (isPassable . fst) $ grEdges r graph
            total = sum . map (edgeConnectivity . snd) $ edges

            toConnection (r, e)  = (r, fromIntegral (edgeConnectivity e), fromIntegral (edgeDistance e))
            nConnections   = U.fromList (map toConnection edges)
        
nodeOutflows :: Int -> U.Vector Float -> RegionIndex -> Node -> Maybe U.Vector Float
nodeAssignments n densities region (Node connections _)  | total > 0 = Just $ U.map ((* scale) .  fromIntegral) flows
                                                         | otherwise = Nothing
    where    
        density     = densities `indexU` region
        outFlow d  = max (density - d) 0
        (neighbors, _, _) = U.unzip3 connections   

        flows = U.map (outFlow . (densities `indexU`))
        
        total = U.sum flows 
        scale = (fromIntegral n ) / total     

        
flowParticles :: Size -> FlowGraph ->  U.Vector Float -> RegionIndex -> [Point] -> [(Point, RegionIndex)]
flowParticles worldSize flow densities region particles = flowParticles' maybeOutflows where 
    maybeOutflows = nodeOutflows (length particles) densities region (flow `indexV` region) 
        
    flowParticles' Nothing         = []
    flowParticles' (Just outflows) =
        
        distance p out outflow = outflow * distance worldSize p out
            
        
    
-- Gauss Sidel diffusion, approximately from
-- http://www.dgp.toronto.edu/people/stam/reality/Research/pdf/GDC03.pdf
diffuse :: Float -> FlowGraph ->  U.Vector Float  -> [U.Vector Float]
diffuse rate flow density0 = iterate diffuse' density0
    where
        diffuse' density' = U.imap (diffuseNode density') density0
    
        diffuseNode density' i d = (d + rate * weighted) / (1.0 + rate * total) where       
            weight (r, w, _) = (density' `indexU` r) * w
            weighted = (U.sum . U.map weight) conn
            (Node conn total) = flow `indexV` i
            