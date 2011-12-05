{-# LANGUAGE PatternGuards #-}

module Ant.Diffusion 
    ( DiffusionGraph
    , diffusionGraph
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

data DiffusionGraph = DiffusionGraph
    { dgFlowGraph :: V.Vector Node
    , dgDensity   :: U.Vector Double
    }

data Node = Node 
    { nConnections :: U.Vector (RegionIndex, Double)
    , nTotal       :: !Double
    }

flowGraph :: Graph -> (RegionIndex -> Bool) -> V.Vector Node
flowGraph graph passable = V.fromList $ map toNode [0.. grSize graph - 1] where
    toNode r | not (passable r) = Node U.empty 0.0  
             | otherwise        = Node nConnections (fromIntegral total)
    
        where   
            edges = filter (passable . fst) $ grEdges r graph
            total = sum . map (edgeConnectivity . snd) $ edges

            toConnection (r, e)  = (r, fromIntegral (edgeConnectivity e))
            nConnections   = U.fromList (map toConnection edges)
        
        
flowParticles :: DiffusionGraph -> (Region, [Point]) -> [(Point, Region)]
flowParticles = error "Not implemented"
        
diffusionGraph :: Graph -> (RegionIndex -> Maybe Double) -> DiffusionGraph
diffusionGraph graph initial = DiffusionGraph 
    { dgFlowGraph = flowGraph graph (isJust . initial)
    , dgDensity   = U.generate (grSize graph) (fromMaybe 0.0 . initial)
    }
    
-- Gauss Sidel diffusion, approximately from
-- http://www.dgp.toronto.edu/people/stam/reality/Research/pdf/GDC03.pdf
diffuse :: Double -> DiffusionGraph -> [U.Vector Double]
diffuse rate dg@(DiffusionGraph flow density0) = iterate diffuse' density0
    where
        diffuse' density' = U.imap (diffuseNode density') density0
    
        diffuseNode density' i d = (d + rate * weighted) / (1.0 + rate * total) where       
            weight (r, w) = (density' `indexU` r) * w
            weighted = (U.sum . U.map weight) conn
            (Node conn total) = flow `indexV` i
            