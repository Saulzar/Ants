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
        
        
flowParticles :: U.Vector Float -> FlowGraph -> RegionIndex -> [Point] -> [(Point, RegionIndex)]
flowParticles = error "Not implemented"
        

    
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
            