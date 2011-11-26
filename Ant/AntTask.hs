{-# LANGUAGE PatternGuards #-}

module Ant.AntTask where

import Ant.Point
import Ant.IO

import qualified Data.Set as S
import Control.Monad


type AntSet = S.Set AntTask
type AntList = [(Point, Player)]


data Task  = Unassigned | Goto !RegionIndex | Gather !Point | Guard
        
data AntTask = AntTask
    { antTask  :: !Task
    , antPos   :: !Point
    }
	
instance Eq AntTask where
    (==) a b = (antPos a == antPos b)
    
instance Ord AntTask where
    compare a b = antPos a `compare` antPos b
	
makeAnt :: SquareContent -> AntTask
makeAnt (p, _) = AntTask 
	{ antTask = Unassigned
	, antPos   = p
	}

