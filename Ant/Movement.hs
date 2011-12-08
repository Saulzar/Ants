module Movement where

import Data.List
import Data.Maybe
import Data.Function

import Debug.Trace

import Control.Monad.RWS.Strict

import Ant.Map
import Ant.Point
import Ant.Search
import Ant.Vector
import Ant.Graph
import Ant.Scheduler
import Ant.IO

import qualified Data.Set as S

    
type Move = RWS Context [Order] SquareSet


data Context = Context 
    { cWorld :: Map
    , cStats :: GameStats
    , cGraph :: Graph
    }
    
moveAnts :: Map -> GameStats -> Graph -> AntSet -> [Order]
moveAnts world stats graph ants = orders 
    where 
        ctx = (Context world stats graph)
        (_, orders) = evalRWS ctx S.empty (moveAnts' ants)


moveAnts' :: AntSet -> Move ()
moveAnts' ants = do
    return ()