module RegionStats where

import Data.List

import Ant.Game
import Ant.Renderer

import qualified Data.IntMap as M
import qualified Data.Vector.Unboxed as U

type AntIndex = Int

data RegionContents
    { friendlyAnts  :: [(Point, AntIndex)]
    , enemySquares  :: [Point]
    , foodSquares   :: [Point]
    , hasFriendlyHill :: !Bool
    , hasEnemyHill    :: !Bool
    }
    
    


data RegionStats = 
    { visibleCount   :: !Int
    , friendlyCount  :: !Int
    , enemyCount     :: !Int

    }
    
    
    
regionStats :: RegionMap -> V.Vector RegionStats