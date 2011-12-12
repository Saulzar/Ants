module Ant.Game 
     ( GameState(..)
     , Game
     , runGame
     , initialState
     
     , module Ant.Point
     , module Ant.Square
     , module Ant.IO
     , module Ant.Map
     , module Ant.Scenario
     , module Ant.RegionBuilder
     , module Ant.Passibility
     , module Ant.RegionStats
     , module Ant.Graph
     , module Ant.Scheduler
	 , module Ant.Movement
     
     )
    where

import Control.Monad
import Control.Monad.State.Strict

import qualified Data.Set as S

import Ant.Point
import Ant.IO
import Ant.Map
import Ant.Scenario
import Ant.Square
import Ant.RegionBuilder
import Ant.Passibility
import Ant.RegionStats
import Ant.Graph
import Ant.Scheduler
import Ant.Movement

import System.IO
import System.Random
import Debug.Trace

type Game a = StateT GameState IO a


data GameState = GameState
    { gameSettings      :: !GameSettings
    , gameMap           :: Map
    , gameBuilder       :: RegionBuilder
    , gamePass          :: Passibility
	, gameStats			:: GameStats
    , gameGraph         :: Graph
    }

 
initialState :: GameSettings -> GameState
initialState settings = GameState 
    { gameSettings    = settings
    , gameMap         = emptyMap unknownSquare (mapDimensions settings) 
    , gameBuilder     = emptyBuilder (mapDimensions settings) 32
    , gamePass        = emptyPassibility (mapDimensions settings) pattern2
    , gameStats       = initialStats 
    , gameGraph       = grEmpty
    }
 
 
getSetting :: (GameSettings -> a) -> Game a
getSetting f = gets (f . gameSettings) 

contentSquares :: (Content -> Bool) -> [SquareContent] -> [Point]
contentSquares f content = map fst $ filter (f . snd) content    
           
initRegions :: [SquareContent] -> RegionBuilder -> RegionBuilder
initRegions content builder = foldr addRegion builder hillRegions where 
    hills = contentSquares (playerHill 0) content
    hillRegions = filter isUnknown hills   
    isUnknown p = invalidRegion == regionAt (regionMap builder) (builderDim builder) p
    
    
modifyBuilder :: (RegionBuilder -> RegionBuilder) -> Game ()
modifyBuilder f = modify $ \state -> state { gameBuilder = f (gameBuilder state) }
 
modifyMap :: (Map -> Map) -> Game ()
modifyMap f = modify $ \state -> state { gameMap = f (gameMap state) }

modifyPass :: (Passibility -> Passibility) -> Game ()
modifyPass f = modify $ \state -> state { gamePass = f (gamePass state) }

updateState :: [SquareContent] -> Game [Order]
updateState content = do
        
	-- Update visibility and get changes in visibility
	world <- gets gameMap
	radiusSq <- getSetting viewRadius2

	let ants = contentSquares (playerAnt 0) content
	let vis = visibleSet (mapSize world) radiusSq ants
	let dVis = newlyVisible vis world

	-- Initialise regions to start at the hills (first turn only)
	n <- gets (numRegions . gameBuilder)
	when (n == 0) $ modifyBuilder (initRegions content) 

	let world' = (updateContent content . updateVisibility vis) world

	pass <- gets gamePass 
	let pass' = updatePassibility dVis world' pass

	builder <- gets gameBuilder
	let builder' = updateBuilder pass' world' builder

	stats <- gets gameStats

	let graph = grCreate (regions builder')
	let content' = filter (not . containsWater . snd) content

	let stats' = updateStats world' graph (regionMap builder') vis content' stats
	let antTasks = scheduleAnts (regionMap builder') world stats' graph ants
	
	liftIO $ hPrint stderr antTasks
	
	let moves = moveAnts (regionMap builder') world stats graph antTasks
		

	stats `seq` modify $ \gameState -> gameState 
		{ gameMap = world'
		, gamePass = pass'
		, gameBuilder = builder'
		, gameStats = stats'
		, gameGraph = graph
		}

	return moves
	
processTurn :: Int -> [SquareContent] -> Game [Order]
processTurn n content = do
    
    orders <- updateState content
    return orders
    
    --orders <- liftM (map toEnum . randomRs (0, 3)) (liftIO getStdGen)
    --return $ zip (map fst ants) orders


runGame :: GameState -> IO GameState
runGame = execStateT (gameLoop processTurn)