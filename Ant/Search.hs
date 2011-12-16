{-# LANGUAGE PatternGuards #-}

module Ant.Search
    ( SearchNode (..)
    , search
    , searchPath
    
    , searchN
    
    )
    
    where


import qualified Data.IntSet as S
import Data.Maybe

import Data.PSQueue ( Binding(..) )
import qualified Data.PSQueue as Q    
    

type Queue m = Q.PSQ SearchNode m

data SearchNode = SearchNode 
    { snKey      :: Int
    , snDistance :: Int
    , snPred     :: Maybe (SearchNode)
    }
    
instance Show (SearchNode) where
    show (SearchNode r d _) = show r ++ " dist: " ++ show d

    
instance Eq (SearchNode) where
    (==) sn sn' = snKey sn == snKey sn'
    
instance Ord (SearchNode) where
    compare sn sn' = snKey sn `compare` snKey sn'
    
searchPath :: SearchNode -> [Int]
searchPath = (`path` []) where
	path (SearchNode r _ Nothing)  xs = r : xs
	path (SearchNode r _ (Just p)) xs = path p (r : xs)

insertMin :: (Ord m) => SearchNode -> m -> Queue m -> Queue m
insertMin node p = Q.alter alter node where
    alter (Just p') = Just (min p p')
    alter Nothing    = Just p
{-# INLINE insertMin #-}    
    
search :: (Ord m) => (SearchNode -> [SearchNode]) -> (SearchNode -> m) -> Int -> [SearchNode]
search succ metric r = searchN succ metric [r]
    
searchN :: (Ord m) => (SearchNode -> [SearchNode]) -> (SearchNode -> m) -> [Int] -> [SearchNode]
searchN succ metric rs = search (S.fromList rs) (Q.fromList (map toPriorityNode rs)) where
    
    toPriorityNode r = node :-> metric node
        where node = SearchNode r 0 Nothing           
          
    search seen queue  | Nothing                      <- view = []
                       | Just ((node :-> _), queue')  <- view = next node seen queue'                    
        where view = Q.minView queue  
        
    next node seen queue = node : (search seen' queue')
        where
            queue'  = foldr insert queue successors
            seen'   = S.insert (snKey node) seen
            
            successors = filter (flip S.notMember seen . snKey) (succ node)
            insert node = insertMin node (metric node)