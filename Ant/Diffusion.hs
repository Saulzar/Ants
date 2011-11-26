{-# LANGUAGE PatternGuards #-}

module Ant.Diffusion where

import Data.List

import Ant.Map
import Ant.Point
import Ant.GraphBuilder
import Ant.IO

import Ant.Graph

import qualified Data.Set as S
import qualified Data.IntMap as M

import Control.Monad.ST
import Control.Monad

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM



