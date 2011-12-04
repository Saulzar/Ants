{-# LANGUAGE PatternGuards, NoMonomorphismRestriction #-}

module Ant.Vector where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM



indexU :: (U.Unbox a) => U.Vector a -> Int -> a
indexU = (U.!)
{-# INLINE indexU #-}

indexV :: V.Vector a -> Int -> a
indexV = (V.!)
{-# INLINE indexV #-}

readV = VM.read
{-# INLINE readV #-}

writeV = VM.write
{-# INLINE writeV #-}

readU = UM.read
{-# INLINE readU #-}

writeU = UM.write
{-# INLINE writeU #-}