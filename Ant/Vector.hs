{-# LANGUAGE PatternGuards, NoMonomorphismRestriction #-}

module Ant.Vector where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

#define DEBUG

#ifndef DEBUG

indexU :: (U.Unbox a) => U.Vector a -> Int -> a
indexU = U.unsafeIndex
{-# INLINE indexU #-}

indexV :: V.Vector a -> Int -> a
indexV = V.unsafeIndex
{-# INLINE indexV #-}

readV = VM.unsafeRead
{-# INLINE readV #-}

writeV = VM.unsafeWrite
{-# INLINE writeV #-}

readU = UM.unsafeRead
{-# INLINE readU #-}

writeU = UM.unsafeWrite
{-# INLINE writeU #-}

#else

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


#endif