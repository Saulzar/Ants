module Ant.Square 
    ( Square
    
    , unknownSquare

    , squareHill
    , squareAnt 
    
    , isHill
    , isAnt
    , isVisible
    , wasSeen
    , isWater
    , hasFood
    
    , tileChar
    
    , setVisibility
    , setContent
    
    )
where

import Ant.IO

import Foreign

import Data.Bits
import Data.Word
import Data.Char

data Square = Square 
    { sAnt         :: {-# UNPACK #-} !Word8  
    , sHill        :: {-# UNPACK #-} !Word8
    , sFlags       :: {-# UNPACK #-} !Word16
    }

-- Instance so we can use Square in a Storable vector
instance Storable Square where
  sizeOf _ = sizeOf (undefined :: Word8) * 4
  alignment _ = alignment (undefined :: Word32)
 
  {-# INLINE peek #-}
  peek p = do
     ant   <- peekByteOff q 0
     hill  <- peekByteOff q 1
     flags <- peekByteOff q 2
     return (Square ant hill flags)
    where
      q = castPtr p
  {-# INLINE poke #-}
  poke p (Square ant hill flags) = do
     pokeByteOff q 0 ant
     pokeByteOff q 1 hill
     pokeByteOff q 2 flags
    where
      q = castPtr p    
    
unknownSquare :: Square
unknownSquare = Square noPlayer noPlayer 0
{-# INLINE unknownSquare #-}         
         
-- Interface getters for squares
squareHill, squareAnt :: Square -> Maybe Player
squareHill = maybePlayer . sHill 
squareAnt  = maybePlayer . sAnt  
{-# INLINE squareHill #-}
{-# INLINE squareAnt #-}


isHill, isAnt, isVisible, wasSeen, isWater, hasFood :: Square -> Bool
isHill    = isPlayer . sHill  
isAnt     = isPlayer . sAnt   
isWater   = (testFlag waterFlag) . sFlags       
isVisible = (testFlag visibleFlag) . sFlags     
wasSeen   = (testFlag seenFlag) . sFlags     
hasFood   = (testFlag foodFlag) . sFlags   
{-# INLINE isHill #-}
{-# INLINE isWater #-} 
{-# INLINE isAnt #-}
{-# INLINE isVisible #-}
{-# INLINE wasSeen #-}
{-# INLINE hasFood #-} 
        
tileChar :: Square -> Char
tileChar square  | not (wasSeen square)  = '?'
                 | isWater square        = '~'
                 | (Just hill) <- squareAnt square = chr(ord 'a' + hill) 
                 | (Just ant) <- squareAnt square  = chr(ord '0' + ant)
                 | hasFood square    = '#'
                 | isVisible square  = '.'
                 | otherwise         = 'o' 

-- Interface setters for squares
setVisibility :: Bool -> Square -> Square
setVisibility True (Square _ _ flags) = Square noPlayer noPlayer (resetFlags flags)
    where
        resetFlags flags = (flags .|. seenFlag .|. visibleFlag) .&. (complement foodFlag)   

setVisibility False (Square p h flags) = Square p h (flags .&. (complement visibleFlag))
        
{-# INLINE setVisibility #-}
   
setContent ::  Content -> Square -> Square
setContent (Ant p)    square = square { sAnt = (fromIntegral p) }
setContent (Hill p)   square = square { sHill = (fromIntegral p) }     
setContent  Water     square = square { sFlags = sFlags square .|. waterFlag }
setContent  Food      square = square { sFlags = sFlags square .|. foodFlag }
setContent  _         square = square


-- Internal constants
visibleFlag, seenFlag, waterFlag :: Word16 
visibleFlag = 1
seenFlag    = 2
waterFlag   = 4
foodFlag    = 8
{-# INLINE waterFlag #-} 
{-# INLINE visibleFlag #-} 
{-# INLINE seenFlag #-}
{-# INLINE foodFlag #-}
        
noPlayer :: Word8 
noPlayer = -1
{-# INLINE noPlayer #-}        
        
-- Helper functions
maybePlayer :: Word8 -> Maybe Player
maybePlayer n | n == noPlayer = Just (fromIntegral n)
         | otherwise     = Nothing
{-# INLINE maybePlayer #-}
         
isPlayer :: Word8 -> Bool
isPlayer n = n /= noPlayer
{-# INLINE isPlayer #-}         
         
testFlag :: Word16 -> Word16 -> Bool
testFlag flag1 = \flag2 -> (flag1 .&. flag2) > 0
{-# INLINE testFlag #-}    
