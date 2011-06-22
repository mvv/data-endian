{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Endian (
    EndianSensitive(..),
    toBigEndian,
    fromBigEndian,
    toLittleEndian,
    fromLittleEndian
  ) where

import Data.Int
import Data.Word
import Data.Bits

-- | Raw, endian-sensitive data
class EndianSensitive α where
  -- | Change the endianness of the argument
  swapEndian ∷ α → α

-- | Convert from the native format to big-endian
toBigEndian      ∷ EndianSensitive α ⇒ α → α
-- | Convert from big-endian to the native format
fromBigEndian    ∷ EndianSensitive α ⇒ α → α
-- | Convert from the native format to little-endian
toLittleEndian   ∷ EndianSensitive α ⇒ α → α
-- | Convert from little-endian to the native format
fromLittleEndian ∷ EndianSensitive α ⇒ α → α

#ifdef WORDS_BIGENDIAN
toBigEndian      = id
toLittleEndian   = swapEndian
#else
toBigEndian      = swapEndian
toLittleEndian   = id
#endif

fromBigEndian    = toBigEndian
fromLittleEndian = toLittleEndian

{-# INLINE toBigEndian #-}
{-# INLINE fromBigEndian #-}
{-# INLINE toLittleEndian #-}
{-# INLINE fromLittleEndian #-}

instance EndianSensitive α ⇒ EndianSensitive [α] where
  swapEndian = map swapEndian

instance EndianSensitive Word16 where
  swapEndian = (`rotateR` 8)
  {-# INLINE swapEndian #-}

instance EndianSensitive Word32 where
  swapEndian x  =  (x                  `shiftR` 24)
               .|. ((x .&. 0x00FF0000) `shiftR` 8)
               .|. ((x .&. 0x0000FF00) `shiftL` 8)
               .|. (x                  `shiftL` 24)
  {-# INLINE swapEndian #-}

instance EndianSensitive Word64 where
  swapEndian x
#if WORD_SIZE_IN_BITS == 32
    =  fromIntegral (swapEndian hi)
   .|. (fromIntegral (swapEndian lo) `shiftL` 32)
      where lo, hi ∷ Word32
            lo = fromIntegral x
            hi = fromIntegral (x `shiftR` 32)
#else
    =  (x                          `shiftR` 56)
   .|. ((x .&. 0x00FF000000000000) `shiftR` 40)
   .|. ((x .&. 0x0000FF0000000000) `shiftR` 24)
   .|. ((x .&. 0x000000FF00000000) `shiftR` 8)
   .|. ((x .&. 0x00000000FF000000) `shiftL` 8)
   .|. ((x .&. 0x0000000000FF0000) `shiftL` 24)
   .|. ((x .&. 0x000000000000FF00) `shiftL` 40)
   .|. (x                          `shiftL` 56)
#endif
  {-# INLINE swapEndian #-}

instance EndianSensitive Int16 where
  swapEndian = (`rotateR` 8)
  {-# INLINE swapEndian #-}

instance EndianSensitive Int32 where
  swapEndian = fromIntegral . (swapEndian ∷ Word32 → Word32) . fromIntegral
  {-# INLINE swapEndian #-}

instance EndianSensitive Int64 where
  swapEndian = fromIntegral . (swapEndian ∷ Word64 → Word64) . fromIntegral
  {-# INLINE swapEndian #-}

