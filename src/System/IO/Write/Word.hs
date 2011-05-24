{-# LANGUAGE CPP, MonoPatBinds #-}
-- |
-- Copyright   : (c) 2010 Jasper Van der Jeugt & Simon Meier
--
--               Original serialization code from 'Data.Binary.Builder':
--               (c) Lennart Kolmodin, Ross Patterson
--
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- 'Write's and 'Builder's for serializing words.
--
-- Note that for serializing a three tuple @(x,y,z)@ of bytes (or other word
-- values) you should use the expression 
--
-- > fromWrite $ word8 x `mappend` word8 y `mappend` word z
--
-- instead of
--
-- > fromWord8 x `mappend` fromWord8 y `mappend` fromWord z
--
-- The first expression will result in a single atomic write of three bytes,
-- while the second expression will check for each byte, if there is free space
-- left in the output buffer. Coalescing these checks can improve performance
-- quite a bit, as long as you use it sensibly.
--
#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif
module System.IO.Write.Word
    ( 
    -- * Writing words to a buffer

      word8

    -- ** Big-endian writes
    , word16BE           -- :: Write Word16
    , word32BE           -- :: Write Word32
    , word64BE           -- :: Write Word64

    -- ** Little-endian writes
    , word16LE           -- :: Write Word16
    , word32LE           -- :: Write Word32
    , word64LE           -- :: Write Word64

    -- ** Host-endian writes
    , wordHost           -- :: Write Word
    , word16Host         -- :: Write Word16
    , word32Host         -- :: Write Word32
    , word64Host         -- :: Write Word64

    ) where

import System.IO.Write.Internal
import System.IO.Write.Internal.UncheckedShifts

import Foreign

------------------------------------------------------------------------------
-- Word writes
--------------
--
-- Based upon the 'putWordX' functions of "Data.Binary.Builder" from the
-- 'binary' package.
-- 
------------------------------------------------------------------------------


-- | Write a single byte.
--
{-# INLINE word8 #-}
word8 :: Write Word8
word8 = writeStorable

--
-- We rely on the fromIntegral to do the right masking for us.
-- The inlining here is critical, and can be worth 4x performance
--

-- | Write a 'Word16' in big endian format.
{-# INLINE word16BE #-}
word16BE :: Write Word16
word16BE = exactWrite 2 $ \w p -> do
    poke p               (fromIntegral (shiftr_w16 w 8) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (w)              :: Word8)

-- | Write a 'Word16' in little endian format.
{-# INLINE word16LE #-}
word16LE :: Write Word16
word16LE = exactWrite 2 $ \w p -> do
    poke p               (fromIntegral (w)              :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w16 w 8) :: Word8)

-- word16LE w16 = exactWrite 2 (\w p -> poke (castPtr p) w16)

-- | Write a 'Word32' in big endian format.
{-# INLINE word32BE #-}
word32BE :: Write Word32
word32BE = exactWrite 4 $ \w p -> do
    poke p               (fromIntegral (shiftr_w32 w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)               :: Word8)

-- | Write a 'Word32' in little endian format.
{-# INLINE word32LE #-}
word32LE :: Write Word32
word32LE = exactWrite 4 $ \w p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 w 24) :: Word8)

-- on a little endian machine:
-- word32LE w32 = exactWrite 4 (\w p -> poke (castPtr p) w32)

-- | Write a 'Word64' in big endian format.
{-# INLINE word64BE #-}
word64BE :: Write Word64
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
word64BE =
    exactWrite 8 $ \w p -> do
        let a = fromIntegral (shiftr_w64 w 32) :: Word32
            b = fromIntegral w                 :: Word32
        poke p               (fromIntegral (shiftr_w32 a 24) :: Word8)
        poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a 16) :: Word8)
        poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a  8) :: Word8)
        poke (p `plusPtr` 3) (fromIntegral (a)               :: Word8)
        poke (p `plusPtr` 4) (fromIntegral (shiftr_w32 b 24) :: Word8)
        poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b 16) :: Word8)
        poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b  8) :: Word8)
        poke (p `plusPtr` 7) (fromIntegral (b)               :: Word8)
#else
word64BE = exactWrite 8 $ \w p -> do
    poke p               (fromIntegral (shiftr_w64 w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (w)               :: Word8)
#endif

-- | Write a 'Word64' in little endian format.
{-# INLINE word64LE #-}
word64LE :: Write Word64
#if WORD_SIZE_IN_BITS < 64
word64LE =
    exactWrite 8 $ \w p -> do
        let b = fromIntegral (shiftr_w64 w 32) :: Word32
            a = fromIntegral w                 :: Word32
        poke (p)             (fromIntegral (a)               :: Word8)
        poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a  8) :: Word8)
        poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a 16) :: Word8)
        poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 a 24) :: Word8)
        poke (p `plusPtr` 4) (fromIntegral (b)               :: Word8)
        poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b  8) :: Word8)
        poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b 16) :: Word8)
        poke (p `plusPtr` 7) (fromIntegral (shiftr_w32 b 24) :: Word8)
#else
word64LE = exactWrite 8 $ \w p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w64 w 56) :: Word8)
#endif

-- on a little endian machine:
-- word64LE w64 = exactWrite 8 (\w p -> poke (castPtr p) w64)

------------------------------------------------------------------------
-- Unaligned, word size ops

-- | Write a single native machine 'Word'. The 'Word' is written in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Word'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this way
-- are not portable to different endian or word sized machines, without
-- conversion.
--
{-# INLINE wordHost #-}
wordHost :: Write Word
wordHost = writeStorable

-- | Write a 'Word16' in native host order and host endianness.
{-# INLINE word16Host #-}
word16Host :: Write Word16
word16Host = writeStorable

-- | Write a 'Word32' in native host order and host endianness.
{-# INLINE word32Host #-}
word32Host :: Write Word32
word32Host = writeStorable

-- | Write a 'Word64' in native host order and host endianness.
{-# INLINE word64Host #-}
word64Host :: Write Word64
word64Host = writeStorable


