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
-- > fromWrite $ writeWord8 x `mappend` writeWord8 y `mappend` writeWord z
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

      writeWord8

    -- ** Big-endian writes
    , writeWord16be           -- :: Word16 -> Write
    , writeWord32be           -- :: Word32 -> Write
    , writeWord64be           -- :: Word64 -> Write

    -- ** Little-endian writes
    , writeWord16le           -- :: Word16 -> Write
    , writeWord32le           -- :: Word32 -> Write
    , writeWord64le           -- :: Word64 -> Write

    -- ** Host-endian writes
    , writeWordhost           -- :: Word -> Write
    , writeWord16host         -- :: Word16 -> Write
    , writeWord32host         -- :: Word32 -> Write
    , writeWord64host         -- :: Word64 -> Write

    ) where

import System.IO.Write.Internal
import System.IO.Write.Internal.UncheckedShifts

import Foreign

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Word (Word32(..),Word16(..),Word64(..))
#else
import Data.Word
#endif

------------------------------------------------------------------------------
-- Word writes
--------------
--
-- Based upon the 'putWordX' functions from "Data.Binary.Builder" from the
-- 'binary' package.
-- 
------------------------------------------------------------------------------


-- | Write a single byte.
--
{-# INLINE writeWord8 #-}
writeWord8 :: Word8 -> Write
writeWord8 x = exactWrite 1 (\pf -> poke pf x)

--
-- We rely on the fromIntegral to do the right masking for us.
-- The inlining here is critical, and can be worth 4x performance
--

-- | Write a 'Word16' in big endian format.
{-# INLINE writeWord16be #-}
writeWord16be :: Word16 -> Write
writeWord16be w = exactWrite 2 $ \p -> do
    poke p               (fromIntegral (shiftr_w16 w 8) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (w)              :: Word8)

-- | Write a 'Word16' in little endian format.
{-# INLINE writeWord16le #-}
writeWord16le :: Word16 -> Write
writeWord16le w = exactWrite 2 $ \p -> do
    poke p               (fromIntegral (w)              :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w16 w 8) :: Word8)

-- writeWord16le w16 = exactWrite 2 (\p -> poke (castPtr p) w16)

-- | Write a 'Word32' in big endian format.
{-# INLINE writeWord32be #-}
writeWord32be :: Word32 -> Write
writeWord32be w = exactWrite 4 $ \p -> do
    poke p               (fromIntegral (shiftr_w32 w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)               :: Word8)

-- | Write a 'Word32' in little endian format.
{-# INLINE writeWord32le #-}
writeWord32le :: Word32 -> Write
writeWord32le w = exactWrite 4 $ \p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 w 24) :: Word8)

-- on a little endian machine:
-- writeWord32le w32 = exactWrite 4 (\p -> poke (castPtr p) w32)

-- | Write a 'Word64' in big endian format.
{-# INLINE writeWord64be #-}
writeWord64be :: Word64 -> Write
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
writeWord64be w =
    let a = fromIntegral (shiftr_w64 w 32) :: Word32
        b = fromIntegral w                 :: Word32
    in exactWrite 8 $ \p -> do
    poke p               (fromIntegral (shiftr_w32 a 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (a)               :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w32 b 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (b)               :: Word8)
#else
writeWord64be w = exactWrite 8 $ \p -> do
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
{-# INLINE writeWord64le #-}
writeWord64le :: Word64 -> Write
#if WORD_SIZE_IN_BITS < 64
writeWord64le w =
    let b = fromIntegral (shiftr_w64 w 32) :: Word32
        a = fromIntegral w                 :: Word32
    in exactWrite 8 $ \p -> do
    poke (p)             (fromIntegral (a)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 a 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (b)               :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b  8) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b 16) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w32 b 24) :: Word8)
#else
writeWord64le w = exactWrite 8 $ \p -> do
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
-- writeWord64le w64 = exactWrite 8 (\p -> poke (castPtr p) w64)

------------------------------------------------------------------------
-- Unaligned, word size ops

-- | Write a single native machine 'Word'. The 'Word' is written in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Word'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this way
-- are not portable to different endian or word sized machines, without
-- conversion.
--
{-# INLINE writeWordhost #-}
writeWordhost :: Word -> Write
writeWordhost w = 
    exactWrite (sizeOf (undefined :: Word)) (\p -> poke (castPtr p) w)

-- | Write a 'Word16' in native host order and host endianness.
{-# INLINE writeWord16host #-}
writeWord16host :: Word16 -> Write
writeWord16host w16 = 
    exactWrite (sizeOf (undefined :: Word16)) (\p -> poke (castPtr p) w16)

-- | Write a 'Word32' in native host order and host endianness.
{-# INLINE writeWord32host #-}
writeWord32host :: Word32 -> Write
writeWord32host w32 = 
    exactWrite (sizeOf (undefined :: Word32)) (\p -> poke (castPtr p) w32)

-- | Write a 'Word64' in native host order and host endianness.
{-# INLINE writeWord64host #-}
writeWord64host :: Word64 -> Write
writeWord64host w = 
    exactWrite (sizeOf (undefined :: Word64)) (\p -> poke (castPtr p) w)


