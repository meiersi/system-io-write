{-# LANGUAGE MonoPatBinds, MagicHash #-}
-- |
-- Copyright   : (c) 2010 Simon Meier
--
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (uses unsafeCoerce)
--
-- 'Write's for encoding floating point numbers represented as 'Float' or
-- 'Double' values using big-endian, little-endian, and host-endian encodings.
--
-- FIXME: Ensure that this module is only compiled on compilers where its use
-- is safe.
--
module System.IO.Write.Floating
    ( 

    -- * Big-endian writes
      floatBE
    , doubleBE

    -- * Little-endian writes
    , floatLE
    , doubleLE

    -- * Host-endian writes
    , floatHost
    , doubleHost

    ) where


import System.IO.Write.Internal (Write, writeStorable, (#.) )
import System.IO.Write.Word     (word32BE, word32LE, word64BE, word64LE)

import Foreign (Word32, Word64)

import Unsafe.Coerce (unsafeCoerce)

-- | Coerce a 'Float' to a 'Word32'; i.e., interpret the 32-bit 'Float' value
-- as an unsigned 32-bit 'Int. 
--
-- FIXME: Check with GHC developers if this is really safe. Does the register
-- allocater handle such a case correctly, if the 'Float' is in an FPU
-- register?
{-# INLINE coerceFloatToWord32 #-}
coerceFloatToWord32 :: Float -> Word32
coerceFloatToWord32 = unsafeCoerce

-- | Coerce a 'Double' to a 'Word64'.
{-# INLINE coerceDoubleToWord64 #-}
coerceDoubleToWord64 :: Double -> Word64
coerceDoubleToWord64 = unsafeCoerce

-- | Write a 'Float' in big endian format.
{-# INLINE floatBE #-}
floatBE :: Write Float
floatBE = word32BE #. coerceFloatToWord32

-- | Write a 'Float' in little endian format.
{-# INLINE floatLE #-}
floatLE :: Write Float
floatLE = word32LE #. coerceFloatToWord32

-- | Write a 'Double' in big endian format.
{-# INLINE doubleBE #-}
doubleBE :: Write Double
doubleBE = word64BE #. coerceDoubleToWord64

-- | Write a 'Double' in little endian format.
{-# INLINE doubleLE #-}
doubleLE :: Write Double
doubleLE = word64LE #. coerceDoubleToWord64



-- | Write a 'Float' in native host order and host endianness. Values written
-- this way are not portable to different endian machines, without conversion.
--
{-# INLINE floatHost #-}
floatHost :: Write Float
floatHost = writeStorable

-- | Write a 'Double' in native host order and host endianness.
{-# INLINE doubleHost #-}
doubleHost :: Write Double
doubleHost = writeStorable

