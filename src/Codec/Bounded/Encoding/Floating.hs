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
-- 'Encoding's for encoding floating point numbers represented as 'Float' or
-- 'Double' values using big-endian, little-endian, and host-endian encodings.
--
-- FIXME: Ensure that this module is only compiled on compilers where its use
-- is safe.
--
module Codec.Bounded.Encoding.Floating
    ( 

    -- * Big-endian encodings
      floatBE
    , doubleBE

    -- * Little-endian encodings
    , floatLE
    , doubleLE

    -- * Host-endian encodings
    , floatHost
    , doubleHost

    ) where


import Codec.Bounded.Encoding.Internal (Encoding, writeStorable, (#.) )
import Codec.Bounded.Encoding.Word     (word32BE, word32LE, word64BE, word64LE)

import Foreign (Word32, Word64)

import Unsafe.Coerce (unsafeCoerce)

{- These implementations are unsound: See http://hackage.haskell.org/trac/ghc/ticket/4092
 - 
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

-}

-- TODO: implement and check "isEEE x" and "(sizeOf x) == 8"
--
{-# INLINE coerceFloatToWord32 #-}
coerceFloatToWord32 :: Float -> Word32
coerceFloatToWord32 = undefined

-- | Coerce a 'Double' to a 'Word64'.
{-# INLINE coerceDoubleToWord64 #-}
coerceDoubleToWord64 :: Double -> Word64
coerceDoubleToWord64 = undefined

-- | Encoding a 'Float' in big endian format.
{-# INLINE floatBE #-}
floatBE :: Encoding Float
floatBE = word32BE #. coerceFloatToWord32

-- | Encoding a 'Float' in little endian format.
{-# INLINE floatLE #-}
floatLE :: Encoding Float
floatLE = word32LE #. coerceFloatToWord32

-- | Encoding a 'Double' in big endian format.
{-# INLINE doubleBE #-}
doubleBE :: Encoding Double
doubleBE = word64BE #. coerceDoubleToWord64

-- | Encoding a 'Double' in little endian format.
{-# INLINE doubleLE #-}
doubleLE :: Encoding Double
doubleLE = word64LE #. coerceDoubleToWord64



-- | Encoding a 'Float' in native host order and host endianness. Values written
-- this way are not portable to different endian machines, without conversion.
--
{-# INLINE floatHost #-}
floatHost :: Encoding Float
floatHost = writeStorable

-- | Encoding a 'Double' in native host order and host endianness.
{-# INLINE doubleHost #-}
doubleHost :: Encoding Double
doubleHost = writeStorable

