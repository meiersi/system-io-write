{-# LANGUAGE MonoPatBinds #-}
-- |
-- Copyright   : (c) 2010 Simon Meier
--
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- 'Write's and 'Builder's for serializing integers.
--
-- See "Blaze.ByteString.Builder.Word" for information about how to best write several
-- integers at once.
--
module System.IO.Write.Int
    ( 
    -- * Writing integers to a buffer

      writeInt8

    -- ** Big-endian writes
    , writeInt16be           -- :: Write Int16
    , writeInt32be           -- :: Write Int32
    , writeInt64be           -- :: Write Int64

    -- ** Little-endian writes
    , writeInt16le           -- :: Write Int16
    , writeInt32le           -- :: Write Int32
    , writeInt64le           -- :: Write Int64

    -- ** Host-endian writes
    , writeInthost           -- :: Write Int
    , writeInt16host         -- :: Write Int16
    , writeInt32host         -- :: Write Int32
    , writeInt64host         -- :: Write Int64

    ) where

import System.IO.Write.Internal (Write, writeStorable, comapWrite)
import System.IO.Write.Word

import Foreign

------------------------------------------------------------------------------
-- Int writes
--------------
--
-- we rely on 'fromIntegral' to do a loss-less conversion to the corresponding
-- 'Word' type
-- 
------------------------------------------------------------------------------


-- | Write a single signed byte.
--
{-# INLINE writeInt8 #-}
writeInt8 :: Write Int8
writeInt8 = comapWrite fromIntegral writeWord8

-- | Write an 'Int16' in big endian format.
{-# INLINE writeInt16be #-}
writeInt16be :: Write Int16
writeInt16be = comapWrite fromIntegral writeWord16be

-- | Write an 'Int16' in little endian format.
{-# INLINE writeInt16le #-}
writeInt16le :: Write Int16
writeInt16le = comapWrite fromIntegral writeWord16le

-- | Write an 'Int32' in big endian format.
{-# INLINE writeInt32be #-}
writeInt32be :: Write Int32
writeInt32be = comapWrite fromIntegral writeWord32be

-- | Write an 'Int32' in little endian format.
{-# INLINE writeInt32le #-}
writeInt32le :: Write Int32
writeInt32le = comapWrite fromIntegral writeWord32le

-- | Write an 'Int64' in big endian format.
{-# INLINE writeInt64be #-}
writeInt64be :: Write Int64
writeInt64be = comapWrite fromIntegral writeWord64be

-- | Write an 'Int64' in little endian format.
{-# INLINE writeInt64le #-}
writeInt64le :: Write Int64
writeInt64le = comapWrite fromIntegral writeWord64le


------------------------------------------------------------------------
-- Unaligned, integer size ops

-- | Write a single native machine 'Int'. The 'Int' is written in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Int'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this way
-- are not portable to different endian or integer sized machines, without
-- conversion.
--
{-# INLINE writeInthost #-}
writeInthost :: Write Int
writeInthost = writeStorable

-- | Write an 'Int16' in native host order and host endianness.
{-# INLINE writeInt16host #-}
writeInt16host :: Write Int16
writeInt16host = writeStorable

-- | Write an 'Int32' in native host order and host endianness.
{-# INLINE writeInt32host #-}
writeInt32host :: Write Int32
writeInt32host = writeStorable

-- | Write an 'Int64' in native host order and host endianness.
{-# INLINE writeInt64host #-}
writeInt64host :: Write Int64
writeInt64host = writeStorable


