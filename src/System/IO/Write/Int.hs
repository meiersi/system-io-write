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
    , writeInt16be           -- :: Int16 -> Write
    , writeInt32be           -- :: Int32 -> Write
    , writeInt64be           -- :: Int64 -> Write

    -- ** Little-endian writes
    , writeInt16le           -- :: Int16 -> Write
    , writeInt32le           -- :: Int32 -> Write
    , writeInt64le           -- :: Int64 -> Write

    -- ** Host-endian writes
    , writeInthost           -- :: Int -> Write
    , writeInt16host         -- :: Int16 -> Write
    , writeInt32host         -- :: Int32 -> Write
    , writeInt64host         -- :: Int64 -> Write

    ) where

import System.IO.Write.Internal (Write, writeStorable)
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
writeInt8 :: Int8 -> Write
writeInt8 = writeWord8 . fromIntegral

-- | Write an 'Int16' in big endian format.
{-# INLINE writeInt16be #-}
writeInt16be :: Int16 -> Write
writeInt16be = writeWord16be . fromIntegral

-- | Write an 'Int16' in little endian format.
{-# INLINE writeInt16le #-}
writeInt16le :: Int16 -> Write
writeInt16le = writeWord16le . fromIntegral

-- | Write an 'Int32' in big endian format.
{-# INLINE writeInt32be #-}
writeInt32be :: Int32 -> Write
writeInt32be = writeWord32be . fromIntegral

-- | Write an 'Int32' in little endian format.
{-# INLINE writeInt32le #-}
writeInt32le :: Int32 -> Write
writeInt32le = writeWord32le . fromIntegral

-- | Write an 'Int64' in big endian format.
{-# INLINE writeInt64be #-}
writeInt64be :: Int64 -> Write
writeInt64be = writeWord64be . fromIntegral

-- | Write an 'Int64' in little endian format.
{-# INLINE writeInt64le #-}
writeInt64le :: Int64 -> Write
writeInt64le = writeWord64le . fromIntegral


------------------------------------------------------------------------
-- Unaligned, integer size ops

-- | Write a single native machine 'Int'. The 'Int' is written in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Int'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this way
-- are not portable to different endian or integer sized machines, without
-- conversion.
--
{-# INLINE writeInthost #-}
writeInthost :: Int -> Write
writeInthost = writeStorable

-- | Write an 'Int16' in native host order and host endianness.
{-# INLINE writeInt16host #-}
writeInt16host :: Int16 -> Write
writeInt16host = writeStorable

-- | Write an 'Int32' in native host order and host endianness.
{-# INLINE writeInt32host #-}
writeInt32host :: Int32 -> Write
writeInt32host = writeStorable

-- | Write an 'Int64' in native host order and host endianness.
{-# INLINE writeInt64host #-}
writeInt64host :: Int64 -> Write
writeInt64host = writeStorable


