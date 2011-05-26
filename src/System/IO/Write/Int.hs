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
-- 'Write's for encoding bounded-size signed integers using big-endian,
-- little-endian, and host-endian encodings.
--
module System.IO.Write.Int
    ( 

    -- * Endianness and order independent writes
      int8

    -- * Big-endian writes
    , int16BE
    , int32BE
    , int64BE

    -- * Little-endian writes
    , int16LE
    , int32LE
    , int64LE

    -- * Host-endian writes
    , intHost  
    , int16Host
    , int32Host
    , int64Host

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
{-# INLINE int8 #-}
int8 :: Write Int8
int8 = comapWrite fromIntegral word8

-- | Write an 'Int16' in big endian format.
{-# INLINE int16BE #-}
int16BE :: Write Int16
int16BE = comapWrite fromIntegral word16BE

-- | Write an 'Int16' in little endian format.
{-# INLINE int16LE #-}
int16LE :: Write Int16
int16LE = comapWrite fromIntegral word16LE

-- | Write an 'Int32' in big endian format.
{-# INLINE int32BE #-}
int32BE :: Write Int32
int32BE = comapWrite fromIntegral word32BE

-- | Write an 'Int32' in little endian format.
{-# INLINE int32LE #-}
int32LE :: Write Int32
int32LE = comapWrite fromIntegral word32LE

-- | Write an 'Int64' in big endian format.
{-# INLINE int64BE #-}
int64BE :: Write Int64
int64BE = comapWrite fromIntegral word64BE

-- | Write an 'Int64' in little endian format.
{-# INLINE int64LE #-}
int64LE :: Write Int64
int64LE = comapWrite fromIntegral word64LE


------------------------------------------------------------------------
-- Unaligned, integer size ops

-- | Write a single native machine 'Int'. The 'Int' is written in host order,
-- host endian form, for the machine you're on. On a 64 bit machine the 'Int'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values written this way
-- are not portable to different endian or integer sized machines, without
-- conversion.
--
{-# INLINE intHost #-}
intHost :: Write Int
intHost = writeStorable

-- | Write an 'Int16' in native host order and host endianness.
{-# INLINE int16Host #-}
int16Host :: Write Int16
int16Host = writeStorable

-- | Write an 'Int32' in native host order and host endianness.
{-# INLINE int32Host #-}
int32Host :: Write Int32
int32Host = writeStorable

-- | Write an 'Int64' in native host order and host endianness.
{-# INLINE int64Host #-}
int64Host :: Write Int64
int64Host = writeStorable


