{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Support for benchmarking encodings. See the file 'bench/BenchAll.hs'
-- distributed with this library for example benchmarks.
--
-- FIXME: Inline a simple example.
--
module Codec.Bounded.Encoding.Bench (
    writeInts
  , writeReplicated
  , writeList
  ) where

import Codec.Bounded.Encoding.Internal

import Foreign

-- | Repeatedly execute an 'Encoding' on a fixed value. The output is written
-- consecutively into a single array to incorporate to-memory-bandwidth in the
-- measurement.
{-# INLINE writeReplicated #-}
writeReplicated :: Int     -- ^ Number of repetitions
                -> Encoding a -- ^ 'Encoding' to execute
                -> a       -- ^ Value to encode
                -> IO ()   -- ^ 'IO' action to benchmark
writeReplicated n0 w x 
  | n0 <= 0   = return ()
  | otherwise = do
      fpbuf <- mallocForeignPtrBytes (n0 * getBound w)
      withForeignPtr fpbuf (loop n0) >> return ()
  where
    loop 0 !op = return op
    loop n !op = runEncoding w x op >>= loop (n - 1)

-- | Execute an 'Encoding' on each value of bounded-length prefix of a list. The
-- output is written consecutively into a single array to incorporate
-- to-memory-bandwidth in the measurement.
{-# INLINE writeInts #-}
writeInts :: Int       -- ^ Maximal 'Int' to write
          -> Encoding Int -- ^ 'Encoding' to execute
          -> IO ()     -- ^ 'IO' action to benchmark
writeInts n0 w
  | n0 <= 0   = return ()
  | otherwise = do
      fpbuf <- mallocForeignPtrBytes (n0 * getBound w)
      withForeignPtr fpbuf (loop n0) >> return ()
  where
    loop !n !op
      | n <= 0    = return op
      | otherwise = runEncoding w n op >>= loop (n - 1)


-- | Execute an 'Encoding' on each value of bounded-length prefix of a list. The
-- output is written consecutively into a single array to incorporate
-- to-memory-bandwidth in the measurement.
{-# INLINE writeList #-}
writeList :: Int     -- ^ Maximal length of prefix
          -> Encoding a -- ^ 'Encoding' to execute
          -> [a]     -- ^ Values to encode
          -> IO ()   -- ^ 'IO' action to benchmark
writeList n0 w xs0
  | n0 <= 0   = return ()
  | otherwise = do
      fpbuf <- mallocForeignPtrBytes (n0 * getBound w)
      withForeignPtr fpbuf (loop n0 xs0) >> return ()
  where
    loop !n xs !op
      | n <= 0    = return op
      | otherwise = case xs of
          []      -> return op
          (x:xs') -> runEncoding w x op >>= loop (n - 1) xs'

