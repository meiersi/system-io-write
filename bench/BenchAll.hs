{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmark all Writes implemented directly.
module BenchAll where

import Prelude hiding (words)
import Foreign
import Criterion.Main

import System.IO.Write
import System.IO.Write.Bench


------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

nRepl :: Int
nRepl = 1000

-- Inline all these lists to ensure a low memory residency. Otherwise, 
-- comparing benchmarks becomes harder as GC takes longer the more benchmarks
-- have been run.

{-# INLINE word8s #-}
word8s :: [Word8]
word8s = map fromIntegral $ [(0::Word)..]

{-# INLINE word16s #-}
word16s :: [Word16]
word16s = map fromIntegral $ [(0::Word)..]

{-# INLINE word32s #-}
word32s :: [Word32]
word32s = map fromIntegral $ [(0::Word)..]

{-# INLINE word64s #-}
word64s :: [Word64]
word64s = map fromIntegral $ [(0::Word)..]

{-# INLINE words #-}
words :: [Word]
words = map fromIntegral $ [(0::Word)..]

{-# INLINE int8s #-}
int8s :: [Int8]
int8s = map fromIntegral $ [(0::Int)..]

{-# INLINE int16s #-}
int16s :: [Int16]
int16s = map fromIntegral $ [(0::Int)..]

{-# INLINE int32s #-}
int32s :: [Int32]
int32s = map fromIntegral $ [(0::Int)..]

{-# INLINE int64s #-}
int64s :: [Int64]
int64s = map fromIntegral $ [(0::Int)..]

{-# INLINE ints #-}
ints :: [Int]
ints = map fromIntegral $ [(0::Int)..]

{-# INLINE chars #-}
chars :: [Char]
chars = map toEnum $ [(0::Int)..]

{-# INLINE benchmark #-}
benchmark :: String -> Write a -> [a] -> Benchmark
benchmark name w xs = 
  bench (name ++" (" ++ show nRepl ++ ")") $ writeList nRepl w xs

main :: IO ()
main = Criterion.Main.defaultMain 
  [ bgroup "Char"
    [ benchmark "utf8"             utf8               chars
      -- cannot benchmark 'ascii' on this data, as it would error
    , benchmark "asciiUnsafe"      unsafeAscii        chars
    , benchmark "asciiDrop"        asciiDrop          chars
    , benchmark "asciiReplace ' '" (asciiReplace ' ') chars
    ]
  , bgroup "Word"
    [ bgroup "HostEndian"
      [ benchmark "8"    word8      word8s
      , benchmark "16"   word16Host word16s
      , benchmark "32"   word32Host word32s
      , benchmark "64"   word64Host word64s
      , benchmark "Host" wordHost   words
      ]
    , bgroup "BigEndian"
      [ benchmark "16"   word16BE word16s
      , benchmark "32"   word32BE word32s
      , benchmark "64"   word64BE word64s
      ]
    , bgroup "LittleEndian"
      [ benchmark "16"   word16LE word16s
      , benchmark "32"   word32LE word32s
      , benchmark "64"   word64LE word64s
      ]
    , bgroup "utf8HexUpper"
      [ benchmark "8"    utf8HexUpper word8s
      , benchmark "16"   utf8HexUpper word16s
      , benchmark "32"   utf8HexUpper word32s
      , benchmark "64"   utf8HexUpper word64s
      , benchmark "Host" utf8HexUpper words
      ]
    , bgroup "utf8HexUpperNoLead"
      [ benchmark "8"    utf8HexUpperNoLead word8s
      , benchmark "16"   utf8HexUpperNoLead word16s
      , benchmark "32"   utf8HexUpperNoLead word32s
      , benchmark "64"   utf8HexUpperNoLead word64s
      , benchmark "Host" utf8HexUpperNoLead words
      ]
    , bgroup "utf8HexLower"
      [ benchmark "8"    utf8HexLower word8s
      , benchmark "16"   utf8HexLower word16s
      , benchmark "32"   utf8HexLower word32s
      , benchmark "64"   utf8HexLower word64s
      , benchmark "Host" utf8HexLower words
      ]
    , bgroup "utf8HexLowerNoLead"
      [ benchmark "8"    utf8HexLowerNoLead word8s
      , benchmark "16"   utf8HexLowerNoLead word16s
      , benchmark "32"   utf8HexLowerNoLead word32s
      , benchmark "64"   utf8HexLowerNoLead word64s
      , benchmark "Host" utf8HexLowerNoLead words
      ]
    ]
  , bgroup "Int"
    [ bgroup "HostEndian"
      [ benchmark "8"    int8      int8s
      , benchmark "16"   int16Host int16s
      , benchmark "32"   int32Host int32s
      , benchmark "64"   int64Host int64s
      , benchmark "Host" intHost   ints
      ]
    , bgroup "BigEndian"
      [ benchmark "16"   int16BE int16s
      , benchmark "32"   int32BE int32s
      , benchmark "64"   int64BE int64s
      ]
    , bgroup "LittleEndian"
      [ benchmark "16"   int16LE int16s
      , benchmark "32"   int32LE int32s
      , benchmark "64"   int64LE int64s
      ]
    , bgroup "utf8HexUpper"
      [ benchmark "8"    utf8HexUpper int8s
      , benchmark "16"   utf8HexUpper int16s
      , benchmark "32"   utf8HexUpper int32s
      , benchmark "64"   utf8HexUpper int64s
      , benchmark "Host" utf8HexUpper ints
      ]
    , bgroup "utf8HexUpperNoLead"
      [ benchmark "8"    utf8HexUpperNoLead int8s
      , benchmark "16"   utf8HexUpperNoLead int16s
      , benchmark "32"   utf8HexUpperNoLead int32s
      , benchmark "64"   utf8HexUpperNoLead int64s
      , benchmark "Host" utf8HexUpperNoLead ints
      ]
    , bgroup "utf8HexLower"
      [ benchmark "8"    utf8HexLower int8s
      , benchmark "16"   utf8HexLower int16s
      , benchmark "32"   utf8HexLower int32s
      , benchmark "64"   utf8HexLower int64s
      , benchmark "Host" utf8HexLower ints
      ]
    , bgroup "utf8HexLowerNoLead"
      [ benchmark "8"    utf8HexLowerNoLead int8s
      , benchmark "16"   utf8HexLowerNoLead int16s
      , benchmark "32"   utf8HexLowerNoLead int32s
      , benchmark "64"   utf8HexLowerNoLead int64s
      , benchmark "Host" utf8HexLowerNoLead ints
      ]
    ]
  ]

