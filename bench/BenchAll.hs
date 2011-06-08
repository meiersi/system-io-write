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
nRepl = 10000

{-# INLINE benchmark #-}
benchmark :: String -> Write Int -> Benchmark
benchmark name w = 
  bench (name ++" (" ++ show nRepl ++ ")") $ writeInts nRepl w

main :: IO ()
main = Criterion.Main.defaultMain 
  [ bgroup "Char"
    [ benchmark "utf8"             $ utf8             #. toEnum
    , benchmark "asciiUnsafe"      $ unsafeAscii      #. toEnum
    , benchmark "asciiDrop"        $ asciiDrop        #. toEnum
    , benchmark "asciiReplace ' '" $ asciiReplace ' ' #. toEnum
    ]
  , bgroup "Word"
    [ bgroup "HostEndian"
      [ benchmark "8"    $ word8      #. fromIntegral
      , benchmark "16"   $ word16Host #. fromIntegral
      , benchmark "32"   $ word32Host #. fromIntegral
      , benchmark "64"   $ word64Host #. fromIntegral
      , benchmark "Host" $ wordHost   #. fromIntegral
      ]
    , bgroup "BigEndian"
      [ benchmark "16"   $ word16BE #. fromIntegral
      , benchmark "32"   $ word32BE #. fromIntegral
      , benchmark "64"   $ word64BE #. fromIntegral
      ]
    , bgroup "LittleEndian"
      [ benchmark "16"   $ word16LE #. fromIntegral
      , benchmark "32"   $ word32LE #. fromIntegral
      , benchmark "64"   $ word64LE #. fromIntegral
      ]
    , bgroup "utf8HexUpper"
      [ benchmark "8"    $ (utf8HexUpper :: Write Word8) #. fromIntegral
      , benchmark "16"   $ (utf8HexUpper :: Write Word16) #. fromIntegral
      , benchmark "32"   $ (utf8HexUpper :: Write Word32) #. fromIntegral
      , benchmark "64"   $ (utf8HexUpper :: Write Word64) #. fromIntegral
      , benchmark "Host" $ (utf8HexUpper :: Write Word) #. fromIntegral
      ]
    , bgroup "utf8HexUpperNoLead"
      [ benchmark "8"    $ (utf8HexUpperNoLead :: Write Word8) #. fromIntegral
      , benchmark "16"   $ (utf8HexUpperNoLead :: Write Word16) #. fromIntegral
      , benchmark "32"   $ (utf8HexUpperNoLead :: Write Word32) #. fromIntegral
      , benchmark "64"   $ (utf8HexUpperNoLead :: Write Word64) #. fromIntegral
      , benchmark "Host" $ (utf8HexUpperNoLead :: Write Word) #. fromIntegral
      ]
    , bgroup "utf8HexLower"
      [ benchmark "8"    $ (utf8HexLower :: Write Word8) #. fromIntegral
      , benchmark "16"   $ (utf8HexLower :: Write Word16) #. fromIntegral
      , benchmark "32"   $ (utf8HexLower :: Write Word32) #. fromIntegral
      , benchmark "64"   $ (utf8HexLower :: Write Word64) #. fromIntegral
      , benchmark "Host" $ (utf8HexLower :: Write Word) #. fromIntegral
      ]
    , bgroup "utf8HexLowerNoLead"
      [ benchmark "8"    $ (utf8HexLowerNoLead :: Write Word8) #. fromIntegral
      , benchmark "16"   $ (utf8HexLowerNoLead :: Write Word16) #. fromIntegral
      , benchmark "32"   $ (utf8HexLowerNoLead :: Write Word32) #. fromIntegral
      , benchmark "64"   $ (utf8HexLowerNoLead :: Write Word64) #. fromIntegral
      , benchmark "Host" $ (utf8HexLowerNoLead :: Write Word) #. fromIntegral
      ]
    ]
  , bgroup "Int"
    [ bgroup "HostEndian"
      [ benchmark "8"    $ int8      #. fromIntegral
      , benchmark "16"   $ int16Host #. fromIntegral
      , benchmark "32"   $ int32Host #. fromIntegral
      , benchmark "64"   $ int64Host #. fromIntegral
      , benchmark "Host" $ intHost
      ]
    , bgroup "BigEndian"
      [ benchmark "16"   $ int16BE #. fromIntegral
      , benchmark "32"   $ int32BE #. fromIntegral
      , benchmark "64"   $ int64BE #. fromIntegral
      ]
    , bgroup "LittleEndian"
      [ benchmark "16"   $ int16LE #. fromIntegral
      , benchmark "32"   $ int32LE #. fromIntegral
      , benchmark "64"   $ int64LE #. fromIntegral
      ]
    , bgroup "utf8HexUpper"
      [ benchmark "8"    $ (utf8HexUpper :: Write Int8) #. fromIntegral
      , benchmark "16"   $ (utf8HexUpper :: Write Int16) #. fromIntegral
      , benchmark "32"   $ (utf8HexUpper :: Write Int32) #. fromIntegral
      , benchmark "64"   $ (utf8HexUpper :: Write Int64) #. fromIntegral
      , benchmark "Host" $ (utf8HexUpper :: Write Int)
      ]
    , bgroup "utf8HexUpperNoLead"
      [ benchmark "8"    $ (utf8HexUpperNoLead :: Write Int8) #. fromIntegral
      , benchmark "16"   $ (utf8HexUpperNoLead :: Write Int16) #. fromIntegral
      , benchmark "32"   $ (utf8HexUpperNoLead :: Write Int32) #. fromIntegral
      , benchmark "64"   $ (utf8HexUpperNoLead :: Write Int64) #. fromIntegral
      , benchmark "Host" $ (utf8HexUpperNoLead :: Write Int)
      ]
    , bgroup "utf8HexLower"
      [ benchmark "8"    $ (utf8HexLower :: Write Int8) #. fromIntegral
      , benchmark "16"   $ (utf8HexLower :: Write Int16) #. fromIntegral
      , benchmark "32"   $ (utf8HexLower :: Write Int32) #. fromIntegral
      , benchmark "64"   $ (utf8HexLower :: Write Int64) #. fromIntegral
      , benchmark "Host" $ (utf8HexLower :: Write Int)
      ]
    , bgroup "utf8HexLowerNoLead"
      [ benchmark "8"    $ (utf8HexLowerNoLead :: Write Int8) #. fromIntegral
      , benchmark "16"   $ (utf8HexLowerNoLead :: Write Int16) #. fromIntegral
      , benchmark "32"   $ (utf8HexLowerNoLead :: Write Int32) #. fromIntegral
      , benchmark "64"   $ (utf8HexLowerNoLead :: Write Int64) #. fromIntegral
      , benchmark "Host" $ (utf8HexLowerNoLead :: Write Int)
      ]
    ]
  ]

