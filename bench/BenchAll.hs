{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmark all Encodings implemented directly.
module BenchAll where

import Prelude hiding (words)
import Foreign
import Criterion.Main

import Codec.Bounded.Encoding
import Codec.Bounded.Encoding.Bench


------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

nRepl :: Int
nRepl = 10000

{-# INLINE benchmark #-}
benchmark :: String -> Encoding Int -> Benchmark
benchmark name w = 
  bench (name ++" (" ++ show nRepl ++ ")") $ writeInts nRepl w

main :: IO ()
main = Criterion.Main.defaultMain 
  [ bgroup "Char"
    [ benchmark "utf8"             $ utf8             #. toEnum
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
      [ benchmark "8"    $ (utf8HexUpper :: Encoding Word8) #. fromIntegral
      , benchmark "16"   $ (utf8HexUpper :: Encoding Word16) #. fromIntegral
      , benchmark "32"   $ (utf8HexUpper :: Encoding Word32) #. fromIntegral
      , benchmark "64"   $ (utf8HexUpper :: Encoding Word64) #. fromIntegral
      , benchmark "Host" $ (utf8HexUpper :: Encoding Word) #. fromIntegral
      ]
    , bgroup "utf8HexUpperNoLead"
      [ benchmark "8"    $ (utf8HexUpperNoLead :: Encoding Word8) #. fromIntegral
      , benchmark "16"   $ (utf8HexUpperNoLead :: Encoding Word16) #. fromIntegral
      , benchmark "32"   $ (utf8HexUpperNoLead :: Encoding Word32) #. fromIntegral
      , benchmark "64"   $ (utf8HexUpperNoLead :: Encoding Word64) #. fromIntegral
      , benchmark "Host" $ (utf8HexUpperNoLead :: Encoding Word) #. fromIntegral
      ]
    , bgroup "utf8HexLower"
      [ benchmark "8"    $ (utf8HexLower :: Encoding Word8) #. fromIntegral
      , benchmark "16"   $ (utf8HexLower :: Encoding Word16) #. fromIntegral
      , benchmark "32"   $ (utf8HexLower :: Encoding Word32) #. fromIntegral
      , benchmark "64"   $ (utf8HexLower :: Encoding Word64) #. fromIntegral
      , benchmark "Host" $ (utf8HexLower :: Encoding Word) #. fromIntegral
      ]
    , bgroup "utf8HexLowerNoLead"
      [ benchmark "8"    $ (utf8HexLowerNoLead :: Encoding Word8) #. fromIntegral
      , benchmark "16"   $ (utf8HexLowerNoLead :: Encoding Word16) #. fromIntegral
      , benchmark "32"   $ (utf8HexLowerNoLead :: Encoding Word32) #. fromIntegral
      , benchmark "64"   $ (utf8HexLowerNoLead :: Encoding Word64) #. fromIntegral
      , benchmark "Host" $ (utf8HexLowerNoLead :: Encoding Word) #. fromIntegral
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
      [ benchmark "8"    $ (utf8HexUpper :: Encoding Int8) #. fromIntegral
      , benchmark "16"   $ (utf8HexUpper :: Encoding Int16) #. fromIntegral
      , benchmark "32"   $ (utf8HexUpper :: Encoding Int32) #. fromIntegral
      , benchmark "64"   $ (utf8HexUpper :: Encoding Int64) #. fromIntegral
      , benchmark "Host" $ (utf8HexUpper :: Encoding Int)
      ]
    , bgroup "utf8HexUpperNoLead"
      [ benchmark "8"    $ (utf8HexUpperNoLead :: Encoding Int8) #. fromIntegral
      , benchmark "16"   $ (utf8HexUpperNoLead :: Encoding Int16) #. fromIntegral
      , benchmark "32"   $ (utf8HexUpperNoLead :: Encoding Int32) #. fromIntegral
      , benchmark "64"   $ (utf8HexUpperNoLead :: Encoding Int64) #. fromIntegral
      , benchmark "Host" $ (utf8HexUpperNoLead :: Encoding Int)
      ]
    , bgroup "utf8HexLower"
      [ benchmark "8"    $ (utf8HexLower :: Encoding Int8) #. fromIntegral
      , benchmark "16"   $ (utf8HexLower :: Encoding Int16) #. fromIntegral
      , benchmark "32"   $ (utf8HexLower :: Encoding Int32) #. fromIntegral
      , benchmark "64"   $ (utf8HexLower :: Encoding Int64) #. fromIntegral
      , benchmark "Host" $ (utf8HexLower :: Encoding Int)
      ]
    , bgroup "utf8HexLowerNoLead"
      [ benchmark "8"    $ (utf8HexLowerNoLead :: Encoding Int8) #. fromIntegral
      , benchmark "16"   $ (utf8HexLowerNoLead :: Encoding Int16) #. fromIntegral
      , benchmark "32"   $ (utf8HexLowerNoLead :: Encoding Int32) #. fromIntegral
      , benchmark "64"   $ (utf8HexLowerNoLead :: Encoding Int64) #. fromIntegral
      , benchmark "Host" $ (utf8HexLowerNoLead :: Encoding Int)
      ]
    ]
  ]

