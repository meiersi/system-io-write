-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Testing all writes provided by this library.
module TestAll where

import Data.Bits
import Data.Char (toUpper, ord)

import Foreign

import Numeric (showHex)

import System.IO.Write
import System.IO.Write.Test
import System.IO.Write.Char.Utf8

import System.ByteOrder  -- from byteorder-1.0.3

import Test.Framework
import Test.Framework.Providers.QuickCheck2


main :: IO ()
main = Test.Framework.defaultMain $ return $ testAll

testAll :: Test
testAll = testGroup "system-io-write"
  [ testWord
  , testInt
  , testCharUtf8
  ]

------------------------------------------------------------------------------
-- Word and Int
------------------------------------------------------------------------------

testWord :: Test
testWord = testGroup "System.IO.Write.Word"
  [ testProperty "word8"      $ prop_bigEndian    word8

  , testProperty "word16BE"   $ prop_bigEndian    word16BE
  , testProperty "word32BE"   $ prop_bigEndian    word32BE
  , testProperty "word64BE"   $ prop_bigEndian    word64BE
                              
  , testProperty "word16LE"   $ prop_littleEndian word16LE
  , testProperty "word32LE"   $ prop_littleEndian word32LE
  , testProperty "word64LE"   $ prop_littleEndian word64LE
                              
  , testProperty "word16Host" $ prop_hostEndian   word16Host
  , testProperty "word32Host" $ prop_hostEndian   word32Host
  , testProperty "word64Host" $ prop_hostEndian   word64Host
  , testProperty "wordHost"   $ prop_hostEndian   wordHost
  ]

testInt :: Test
testInt = testGroup "System.IO.Write.Int"
  [ testProperty "int8"      $ prop_bigEndian    int8

  , testProperty "int16BE"   $ prop_bigEndian    int16BE
  , testProperty "int32BE"   $ prop_bigEndian    int32BE
  , testProperty "int64BE"   $ prop_bigEndian    int64BE
                              
  , testProperty "int16LE"   $ prop_littleEndian int16LE
  , testProperty "int32LE"   $ prop_littleEndian int32LE
  , testProperty "int64LE"   $ prop_littleEndian int64LE
                              
  , testProperty "int16Host" $ prop_hostEndian   int16Host
  , testProperty "int32Host" $ prop_hostEndian   int32Host
  , testProperty "int64Host" $ prop_hostEndian   int64Host
  , testProperty "intHost"   $ prop_hostEndian   intHost
  ]

prop_bigEndian :: (Storable a, Bits a, Integral a) => Write a -> a -> Bool
prop_bigEndian =
    cmpWriteErr f
  where
    f x = map (fromIntegral . (x `shiftR`) . (8*)) $ reverse [0..sizeOf x - 1]

prop_littleEndian :: (Storable a, Bits a, Integral a) => Write a -> a -> Bool
prop_littleEndian =
    cmpWriteErr f
  where
    f x = map (fromIntegral . (x `shiftR`) . (8*)) $ [0..sizeOf x - 1]

prop_hostEndian :: (Storable a, Bits a, Integral a) => Write a -> a -> Bool
prop_hostEndian = case byteOrder of
  LittleEndian -> prop_littleEndian
  BigEndian    -> prop_bigEndian
  _            -> error $ 
      "system-io-write: unsupported byteorder '" ++ show byteOrder ++ "'"


------------------------------------------------------------------------------
-- Utf-8 encoding
------------------------------------------------------------------------------

testCharUtf8 :: Test
testCharUtf8 = testGroup "System.IO.Write.Char.Utf8"
  [ testProperty "utf8" (cmpWriteErr (encodeUtf8 . return) utf8)

  , testProperty "utf8HexLower :: Word8"  $ prop_hexLower id (utf8HexLower :: Write Word8 )
  , testProperty "utf8HexLower :: Word16" $ prop_hexLower id (utf8HexLower :: Write Word16)
  , testProperty "utf8HexLower :: Word32" $ prop_hexLower id (utf8HexLower :: Write Word32)
  , testProperty "utf8HexLower :: Word64" $ prop_hexLower id (utf8HexLower :: Write Word64)

  , testProperty "utf8HexUpper :: Word8"  $ prop_hexUpper id (utf8HexUpper :: Write Word8 )
  , testProperty "utf8HexUpper :: Word16" $ prop_hexUpper id (utf8HexUpper :: Write Word16)
  , testProperty "utf8HexUpper :: Word32" $ prop_hexUpper id (utf8HexUpper :: Write Word32)
  , testProperty "utf8HexUpper :: Word64" $ prop_hexUpper id (utf8HexUpper :: Write Word64)

  , testProperty "utf8HexLowerNoLead :: Word8"  $ prop_hexLowerNoLead id (utf8HexLowerNoLead :: Write Word8 )
  , testProperty "utf8HexLowerNoLead :: Word16" $ prop_hexLowerNoLead id (utf8HexLowerNoLead :: Write Word16)
  , testProperty "utf8HexLowerNoLead :: Word32" $ prop_hexLowerNoLead id (utf8HexLowerNoLead :: Write Word32)
  , testProperty "utf8HexLowerNoLead :: Word64" $ prop_hexLowerNoLead id (utf8HexLowerNoLead :: Write Word64)

  , testProperty "utf8HexUpperNoLead :: Word8"  $ prop_hexUpperNoLead id (utf8HexUpperNoLead :: Write Word8 )
  , testProperty "utf8HexUpperNoLead :: Word16" $ prop_hexUpperNoLead id (utf8HexUpperNoLead :: Write Word16)
  , testProperty "utf8HexUpperNoLead :: Word32" $ prop_hexUpperNoLead id (utf8HexUpperNoLead :: Write Word32)
  , testProperty "utf8HexUpperNoLead :: Word64" $ prop_hexUpperNoLead id (utf8HexUpperNoLead :: Write Word64)

    -- we need an explicit conversion to the corresponding WordX type because
    -- showHex only works for positive numbers.
  , testProperty "utf8HexLower :: Int8"  $ prop_hexLower (fromIntegral :: Int8 -> Word8)   (utf8HexLower :: Write Int8 )
  , testProperty "utf8HexLower :: Int16" $ prop_hexLower (fromIntegral :: Int16 -> Word16) (utf8HexLower :: Write Int16)
  , testProperty "utf8HexLower :: Int32" $ prop_hexLower (fromIntegral :: Int32 -> Word32) (utf8HexLower :: Write Int32)
  , testProperty "utf8HexLower :: Int64" $ prop_hexLower (fromIntegral :: Int64 -> Word64) (utf8HexLower :: Write Int64)

  , testProperty "utf8HexUpper :: Int8"  $ prop_hexUpper (fromIntegral :: Int8 -> Word8)   (utf8HexUpper :: Write Int8 )
  , testProperty "utf8HexUpper :: Int16" $ prop_hexUpper (fromIntegral :: Int16 -> Word16) (utf8HexUpper :: Write Int16)
  , testProperty "utf8HexUpper :: Int32" $ prop_hexUpper (fromIntegral :: Int32 -> Word32) (utf8HexUpper :: Write Int32)
  , testProperty "utf8HexUpper :: Int64" $ prop_hexUpper (fromIntegral :: Int64 -> Word64) (utf8HexUpper :: Write Int64)

  , testProperty "utf8HexLowerNoLead :: Int8"  $ prop_hexLowerNoLead (fromIntegral :: Int8 -> Word8)   (utf8HexLowerNoLead :: Write Int8 )
  , testProperty "utf8HexLowerNoLead :: Int16" $ prop_hexLowerNoLead (fromIntegral :: Int16 -> Word16) (utf8HexLowerNoLead :: Write Int16)
  , testProperty "utf8HexLowerNoLead :: Int32" $ prop_hexLowerNoLead (fromIntegral :: Int32 -> Word32) (utf8HexLowerNoLead :: Write Int32)
  , testProperty "utf8HexLowerNoLead :: Int64" $ prop_hexLowerNoLead (fromIntegral :: Int64 -> Word64) (utf8HexLowerNoLead :: Write Int64)

  , testProperty "utf8HexUpperNoLead :: Int8"  $ prop_hexUpperNoLead (fromIntegral :: Int8 -> Word8)   (utf8HexUpperNoLead :: Write Int8 )
  , testProperty "utf8HexUpperNoLead :: Int16" $ prop_hexUpperNoLead (fromIntegral :: Int16 -> Word16) (utf8HexUpperNoLead :: Write Int16)
  , testProperty "utf8HexUpperNoLead :: Int32" $ prop_hexUpperNoLead (fromIntegral :: Int32 -> Word32) (utf8HexUpperNoLead :: Write Int32)
  , testProperty "utf8HexUpperNoLead :: Int64" $ prop_hexUpperNoLead (fromIntegral :: Int64 -> Word64) (utf8HexUpperNoLead :: Write Int64)
  ]


-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
--
-- Copied from 'utf8-string-0.3.6' to make tests self-contained. 
-- Copyright (c) 2007, Galois Inc. All rights reserved.
--
encodeUtf8 :: String -> [Word8]
encodeUtf8 = concatMap (map fromIntegral . go . ord)
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]


-- Hex encoding properties
--------------------------

prop_hexLower :: (Show a, Storable a, Integral b) => (a -> b) -> Write a -> a -> Bool
prop_hexLower = prop_hex id 

prop_hexUpper :: (Show a, Storable a, Integral b) => (a -> b) -> Write a -> a -> Bool
prop_hexUpper = prop_hex toUpper

prop_hexLowerNoLead :: (Show a, Integral b) => (a -> b) -> Write a -> a -> Bool
prop_hexLowerNoLead = prop_hexNoLead id

prop_hexUpperNoLead :: (Show a, Integral b) => (a -> b) -> Write a -> a -> Bool
prop_hexUpperNoLead = prop_hexNoLead toUpper

prop_hex :: (Show a, Storable a, Integral b) 
         => (Char -> Char) -> (a -> b) -> Write a -> a -> Bool
prop_hex convChar convArg =
    cmpWriteErr f
  where
    f x      = encodeUtf8 $ pad (2 * sizeOf x) $ map convChar $ showHex (convArg x) ""
    pad n cs = replicate (n - length cs) '0' ++ cs

prop_hexNoLead :: (Show a, Integral b) 
               => (Char -> Char) -> (a -> b) -> Write a -> a -> Bool
prop_hexNoLead convChar convArg =
    cmpWriteErr f
  where
    f x = encodeUtf8 $ map convChar $ showHex (convArg x) ""

