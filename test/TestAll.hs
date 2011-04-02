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

import           System.IO.Write
import           System.IO.Write.Test
import qualified System.IO.Write.Char.Utf8 as Utf8

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
  [ testProperty "word8"      $ prop_bigEndian    writeWord8

  , testProperty "word16BE"   $ prop_bigEndian    writeWord16be
  , testProperty "word32BE"   $ prop_bigEndian    writeWord32be
  , testProperty "word64BE"   $ prop_bigEndian    writeWord64be
                              
  , testProperty "word16LE"   $ prop_littleEndian writeWord16le
  , testProperty "word32LE"   $ prop_littleEndian writeWord32le
  , testProperty "word64LE"   $ prop_littleEndian writeWord64le
                              
  , testProperty "word16Host" $ prop_hostEndian   writeWord16host
  , testProperty "word32Host" $ prop_hostEndian   writeWord32host
  , testProperty "word64Host" $ prop_hostEndian   writeWord64host
  , testProperty "wordHost"   $ prop_hostEndian   writeWordhost
  ]

testInt :: Test
testInt = testGroup "System.IO.Write.Int"
  [ testProperty "int8"      $ prop_bigEndian    writeInt8

  , testProperty "int16BE"   $ prop_bigEndian    writeInt16be
  , testProperty "int32BE"   $ prop_bigEndian    writeInt32be
  , testProperty "int64BE"   $ prop_bigEndian    writeInt64be
                              
  , testProperty "int16LE"   $ prop_littleEndian writeInt16le
  , testProperty "int32LE"   $ prop_littleEndian writeInt32le
  , testProperty "int64LE"   $ prop_littleEndian writeInt64le
                              
  , testProperty "int16Host" $ prop_hostEndian   writeInt16host
  , testProperty "int32Host" $ prop_hostEndian   writeInt32host
  , testProperty "int64Host" $ prop_hostEndian   writeInt64host
  , testProperty "intHost"   $ prop_hostEndian   writeInthost
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

-- | TODO: Use conditional compilation to select correct endianness.
prop_hostEndian :: (Storable a, Bits a, Integral a) => Write a -> a -> Bool
prop_hostEndian = prop_littleEndian


------------------------------------------------------------------------------
-- Utf-8 encoding
------------------------------------------------------------------------------

testCharUtf8 :: Test
testCharUtf8 = testGroup "System.IO.Write.Char.Utf8"
  [ testProperty "char" (cmpWriteErr (encodeUtf8 . return) Utf8.writeChar)

  , testProperty "hex :: Word8"  $ prop_base16Lower (Utf8.base16Lower :: Write Word8 )
  , testProperty "hex :: Word16" $ prop_base16Lower (Utf8.base16Lower :: Write Word16)
  , testProperty "hex :: Word32" $ prop_base16Lower (Utf8.base16Lower :: Write Word32)
  , testProperty "hex :: Word64" $ prop_base16Lower (Utf8.base16Lower :: Write Word64)

  , testProperty "hexUpper :: Word8"  $ prop_base16Upper (Utf8.base16Upper :: Write Word8 )
  , testProperty "hexUpper :: Word16" $ prop_base16Upper (Utf8.base16Upper :: Write Word16)
  , testProperty "hexUpper :: Word32" $ prop_base16Upper (Utf8.base16Upper :: Write Word32)
  , testProperty "hexUpper :: Word64" $ prop_base16Upper (Utf8.base16Upper :: Write Word64)

  , testProperty "hexNoLead :: Word8"  $ prop_base16LowerNoLead (Utf8.base16LowerNoLead :: Write Word8 )
  , testProperty "hexNoLead :: Word16" $ prop_base16LowerNoLead (Utf8.base16LowerNoLead :: Write Word16)
  , testProperty "hexNoLead :: Word32" $ prop_base16LowerNoLead (Utf8.base16LowerNoLead :: Write Word32)
  , testProperty "hexNoLead :: Word64" $ prop_base16LowerNoLead (Utf8.base16LowerNoLead :: Write Word64)

  , testProperty "hexUpperNoLead :: Word8"  $ prop_base16UpperNoLead (Utf8.base16UpperNoLead :: Write Word8 )
  , testProperty "hexUpperNoLead :: Word16" $ prop_base16UpperNoLead (Utf8.base16UpperNoLead :: Write Word16)
  , testProperty "hexUpperNoLead :: Word32" $ prop_base16UpperNoLead (Utf8.base16UpperNoLead :: Write Word32)
  , testProperty "hexUpperNoLead :: Word64" $ prop_base16UpperNoLead (Utf8.base16UpperNoLead :: Write Word64)
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

prop_base16LowerNoLead :: Integral a => Write a -> a -> Bool
prop_base16LowerNoLead = prop_base16NoLead id

prop_base16UpperNoLead :: Integral a => Write a -> a -> Bool
prop_base16UpperNoLead = prop_base16NoLead toUpper

prop_base16NoLead :: Integral a => (Char -> Char) -> Write a -> a -> Bool
prop_base16NoLead conv =
    cmpWriteErr f
  where
    f x = encodeUtf8 $ map conv $ showHex x ""

prop_base16Lower :: (Storable a, Integral a) => Write a -> a -> Bool
prop_base16Lower = prop_base16 id

prop_base16Upper :: (Storable a, Integral a) => Write a -> a -> Bool
prop_base16Upper = prop_base16 toUpper

prop_base16 :: (Storable a, Integral a) => (Char -> Char) -> Write a -> a -> Bool
prop_base16 conv =
    cmpWriteErr f
  where
    f x      = encodeUtf8 $ pad (2 * sizeOf x) $ map conv $ showHex x ""
    pad n cs = replicate (n - length cs) '0' ++ cs


