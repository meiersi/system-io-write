{-# LANGUAGE CPP, ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- 'Write's for the ASCII encoding of characters 
-- (cf. <http://tools.ietf.org/html/rfc20>).
--
-- They are intended for constructing output in formats that explicitly
-- restrict the character encoding to 7-bit ASCII encoded characters. In all
-- other cases, a lossless Unicode encoding (e.g., "System.IO.Write.Char.Utf8")
-- is a better choice.
--
module System.IO.Write.Char.Ascii
    ( 
      -- * ASCII encoding of single characters
      ascii
    , asciiReplace 
    , asciiDrop
    , unsafeAscii

      -- * Hexadecimal encoding using ASCII encoded characters
    , AsciiHexWritable(..)
    ) where

import Foreign
import Data.Char

import System.IO.Write.Internal
import System.IO.Write.Internal.Base16
import System.IO.Write.Word

-- | Write a 'Char' with a Unicode codepoint less than 128 using the ASCII
-- encoding. This function is unsafe because, for codepoints greater or equal to
-- 128, no guarantee on the written byte is given except that exactly one byte
-- is written.
--
{-# INLINE unsafeAscii #-}
unsafeAscii :: Write Char
unsafeAscii = word8 #. fromIntegral #. ord

-- | Write a 'Char' with a Unicode codepoint below 128 using the ASCII
-- encoding. If the codepoint is greater or equal to 128, an error is thrown.
--
{-# INLINE ascii #-}
ascii :: Write Char
ascii =
    writeIf (< '\128') unsafeAscii (writeNothing #. err)
  where
    err c = error $ "ascii: cannot encode `" ++ [c] ++ "' in ASCII"

-- | Write a 'Char' with a Unicode codepoint below 128 using the ASCII
-- encoding. If the codepoint is greater or equal to 128, the replacement
-- character is written. The replacement character must have a codepoint below
-- 128, as otherwise an error is thrown.
--
{-# INLINE asciiReplace #-}
asciiReplace :: Char       -- ^ Replacement character for codepoints greater or equal to 128
             -> Write Char
asciiReplace replacement = 
    writeIf (< '\128') unsafeAscii (ascii #. const replacement) 

-- | Write a 'Char' with a Unicode codepoint below 128 using the ASCII encoding.
-- If the codepoint is greater or equal to 128, nothing is written.
--
-- Note that dropping characters during encoding is often dangerous, as it may
-- lead to unintended interpretations of the written output. Either use
-- 'asciiReplace' to insert a character whose insertion is safe in your context
-- or, if possible, use a lossless encoding like UTF-8.
--
{-# INLINE asciiDrop #-}
asciiDrop :: Write Char
asciiDrop = writeIf (< '\128') unsafeAscii writeNothing


------------------------------------------------------------------------------
-- Hexadecimal Encoding
------------------------------------------------------------------------------

-- | Values that support a hexadecimal encoding with ASCII encoded characters.
class AsciiHexWritable a where
    -- | Fixed-width hexadecimal encoding with lower-case characters.
    --
    -- > showWrite asciiHexLower (26 :: Word16) = "001a"
    --
    asciiHexLower       :: Write a
    -- | Fixed-width hexadecimal encoding with upper-case characters.
    --
    -- > showWrite asciiHexUpper (26 :: Word16) = "001A"
    --
    asciiHexUpper       :: Write a
    -- | Hexadecimal encoding with upper-case characters and no leading zeros.
    --
    -- > showWrite asciiHexLowerNoLead (26 :: Word16) = "1a"
    --
    asciiHexLowerNoLead :: Write a
    -- | Hexadecimal encoding with lower-case characters and no leading zeros.
    --
    -- > showWrite asciiHexUpperNoLead (26 :: Word16) = "1A"
    --
    asciiHexUpperNoLead :: Write a
#if WORD_SIZE_IN_BITS < 64
instance AsciiHexWritable Word where
    {-# INLINE asciiHexLower #-}
    {-# INLINE asciiHexUpper #-}
    {-# INLINE asciiHexLowerNoLead #-}
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexLower       = word32Hex lowerTable #. fromIntegral
    asciiHexUpper       = word32Hex upperTable #. fromIntegral
    asciiHexLowerNoLead = asciiHexNoLead lowerTable
    asciiHexUpperNoLead = asciiHexNoLead upperTable

instance AsciiHexWritable Int where
    {-# INLINE asciiHexLower #-}
    {-# INLINE asciiHexUpper #-}
    {-# INLINE asciiHexLowerNoLead #-}
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexLower       = word32Hex lowerTable #. fromIntegral
    asciiHexUpper       = word32Hex upperTable #. fromIntegral
    asciiHexLowerNoLead = asciiHexNoLead lowerTable
    asciiHexUpperNoLead = asciiHexNoLead upperTable
#else
instance AsciiHexWritable Word where
    {-# INLINE asciiHexLower #-}
    {-# INLINE asciiHexUpper #-}
    {-# INLINE asciiHexLowerNoLead #-}
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexLower       = word64Hex lowerTable #. fromIntegral
    asciiHexUpper       = word64Hex upperTable #. fromIntegral
    asciiHexLowerNoLead = asciiHexNoLead lowerTable
    asciiHexUpperNoLead = asciiHexNoLead upperTable

instance AsciiHexWritable Int where
    {-# INLINE asciiHexLower #-}
    {-# INLINE asciiHexUpper #-}
    {-# INLINE asciiHexLowerNoLead #-}
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexLower       = word64Hex lowerTable #. fromIntegral
    asciiHexUpper       = word64Hex upperTable #. fromIntegral
    asciiHexLowerNoLead = asciiHexNoLead lowerTable
    asciiHexUpperNoLead = asciiHexNoLead upperTable
#endif

instance AsciiHexWritable Word8 where
    {-# INLINE asciiHexLower #-}
    {-# INLINE asciiHexUpper #-}
    {-# INLINE asciiHexLowerNoLead #-}
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexLower       = word8Hex lowerTable
    asciiHexUpper       = word8Hex upperTable
    asciiHexLowerNoLead = word8HexNoLead lowerTable
    asciiHexUpperNoLead = word8HexNoLead upperTable

instance AsciiHexWritable Word16 where
    {-# INLINE asciiHexLower #-}
    {-# INLINE asciiHexUpper #-}
    {-# INLINE asciiHexLowerNoLead #-}
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexLower       = word16Hex lowerTable
    asciiHexUpper       = word16Hex upperTable
    asciiHexLowerNoLead = asciiHexNoLead lowerTable
    asciiHexUpperNoLead = asciiHexNoLead upperTable

instance AsciiHexWritable Word32 where
    {-# INLINE asciiHexLower #-}
    {-# INLINE asciiHexUpper #-}
    {-# INLINE asciiHexLowerNoLead #-}
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexLower       = word32Hex lowerTable
    asciiHexUpper       = word32Hex upperTable
    asciiHexLowerNoLead = asciiHexNoLead lowerTable
    asciiHexUpperNoLead = asciiHexNoLead upperTable

instance AsciiHexWritable Word64 where
    {-# INLINE asciiHexLower #-}
    {-# INLINE asciiHexUpper #-}
    {-# INLINE asciiHexLowerNoLead #-}
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexLower       = word64Hex lowerTable
    asciiHexUpper       = word64Hex upperTable
    asciiHexLowerNoLead = asciiHexNoLead lowerTable
    asciiHexUpperNoLead = asciiHexNoLead upperTable

instance AsciiHexWritable Int8 where
    {-# INLINE asciiHexLower #-}
    {-# INLINE asciiHexUpper #-}
    {-# INLINE asciiHexLowerNoLead #-}
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexLower       = word8Hex lowerTable #. fromIntegral
    asciiHexUpper       = word8Hex upperTable #. fromIntegral
    asciiHexLowerNoLead = word8HexNoLead lowerTable #. fromIntegral
    asciiHexUpperNoLead = word8HexNoLead upperTable #. fromIntegral

instance AsciiHexWritable Int16 where
    {-# INLINE asciiHexLower #-}
    {-# INLINE asciiHexUpper #-}
    {-# INLINE asciiHexLowerNoLead #-}
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexLower       = word16Hex lowerTable #. fromIntegral
    asciiHexUpper       = word16Hex upperTable #. fromIntegral
    asciiHexLowerNoLead = asciiHexNoLead lowerTable 
    asciiHexUpperNoLead = asciiHexNoLead upperTable

instance AsciiHexWritable Int32 where
    {-# INLINE asciiHexLower #-}
    {-# INLINE asciiHexUpper #-}
    {-# INLINE asciiHexLowerNoLead #-}
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexLower       = word32Hex lowerTable #. fromIntegral
    asciiHexUpper       = word32Hex upperTable #. fromIntegral
    asciiHexLowerNoLead = asciiHexNoLead lowerTable
    asciiHexUpperNoLead = asciiHexNoLead upperTable

instance AsciiHexWritable Int64 where
    {-# INLINE asciiHexLower #-}
    {-# INLINE asciiHexUpper #-}
    {-# INLINE asciiHexLowerNoLead #-}
    {-# INLINE asciiHexUpperNoLead #-}
    asciiHexLower       = word64Hex lowerTable #. fromIntegral
    asciiHexUpper       = word64Hex upperTable #. fromIntegral
    asciiHexLowerNoLead = asciiHexNoLead lowerTable
    asciiHexUpperNoLead = asciiHexNoLead upperTable

{-# INLINE word8Hex #-}
word8Hex :: EncodingTable -> Write Word8
word8Hex table = 
    exactWrite 2 $ \x op -> poke (castPtr op) =<< encode8_as_16h table x

{-# INLINE word16Hex #-}
word16Hex :: EncodingTable -> Write Word16
word16Hex table = 
    write2 (word8Hex table) (word8Hex table) #.
           (\x -> let {-# INLINE byte #-}
                      byte n = fromIntegral $ x `shiftR` (n * 8) in
                  (byte 1, byte 0) 
            )

{-# INLINE word32Hex #-}
word32Hex :: EncodingTable -> Write Word32
word32Hex table = 
    write4 (word8Hex table) (word8Hex table) 
           (word8Hex table) (word8Hex table) #.
           (\x -> let {-# INLINE byte #-}
                      byte n = fromIntegral $ x `shiftR` (n * 8) in
                  (byte 3, byte 2, byte 1, byte 0) 
            )

{-# INLINE word64Hex #-}
word64Hex :: EncodingTable -> Write Word64
word64Hex table = 
    write8 (word8Hex table) (word8Hex table) 
           (word8Hex table) (word8Hex table)
           (word8Hex table) (word8Hex table)
           (word8Hex table) (word8Hex table) #.
           (\x -> let {-# INLINE byte #-}
                      byte n = fromIntegral $ x `shiftR` (n * 8) in
                  ( byte 7, byte 6, byte 5, byte 4
                  , byte 3, byte 2, byte 1, byte 0 ) 
            )

{-# INLINE word8HexNoLead #-}
word8HexNoLead :: EncodingTable -> Write Word8
word8HexNoLead table =
    writeIf (<16) word4Hex (word8Hex table)
  where
    {-# INLINE word4Hex #-}
    word4Hex =
        exactWrite 1 $ \x op -> poke op =<< encode4_as_8 table x

{-# INLINE asciiHexNoLead #-}
asciiHexNoLead :: forall a. (Storable a, Bits a, Integral a) 
                     => EncodingTable -> Write a
asciiHexNoLead table =
    boundedWrite (2 * maxBytes) (pokeIO . f)
  where
    maxBytes = (sizeOf (undefined :: a))

    f 0  op0 = do runWrite word8 (fromIntegral $ fromEnum '0') op0
    f x0 op0 = do
        let n0 = findNonZeroByte (maxBytes - 1)
            x  = fromIntegral $ x0 `shiftR` (n0 * 8)
        if x < 16
          then do poke op0 =<< encode4_as_8 table x
                  runWrite (asciiHexBytes n0      ) x0 (op0 `plusPtr` 1)
          else do runWrite (asciiHexBytes (n0 + 1)) x0 op0
      where
        findNonZeroByte !n
          | (x0 `shiftR` (8 * n) .&. 0xff) == 0 = findNonZeroByte (n - 1)
          | otherwise                           = n


    {-# INLINE asciiHexBytes #-}
    asciiHexBytes :: (Bits a, Integral a) => Int -> Write a
    asciiHexBytes n0 =
        boundedWrite (2 * max 0 n0) (pokeIO . g)
      where
        g !x0 !op0 = 
            loop (n0 - 1) op0
          where
            loop n op
              | n < 0     = do return op
              | otherwise = do
                  x <- encode8_as_16h table (fromIntegral $ x0 `shiftR` (n * 8))
                  poke (castPtr op) x
                  loop (n - 1) (op `plusPtr` 2)

      
