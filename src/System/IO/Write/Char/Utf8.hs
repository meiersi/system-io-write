{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2010 Jasper Van der Jeugt & Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- 'Write's to handle UTF-8 encoding of characters. 
--
module System.IO.Write.Char.Utf8
    ( 
      -- * UTF-8 encoded characters
      char

      -- * Hexadecimal encoding using UTF-8 encoded characters
    , HexWritable(..)
    ) where

import Foreign
import Data.Char (ord)

import System.IO.Write.Internal
import System.IO.Write.Internal.Base16
import System.IO.Write.Word

-- | Write a UTF-8 encoded Unicode character to a buffer.
--
{-# INLINE char #-}
char :: Write Char
char = write 4 (encodeCharUtf8 f1 f2 f3 f4)
  where
    f1 x1          = pokeN 1 $ \op -> do pokeByteOff op 0 x1

    f2 x1 x2       = pokeN 2 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2
                   
    f3 x1 x2 x3    = pokeN 3 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2
                                         pokeByteOff op 2 x3

    f4 x1 x2 x3 x4 = pokeN 4 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2
                                         pokeByteOff op 2 x3
                                         pokeByteOff op 3 x4

-- | Encode a Unicode character to another datatype, using UTF-8. This function
-- acts as an abstract way of encoding characters, as it is unaware of what
-- needs to happen with the resulting bytes: you have to specify functions to
-- deal with those.
--
encodeCharUtf8 :: (Word8 -> a)                             -- ^ 1-byte UTF-8
               -> (Word8 -> Word8 -> a)                    -- ^ 2-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> a)           -- ^ 3-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> Word8 -> a)  -- ^ 4-byte UTF-8
               -> Char                                     -- ^ Input 'Char'
               -> a                                        -- ^ Result
encodeCharUtf8 f1 f2 f3 f4 c = case ord c of
    x | x <= 0x7F -> f1 $ fromIntegral x
      | x <= 0x07FF ->
           let x1 = fromIntegral $ (x `shiftR` 6) + 0xC0
               x2 = fromIntegral $ (x .&. 0x3F)   + 0x80
           in f2 x1 x2
      | x <= 0xFFFF ->
           let x1 = fromIntegral $ (x `shiftR` 12) + 0xE0
               x2 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x3 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f3 x1 x2 x3
      | otherwise ->
           let x1 = fromIntegral $ (x `shiftR` 18) + 0xF0
               x2 = fromIntegral $ ((x `shiftR` 12) .&. 0x3F) + 0x80
               x3 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x4 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f4 x1 x2 x3 x4
{-# INLINE encodeCharUtf8 #-}


------------------------------------------------------------------------------
-- Hexadecimal Encoding
------------------------------------------------------------------------------

class HexWritable a where
    hexLower       :: Write a
    hexUpper       :: Write a
    hexUpperNoLead :: Write a
    hexLowerNoLead :: Write a

instance HexWritable Word8 where
    {-# INLINE hexLower #-}
    {-# INLINE hexUpper #-}
    {-# INLINE hexLowerNoLead #-}
    {-# INLINE hexUpperNoLead #-}
    hexLower       = word8Hex lowerTable
    hexUpper       = word8Hex upperTable
    hexLowerNoLead = word8HexNoLead lowerTable
    hexUpperNoLead = word8HexNoLead upperTable

instance HexWritable Word16 where
    {-# INLINE hexLower #-}
    {-# INLINE hexUpper #-}
    {-# INLINE hexLowerNoLead #-}
    {-# INLINE hexUpperNoLead #-}
    hexLower       = word16Hex lowerTable
    hexUpper       = word16Hex upperTable
    hexLowerNoLead = hexNoLead lowerTable
    hexUpperNoLead = hexNoLead upperTable

instance HexWritable Word32 where
    {-# INLINE hexLower #-}
    {-# INLINE hexUpper #-}
    {-# INLINE hexLowerNoLead #-}
    {-# INLINE hexUpperNoLead #-}
    hexLower       = word32Hex lowerTable
    hexUpper       = word32Hex upperTable
    hexLowerNoLead = hexNoLead lowerTable
    hexUpperNoLead = hexNoLead upperTable

instance HexWritable Word64 where
    {-# INLINE hexLower #-}
    {-# INLINE hexUpper #-}
    {-# INLINE hexLowerNoLead #-}
    {-# INLINE hexUpperNoLead #-}
    hexLower       = word64Hex lowerTable
    hexUpper       = word64Hex upperTable
    hexLowerNoLead = hexNoLead lowerTable
    hexUpperNoLead = hexNoLead upperTable

instance HexWritable Int8 where
    {-# INLINE hexLower #-}
    {-# INLINE hexUpper #-}
    {-# INLINE hexLowerNoLead #-}
    {-# INLINE hexUpperNoLead #-}
    hexLower       = word8Hex lowerTable #. fromIntegral
    hexUpper       = word8Hex upperTable #. fromIntegral
    hexLowerNoLead = word8HexNoLead lowerTable #. fromIntegral
    hexUpperNoLead = word8HexNoLead upperTable #. fromIntegral

instance HexWritable Int16 where
    {-# INLINE hexLower #-}
    {-# INLINE hexUpper #-}
    {-# INLINE hexLowerNoLead #-}
    {-# INLINE hexUpperNoLead #-}
    hexLower       = word16Hex lowerTable #. fromIntegral
    hexUpper       = word16Hex upperTable #. fromIntegral
    hexLowerNoLead = hexNoLead lowerTable 
    hexUpperNoLead = hexNoLead upperTable

instance HexWritable Int32 where
    {-# INLINE hexLower #-}
    {-# INLINE hexUpper #-}
    {-# INLINE hexLowerNoLead #-}
    {-# INLINE hexUpperNoLead #-}
    hexLower       = word32Hex lowerTable #. fromIntegral
    hexUpper       = word32Hex upperTable #. fromIntegral
    hexLowerNoLead = hexNoLead lowerTable
    hexUpperNoLead = hexNoLead upperTable

instance HexWritable Int64 where
    {-# INLINE hexLower #-}
    {-# INLINE hexUpper #-}
    {-# INLINE hexLowerNoLead #-}
    {-# INLINE hexUpperNoLead #-}
    hexLower       = word64Hex lowerTable #. fromIntegral
    hexUpper       = word64Hex upperTable #. fromIntegral
    hexLowerNoLead = hexNoLead lowerTable
    hexUpperNoLead = hexNoLead upperTable

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

{-# INLINE hexNoLead #-}
hexNoLead :: forall a. (Storable a, Bits a, Integral a) 
                     => EncodingTable -> Write a
hexNoLead table =
    write (2 * maxBytes) (pokeIO . f)
  where
    maxBytes = (sizeOf (undefined :: a))

    f 0  op0 = do runWrite word8 (fromIntegral $ fromEnum '0') op0
    f x0 op0 = do
        let n0 = findNonZeroByte (maxBytes - 1)
            x  = fromIntegral $ x0 `shiftR` (n0 * 8)
        if x < 16
          then do poke op0 =<< encode4_as_8 table x
                  runWrite (hexBytes n0      ) x0 (op0 `plusPtr` 1)
          else do runWrite (hexBytes (n0 + 1)) x0 op0
      where
        findNonZeroByte !n
          | (x0 `shiftR` (8 * n) .&. 0xff) == 0 = findNonZeroByte (n - 1)
          | otherwise                           = n


    {-# INLINE hexBytes #-}
    hexBytes :: (Bits a, Integral a) => Int -> Write a
    hexBytes n0 =
        write (2 * max 0 n0) (pokeIO . g)
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

      
