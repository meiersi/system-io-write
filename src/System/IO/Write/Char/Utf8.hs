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
      writeChar

      -- * Hexadecimal encoding using UTF-8 encoded characters
    , Base16Utf8Writable(..)
    , hex
    , hexNoLead
    ) where

import Foreign
import Data.Char (ord)

import System.IO.Write.Internal
import System.IO.Write.Internal.Base16
import System.IO.Write.Word

-- | Write a UTF-8 encoded Unicode character to a buffer.
--
{-# INLINE writeChar #-}
writeChar :: Write Char
writeChar = write 4 (encodeCharUtf8 f1 f2 f3 f4)
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

class Base16Utf8Writable a where
    base16Lower       :: Write a
    base16Upper       :: Write a
    base16UpperNoLead :: Write a
    base16LowerNoLead :: Write a

hex :: Base16Utf8Writable a => Write a
hex = base16Lower

hexNoLead :: Base16Utf8Writable a => Write a
hexNoLead = base16LowerNoLead

instance Base16Utf8Writable Word8 where
    {-# INLINE base16Lower #-}
    {-# INLINE base16Upper #-}
    {-# INLINE base16LowerNoLead #-}
    {-# INLINE base16UpperNoLead #-}
    base16Lower       = word8Base16 lowerTable
    base16Upper       = word8Base16 upperTable
    base16LowerNoLead = word8Base16NoLead lowerTable
    base16UpperNoLead = word8Base16NoLead upperTable

instance Base16Utf8Writable Word16 where
    {-# INLINE base16Lower #-}
    {-# INLINE base16Upper #-}
    {-# INLINE base16LowerNoLead #-}
    {-# INLINE base16UpperNoLead #-}
    base16Lower       = word16Base16 lowerTable
    base16Upper       = word16Base16 upperTable
    base16LowerNoLead = base16NoLead lowerTable
    base16UpperNoLead = base16NoLead upperTable

instance Base16Utf8Writable Word32 where
    {-# INLINE base16Lower #-}
    {-# INLINE base16Upper #-}
    {-# INLINE base16LowerNoLead #-}
    {-# INLINE base16UpperNoLead #-}
    base16Lower       = word32Base16 lowerTable
    base16Upper       = word32Base16 upperTable
    base16LowerNoLead = base16NoLead lowerTable
    base16UpperNoLead = base16NoLead upperTable

instance Base16Utf8Writable Word64 where
    {-# INLINE base16Lower #-}
    {-# INLINE base16Upper #-}
    {-# INLINE base16LowerNoLead #-}
    {-# INLINE base16UpperNoLead #-}
    base16Lower       = word64Base16 lowerTable
    base16Upper       = word64Base16 upperTable
    base16LowerNoLead = base16NoLead lowerTable
    base16UpperNoLead = base16NoLead upperTable

{-# INLINE word8Base16 #-}
word8Base16 :: EncodingTable -> Write Word8
word8Base16 table = 
    exactWrite 2 $ \x op -> poke (castPtr op) =<< encode8_as_16h table x

{-# INLINE word16Base16 #-}
word16Base16 :: EncodingTable -> Write Word16
word16Base16 table = 
    write2 (word8Base16 table) (word8Base16 table) #.
           (\x -> let {-# INLINE byte #-}
                      byte n = fromIntegral $ x `shiftR` (n * 8) in
                  (byte 1, byte 0) 
            )

{-# INLINE word32Base16 #-}
word32Base16 :: EncodingTable -> Write Word32
word32Base16 table = 
    write4 (word8Base16 table) (word8Base16 table) 
           (word8Base16 table) (word8Base16 table) #.
           (\x -> let {-# INLINE byte #-}
                      byte n = fromIntegral $ x `shiftR` (n * 8) in
                  (byte 3, byte 2, byte 1, byte 0) 
            )

{-# INLINE word64Base16 #-}
word64Base16 :: EncodingTable -> Write Word64
word64Base16 table = 
    write8 (word8Base16 table) (word8Base16 table) 
           (word8Base16 table) (word8Base16 table)
           (word8Base16 table) (word8Base16 table)
           (word8Base16 table) (word8Base16 table) #.
           (\x -> let {-# INLINE byte #-}
                      byte n = fromIntegral $ x `shiftR` (n * 8) in
                  ( byte 7, byte 6, byte 5, byte 4
                  , byte 3, byte 2, byte 1, byte 0 ) 
            )

{-# INLINE word8Base16NoLead #-}
word8Base16NoLead :: EncodingTable -> Write Word8
word8Base16NoLead table =
    writeIf (<16) word4Base16 (word8Base16 table)
  where
    {-# INLINE word4Base16 #-}
    word4Base16 =
        exactWrite 1 $ \x op -> poke op =<< encode4_as_8 table x

{-# INLINE base16NoLead #-}
base16NoLead :: forall a. (Storable a, Bits a, Integral a) 
                     => EncodingTable -> Write a
base16NoLead table =
    write (2 * maxBytes) (pokeIO . f)
  where
    maxBytes = (sizeOf (undefined :: a))

    f 0  op0 = do runWrite word8 (fromIntegral $ fromEnum '0') op0
    f x0 op0 = do
        let n0 = findNonZeroByte (maxBytes - 1)
            x  = fromIntegral $ x0 `shiftR` (n0 * 8)
        if x < 16
          then do poke op0 =<< encode4_as_8 table x
                  runWrite (base16Bytes n0      ) x0 (op0 `plusPtr` 1)
          else do runWrite (base16Bytes (n0 + 1)) x0 op0
      where
        findNonZeroByte !n
          | (x0 `shiftR` (8 * n) .&. 0xff) == 0 = findNonZeroByte (n - 1)
          | otherwise                           = n


    {-# INLINE base16Bytes #-}
    base16Bytes :: (Bits a, Integral a) => Int -> Write a
    base16Bytes n0 =
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

      
