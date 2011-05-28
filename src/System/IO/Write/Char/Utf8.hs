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
      -- * UTF-8 encoding of single characters
      utf8

      -- * Hexadecimal encoding using UTF-8 encoded characters
    , utf8HexLower
    , utf8HexUpper
    , utf8HexLowerNoLead
    , utf8HexUpperNoLead
    ) where

import Foreign
import Data.Char (ord)

import System.IO.Write.Internal
import System.IO.Write.Char.Ascii

-- | Write a 'Char' using the UTF-8 encoding.
--
{-# INLINE utf8 #-}
utf8 :: Write Char
utf8 = boundedWrite 4 (encodeCharUtf8 f1 f2 f3 f4)
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

-- | Fixed-width hexadecimal encoding with lower-case characters encoded using
-- the UTF-8 encoding.
--
-- Note that we exploit that the UTF-8 encoding coincides with the ASCII
-- encoding on codepoints below 128. This is the origin of the
-- 'AsciiHexWritable' class constraint.
--
{-# INLINE utf8HexLower #-}
utf8HexLower :: AsciiHexWritable a => Write a
utf8HexLower = asciiHexLower

-- | Fixed-width hexadecimal encoding with upper-case characters encoded using
-- the UTF-8 encoding.
--
{-# INLINE utf8HexUpper #-}
utf8HexUpper :: AsciiHexWritable a => Write a
utf8HexUpper = asciiHexUpper

-- | Hexadecimal encoding with no leading zeros and lower-case characters
-- encoded using the UTF-8 encoding.
--
{-# INLINE utf8HexLowerNoLead #-}
utf8HexLowerNoLead :: AsciiHexWritable a => Write a
utf8HexLowerNoLead = asciiHexLowerNoLead
                  
-- | Hexadecimal encoding with  no leading zeros and upper-case characters
-- encoded using the UTF-8 encoding.
--
{-# INLINE utf8HexUpperNoLead #-}
utf8HexUpperNoLead :: AsciiHexWritable a => Write a
utf8HexUpperNoLead = asciiHexUpperNoLead

