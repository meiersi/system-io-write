{-# LANGUAGE BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- 'Write' combinators to build hexadecimal encodings for enumerable types
-- given a concrete character encoding.
module System.IO.Write.Char.Hex where

import Data.Bits

import Foreign

import System.IO.Write
import System.IO.Write.Internal

import qualified Data.ByteString.Internal as S


-- | A value from @0@ to @15@ represented using a machine-dependent integer as
-- this is assumed to be the fastest representation of integers.
newtype Nibble = Nibble { getNibble :: Int }

nibble :: Enum a => a -> Nibble
nibble = Nibble . (0xf .&.) . fromEnum

infixl 4 #.#

(#.#) :: Write a -> (b -> a) -> Write b
(#.#) = flip comapWrite

nibbleUtf8 :: Char -> Write Nibble
nibbleUtf8 a =
    writeWord8 #.# (fromIntegral . nibbleToChr)
  where
    nibbleToChr (Nibble x)
      | x < 10    = fromEnum '0'      + x
      | otherwise = (fromEnum a - 10) + x

nibbleUtf8Upper, nibbleUtf8Lower :: Write Nibble
nibbleUtf8Upper = nibbleUtf8 'A'
nibbleUtf8Lower = nibbleUtf8 'a'

write2times :: Write a -> Write (a, a)
write2times w = write2 w w

hexWord8 :: Write Nibble -> Write Word8
hexWord8 w = write2times (w #.# nibble) #.# (\x -> (x `shiftR` 4, x))

toByteString :: Write a -> a -> S.ByteString
toByteString w x = S.inlinePerformIO $ do
    fpbuf <- S.mallocByteString (writeBound w)
    let op = unsafeForeignPtrToPtr fpbuf
    op' <- runWrite w x op
    return $ S.PS fpbuf 0 (op' `minusPtr` op)

toByteStringReplicated :: Int -> Write a -> a -> S.ByteString
toByteStringReplicated n0 w x = S.inlinePerformIO $ do
    fpbuf <- S.mallocByteString (n0 * writeBound w)
    let op0      = unsafeForeignPtrToPtr fpbuf
        loop 0 !op = return op
        loop n !op = runWrite w x op >>= loop (pred n)
    op <- loop n0 op0
    return $ S.PS fpbuf 0 (op `minusPtr` op0)

hexWord8Lower, hexWord8Upper :: Write Word8
hexWord8Lower = hexWord8 nibbleUtf8Lower
hexWord8Upper = hexWord8 nibbleUtf8Upper

test :: Word8 -> S.ByteString
test x = toByteStringReplicated 3 (write2 hexWord8Lower hexWord8Upper) (x, x)







