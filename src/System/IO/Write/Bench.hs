{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Support for benchmarking writes.
module System.IO.Write.Bench where

import Foreign

import           System.IO.Write.Internal hiding ( (#.#) )

import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S

{-
toByteString :: Write a -> a -> S.ByteString
toByteString w x = S.inlinePerformIO $ do
    fpbuf <- S.mallocByteString (writeBound w)
    let op = unsafeForeignPtrToPtr fpbuf
    op' <- runWrite w x op
    return $ S.PS fpbuf 0 (op' `minusPtr` op)
-}

{-# INLINE toByteStringReplicated #-}
toByteStringReplicated :: Int -> Write a -> a -> S.ByteString
toByteStringReplicated n0 w x = S.inlinePerformIO $ do
    fpbuf <- S.mallocByteString (n0 * writeBound w)
    let op0      = unsafeForeignPtrToPtr fpbuf
        loop 0 !op = return op
        loop n !op = runWrite w x op >>= loop (pred n)
    op <- loop n0 op0
    return $ S.PS fpbuf 0 (op `minusPtr` op0)

{-# INLINE toByteStringList #-}
toByteStringList :: Int -> Write a -> [a] -> S.ByteString
toByteStringList n0 w xs0 = unsafePerformIO $ do
    fpbuf <- S.mallocByteString (n0 * writeBound w)
    let loop !n xs !op
          | n <= 0    = return op
          | otherwise = case xs of
              []      -> return op
              (x:xs') -> runWrite w x op >>= loop (n - 1) xs'

        op0 = unsafeForeignPtrToPtr fpbuf
    op <- loop n0 xs0 op0
    return $ S.PS fpbuf 0 (op `minusPtr` op0)
