-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- | A minimal version of bytestrings for testing writes and building encoding
-- tables.
-- 
module System.IO.Write.Internal.Region (
    Region
  , fromList
  -- , toList
  , unsafeIndex
  ) where

import Foreign

-- | A region of memory referenced by a base pointer and its size.
type Region = (ForeignPtr Word8, Int)

-- | Convert a list to a 'Region'.
fromList :: [Word8] -> IO Region 
fromList ys0 = do
    let n = length ys0
    fpbuf <- mallocForeignPtrBytes n
    withForeignPtr fpbuf $ \sp -> do
        ep <- fill ys0 sp
        return (fpbuf, ep `minusPtr` sp)
  where
    fill []     op = do return op
    fill (y:ys) op = do poke op y
                        fill ys (op `plusPtr` 1)

-- | @unsafeIndex r i@ returns the value of the @i@-th byte in 'Region' @r@.
unsafeIndex :: Region -> Int -> IO Word8
unsafeIndex = peekElemOff . unsafeForeignPtrToPtr . fst

