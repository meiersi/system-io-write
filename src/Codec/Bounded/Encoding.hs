{-# LANGUAGE CPP, BangPatterns, MonoPatBinds #-}
-- |
-- Copyright   : 2010, 2011 Simon Meier, 2010 Jasper van der Jeugt
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Bounded encodings are encodings of Haskell values where the maximal number
-- of bytes written can be bounded /independent/ of the value being encoded.
-- This bound is exploited in the 'bytestring' library
-- (<http://hackage.haskell.org/package/bytestring>) to provide more efficient
-- lazy bytestring builders for encoding Haskell values as sequences of bytes.
--  
-- This library provides bounded encodings for standard encoding formats of
-- standard Haskell types together with a set of combinators that allow
-- defining bounded encodings for further encoding formats. A typical use-case
-- of the combinators is implementing a bounded encoding that fuses 
-- escaping and character encoding. Check the corresponding example in the
-- 'Data.ByteString.Lazy.Builder.Extras' module of the 'bytestring' library.
--
module Codec.Bounded.Encoding (
  -- * Bounded encodings
    Encoding

  -- ** Basic combinators
  , (#.)
  , comapEncoding
  , emptyEncoding
  , encodeIf
  , encodeMaybe
  , encodeEither
  , encode2

  -- ** Convenience combinators
  , (#>)
  , prepend
  , (<#)
  , append
  , encode3
  , encode4
  , encode8

  -- ** Encoding Unicode characters
  , module Codec.Bounded.Encoding.Char.Ascii
  , module Codec.Bounded.Encoding.Char.Utf8

  -- ** Encoding fixed-width integers
  , module Codec.Bounded.Encoding.Int
  , module Codec.Bounded.Encoding.Word

  -- -- * Encoding floating point numbers
  -- , module Codec.Bounded.Encoding.Floating

  -- * Debugging
  -- | Note that the following two functions are intended for debugging use
  -- only. They are not efficient. Bounded encodings are efficently executed
  -- using the lazy bytestring builders provided in the
  -- 'Data.ByteString.Lazy.Builder.Extras' module of the 'bytestring' library.
  , evalEncoding
  , showEncoding

  ) where

import Codec.Bounded.Encoding.Internal
import Codec.Bounded.Encoding.Char.Ascii
import Codec.Bounded.Encoding.Char.Utf8
import Codec.Bounded.Encoding.Word
import Codec.Bounded.Encoding.Int
-- import Codec.Bounded.Encoding.Floating

import Codec.Bounded.Encoding.Internal.Test
