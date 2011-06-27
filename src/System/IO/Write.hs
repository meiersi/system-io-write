{-# LANGUAGE CPP, BangPatterns, MonoPatBinds #-}
-- |
-- Copyright   : 2010, 2011 Simon Meier, 2010 Jasper van der Jeugt
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- This library provides a compile-time abstraction of writing a bounded number
-- of bytes to memory; i.e., all functions in this module are intended to be
-- inlined at compile time. The main use 'Write's is to share the
-- implementation of encodings of Haskell values where the maximal number of
-- bytes written is /independent/ of the value being encoded.
--
-- Apart from combinators for defining 'Write's, this library provides a number
-- of 'Write's abstracting standard encoding of standard Haskell values. They
-- are used in the 'bytestring' library to provide the implementations of the
-- primitive 'Builder's. See the module @Data.ByteString.Builder.Write@ in the
-- 'bytestring' library for an example of how to use 'Write's.
--
module System.IO.Write (
  -- * The Write type
    Write
  

  -- ** Basic combinators
  , (#.)
  , comapWrite
  , writeNothing
  , writeIf
  , writeMaybe
  , writeEither
  , write2

  -- ** Convenience combinators
  , (#>)
  , prepend
  , (<#)
  , append
  , write3
  , write4
  , write8

  -- ** Writing Unicode characters
  , module System.IO.Write.Char.Ascii
  , module System.IO.Write.Char.Utf8

  -- ** Writing fixed-width integers
  , module System.IO.Write.Int
  , module System.IO.Write.Word

  -- -- * Writing floating point numbers
  -- , module System.IO.Write.Floating

  -- * Debugging
  , evalWrite
  , showWrite

  ) where

import System.IO.Write.Internal
import System.IO.Write.Char.Ascii
import System.IO.Write.Char.Utf8
import System.IO.Write.Word
import System.IO.Write.Int
-- import System.IO.Write.Floating

import System.IO.Write.Test
