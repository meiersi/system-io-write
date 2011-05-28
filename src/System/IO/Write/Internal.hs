{-# LANGUAGE ScopedTypeVariables, CPP, BangPatterns, MonoPatBinds #-}
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
-- inlined at compile time. The main use of this /write abstraction/ is to
-- share the implementation of encodings of Haskell values where the maximal
-- number of bytes to be written per value is statically known.
-- 
-- This library only provides types and combinators for defining writes. A
-- number of actual writes is provided by the @encoding-writes@ library. They
-- are used together with the @Builder@ type from the @bytestring@ library to
-- provide builders for a variety of encodings in the @encoding-builders@
-- library.
-- 
-- A word of caution: if you just need to serialize some data in some standard
-- encoding format, then use the builders provided in the @encoding-builders@
-- library. They provide an easy to use IO-free interface, that gives you very
-- good performance for most use cases. If you need to write your own encoding,
-- then be aware that
--
--  1. compile-time abstractions can result in rather inefficient code, if used
--     the wrong way, and
--
--  2. you are writing code with /all saftey belts off/; i.e., 
--     /this is the code that makes your application vulnerable to buffer-overflow
--     attacks!/
-- .
-- Note that if your encoding is not yet supported by @encoding-builders@, but
-- it is standardized, then I'm very happy to accept patches for the
-- @encoding-builders@ library.
--
-- /TODO: Improve documentation of module contents./
module System.IO.Write.Internal (
  -- * Poking a buffer
    Poke
  , pokeN
  , pokeIO

  -- * Writing a bounded number of bytes
  , Write
  , runWrite
  , getBound
  , getPoke

  -- ** Unsafe creation of Writes
  , boundedWrite
  , exactWrite
  , writeStorable

  -- ** Safe combinators
  , comapWrite
  , (#.)
  , append 
  , (#>)
  , prepend 
  , (<#)
  , writeNothing
  , writeIf
  , writeMaybe
  , writeEither
  , write2
  , write3
  , write4
  , write8
  
  -- ** Using IO inside a Write
  , writeLiftIO

  ) where

import Control.Monad ( (>=>) )
import Data.Monoid
import Foreign
import Prelude hiding (maxBound)

------------------------------------------------------------------------------
-- Poking a buffer
------------------------------------------------------------------------------

-- Sadly GHC is not smart enough: code where we branch and each branch should
-- execute a few IO actions and then return a value cannot be taught to GHC. At
-- least not such that it returns the value of the branches unpacked.
--
-- Hmm.. at least he behaves much better for the Monoid instance of Write
-- than the one for Poke. Serializing UTF-8 chars gets a slowdown of a
-- factor 2 when 2 chars are composed. Perhaps I should try out the writeList
-- instances also, as they may be more sensitive to too much work per Char.
--

-- | Changing a sequence of bytes starting from the given pointer. 'Poke's are
-- the most primitive buffer manipulation. In most cases, you don't use the
-- explicitely but as part of a 'Write', which also tells how many bytes will
-- be changed at most. 
newtype Poke = Poke { runPoke :: Ptr Word8 -> IO (Ptr Word8) }

instance Monoid Poke where
  {-# INLINE mempty #-}
  mempty = Poke $ return

  {-# INLINE mappend #-}
  (Poke po1) `mappend` (Poke po2) = Poke $ po1 >=> po2

  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

-- | @pokeN size io@ creates a write that denotes the writing of @size@ bytes
-- to a buffer using the IO action @io@. Note that @io@ MUST write EXACTLY @size@
-- bytes to the buffer!
{-# INLINE pokeN #-}
pokeN :: Int -> (Ptr Word8 -> IO ()) -> Poke
pokeN size io = Poke $ \op -> io op >> return (op `plusPtr` size)

{-# INLINE pokeIO #-}
pokeIO :: (Ptr Word8 -> IO (Ptr Word8)) -> Poke
pokeIO = Poke



------------------------------------------------------------------------------
-- Writing to a buffer
------------------------------------------------------------------------------

infixl 4 #.   -- comapWrite
infixr 5 #>   -- prepend
infixl 4 <#   -- append



data Write a = Write {-# UNPACK #-} !Int (a -> Poke)

-- | Get the raw encoding function encapsulated by the 'Write'.
{-# INLINE getPoke #-}
getPoke :: Write a -> a -> Poke
getPoke (Write _ f) = f

-- | The maximal number of bytes written by this 'Write'.
{-# INLINE getBound #-}
getBound :: Write a -> Int
getBound (Write b _) = b


-- | Run a 'Write' to encode a value starting from the given address and return
-- the address of the next free byte.
{-# INLINE runWrite #-}
runWrite :: Write a -> a -> Ptr Word8 -> IO (Ptr Word8)
runWrite (Write _ f) = runPoke . f

-- | Utility function to compute the maximal bound of two writes.
{-# INLINE maxBound #-}
maxBound :: Write a -> Write b -> Int
maxBound w1 w2 = max (getBound w1) (getBound w2)

-- | Utility function to compute the sum of the bounds of two writes.
{-# INLINE addBounds #-}
addBounds :: Write a -> Write b -> Int
addBounds w1 w2 = getBound w1 + getBound w2


-- Unsafe creation of writes
----------------------------

{-# INLINE boundedWrite #-}
boundedWrite :: Int          -- ^ Maximal number of bytes written
             -> (a -> Poke)  -- ^ Implementation of encoding scheme
             -> Write a      
boundedWrite = Write

{-# INLINE exactWrite #-}
exactWrite :: Int -> (a -> Ptr Word8 -> IO ()) -> Write a
exactWrite size io = Write size (pokeN size . io)

-- | Write a storable value.
{-# INLINE writeStorable #-}
writeStorable :: forall a. Storable a => Write a
writeStorable = 
    exactWrite (sizeOf (undefined :: a)) (\x op -> poke (castPtr op) x)


-- Safe combinators
-------------------

{-# INLINE comapWrite #-}
comapWrite :: (b -> a) -> Write a -> Write b
comapWrite g (Write b f) = Write b (f . g)

{-# INLINE (#.) #-}
(#.) :: Write a -> (b -> a) -> Write b
(#.) = flip comapWrite

{-# INLINE prepend #-}
prepend :: (Write a, a) -> Write b -> Write b
prepend (w1, x) w2 = write2 w1 w2 #. (\y -> (x, y))

{-# INLINE append #-}
append :: Write a -> (Write b, b) -> Write a
append w1 (w2, y) = write2 w1 w2 #. (\x -> (x, y))

{-# INLINE (<#) #-}
(<#) :: Write a -> (Write b, b) -> Write a
(<#) = append

{-# INLINE (#>) #-}
(#>) :: (Write a, a) -> Write b -> Write b
(#>) = prepend

-- | @writeNothing x@ never writes anything to memory.
{-# INLINE writeNothing #-}
writeNothing :: Write a
writeNothing = Write 0 mempty

-- | @writeIf p wTrue wFalse x@ creates a 'Write' with a 'Poke' equal to @wTrue
-- x@, if @p x@ and equal to @wFalse x@ otherwise. The bound of this new
-- 'Write' is the maximum of the bounds for either 'Write'. This yields a data
-- independent bound, if the bound for @wTrue@ and @wFalse@ is already data
-- independent.
{-# INLINE writeIf #-}
writeIf :: (a -> Bool) -> Write a -> Write a -> Write a
writeIf p wTrue wFalse = 
    Write (maxBound wTrue wFalse) f
  where
    f x = Poke $ if p x then runWrite wTrue x else runWrite wFalse x

{-# INLINE writeMaybe #-}
writeMaybe :: Write ()
           -> Write a
           -> Write (Maybe a)
writeMaybe wNothing wJust =
    Write (max (getBound wNothing) (getBound wJust))
          (Poke . maybe (runWrite wNothing ()) (runWrite wJust))

{-# INLINE writeEither #-}
writeEither :: Write a
            -> Write b
            -> Write (Either a b)
writeEither wLeft wRight =
    Write (maxBound wLeft wRight)
          (Poke . either (runWrite wLeft) (runWrite wRight))

{-# INLINE write2 #-}
write2 :: Write a -> Write b -> Write (a, b)
write2 w1 w2 =
    Write (addBounds w1 w2) f
  where
    f (a, b) = getPoke w1 a `mappend` getPoke w2 b

{-# INLINE write3 #-}
write3 :: Write a -> Write b -> Write c -> Write (a, b, c)
write3 w1 w2 w3 =
    Write (addBounds w1 w2 + getBound w3) f
  where
    f (a, b, c) = getPoke w1 a `mappend` getPoke w2 b 
                                 `mappend` getPoke w3 c

{-# INLINE write4 #-}
write4 :: Write a -> Write b -> Write c -> Write d -> Write (a, b, c, d)
write4 w1 w2 w3 w4 =
    Write (addBounds w1 w2 + addBounds w3 w4) f
  where
    f (a, b, c, d) = getPoke w1 a `mappend` getPoke w2 b 
                                    `mappend` getPoke w3 c
                                    `mappend` getPoke w4 d
{-# INLINE write8 #-}
write8 :: Write a1 -> Write a2 -> Write a3 -> Write a4
       -> Write a5 -> Write a6 -> Write a7 -> Write a8
       -> Write (a1, a2, a3, a4, a5, a6, a7, a8)
write8 w1 w2 w3 w4 w5 w6 w7 w8=
    Write (addBounds w1 w2 + addBounds w3 w4 +
           addBounds w5 w6 + addBounds w7 w8) f
  where
    f (x1, x2, x3, x4, x5, x6, x7, x8) =
        getPoke w1 x1 `mappend` getPoke w2 x2 `mappend`
        getPoke w3 x3 `mappend` getPoke w4 x4 `mappend`
        getPoke w5 x5 `mappend` getPoke w6 x6 `mappend`
        getPoke w7 x7 `mappend` getPoke w8 x8


-- IO inside a Write
--------------------

-- | @writeLiftIO io write@ creates a write executes the @io@ action to compute
-- the value that is then written.
{-# INLINE writeLiftIO #-}
writeLiftIO :: Write a -> Write (IO a)
writeLiftIO w =
    Write (getBound w) (\io -> Poke $ \op -> do x <- io; runWrite w x op)


