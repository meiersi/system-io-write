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
  
  -- | We abstract raw 'IO' actions for poking a buffer in a separate type.
  -- This allows us to change the actual implementation of buffer-poking in the
  -- future with minimal breakage of client code. Note that, in contrast to the
  -- 'poke' function provided by 'Storable', a 'Poke' is not restricted to
  -- poking a fixed number of bytes.
  --
    Poke
  , pokeIO
  , pokeN

  -- * Writing a bounded number of bytes
  , Write
  , runWrite
  , getBound
  , getPoke

  -- ** Unsafe creation of Writes
  , boundedWrite
  , exactWrite
  , writeStorable

  -- ** Using IO inside a Write
  , writeLiftIO

  -- ** Safe combinators

  -- | The following combinators ensure that the bound on the maximal
  -- number of bytes written is always computed correctly. Hence, applying them 
  -- to safe writes always results in a safe write.
  -- Moreover, care is taken to compute that bound such that the compiler can
  -- optimize it to a compile time constant, if that is possible.
  
  -- *** Basic building blocks
  
  -- | These combinators cannot be implemented without breaking the 'Write' abstraction.
  , (#.)
  , comapWrite
  , write2
  , writeNothing
  , writeIf
  , writeMaybe
  , writeEither

  -- *** Convenience
  
  -- | These combinators can be implemented on top of the basic building
  -- blocks. We provide them here for convenience.
  , (#>)
  , prepend 
  , (<#)
  , append 
  , write3
  , write4
  , write8
  
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

-- | Poking a sequence of bytes. 'Poke's can be sequenced using their 'Monoid'
-- instance. 
newtype Poke = Poke { runPoke :: Ptr Word8 -> IO (Ptr Word8) }

instance Monoid Poke where
  {-# INLINE mempty #-}
  mempty = Poke $ return

  {-# INLINE mappend #-}
  (Poke po1) `mappend` (Poke po2) = Poke $ po1 >=> po2

  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

-- | Poke a sequence of bytes starting from the given pointer and return the
-- pointer to the next free byte.
--
-- Note that the 'IO' action underlying this 'Poke' must poke /exactly/ the
-- bytes between the given start-pointer and the returned end-pointer. If more bytes
-- were poked, then data outside the buffer might be overwritten; i.e, the
-- resulting code would likely be vulnerable to a buffer-overflow attack. If
-- fewer bytes were poked, then some sensitive data might leak because not all
-- data in the buffer is overwritten.
{-# INLINE pokeIO #-}
pokeIO :: (Ptr Word8 -> IO (Ptr Word8)) -> Poke
pokeIO = Poke

-- | An abbrevation for constructing 'Poke's of fixed-length sequences.
--
-- /Preconditions:/ the given number of the poked bytes must agree precisely
-- with the actual implementation (analogously to 'pokeIO').
{-# INLINE pokeN #-}
pokeN :: Int -> (Ptr Word8 -> IO ()) -> Poke
pokeN size io = Poke $ \op -> io op >> return (op `plusPtr` size)



------------------------------------------------------------------------------
-- Writing to a buffer
------------------------------------------------------------------------------

infixl 4 #.   -- comapWrite
infixr 5 #>   -- prepend
infixl 4 <#   -- append


-- | Encodings of Haskell values that can be implemented by writing a
-- bounded-length sequence of bytes directly to memory.
data Write a = Write {-# UNPACK #-} !Int (a -> Poke)

-- | Get the raw encoding function encapsulated by a 'Write'.
{-# INLINE getPoke #-}
getPoke :: Write a -> a -> Poke
getPoke (Write _ f) = f

-- | The maximal number of bytes written by a 'Write'.
{-# INLINE getBound #-}
getBound :: Write a -> Int
getBound (Write b _) = b


-- | Run a 'Write' to encode a value starting from the given pointer and return
-- the pointer of the next free byte.
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

-- | Create a 'Write' from a bound on the maximal number of bytes written and
-- an implementation of the encoding scheme.
--
-- /Precondition:/ the bound must be valid for the implementation of the
-- encoding scheme.
{-# INLINE boundedWrite #-}
boundedWrite :: Int          -- ^ Maximal number of bytes written
             -> (a -> Poke)  -- ^ Implementation of the encoding scheme
             -> Write a      
boundedWrite = Write

-- | Create a 'Write' from an 'IO' action that writes a fixed number of bytes.
--
-- /Preconditions:/ the given number of the written bytes must agree precisely
-- with the actual implementation (analogously to 'pokeIO').
{-# INLINE exactWrite #-}
exactWrite :: Int                       -- ^ Number of bytes written
           -> (a -> Ptr Word8 -> IO ()) -- ^ 'IO' action writing exactly that
                                        -- many bytes from the given start pointer
           -> Write a
exactWrite size io = Write size (pokeN size . io)

-- | 'Write' a 'Storable' value using 'poke'.
{-# INLINE writeStorable #-}
writeStorable :: forall a. Storable a => Write a
writeStorable = 
    exactWrite (sizeOf (undefined :: a)) (\x op -> poke (castPtr op) x)


-- Safe combinators
-------------------

-- | 'Write's are cofunctors. The following laws hold.
--
-- > w #. id      = w
-- > w #. (f . g) = w #. f #. g
--
-- A typical use of 'comapWrite' is the definition of 
--
-- > writeInt32 = writeWord32 #. fromIntegral@
--
-- Once the the base library provides a Cofunctor class, we will make 'Write's an instance of it.
{-# INLINE comapWrite #-}
comapWrite :: (b -> a) -> Write a -> Write b
comapWrite g (Write b f) = Write b (f . g)

-- | An infix synonym for 'comapWrite'.
{-# INLINE (#.) #-}
(#.) :: Write a -> (b -> a) -> Write b
(#.) = flip comapWrite

-- | Prepend the writing of a fixed sequence of bytes to a 'Write'.
--
-- > showWrite ((utf8, '0') #> (utf8, 'x') #> utf8HexLower) (26 :: Word16) = "0x001a"
--
{-# INLINE prepend #-}
prepend :: (Write a, a) -> Write b -> Write b
prepend (w1, x) w2 = write2 w1 w2 #. (\y -> (x, y))

-- | An infix synonym for 'prepend'.
{-# INLINE (#>) #-}
(#>) :: (Write a, a) -> Write b -> Write b
(#>) = prepend


-- | Append the writing of a fixed sequence of bytes to a 'Write'.
--
-- > showWrite (utf8HexLower <# (utf8, '\'')) (26 :: Word16) = "001a'"
--
{-# INLINE append #-}
append :: Write a -> (Write b, b) -> Write a
append w1 (w2, y) = write2 w1 w2 #. (\x -> (x, y))

-- | An infix synonym for 'append'.
{-# INLINE (<#) #-}
(<#) :: Write a -> (Write b, b) -> Write a
(<#) = append

-- | Write nothing. The encoding scheme of 'writeNothing' does not inspect its
-- argument at all. Therefore
--
-- > showWrite writeNothing undefined = ""
--
{-# INLINE writeNothing #-}
writeNothing :: Write a
writeNothing = Write 0 mempty

-- | Conditionally select a 'Write'.
--
-- > asciiDrop = writeIf (< '\128') unsafeAscii writeNothing
--
{-# INLINE writeIf #-}
writeIf :: (a -> Bool) -> Write a -> Write a -> Write a
writeIf p wTrue wFalse = 
    Write (maxBound wTrue wFalse) f
  where
    f x = Poke $ if p x then runWrite wTrue x else runWrite wFalse x

-- | Select a 'Write' depending on a 'Maybe' value.
{-# INLINE writeMaybe #-}
writeMaybe :: (Write a, a) -> Write b -> Write (Maybe b)
writeMaybe (wNothing, x) wJust =
    Write (max (getBound wNothing) (getBound wJust))
          (Poke . maybe (runWrite wNothing x) (runWrite wJust))

-- | Select a 'Write' depending on an 'Either' value.
{-# INLINE writeEither #-}
writeEither :: Write a
            -> Write b
            -> Write (Either a b)
writeEither wLeft wRight =
    Write (maxBound wLeft wRight)
          (Poke . either (runWrite wLeft) (runWrite wRight))

-- | Sequentially compose two 'Write's.
--
-- > showWrite (write2 utf8 utf8) ('x','y') = "xy"
--
{-# INLINE write2 #-}
write2 :: Write a -> Write b -> Write (a, b)
write2 w1 w2 =
    Write (addBounds w1 w2) f
  where
    f (a, b) = getPoke w1 a `mappend` getPoke w2 b

-- | Sequentially compose three 'Write's.
{-# INLINE write3 #-}
write3 :: Write a -> Write b -> Write c -> Write (a, b, c)
write3 w1 w2 w3 =
    Write (addBounds w1 w2 + getBound w3) f
  where
    f (a, b, c) = getPoke w1 a `mappend` getPoke w2 b 
                                 `mappend` getPoke w3 c

-- | Sequentially compose four 'Write's.
{-# INLINE write4 #-}
write4 :: Write a -> Write b -> Write c -> Write d -> Write (a, b, c, d)
write4 w1 w2 w3 w4 =
    Write (addBounds w1 w2 + addBounds w3 w4) f
  where
    f (a, b, c, d) = getPoke w1 a `mappend` getPoke w2 b 
                                    `mappend` getPoke w3 c
                                    `mappend` getPoke w4 d

-- | Sequentially compose eight 'Write's.
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


