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
  , runPoke
  , pokeN
  , pokeIO

  -- * Writing to a buffer
  , FixedWrite
  , runFixedWrite
  , fixedWriteBound

  -- * Statically bounded @Write@s
  , Write
  , runWrite
  , writeBound
  , writePoke
  , write
  , exactWrite

  , comapWrite
  , fixWrite
  , (#)
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

  , writeLiftIO
  , writeStorable

  ) where

import Control.Monad ( (>=>) )

import Data.Monoid

import Foreign


------------------------------------------------------------------------------
-- Poking a buffer and writing to a buffer
------------------------------------------------------------------------------

-- Sadly GHC is not smart enough: code where we branch and each branch should
-- execute a few IO actions and then return a value cannot be taught to GHC. At
-- least not such that it returns the value of the branches unpacked.
--
-- Hmm.. at least he behaves much better for the Monoid instance of Write
-- than the one for Poke. Serializing UTF-8 chars gets a slowdown of a
-- factor 2 when 2 chars are composed. Perhaps I should try out the writeList
-- instances also, as they may be more sensitive to to much work per Char.
--

-- | Changing a sequence of bytes starting from the given pointer. 'Poke's are
-- the most primitive buffer manipulation. In most cases, you don't use the
-- explicitely but as part of a 'Write', which also tells how many bytes will
-- be changed at most. 
newtype Poke = Poke { runPoke :: Ptr Word8 -> IO (Ptr Word8) }

data FixedWrite = FixedWrite {-# UNPACK #-} !Int Poke

data Write a = Write {-# UNPACK #-} !Int (a -> Poke)

infixr 5 #>
infixl 4 <#
infix  1 # 
infixl 4 #.


-- | @pokeN size io@ creates a write that denotes the writing of @size@ bytes
-- to a buffer using the IO action @io@. Note that @io@ MUST write EXACTLY @size@
-- bytes to the buffer!
{-# INLINE pokeN #-}
pokeN :: Int 
       -> (Ptr Word8 -> IO ()) -> Poke
pokeN size io = Poke $ \op -> io op >> return (op `plusPtr` size)

pokeIO :: (Ptr Word8 -> IO (Ptr Word8)) -> Poke
pokeIO = Poke

comapWrite :: (b -> a) -> Write a -> Write b
comapWrite g (Write b f) = Write b (f . g)

(#.) :: Write a -> (b -> a) -> Write b
(#.) = flip comapWrite

fixWrite :: Write a -> a -> FixedWrite
fixWrite (Write b f) x = FixedWrite b (f x)

(#) :: Write a -> a -> FixedWrite
(#) = fixWrite

fixedWriteBound :: FixedWrite -> Int
fixedWriteBound (FixedWrite b _) = b

runFixedWrite :: FixedWrite -> Ptr Word8 -> IO (Ptr Word8)
runFixedWrite (FixedWrite _ p) = runPoke p

runWrite :: Write a -> a -> Ptr Word8 -> IO (Ptr Word8)
runWrite (Write _ f) = runPoke . f

writeBound :: Write a -> Int
writeBound (Write b _) = b

maxWriteBound :: Write a -> Write b -> Int
maxWriteBound w1 w2 = max (writeBound w1) (writeBound w2)

addWriteBounds :: Write a -> Write b -> Int
addWriteBounds w1 w2 = writeBound w1 + writeBound w2


prepend :: FixedWrite -> Write a -> Write a
prepend (FixedWrite b1 p) (Write b2 f) = Write (b1 + b2) ((p `mappend`) . f)

append :: Write a -> FixedWrite -> Write a
append (Write b2 f) (FixedWrite b1 p) = Write (b1 + b2) ((`mappend` p) . f)

(<#) :: Write a -> FixedWrite -> Write a
(<#) = append

(#>) :: FixedWrite -> Write a -> Write a
(#>) = prepend


instance Monoid Poke where
  {-# INLINE mempty #-}
  mempty = Poke $ return

  {-# INLINE mappend #-}
  (Poke po1) `mappend` (Poke po2) = Poke $ po1 >=> po2

  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

instance Monoid FixedWrite where
  {-# INLINE mempty #-}
  mempty = FixedWrite 0 mempty

  {-# INLINE mappend #-}
  (FixedWrite bound1 w1) `mappend` (FixedWrite bound2 w2) =
    FixedWrite (bound1 + bound2) (w1 `mappend` w2)
  
  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty


-- | @writeLiftIO io write@ creates a write executes the @io@ action to compute
-- the value that is then written.
{-# INLINE writeLiftIO #-}
writeLiftIO :: Write a -> Write (IO a)
writeLiftIO w =
    Write (writeBound w) (\io -> Poke $ \op -> do x <- io; runWrite w x op)

-- | @writeIf p wTrue wFalse x@ creates a 'Write' with a 'Poke' equal to @wTrue
-- x@, if @p x@ and equal to @wFalse x@ otherwise. The bound of this new
-- 'Write' is the maximum of the bounds for either 'Write'. This yields a data
-- independent bound, if the bound for @wTrue@ and @wFalse@ is already data
-- independent.
{-# INLINE writeIf #-}
writeIf :: (a -> Bool) -> Write a -> Write a -> Write a
writeIf p wTrue wFalse = 
    Write (maxWriteBound wTrue wFalse) f
  where
    f x = Poke $ if p x then runWrite wTrue x else runWrite wFalse x

{-# INLINE writeMaybe #-}
writeMaybe :: FixedWrite
           -> Write a
           -> Write (Maybe a)
writeMaybe wNothing wJust =
    Write (max (fixedWriteBound wNothing) (writeBound wJust))
          (Poke . maybe (runFixedWrite wNothing) (runWrite wJust))

{-# INLINE writeEither #-}
writeEither :: Write a
            -> Write b
            -> Write (Either a b)
writeEither wLeft wRight =
    Write (maxWriteBound wLeft wRight)
          (Poke . either (runWrite wLeft) (runWrite wRight))

writePoke :: Write a -> a -> Poke
writePoke (Write _ f) = f

{-# INLINE write2 #-}
write2 :: Write a -> Write b -> Write (a, b)
write2 w1 w2 =
    Write (addWriteBounds w1 w2) f
  where
    f (a, b) = writePoke w1 a `mappend` writePoke w2 b

{-# INLINE write3 #-}
write3 :: Write a -> Write b -> Write c -> Write (a, b, c)
write3 w1 w2 w3 =
    Write (addWriteBounds w1 w2 + writeBound w3) f
  where
    f (a, b, c) = writePoke w1 a `mappend` writePoke w2 b 
                                 `mappend` writePoke w3 c

{-# INLINE write4 #-}
write4 :: Write a -> Write b -> Write c -> Write d -> Write (a, b, c, d)
write4 w1 w2 w3 w4 =
    Write (addWriteBounds w1 w2 + addWriteBounds w3 w4) f
  where
    f (a, b, c, d) = writePoke w1 a `mappend` writePoke w2 b 
                                    `mappend` writePoke w3 c
                                    `mappend` writePoke w4 d
{-# INLINE write8 #-}
write8 :: Write a1 -> Write a2 -> Write a3 -> Write a4
       -> Write a5 -> Write a6 -> Write a7 -> Write a8
       -> Write (a1, a2, a3, a4, a5, a6, a7, a8)
write8 w1 w2 w3 w4 w5 w6 w7 w8=
    Write (addWriteBounds w1 w2 + addWriteBounds w3 w4 +
           addWriteBounds w5 w6 + addWriteBounds w7 w8) f
  where
    f (x1, x2, x3, x4, x5, x6, x7, x8) =
        writePoke w1 x1 `mappend` writePoke w2 x2 `mappend`
        writePoke w3 x3 `mappend` writePoke w4 x4 `mappend`
        writePoke w5 x5 `mappend` writePoke w6 x6 `mappend`
        writePoke w7 x7 `mappend` writePoke w8 x8



-- | @writeNothing x@ never writes anything to memory.
{-# INLINE writeNothing #-}
writeNothing :: Write a
writeNothing = Write 0 mempty

{-
-- | @exactWrite size io@ creates a bounded write that can later be converted to
-- a builder that writes exactly @size@ bytes. Note that @io@ MUST write
-- EXACTLY @size@ bytes to the buffer!
{-# INLINE exactWrite #-}
exactWrite :: Int 
           -> (Ptr Word8 -> IO ()) 
           -> Write
exactWrite size io = Write size (pokeN size io)

-- | @boundedWrite size write@ creates a bounded write from a @write@ that does
-- not write more than @size@ bytes.
{-# INLINE boundedWrite #-}
fixedWrite :: Int -> Poke -> Write
fixedWrite = Write
-}

write :: Int -> (a -> Poke) -> Write a
write = Write

exactWrite :: Int -> (a -> Ptr Word8 -> IO ()) -> Write a
exactWrite size io = Write size (pokeN size . io)

-- | Write a storable value.
{-# INLINE writeStorable #-}
writeStorable :: forall a. Storable a => Write a
writeStorable = 
    exactWrite (sizeOf (undefined :: a)) (\x op -> poke (castPtr op) x)


{-

-- | A write of a bounded number of bytes.
--
-- When defining a function @write :: a -> Write@ for some @a@, then it is
-- important to ensure that the bound on the number of bytes written is
-- data-independent. Formally, 
--
--  @ forall x y. getBound (write x) = getBound (write y) @
--
-- The idea is that this data-independent bound is specified such that the
-- compiler can optimize the check, if there are enough free bytes in the buffer,
-- to a single subtraction between the pointer to the next free byte and the
-- pointer to the end of the buffer with this constant bound of the maximal
-- number of bytes to be written.
--
data Write = Write {-# UNPACK #-} !Int Poke

-- | Extract the 'Poke' action of a write.
{-# INLINE getPoke #-}
getPoke :: Write -> Poke
getPoke (Write _ wio) = wio

-- | Run the 'Poke' action of a write.
{-# INLINE runWrite #-}
runWrite :: Write -> Ptr Word8 -> IO (Ptr Word8)
runWrite = runPoke . getPoke

-- | Extract the maximal number of bytes that this write could write.
{-# INLINE getBound #-}
getBound :: Write -> Int
getBound (Write bound _) = bound

-- | Extract the maximal number of bytes that this write could write in any
-- case. Assumes that the bound of the write is data-independent.
{-# INLINE getBound' #-}
getBound' :: String             -- ^ Name of caller: for debugging purposes.
          -> StaticWrite a 
          -> Int
getBound' msg write =
    getBound $ write $ error $ 
    "getBound' called from " ++ msg ++ ": write bound is not data-independent."



------------------------------------------------------------------------------
-- Statically Bounded Writes
------------------------------------------------------------------------------

-- | A @StaticWrite a@ is a function @w :: a -> Write@ such that the number of
-- bytes written by @w x@ can be statically bounded for any value @x@; i.e.,
-- the bound of @w x@ is independent of @x@. Hence,
--
-- > getBound (w undefined) = maxBytesWritten
--
-- Statically bounded writes are used to abstract over writing a bounded number
-- of bytes to memory, while guaranteeing that the bound checks can be done
-- with a statically known bound. 
--
-- Note that not all functions @a -> Write@ are statically bounded writes. For
-- example, @writeAscii7@ defined as follows
--
-- > writeAscii7 :: Word8 -> Write
-- > writeAscii7 w | w < 128   = writeWord8
-- >               | otherwise = mempty
--
-- is no @StaticWrite Word8@. Written as it is, the resulting bound is
-- dependent on the argument. The combinators given below simplify the
-- combination of @StaticWrite@'s to form new @StaticWrite@'s that again
-- use a data inpependent bound combination.
--
-- Using the @writeIf@ combinator, we can implement the above @writeAscii7@
-- function such that it is a statically bounded write.
--
-- > writeAscii7' :: StaticWrite Word8
-- > writeAscii7' = writeIf (<128) writeWord8 (const mempty)
type StaticWrite a = a -> Write

-- | @staticBound w@ is the maximal number of bytes written by @w x@ for any
-- value @x@.
--
-- > staticBound w = getBound $ (w (error "staticBound: data-dependent bound"))
--
{-# INLINE staticBound #-}
staticBound :: StaticWrite a -> Int
staticBound w = getBound $ (w (error "staticBound: data-dependent bound"))

maxStaticBound :: StaticWrite a -> StaticWrite b -> Int
maxStaticBound w1 w2 = max (staticBound w1) (staticBound w2)


-- | Compare the value to a test value and use the first write action for the
-- equal case and the second write action for the non-equal case.
{-# INLINE writeEq #-}
writeEq :: Eq a => a -> StaticWrite a -> StaticWrite a -> StaticWrite a
writeEq test = writeIf (test ==)
 
--   FIXME: Better name required!
{-# INLINE writeOrdering #-}
writeOrdering :: (a -> Ordering) 
              -> StaticWrite a   -- ^ @StaticWrite@ for case 'LT'
              -> StaticWrite a   -- ^ @StaticWrite@ for case 'EQ'
              -> StaticWrite a   -- ^ @StaticWrite@ for case 'GT'
              -> StaticWrite a
writeOrdering ord wLT wEQ wGT x = 
    boundedWrite bound (case ord x of LT -> getPoke $ wLT x; 
                                      EQ -> getPoke $ wEQ x; 
                                      GT -> getPoke $ wGT x)
  where
    bound = max (staticBound wLT) (maxStaticBound wEQ wGT)

-- | A write combinator useful to build decision trees for deciding what value
-- to write with a constant bound on the maximal number of bytes written.
{-# INLINE writeOrd #-}
writeOrd :: Ord a 
       => a
       -> StaticWrite a -> StaticWrite a -> StaticWrite a
       -> StaticWrite a
writeOrd test = writeOrdering (`compare` test)

-}
