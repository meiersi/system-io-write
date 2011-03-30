{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmark 'Write' combinators to build hexadecimal encodings for enumerable
-- types given a concrete character encoding.
module Hex where

import Control.Applicative 

import Data.Bits
import Data.Char

import Foreign

import Numeric

import System.IO.Write
import System.IO.Write.Internal

import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S

import Criterion.Main

import Test.Framework
import Test.Framework.Providers.QuickCheck2
-- import Test.Framework.Providers.HUnit
-- import Test.QuickCheck
-- import Test.HUnit hiding (Test)


------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

nRepl :: Int
nRepl = 1000

{-# NOINLINE word8s #-}
word8s :: [Word8]
word8s = map fromIntegral $ [(0::Word64)..]

{-# NOINLINE word16s #-}
word16s :: [Word16]
word16s = map fromIntegral $ [(0::Word64)..]

{-# NOINLINE word32s #-}
word32s :: [Word32]
word32s = map fromIntegral $ [(0::Word64)..]

{-# NOINLINE word64s #-}
word64s :: [Word64]
word64s = map fromIntegral $ [(0::Word64)..]

main :: IO ()
main = Criterion.Main.defaultMain 
    [ 
      benchmark "no leading zeros - Word8"  hexNoLead word8s
    , benchmark "no leading zeros - Word16" hexNoLead word16s
    , benchmark "no leading zeros - Word32" hexNoLead word32s
    , benchmark "no leading zeros - Word64" hexNoLead word64s

    , benchmark "fixed (hand unrolled) - Word8"  hex word8s
    , benchmark "fixed (hand unrolled) - Word16" hex word16s
    , benchmark "fixed (hand unrolled) - Word32" hex word32s
    , benchmark "fixed (hand unrolled) - Word64" hex word64s

      {-
      benchmark "fixed (loop) - Word8"  (writeBase16Bytes 1) word8s
    , benchmark "fixed (loop) - Word16" (writeBase16Bytes 2) word16s
    , benchmark "fixed (loop) - Word32" (writeBase16Bytes 4) word32s
    , benchmark "fixed (loop) - Word64" (writeBase16Bytes 8) word64s
    -}

    ]
    {-
    [ bench ("thexWord16sLower (" ++ show nRepl ++ ")") $
        whnf tbenchHexWord16sLower word16s
    , bench ("hexWord16sLower (" ++ show nRepl ++ ")") $
        whnf benchHexWord16sLower word16s
    , bench ("thexWord8sLower (" ++ show nRepl ++ ")") $
        whnf tbenchHexWord8sLower word8s
    , bench ("hexWord8sLower (" ++ show nRepl ++ ")") $
        whnf benchHexWord8sLower word8s
    , bench ("lhexWord8sLower (" ++ show nRepl ++ ")") $
        whnf lbenchHexWord8sLower word8s
    -- , bench ("ehexWord8sLower (" ++ show nRepl ++ ")") $
        -- whnf ebenchHexWord8sLower word8s
    ]
    -}


{-# INLINE benchmark #-}
benchmark :: String -> Write a -> [a] -> Benchmark
benchmark name w xs = 
  bench (name ++" (" ++ show nRepl ++ ")") $ 
    whnf (S.length . toByteStringList nRepl w) xs

{-# NOINLINE benchHexWord8sLower #-}
benchHexWord8sLower :: [Word8] -> Int
benchHexWord8sLower = S.length . toByteStringList nRepl hexWord8Lower

{-# NOINLINE benchHexWord16sLower #-}
benchHexWord16sLower :: [Word16] -> Int
benchHexWord16sLower = S.length . toByteStringList nRepl hexWord16Lower

-- {-# NOINLINE ebenchHexWord8sLower #-}
-- ebenchHexWord8sLower :: [Word8] -> Int
-- ebenchHexWord8sLower = S.length . etoByteStringList nRepl ehexWord8Lower

-- {-# NOINLINE tbenchHexWord8sLower #-}
-- tbenchHexWord8sLower :: [Word8] -> Int
-- tbenchHexWord8sLower = S.length . toByteStringList nRepl writeWord8Base16

-- {-# NOINLINE tbenchHexWord16sLower #-}
-- tbenchHexWord16sLower :: [Word16] -> Int
-- tbenchHexWord16sLower = S.length . toByteStringList nRepl writeWord16Base16

{-# NOINLINE lbenchHexWord8sLower #-}
lbenchHexWord8sLower :: [Word8] -> Int
lbenchHexWord8sLower = S.length . toByteStringList nRepl lhexWord8Lower


{-
{-# NOINLINE benchHexWord8Lower #-}
benchHexWord8Lower :: Word8 -> Int
benchHexWord8Lower = S.length . toByteStringReplicated nRepl hexWord8Lower

{-# NOINLINE ebenchHexWord8Lower #-}
ebenchHexWord8Lower :: Word8 -> Int
ebenchHexWord8Lower = S.length . etoByteStringReplicated nRepl ehexWord8Lower

{-# NOINLINE tbenchHexWord8Lower #-}
tbenchHexWord8Lower :: Word8 -> Int
tbenchHexWord8Lower = S.length . toByteStringReplicated nRepl writeBase16

{-# NOINLINE lbenchHexWord8Lower #-}
lbenchHexWord8Lower :: Word8 -> Int
lbenchHexWord8Lower = S.length . toByteStringReplicated nRepl lhexWord8Lower
-}

------------------------------------------------------------------------------
-- Combinators
------------------------------------------------------------------------------

-- | A value from @0@ to @15@ represented using a machine-dependent integer as
-- this is assumed to be the fastest representation of integers.
newtype Nibble = Nibble { getNibble :: Word8 }

{-# INLINE nibble #-}
nibble :: Word8 -> Nibble
nibble = Nibble . (0xf .&.)

infixl 4 #.#

class WriteC w where
    (#.#) :: w a -> (b -> a) -> w b

instance WriteC Write where
    (#.#) = flip comapWrite

{-# INLINE nibbleUtf8 #-}
nibbleUtf8 :: Word8 -> Write Nibble
nibbleUtf8 a =
    writeWord8 #.# (fromIntegral . nibbleToChr)
  where
    nibbleToChr (Nibble x)
      | x < 10    = 48       + x
      | otherwise = (a - 10) + x

nibbleUtf8Upper, nibbleUtf8Lower :: Write Nibble
nibbleUtf8Upper = nibbleUtf8 65
nibbleUtf8Lower = nibbleUtf8 97

write2times :: Write a -> Write (a, a)
write2times w = write2 w w

{-# INLINE hexWord8 #-}
hexWord8 :: Write Nibble -> Write Word8
hexWord8 w = write2times (w #.# nibble) #.# (\x -> (x `shiftR` 4, x))

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

{-# INLINE hexWord8Lower #-}
hexWord8Lower, hexWord8Upper :: Write Word8
hexWord8Lower = hexWord8 nibbleUtf8Lower
hexWord8Upper = hexWord8 nibbleUtf8Upper

{-# INLINE hexWord16Lower #-}
hexWord16Lower :: Write Word16
hexWord16Lower = 
    write2 hexWord8Lower hexWord8Lower #.# 
        (\x -> (fromIntegral $ x `shiftR` 8, fromIntegral x))

{-
test :: Word8 -> S.ByteString
test x = toByteStringReplicated 3 (write2 hexWord8Lower hexWord8Upper) (x, x)
-}


data EWrite a = EWrite { ebound :: {-# UNPACK #-} !Int
                       , eio    :: (a -> Ptr Word8 -> IO ()) }

{-# INLINE ewriteWord8 #-}
ewriteWord8 :: EWrite Word8
ewriteWord8 = EWrite 1 (\x op -> poke op x)

{-# INLINE comapEWrite #-}
comapEWrite :: (b -> a) -> EWrite a -> EWrite b
comapEWrite f w = EWrite (ebound w) (eio w . f)

instance WriteC EWrite where
    {-# INLINE (#.#) #-}
    (#.#) = flip comapEWrite

{-# INLINE enibbleUtf8 #-}
enibbleUtf8 :: Word8 -> EWrite Nibble
enibbleUtf8 a =
    ewriteWord8 #.# (fromIntegral . nibbleToChr)
  where
    nibbleToChr (Nibble x)
      | x < 10    = 48       + x
      | otherwise = (a - 10) + x

{-# INLINE enibbleUtf8Lower #-}
enibbleUtf8Upper, enibbleUtf8Lower :: EWrite Nibble
enibbleUtf8Upper = enibbleUtf8 65
enibbleUtf8Lower = enibbleUtf8 97

{-# INLINE ewrite2#-}
ewrite2 :: EWrite a -> EWrite b -> EWrite (a, b)
ewrite2 w1 w2 = 
    EWrite (ebound w1 + ebound w2)
           (\(x,y) op -> eio w1 x op >> eio w2 y (op `plusPtr` ebound w1))

{-# INLINE ewrite2times #-}
ewrite2times :: EWrite a -> EWrite (a, a)
ewrite2times w = ewrite2 w w
    {-
    EWrite (2 * b)
           (\(x,y) op -> io x op >> io y (op `plusPtr` b))
  where
    !b  = ebound w
    !io = eio w
    -}

{-# INLINE ehexWord8 #-}
ehexWord8 :: EWrite Nibble -> EWrite Nibble -> EWrite Word8
ehexWord8 w1 w2 = 
    ewrite2 w1 w2 #.# (\x -> (nibble $ x `shiftR` 4, nibble x))

{-# INLINE ehexWord8Lower #-}
ehexWord8Lower :: EWrite Word8
ehexWord8Lower = ehexWord8 enibbleUtf8Lower enibbleUtf8Lower
-- ehexWord8Upper = ehexWord8 enibbleUtf8Upper

{-# INLINE etoByteStringReplicated #-}
etoByteStringReplicated :: Int -> EWrite a -> a -> S.ByteString
etoByteStringReplicated n0 w x = S.inlinePerformIO $ do
    fpbuf <- S.mallocByteString size
    let op0      = unsafeForeignPtrToPtr fpbuf
        ep0      = op0 `plusPtr` size 
        loop op
          | op < ep0  = do eio w x op 
                           loop (op `plusPtr` ebound w)
          | otherwise = return ()
    loop op0
    return $ S.PS fpbuf 0 size
  where
    size = n0 * ebound w

------------------------------------------------------------------------------
-- Loop based encoding of individual nibbles
------------------------------------------------------------------------------

{-# INLINE writeNibbles #-}
writeNibbles :: Write Nibble -> Word8 -> Write Word8
writeNibbles w n0 =
    write (fromIntegral n0 * writeBound w) $ \x -> pokeIO $ \op0 -> do
        let loop !n !op 
              | n < 0     = do return op
              | otherwise = do
                    let x' = nibble $ x `shiftR` (4 * n)
                    loop (n - 1) =<< runWrite w x' op
        loop (fromIntegral $ n0 - 1) op0

{-# INLINE lhexWord8Lower #-}
lhexWord8Lower :: Write Word8
lhexWord8Lower = writeNibbles nibbleUtf8Lower 2 #.# fromIntegral


------------------------------------------------------------------------------
-- Table based encoding
------------------------------------------------------------------------------

upperAlphabet :: S.ByteString
upperAlphabet = S.pack $ map (fromIntegral . fromEnum) $ ['0'..'9'] ++ ['A'..'F']

lowerAlphabet :: S.ByteString
lowerAlphabet = S.pack $ map (fromIntegral . fromEnum) $ ['0'..'9'] ++ ['a'..'f']

encodingTable :: S.ByteString -> EncodingTable
encodingTable alphabet =
    case table of S.PS fp _ _ -> EncodingTable fp
  where
    ix    = fromIntegral . S.index alphabet
    table = S.pack $ concat $ [ [ix j, ix k] | j <- [0..15], k <- [0..15] ]

newtype EncodingTable = EncodingTable (ForeignPtr Word8)

{-# NOINLINE upperTable #-}
upperTable :: EncodingTable
upperTable = encodingTable upperAlphabet

{-# NOINLINE lowerTable #-}
lowerTable :: EncodingTable
lowerTable = encodingTable lowerAlphabet

{-# INLINE encode4_as_8 #-}
encode4_as_8 :: EncodingTable -> Word8 -> IO Word8
encode4_as_8 (EncodingTable table) x = 
    peek8 (2 * fromIntegral x + 1)
  where
    peek8 :: Int -> IO Word8
    peek8 = peekElemOff (unsafeForeignPtrToPtr table)

{-# INLINE encode8_as_16h #-}
encode8_as_16h :: EncodingTable -> Word8 -> IO Word16
encode8_as_16h (EncodingTable table) = 
    peekElemOff (castPtr $ unsafeForeignPtrToPtr table) . fromIntegral

{-# INLINE encode8_as_8_8 #-}
encode8_as_8_8 :: EncodingTable -> Word8 -> IO (Word8, Word8)
encode8_as_8_8 (EncodingTable table) x = 
    (,) <$> peek8 i <*> peek8 (i +1)
  where
    i = 2 * fromIntegral x
    peek8 :: Int -> IO Word8
    peek8 = peekElemOff (unsafeForeignPtrToPtr table)



------------------------------------------------------------------------------
-- Class based interface
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
    write2 (word8Base16 table) (word8Base16 table) #.# 
           (\x -> let {-# INLINE byte #-}
                      byte n = fromIntegral $ x `shiftR` (n * 8) in
                  (byte 1, byte 0) 
            )

{-# INLINE word32Base16 #-}
word32Base16 :: EncodingTable -> Write Word32
word32Base16 table = 
    write4 (word8Base16 table) (word8Base16 table) 
           (word8Base16 table) (word8Base16 table) #.# 
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
           (word8Base16 table) (word8Base16 table) #.# 
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

    f 0  op0 = do runWrite writeWord8 (fromIntegral $ fromEnum '0') op0
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

      
------------------------------------------------------------------------------
-- Testing Writes
------------------------------------------------------------------------------

type WriteResult = ( [Word8]         -- full list
                   , [[Word8]]       -- split list
                   , [Ptr Word8] )   -- in-write pointers
data WriteFailure = WriteFailure  String  WriteResult  WriteResult
       deriving( Eq )

showWriteResult :: ([Word8] -> String) -> Int -> WriteResult -> String
showWriteResult line i (full, splits, ptrs) =
    unlines $ zipWith (++) names 
            $ map (quotes . line) (full : splits) ++ [ppPtrs]
  where
    names = [ show i ++ " total result:   "
            , "  front slack:    "
            , "  write result:   "
            , "  reserved space: "
            , "  back slack:     "
            , "  pointers/diffs: "
            ]
    quotes xs = "'" ++ xs ++ "'"
    ppPtrs = show (head ptrs) ++ do
        (p1,p2) <- zip ptrs (tail ptrs)
        "|" ++ show (p2 `minusPtr` p1) ++ "|" ++ show p2

 
instance Show WriteFailure where
    show (WriteFailure cause res1 res2) = unlines $
            [ ""
            , "Write violated post-condition: " ++ cause ++ "!"
            ] ++
            (map ("  " ++) $ lines $ unlines
                [ "String based result comparison:"
                , showWriteResult stringLine 1 res1, showWriteResult stringLine 2 res2
                , "Hex based result comparison:"
                , showWriteResult hexLine 1 res1, showWriteResult hexLine 2 res2 
                ] )
      where
        hexLine = concatMap (\x -> pad2 $ showHex x "")
        pad2 [ ] = '0':'0':[]
        pad2 [x] = '0':x  :[]
        pad2 xs  = xs

        stringLine = map (toEnum . fromIntegral)

testWrite :: Write a -> a -> Either WriteFailure [Word8]
testWrite = testWriteWith (5, 11)

testWriteWith :: (Int, Int) -> Write a -> a -> Either WriteFailure [Word8]
testWriteWith (slackF, slackB) w x = unsafePerformIO $ do
    res1@(xs1, _, _) <- execWrite (replicate (slackF + slackB + bound) 40)
    res2             <- execWrite (invert xs1)
    return $ check res1 res2
  where
    bound = writeBound w

    invert = map complement

    check res1@(xs1, [frontSlack1, written1, reserved1, backSlack1], ptrs1)
          res2@(_  , [frontSlack2, written2, reserved2, backSlack2], ptrs2)
      -- test properties of first write
      | length frontSlack1 /= slackF                = err "front slack length"
      | length backSlack1   /= slackB               = err "back slack length"
      | length xs1 /= slackF + slackB + bound       = err "total length"
      | not (ascending ptrs1)                       = err "pointers 1"
      -- test remaining properties of second write
      | not (ascending ptrs2)                       = err "pointers 2"
      -- compare writes
      | frontSlack1      /= invert frontSlack2      = err "front over-write"
      | backSlack1       /= invert backSlack2       = err "back over-write"
      | written1         /= written2                = err "different writes"
      | length reserved1 /= length reserved2        = err "different reserved lengths"
      | any (\(a,b) -> a /= complement b) untouched = err "different reserved usage"
      | otherwise                                   = Right written1
      where
        (_, untouched) = break (uncurry (/=)) $ zip reserved1 reserved2
        err info = Left (WriteFailure info res1 res2)
        ascending xs = all (uncurry (<=)) $ zip xs (tail xs)
    check _ _ = error "impossible"

    execWrite ys0 = do
      -- list-to-memory, run write, memory-to-list
      (buf, r@(sp, ep)) <- listToRegion ys0
      let op      = sp `plusPtr` slackF
          opBound = op `plusPtr` bound
      op' <- runWrite w x op
      ys1 <- regionToList r
      touchForeignPtr buf
      -- cut the written list into: front slack, written, reserved, back slack
      case splitAt (op `minusPtr` sp) ys1 of
          (frontSlack, ys2) -> case splitAt (op' `minusPtr` op) ys2 of
              (written, ys3) -> case splitAt (opBound `minusPtr` op') ys3 of
                  (reserved, backSlack) -> return $
                      (ys1, [frontSlack, written, reserved, backSlack], [sp, op, op', opBound, ep])


    -- touch the returned foreign ptr for the region to stay valid!
    listToRegion :: [Word8] -> IO (ForeignPtr Word8, (Ptr Word8, Ptr Word8))
    listToRegion ys0 = do
        let n = length ys0
        fpbuf <- S.mallocByteString n
        let sp = unsafeForeignPtrToPtr fpbuf
        ep <- fill ys0 sp
        return (fpbuf, (sp, ep))
      where
        fill []     op = do return op
        fill (y:ys) op = do poke op y
                            fill ys (op `plusPtr` 1)

    regionToList :: (Ptr Word8, Ptr Word8) -> IO [Word8]
    regionToList (sp0, ep0) =
        go sp0
      where
        go sp | sp < ep0  = (:) <$> peek sp <*> go (sp `plusPtr` 1)
              | otherwise = return []


test :: Word8 -> Either WriteFailure [Word8]
test = testWrite writeWord8

wrongWrite :: Write Word8
wrongWrite = 
    write 1 (\_ -> pokeIO $ \op -> 
                poke (op `plusPtr` (1)) (40::Word8) >> return (op `plusPtr` 2))

mainTest :: IO ()
mainTest = Test.Framework.defaultMain $ return $ testGroup "Tests" tests

tests :: [Test]
tests =
    [ testProperty "word8"  prop_word8
    , testProperty "word16BE" $ prop_bigEndian    writeWord16be
    , testProperty "word32BE" $ prop_bigEndian    writeWord32be
    , testProperty "word64BE" $ prop_bigEndian    writeWord64be

    , testProperty "word16LE" $ prop_littleEndian writeWord16le
    , testProperty "word32LE" $ prop_littleEndian writeWord32le
    , testProperty "word64LE" $ prop_littleEndian writeWord64le

    , testProperty "base16LowerNoLead :: Word8"  $ prop_base16NoLead id (base16LowerNoLead :: Write Word8)
    , testProperty "base16LowerNoLead :: Word16" $ prop_base16NoLead id (base16LowerNoLead :: Write Word16)
    , testProperty "base16LowerNoLead :: Word32" $ prop_base16NoLead id (base16LowerNoLead :: Write Word32)
    , testProperty "base16LowerNoLead :: Word64" $ prop_base16NoLead id (base16LowerNoLead :: Write Word64)
    ]

prop_write :: Show a => (a -> [Word8]) -> Write a -> a -> Bool
prop_write f w x 
  | testWrite w x == Right (f x) = True
  | otherwise                    = 
        error $ unlines [show x, show (testWrite w x), show (f x)]

prop_word8 :: Word8 -> Bool
prop_word8 = prop_write (return . fromIntegral) writeWord8 

prop_bigEndian :: (Storable a, Bits a, Integral a) => Write a -> a -> Bool
prop_bigEndian =
    prop_write f
  where
    f x = map (fromIntegral . (x `shiftR`) . (8*)) $ reverse [0..sizeOf x - 1]

prop_littleEndian :: (Storable a, Bits a, Integral a) => Write a -> a -> Bool
prop_littleEndian =
    prop_write f
  where
    f x = map (fromIntegral . (x `shiftR`) . (8*)) $ [0..sizeOf x - 1]

prop_base16NoLead :: Integral a => (Char -> Char) -> Write a -> a -> Bool
prop_base16NoLead conv =
    prop_write f
  where
    f x = map (fromIntegral . ord . conv) $ showHex x ""


