{-# LANGUAGE BangPatterns #-}
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

import Data.Bits

import Foreign

import System.IO.Write
import System.IO.Write.Internal

import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S

import Criterion.Main

------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

nRepl :: Int
nRepl = 100000

{-# NOINLINE word8s #-}
word8s :: [Word8]
word8s = map fromIntegral $ [(0::Int)..]

{-# NOINLINE word16s #-}
word16s :: [Word16]
word16s = map fromIntegral $ [(0::Int)..]

main :: IO ()
main = defaultMain 
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

{-# NOINLINE benchHexWord8sLower #-}
benchHexWord8sLower :: [Word8] -> Int
benchHexWord8sLower = S.length . toByteStringList nRepl hexWord8Lower

{-# NOINLINE benchHexWord16sLower #-}
benchHexWord16sLower :: [Word16] -> Int
benchHexWord16sLower = S.length . toByteStringList nRepl hexWord16Lower

-- {-# NOINLINE ebenchHexWord8sLower #-}
-- ebenchHexWord8sLower :: [Word8] -> Int
-- ebenchHexWord8sLower = S.length . etoByteStringList nRepl ehexWord8Lower

{-# NOINLINE tbenchHexWord8sLower #-}
tbenchHexWord8sLower :: [Word8] -> Int
tbenchHexWord8sLower = S.length . toByteStringList nRepl writeBase16

{-# NOINLINE tbenchHexWord16sLower #-}
tbenchHexWord16sLower :: [Word16] -> Int
tbenchHexWord16sLower = S.length . toByteStringList nRepl writeWord16Base16

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
-- Table based encoding
------------------------------------------------------------------------------


{-# INLINE writeBase16 #-}
writeBase16 :: Write Word8
writeBase16 = 
    exactWrite 2 $ \x op -> poke (castPtr op) =<< enc x
  where
    enc :: Word8 -> IO Word16
    enc = peekElemOff (unsafeForeignPtrToPtr encodeTable) . fromIntegral
            
{-# NOINLINE alphabet #-}
alphabet :: S.ByteString
alphabet = S.pack $ map (fromIntegral . fromEnum) $ ['0'..'9'] ++ ['A'..'F']

-- FIXME: Check that the implementation of the lookup table aslo works on
-- big-endian systems.
--
-- !! Does not work. !!
{-# NOINLINE encodeTable #-} 
encodeTable :: ForeignPtr Word16
encodeTable = unsafePerformIO $ do
    fp <- mallocForeignPtrArray (16*16)
    let ix = fromIntegral . S.index alphabet
    withForeignPtr fp $ \p ->
        sequence_ [ pokeElemOff p (j*16+k) ((ix k `shiftL` 8) .|. ix j)
                  | j <- [0..15], k <- [0..15] ]
    return fp

encodeByteString = S.PS (castForeignPtr encodeTable) 0 512

encodeByteString2 = S.PS (castForeignPtr encodeTable2) 0 512

encodeTable2 :: ForeignPtr Word16
encodeTable2 = 
    case table of S.PS fp _ _ -> castForeignPtr fp
  where
    ix    = fromIntegral . S.index alphabet
    table = S.pack $ concat $ [ [ix j, ix k] | j <- [0..15], k <- [0..15] ]


xalphabet :: S.ByteString
xalfaFP :: ForeignPtr Word8
xalphabet@(S.PS xalfaFP _ _) = S.pack $ [65..90] ++ [97..122] ++ [48..57] ++ [43,47]

xencodeTable :: ForeignPtr Word16
xencodeTable = unsafePerformIO $ do
  fp <- mallocForeignPtrArray 4096
  let ix = fromIntegral . S.index xalphabet
  withForeignPtr fp $ \p ->
    sequence_ [ pokeElemOff p (j*64+k) ((ix k `shiftL` 8) .|. ix j)
                | j <- [0..63], k <- [0..63] ]
  return fp

xencodeByteString = S.PS (castForeignPtr xencodeTable) 0 (2*4096)

xencodeByteString2 = S.PS (castForeignPtr xencodeTable2) 0 (2*4096)

xencodeTable2 :: ForeignPtr Word16
xencodeTable2 = 
    case table of S.PS fp _ _ -> castForeignPtr fp
  where
    ix    = fromIntegral . S.index xalphabet
    table = S.pack $ concat $ [ [ix j, ix k] | j <- [0..63], k <- [0..63] ]

{-# INLINE writeWord16Base16 #-}
writeWord16Base16 :: Write Word16
writeWord16Base16 = 
    write2 writeBase16 writeBase16 #.# 
        (\x -> (fromIntegral $ x `shiftR` 8, fromIntegral x))
{-
    exactWrite 4 $ \x op -> do
        poke (castPtr op              ) =<< enc (fromIntegral (x `shiftR` 8))
        poke (castPtr (op `plusPtr` 2)) =<< enc (fromIntegral x             )
  where
    enc :: Word8 -> IO Word16
    enc = peekElemOff (unsafeForeignPtrToPtr encodeTable) . fromIntegral
-}

------------------------------------------------------------------------------
-- Loop based encoding
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
