-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Testing the correctness of writes.
-- 
module System.IO.Write.Test (
  -- * Testing writes
    WriteFailure
  , evalWrite
  , showWrite
  , testWrite
  , cmpWrite
  , cmpWrite_
  , cmpWriteErr

  ) where

import Data.Maybe
import Foreign
import Numeric (showHex)

import System.IO.Write.Internal
import qualified System.IO.Write.Internal.Region as R

------------------------------------------------------------------------------
-- Testing Writes
------------------------------------------------------------------------------

-- Representing failures
------------------------

-- | A failure of a 'Write'.
data WriteFailure = WriteFailure  String  WriteResult  WriteResult
       deriving( Eq )

type WriteResult = ( [Word8]         -- full list
                   , [[Word8]]       -- split list
                   , [Ptr Word8] )   -- in-write pointers

instance Show WriteFailure where
    show (WriteFailure cause res1 res2) = unlines $
            [ ""
            , "Write violated post-condition: " ++ cause ++ "!"
            ] ++
            (map ("  " ++) $ lines $ unlines
                [ "String based result comparison:"
                , showWriteResult stringLine 1 res1
                , showWriteResult stringLine 2 res2
                , "Hex based result comparison:"
                , showWriteResult hexLine 1 res1
                , showWriteResult hexLine 2 res2 
                ] )
      where
        hexLine = concatMap (\x -> pad2 $ showHex x "")
        pad2 [ ] = '0':'0':[]
        pad2 [x] = '0':x  :[]
        pad2 xs  = xs

        stringLine = map (toEnum . fromIntegral)

        showWriteResult line i (full, splits, ptrs) =
            unlines $ zipWith (++) names 
                    $ map (quotes . line) (full : splits) ++ [ppPtrs]
          where
            names = [ show (i::Int) ++ " total result:   "
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

 
-- Execution a write and testing its invariants
-----------------------------------------------

-- | Execute a 'Write' and return the written list of bytes.
evalWrite :: Write a -> a -> [Word8]
evalWrite w x = case testWrite w x of
    Left err  -> error $ "evalWrite: " ++ show err
    Right res -> res

-- | Execute a 'Write' and return the written list of bytes interpreted as
-- Unicode codepoints.
showWrite :: Write a -> a -> [Char]
showWrite w = map (toEnum . fromEnum) . evalWrite w

-- | Execute a 'Write' twice and check that all post-conditions hold and the
-- written values are identical. In case of success, a list of the written
-- bytes is returned.
testWrite :: Write a -> a -> Either WriteFailure [Word8]
testWrite = testWriteWith (5, 11)

testWriteWith :: (Int, Int) -> Write a -> a -> Either WriteFailure [Word8]
testWriteWith (slackF, slackB) w x = unsafePerformIO $ do
    res1@(xs1, _, _) <- execWrite (replicate (slackF + slackB + bound) 40)
    res2             <- execWrite (invert xs1)
    return $ check res1 res2
  where
    bound = getBound w

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

    -- list-to-memory, run write, memory-to-list, report results
    execWrite ys0 = do
      r@(buf, size) <- R.fromList ys0
      withForeignPtr buf $ \sp -> do
          let ep      = sp `plusPtr` size
              op      = sp `plusPtr` slackF
              opBound = op `plusPtr` bound
          op' <- runWrite w x op
          ys1 <- R.toList r
          touchForeignPtr buf
          -- cut the written list into: front slack, written, reserved, back slack
          case splitAt (op `minusPtr` sp) ys1 of
              (frontSlack, ys2) -> case splitAt (op' `minusPtr` op) ys2 of
                  (written, ys3) -> case splitAt (opBound `minusPtr` op') ys3 of
                      (reserved, backSlack) -> return $
                          (ys1, [frontSlack, written, reserved, backSlack], [sp, op, op', opBound, ep])

-- | Compare a 'Write' against a reference implementation. @cmpWrite f w x@
-- returns 'Nothing' iff the write @w@ and the function @f@ yield the same
-- result when applied to @x@.
cmpWrite :: (a -> [Word8]) -> Write a -> a 
         -> Maybe (a, [Word8], Either WriteFailure [Word8])
cmpWrite f w x 
  | result == Right (f x) = Nothing
  | otherwise             = Just (x, f x, result)
  where
    result = testWrite w x

-- | Like 'cmpWrite', but return only whether the write yielded the same result
-- as the reference implementation.
cmpWrite_ :: Show a => (a -> [Word8]) -> Write a -> a -> Bool
cmpWrite_ f w = isNothing . cmpWrite f w

-- | Like 'cmpWrite', but return an error using @error . show@. This is a
-- convenient way to get a QuickCheck test to output debug information about
-- what went wrong.
cmpWriteErr :: Show a => (a -> [Word8]) -> Write a -> a -> Bool
cmpWriteErr f w = maybe True (error . show) . cmpWrite f w


