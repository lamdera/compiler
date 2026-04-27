{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE UnboxedTuples #-}
module Lamdera.Parse.Extra
  ( fromByteStringWithContext
  , startsWith
  )
  where


import qualified Data.ByteString.Internal as B
import Data.Either (isRight)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import qualified Parse.Primitives as P


fromByteStringWithContext :: P.Parser x a -> (P.Cursor -> x) -> B.ByteString -> Either (B.ByteString, x, B.ByteString) (B.ByteString, a, B.ByteString)
fromByteStringWithContext parser toEnd src =
  let
    parserWithContext =
      do  value <- specializeAtPos (specializer src) parser
          withContext src value <$> getOffset

    toEndWithContext row col =
      specializer src (toEnd row col) row col
  in
  fromByteStringIgnoringRest parserWithContext toEndWithContext src


specializeAtPos :: (x -> P.Cursor -> y) -> P.Parser x a -> P.Parser y a
specializeAtPos addContext (P.Parser parser) =
  P.Parser $ \state cok eok cerr eerr ->
    let
      cerr' r c tx = cerr r c (addContext (tx r c))
      eerr' r c tx = eerr r c (addContext (tx r c))
    in
    parser state cok eok cerr' eerr'


specializer :: B.ByteString -> value -> P.Cursor -> (B.ByteString, value, B.ByteString)
specializer src value row col =
  withContext src value
    $ either id id
    $ fromByteStringIgnoringRest (toOffset row col) (\_ _ -> 0) src


withContext :: B.ByteString -> value -> Int -> (B.ByteString, value, B.ByteString)
withContext (B.PS fptr _ length) value offset =
  (B.fromForeignPtr fptr 0 offset, value, B.fromForeignPtr fptr offset (length - offset))


getOffset :: P.Parser x Int
getOffset =
  P.Parser $ \state@(P.State src pos _ _ _ _) _ eok _ _ ->
    eok (minusPtr pos (unsafeForeignPtrToPtr src)) state


toOffset :: P.Cursor -> P.Parser x Int
toOffset targetRow targetCol =
  P.Parser $ \_ (P.State pos end indent cursor) cok _ _ _ ->
    let
      (# newPos, newRow, newCol #) = moveTo targetRow targetCol pos end cursor
    in
    cok (minusPtr newPos (unsafeForeignPtrToPtr src)) (P.State newPos end indent newRow newCol)


moveTo :: P.Cursor -> Ptr Word8 -> Ptr Word8 -> P.Cursor -> (# Ptr Word8, P.Cursor #)
moveTo targetRow targetCol pos end row col =
  if pos >= end || row > targetRow || row == targetRow && col >= targetCol then
    (# pos, row, col #)

  else
    case unsafeIndex pos of
      0x0A {- \n -} ->
        moveTo targetRow targetCol (plusPtr pos 1) end (row + 1) 1

      _ ->
        moveTo targetRow targetCol (plusPtr pos 1) end row (col + 1)


unsafeIndex :: Ptr Word8 -> Word8
unsafeIndex ptr =
  B.accursedUnutterablePerformIO (peek ptr)


startsWith :: P.Parser x a -> B.ByteString -> Bool
startsWith parser =
  isRight . fromByteStringIgnoringRest (P.specialize (\_ _ _ -> ()) parser) (\_ _ -> ())


fromByteStringIgnoringRest :: P.Parser x a -> (P.Cursor -> x) -> B.ByteString -> Either x a
fromByteStringIgnoringRest = P.fromByteString . stopAfter


stopAfter :: P.Parser x a -> P.Parser x a
stopAfter = (<* ignoreRest)


ignoreRest :: P.Parser x ()
ignoreRest =
  P.Parser $ \_ (P.State pos _ indent cursor) _ eok _ _ ->
    -- set end to current pos to avoid errors in P.fromByteString when pos is not at the end of input
    eok () (P.State pos pos indent cursor)
