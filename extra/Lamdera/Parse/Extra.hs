{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash #-}
module Lamdera.Parse.Extra
  ( fromByteStringWithContext
  , startsWith
  )
  where


import qualified Bytes
import qualified Data.ByteString.Internal as B
import Data.Either (isRight)
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Exts (isTrue#)
import GHC.Int (Int(..))
import GHC.Prim

import qualified Parse.Primitives as P


fromByteStringWithContext :: P.Parser x a -> B.ByteString -> IO (Either (B.ByteString, x, B.ByteString) (B.ByteString, a, B.ByteString))
fromByteStringWithContext parser src =
  fromByteStringIgnoringRestHelp (toOkWithContext src) (toErrWithContext src) parser src


toOkWithContext :: B.ByteString -> a -> P.State -> IO (Either (B.ByteString, x, B.ByteString) (B.ByteString, a, B.ByteString))
toOkWithContext src value state =
  return $ Right $ withContext src value (offsetFromState src state)


toErrWithContext :: B.ByteString -> P.Cursor -> (P.Cursor -> x) -> IO (Either (B.ByteString, x, B.ByteString) (B.ByteString, a, B.ByteString))
toErrWithContext src cursor toError =
  return $ Left $ withContext src (toError cursor) (offsetFromCursor src cursor)


withContext :: B.ByteString -> value -> Int -> (B.ByteString, value, B.ByteString)
withContext src value offset =
  (lowLevelTake offset src, value, lowLevelDrop offset src)


lowLevelTake :: Int -> B.ByteString -> B.ByteString
lowLevelTake (I# n) bs@(B.BS (ForeignPtr start fpc) (I# len))
  | isTrue# (n <=# 0#)   = B.empty
  | isTrue# (n >=# len)  = bs
  | otherwise            = B.BS (ForeignPtr start fpc) (I# n)


lowLevelDrop :: Int -> B.ByteString -> B.ByteString
lowLevelDrop (I# n) bs@(B.BS (ForeignPtr start fpc) (I# len))
  | isTrue# (n <=# 0#)   = bs
  | isTrue# (n >=# len)  = B.empty
  | otherwise            = B.BS (ForeignPtr (plusAddr# start n) fpc) (I# (len -# n))


offsetFromState :: B.ByteString -> P.State -> Int
offsetFromState (B.BS (ForeignPtr start _) _) (P.State pos _ _ _) =
  I# (minusAddr# pos start)


offsetFromCursor :: B.ByteString -> P.Cursor -> Int
offsetFromCursor (B.BS (ForeignPtr start _) (I# len)) target =
  let
    end = plusAddr# start len

    go pos cur
      | isTrue# (geWord64# cur target) = minusAddr# pos start
      | P.notLtAddr pos end = len
      | otherwise =
          case indexWord8OffAddr# pos 0# of
            0x0A#Word8 {- \n -} ->
              go (plusAddr# pos 1#) (P.newline cur)

            word ->
              let !newPos = P.skipUtf8 pos end word in
              if P.ltAddr pos newPos
              then go newPos (P.slide cur 1#Word64)
              else minusAddr# pos start
  in
  I# (go start 0#Word64)


startsWith :: P.Parser x a -> B.ByteString -> IO Bool
startsWith parser src =
  isRight <$> fromByteStringIgnoringRest parser src


fromByteStringIgnoringRest :: P.Parser x a -> B.ByteString -> IO (Either x a)
fromByteStringIgnoringRest = fromByteStringIgnoringRestHelp toOk toErr


fromByteStringIgnoringRestHelp :: (a -> P.State -> IO b) -> (P.Cursor -> (P.Cursor -> x) -> IO b) -> P.Parser x a -> B.ByteString -> IO b
fromByteStringIgnoringRestHelp toOk' toErr' (P.Parser parser) (B.BS (ForeignPtr pos fpc) (I# len)) =
  do  !result <- parser fpc state toOk' toOk' toErr' toErr'
      Bytes.touch fpc result
  where
    state = P.State pos (plusAddr# pos len) 0#Word32 0#Word64


toOk :: a -> P.State -> IO (Either x a)
toOk a _ =
  return (Right a)


toErr :: P.Cursor -> (P.Cursor -> x) -> IO (Either x a)
toErr cur toError =
  return (Left (toError cur))
