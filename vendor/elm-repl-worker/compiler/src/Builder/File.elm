{- MANUALLY FORMATTED -}
module Builder.File exposing
  ( Time(..), bTime
  , getTime
  , zeroTime
  , writeBinary
  , readBinary
  , writeUtf8
  , readUtf8
  , writeBuilder
  , writePackage
  , exists
  , remove
  --, removeDir
  , toMillis
  )


import BigInt exposing (BigInt)
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Extra.Data.Binary as B
import Extra.System.Dir as Dir exposing (FilePath)

import Extra.System.IO as IO
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList
import Time as T
import Zip
import Zip.Entry



-- PRIVATE IO


type alias IO c d e f g h v =
  IO.IO (Dir.GlobalState c d e f g h) v



-- TIME


type Time = Time T.Posix


getTime : FilePath -> IO c d e f g h Time
getTime path =
  IO.fmap Time (Dir.getModificationTime path)


toMillis : Time -> Int
toMillis (Time time) =
  T.posixToMillis time


zeroTime : Time
zeroTime =
  Time (T.millisToPosix 0)


bTime : B.Binary Time
bTime =
  B.bin1 bigToTime timeToBig B.bBigInt


bigToTime : BigInt -> Time
bigToTime big =
  case BigInt.divmod big bigTimeFactor of
    Just (div, rem) ->
      Time (T.millisToPosix (B.bigToInt div + if B.bigToInt rem < halfTimeFactor then 0 else 1))
    Nothing ->
      zeroTime


timeToBig : Time -> BigInt
timeToBig (Time time) =
  BigInt.mul (BigInt.fromInt (T.posixToMillis time)) bigTimeFactor


bigTimeFactor : BigInt
bigTimeFactor =
  BigInt.fromInt (2 * halfTimeFactor)


halfTimeFactor : Int
halfTimeFactor =
  500000000



-- BINARY


writeBinary : B.Binary v -> FilePath -> v -> IO c d e f g h ()
writeBinary binA path value =
  let dir = Dir.dropLastName path in
  IO.bind (Dir.createDirectoryIfMissing True dir) <| \_ ->
  B.encodeFile binA path value


readBinary : B.Binary v -> FilePath -> IO c d e f g h (Maybe v)
readBinary binA path =
  IO.bind (Dir.doesFileExist path) <| \pathExists ->
  if pathExists
    then
      IO.bind (B.decodeFileOrFail binA path) <| \result ->
      case result of
        Right a ->
          IO.return (Just a)

        Left (offset, message) ->
          IO.bind (IO.log "readBinary" <|
            [ "+-------------------------------------------------------------------------------"
            , "|  Corrupt File: " ++ Dir.toString path
            , "|   Byte Offset: " ++ String.fromInt offset
            , "|       Message: " ++ message
            , "|"
            , "| Please report this to https://github.com/elm/compiler/issues"
            , "| Trying to continue anyway."
            , "+-------------------------------------------------------------------------------"
            ]) <| \_ ->
          IO.return Nothing
    else
      IO.return Nothing



-- WRITE UTF-8


writeUtf8 : FilePath -> String -> IO c d e f g h ()
writeUtf8 filePath contents =
  Dir.writeFile filePath <| Bytes.Encode.encode <| Bytes.Encode.string contents



-- READ UTF-8


readUtf8 : FilePath -> IO c d e f g h String
readUtf8 path =
  Dir.readFile path |> IO.fmap (Maybe.andThen bytesToString >> Maybe.withDefault "")


bytesToString : Bytes -> Maybe String
bytesToString bytes =
  Bytes.Decode.decode (Bytes.Decode.string (Bytes.width bytes)) bytes



-- WRITE BUILDER


writeBuilder : FilePath -> String -> IO c d e f g h ()
writeBuilder =
  writeUtf8



-- WRITE PACKAGE


writePackage : FilePath -> Zip.Zip -> IO c d e f g h ()
writePackage destination archive =
  case Zip.entries archive of
    [] ->
      IO.return ()

    entry::entries ->
      let root = String.length (Zip.Entry.path entry) in
      MList.sortOn Zip.Entry.path entries
        |> MList.mapM_ IO.return IO.bind (writeEntry destination root)


writeEntry : FilePath -> Int -> Zip.Entry.Entry -> IO c d e f g h ()
writeEntry destination root entry =
  let
    path = String.dropLeft root (Zip.Entry.path entry)
  in
  if String.startsWith "src/" path
    || path == "LICENSE"
    || path == "README.md"
    || path == "elm.json"
  then
    if not (String.isEmpty path) && String.endsWith "/" path
    then Dir.createDirectoryIfMissing True (Dir.combine destination (Dir.fromString path))
    else
      case Zip.Entry.toBytes entry of
        Err _ ->
          IO.return ()

        Ok bytes ->
          Dir.writeFile (Dir.combine destination (Dir.fromString path)) bytes
  else
    IO.return ()



-- EXISTS


exists : FilePath -> IO c d e f g h Bool
exists path =
  Dir.doesFileExist path



-- REMOVE FILES


remove : FilePath -> IO c d e f g h ()
remove path =
  IO.bind (Dir.doesFileExist path) <| \exists_ ->
    if exists_
      then Dir.removeFile path
      else IO.return ()
