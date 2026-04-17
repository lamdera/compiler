module File
  ( Time
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
  , removeDir
  , getFileTimings
  )
  where


import qualified Codec.Archive.Zip as Zip
import Control.Exception (catch)
import qualified Data.Binary as Binary
import qualified System.IO.Unsafe as Unsafe
import qualified Data.IORef as IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Fixed as Fixed
import qualified Data.List as List
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified Foreign.ForeignPtr as FPtr
import GHC.IO.Exception (IOException, IOErrorType(InvalidArgument))
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.FilePath ((</>))
import qualified System.IO as IO
import System.IO.Error (ioeGetErrorType, annotateIOError, modifyIOError)


import qualified Ext.Common as Ext
import Lamdera ((&), alternativeImplementation)

-- TIME


newtype Time = Time Fixed.Pico
  deriving (Eq, Ord)


getTime :: FilePath -> IO Time
getTime path =
  fmap
    (Time . Time.nominalDiffTimeToSeconds . Time.utcTimeToPOSIXSeconds)
    (Dir.getModificationTime path)


zeroTime :: Time
zeroTime =
  Time 0


instance Binary.Binary Time where
  put (Time time) = Binary.put time
  get = Time <$> Binary.get



-- BINARY


{-# NOINLINE writeNanos #-}
writeNanos :: IORef.IORef Integer
writeNanos = Unsafe.unsafePerformIO (IORef.newIORef 0)

{-# NOINLINE readNanos #-}
readNanos :: IORef.IORef Integer
readNanos = Unsafe.unsafePerformIO (IORef.newIORef 0)

{-# NOINLINE timingEnabled #-}
timingEnabled :: Bool
timingEnabled = Ext.envFlag "LDEBUG_FILE_TIMING"


timeIt :: IORef.IORef Integer -> IO a -> IO a
timeIt ref action =
  if timingEnabled
    then do
      t0 <- Time.getCurrentTime
      result <- action
      t1 <- Time.getCurrentTime
      let dt = Time.diffUTCTime t1 t0
          nanos = round (realToFrac dt * 1e9 :: Double) :: Integer
      IORef.atomicModifyIORef' ref (\acc -> (acc + nanos, ()))
      return result
    else action


-- | Get accumulated write/read times in milliseconds (for reporting).
getFileTimings :: IO (Double, Double)
getFileTimings = do
  w <- IORef.readIORef writeNanos
  r <- IORef.readIORef readNanos
  return (fromIntegral w / 1e6, fromIntegral r / 1e6)


writeBinary :: (Binary.Binary a) => FilePath -> a -> IO ()
writeBinary path value =
  timeIt writeNanos $ do
      let dir = FP.dropFileName path
      Dir.createDirectoryIfMissing True dir
      Binary.encodeFile path value


readBinary :: (Binary.Binary a) => FilePath -> IO (Maybe a)
readBinary path =
  timeIt readNanos $ do
      pathExists <- Dir.doesFileExist path
      if pathExists
        then
          do  result <- Binary.decodeFileOrFail path
              case result of
                Right a ->
                  return (Just a)

                Left (offset, message) ->
                  do  IO.hPutStrLn IO.stderr $ unlines $
                        [ "+-------------------------------------------------------------------------------"
                        , "|  Corrupt File: " ++ path
                        , "|   Byte Offset: " ++ show offset
                        , "|       Message: " ++ message
                        , "|"
                        , "| Please report this to https://github.com/elm/compiler/issues"
                        , "| Trying to continue anyway."
                        , "+-------------------------------------------------------------------------------"
                        ]
                      return Nothing
        else
          return Nothing



-- WRITE UTF-8


writeUtf8 :: FilePath -> BS.ByteString -> IO ()
writeUtf8 path content =
  -- Try solve "resource exhausted (Too many open files)" issues on OS X by using strict IO functions
  alternativeImplementation (BS.writeFile path content) $
  withUtf8 path IO.WriteMode $ \handle ->
    BS.hPut handle content


withUtf8 :: FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withUtf8 path mode callback =
  IO.withFile path mode $ \handle ->
    do  IO.hSetEncoding handle IO.utf8
        callback handle



-- READ UTF-8


readUtf8 :: FilePath -> IO BS.ByteString
readUtf8 path =
  -- Try solve "resource exhausted (Too many open files)" issues on OS X by using strict IO functions
  alternativeImplementation (BS.readFile path) $
  withUtf8 path IO.ReadMode $ \handle ->
    modifyIOError (encodingError path) $
      do  fileSize <- catch (IO.hFileSize handle) useZeroIfNotRegularFile
          let readSize = max 0 (fromIntegral fileSize) + 1
          hGetContentsSizeHint handle readSize (max 255 readSize)


useZeroIfNotRegularFile :: IOException -> IO Integer
useZeroIfNotRegularFile _ =
  return 0


hGetContentsSizeHint :: IO.Handle -> Int -> Int -> IO BS.ByteString
hGetContentsSizeHint handle =
    readChunks []
  where
    readChunks chunks readSize incrementSize =
      do  fp <- BS.mallocByteString readSize
          readCount <- FPtr.withForeignPtr fp $ \buf -> IO.hGetBuf handle buf readSize
          let chunk = BS.PS fp 0 readCount
          if readCount < readSize && readSize > 0
            then return $! BS.concat (reverse (chunk:chunks))
            else readChunks (chunk:chunks) incrementSize (min 32752 (readSize + incrementSize))


encodingError :: FilePath -> IOError -> IOError
encodingError path ioErr =
  case ioeGetErrorType ioErr of
    InvalidArgument ->
      annotateIOError
        (userError "Bad encoding; the file must be valid UTF-8")
        ""
        Nothing
        (Just path)

    _ ->
      ioErr



-- WRITE BUILDER


writeBuilder :: FilePath -> B.Builder -> IO ()
writeBuilder path builder =
  IO.withBinaryFile path IO.WriteMode $ \handle ->
    do  IO.hSetBuffering handle (IO.BlockBuffering Nothing)
        B.hPutBuilder handle builder



-- WRITE PACKAGE


writePackage :: FilePath -> Zip.Archive -> IO ()
writePackage destination archive =
  case Zip.zEntries archive of
    [] ->
      return ()

    entry:entries ->
      do  let root = length (Zip.eRelativePath entry)
          mapM_ (writeEntry destination root) entries


writeEntry :: FilePath -> Int -> Zip.Entry -> IO ()
writeEntry destination root entry =
  let
    path = drop root (Zip.eRelativePath entry)
  in
  if List.isPrefixOf "src/" path
    || path == "LICENSE"
    || path == "README.md"
    || path == "elm.json"
  then
      if not (null path) && last path == '/'
      then Dir.createDirectoryIfMissing True (destination </> path)
      else LBS.writeFile (destination </> path) (Zip.fromEntry entry)
  else
      return ()



-- EXISTS


exists :: FilePath -> IO Bool
exists path =
  Dir.doesFileExist path



-- REMOVE FILES


remove :: FilePath -> IO ()
remove path =
  do  exists_ <- Dir.doesFileExist path
      if exists_
        then Dir.removeFile path
        else return ()


removeDir :: FilePath -> IO ()
removeDir path =
  do  exists_ <- Dir.doesDirectoryExist path
      if exists_
        then Dir.removeDirectoryRecursive path
        else return ()
