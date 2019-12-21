{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lamdera
  ( lamderaVersion
  , debugTrace
  , debug_
  , debug
  , debugT
  , PP.sShow
  , PP.tShow
  , T.Text
  , (<>)
  , mconcat
  , (&)
  , first
  , second
  , List
  , ppElm
  , Data.Witherable.mapMaybe
  , Data.Witherable.catMaybes
  , isDebug
  , unsafe
  , lamderaLiveSrc
  )
  where

-- A prelude-like thing that contains the commonly used things in elm.
-- Names differ, but semantics are similar.

-- import Prelude (Maybe(..), String, IO)

import qualified Debug.Trace as DT
import qualified Wire.PrettyPrint as PP
import qualified Data.Text as T
import Data.Monoid ((<>), mconcat)
import Data.Function ((&))
import Control.Arrow (first, second)
import qualified System.Environment as Env
import Control.Monad.Except (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import qualified Debug.Trace as DT
import Data.Text

import CanSer.CanSer (ppElm)
import qualified Data.Witherable

-- import qualified File.IO as IO
import qualified Data.Text.Encoding as T

import Prelude hiding (lookup)
import qualified Data.ByteString as BS
import Data.FileEmbed (bsToExp)
import Language.Haskell.TH (runIO)
import System.FilePath ((</>))

-- Vendored from File.IO due to recursion errors
import qualified System.IO as IO
import GHC.IO.Exception (IOException, IOErrorType(InvalidArgument))
import qualified Data.ByteString.Internal as BS
import qualified Foreign.ForeignPtr as FPtr
import Control.Exception (catch)
import System.IO.Error (ioeGetErrorType, annotateIOError, modifyIOError)

lamderaVersion :: String
lamderaVersion = "0.0.1-alpha2"

-- import qualified Reporting.Task as Task

type List a = [a]


-- debug :: String -> Task.Task ()
debug str =
  liftIO $ debug_ str


debugT text =
  liftIO $ debug_ (T.unpack text)


debug_ :: String -> IO ()
debug_ str = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> putStrLn $ "DEBUG: " ++ str
    Nothing -> pure ()


debugTrace :: String -> a -> a
debugTrace msg value =
  unsafePerformIO $ do
    debugM <- Env.lookupEnv "LDEBUG"
    case debugM of
      Just _ -> pure $ DT.trace msg value
      Nothing -> pure value


isDebug :: IO Bool
isDebug = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> pure True
    Nothing -> pure False


unsafe = unsafePerformIO


lamderaLiveSrc =
  Lamdera.unsafe $ do
    debug <- Lamdera.isDebug
    if debug
      then do
        Lamdera.debug "Using elmx/ui/browser/dist/live.js for lamderaLive"
        res <- readUtf8 "/Users/mario/dev/projects/elmx/ui/browser/dist/live.js"
        pure (T.encodeUtf8Builder (T.decodeUtf8 res))
      else
        pure (T.encodeUtf8Builder (T.decodeUtf8 lamderaLive))


lamderaLive :: BS.ByteString
lamderaLive =
  $(bsToExp =<< runIO (BS.readFile ("ui" </> "browser" </> "dist" </> "live.js")))



-- Vendored from File.IO due to recursion errors

readUtf8 :: FilePath -> IO BS.ByteString
readUtf8 filePath =
  withUtf8 filePath IO.ReadMode $ \handle ->
    modifyIOError (encodingError filePath) $
      do  fileSize <- catch (IO.hFileSize handle) useZeroIfNotRegularFile
          let readSize = max 0 (fromIntegral fileSize) + 1
          hGetContentsSizeHint handle readSize (max 255 readSize)


withUtf8 :: FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withUtf8 filePath mode callback =
  IO.withFile filePath mode $ \handle ->
    do  IO.hSetEncoding handle IO.utf8
        callback handle


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
            then return $! BS.concat (Prelude.reverse (chunk:chunks))
            else readChunks (chunk:chunks) incrementSize (min 32752 (readSize + incrementSize))

encodingError :: FilePath -> IOError -> IOError
encodingError filePath ioErr =
  case ioeGetErrorType ioErr of
    InvalidArgument ->
      annotateIOError
        (userError "Bad encoding; the file must be valid UTF-8")
        ""
        Nothing
        (Just filePath)

    _ ->
      ioErr
