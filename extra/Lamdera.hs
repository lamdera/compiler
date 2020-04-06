{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera
  ( lamderaVersion
  , stdoutSetup
  , unsafePerformIO
  , debugTrace
  , debug_
  , debug
  , debugT
  , dt
  , PP.sShow
  , PP.tShow
  , T.Text
  , (<>)
  , mconcat
  , (&)
  , first
  , second
  , ppElm
  , Data.Witherable.mapMaybe
  , Data.Witherable.catMaybes
  , isDebug
  , ostype
  , env
  , unsafe
  , lamderaLiveSrc
  , Data.List.Index.imap
  , onlyWhen
  , textContains
  , hunt
  , formatHaskellValue
  , readUtf8Text
  , writeUtf8
  , Dir.doesFileExist
  , remove
  , mkdir
  , copyFile
  , replaceInFile
  , touch
  , lamderaHashesPath
  , lamderaExternalWarningsPath
  , getProjectRoot
  , justs
  , lowerFirstLetter
  , pDocLn
  , findElmFiles
  , tshow
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
import qualified Data.Text.Encoding as Text
import qualified Data.Char as Char

import CanSer.CanSer (ppElm)
import qualified Data.Witherable

-- import qualified File.IO as IO
import qualified Data.Text.Encoding as T

import Prelude hiding (lookup)
import qualified Data.ByteString as BS
import Data.FileEmbed (bsToExp)
import Language.Haskell.TH (runIO)
import System.FilePath as FP ((</>), joinPath, splitDirectories)
import qualified System.Directory as Dir
import Control.Monad (unless, filterM)
import System.Info
import System.FilePath.Find (always, directory, extension, fileName, find, (&&?), (/~?), (==?))

-- Vendored from File.IO due to recursion errors
import qualified System.IO as IO
import GHC.IO.Exception (IOException, IOErrorType(InvalidArgument))
import qualified Data.ByteString.Internal as BS
import qualified Foreign.ForeignPtr as FPtr
import Control.Exception (catch)
import System.IO.Error (ioeGetErrorType, annotateIOError, modifyIOError)
import Data.List.Index
import Text.Show.Unicode
import qualified System.Process
import Data.Text.Internal.Search (indices)
import qualified Reporting.Doc as D




lamderaVersion :: String
lamderaVersion = "0.0.1-alpha4"


stdoutSetup :: IO ()
stdoutSetup = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetEncoding  IO.stdout IO.utf8


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


dt :: Show a => String -> a -> a
dt msg value =
  unsafePerformIO $ do
    debugM <- Env.lookupEnv "LDEBUG"
    case debugM of
      Just _ -> pure $ DT.trace (msg ++ ":" ++ show value) value
      Nothing -> pure value


isDebug :: IO Bool
isDebug = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> pure True
    Nothing -> pure False


ostype :: String
ostype = do
  dt "OSTYPE:" System.Info.os


env =
  Env.getEnvironment


unsafe = unsafePerformIO


lamderaLiveSrc =
  Lamdera.unsafe $ do
    debug <- Lamdera.isDebug
    if debug
      then do
        let overridePath = "/Users/mario/dev/projects/elmx/ui/browser/dist/live.js"
        exists <- Dir.doesFileExist overridePath
        if exists
          then do
            Lamdera.debug "Using elmx/ui/browser/dist/live.js for lamderaLive"
            res <- readUtf8 overridePath
            pure (T.encodeUtf8Builder (T.decodeUtf8 res))
          else
            pure (T.encodeUtf8Builder (T.decodeUtf8 lamderaLive))
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


readUtf8Text :: FilePath -> IO (Maybe Text)
readUtf8Text filePath =
  do  exists_ <- Dir.doesFileExist filePath
      if exists_
        then
          Just <$> Text.decodeUtf8 <$> readUtf8 filePath
        else
          pure Nothing


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


-- Inversion of `unless` that runs IO only when condition is True
onlyWhen condition io =
  unless (not condition) io


textContains :: Text -> Text -> Bool
textContains needle haystack = indices needle haystack /= []


hunt_ thing label = do
  let
      !_ =
        unsafePerformIO $ do
             hunt thing label

  pure ()


hunt thing label =
  if textContains "AnotherExternalThing" $ T.pack $ show thing then
    putStrLn $ "found it!!!!!!!!!!!!!!!!!!!!!!!!" <> label
  else
    putStrLn $ "not in " <> label


{-|

Useful when trying to understand AST values or just unknown values in general.

Requires hindent to be installed; try stack install hindent

Most conveniently used like so;

{-# LANGUAGE BangPatterns #-}

let
  !_ = formatHaskellValue "some sensible label" (blah) :: IO ()
in
blah

The bang pattern forces evaluation and you don't have to worry about the type-context, i.e. not being in IO.

-}
formatHaskellValue label v =
  unsafePerformIO $ do
    _ <- putStrLn $ "----------------------------------------------------------------------------------------------------------------" <> label
    (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "hindent" [] (Text.Show.Unicode.ushow v)
    _ <- putStrLn stdout
    pure $ pure ()


-- Copied from File.IO due to cyclic imports and adjusted for Text
writeUtf8 :: FilePath -> Text -> IO ()
writeUtf8 filePath content = do
  debug_ $ "writeUtf8: " ++ show filePath
  withUtf8 filePath IO.WriteMode $ \handle ->
    BS.hPut handle (Text.encodeUtf8 content)

remove :: FilePath -> IO ()
remove filePath =
  do  exists_ <- Dir.doesFileExist filePath
      if exists_
        then Dir.removeFile filePath
        else return ()

mkdir :: FilePath -> IO ()
mkdir dir =
  Dir.createDirectoryIfMissing True dir

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest =
  Dir.copyFileWithMetadata source dest

replaceInFile :: Text -> Text -> FilePath -> IO ()
replaceInFile find replace filename = do
  textM <- readUtf8Text filename
  case textM of
    Just text ->
      writeUtf8 filename $ T.replace find replace text

    Nothing ->
      pure ()

touch :: String -> IO ()
touch path = do
  case ostype of
    "mingw32" ->
      System.Process.callCommand $ "copy /b " <> path <> " +,,"

    "darwin" ->
      System.Process.callCommand $ "touch " ++ path

    "linux" ->
      System.Process.callCommand $ "touch " ++ path

    _ -> do
      -- Default to trying touch...
      putStrLn $ "WARNING: please report: skipping touch on unknown OSTYPE: " <> show ostype
      pure ()

lamderaHashesPath :: FilePath -> FilePath
lamderaHashesPath root =
  root </> "lamdera-stuff" </> ".lamdera-hashes"


lamderaExternalWarningsPath :: FilePath -> FilePath
lamderaExternalWarningsPath root =
  root </> "lamdera-stuff" </> ".lamdera-external-warnings"


-- Copy of combined internals of Project.getRoot as it seems to notoriously cause cyclic wherever imported
getProjectRoot :: IO FilePath
getProjectRoot = do
  subDir <- Dir.getCurrentDirectory
  res <- findHelp "elm.json" (FP.splitDirectories subDir)
  case res of
    Just filepath -> pure filepath
    Nothing -> error "derp"


findHelp :: FilePath -> [String] -> IO (Maybe FilePath)
findHelp name dirs =
  if Prelude.null dirs then
    return Nothing

  else
    do  exists_ <- Dir.doesFileExist (FP.joinPath dirs </> name)
        if exists_
          then return (Just (FP.joinPath dirs))
          else findHelp name (Prelude.init dirs)


justs :: [Maybe a] -> [a]
justs xs = [ x | Just x <- xs ]


lowerFirstLetter :: String -> Text
lowerFirstLetter text =
  case text of
    first:rest -> T.pack $ [Char.toLower first] <> rest


pDocLn doc =
  liftIO $ do
    putStrLn ""
    D.toAnsi IO.stdout doc
    putStrLn ""


findElmFiles :: FilePath -> IO [FilePath]
findElmFiles fp = System.FilePath.Find.find isVisible (isElmFile &&? isVisible &&? isntEvergreen) fp
    where
      isElmFile = extension ==? ".elm"
      isVisible = fileName /~? ".?*"
      isntEvergreen = directory /~? "src/Evergreen/*"


tshow :: Show a => a -> Text
tshow = T.pack . show


x = 1
