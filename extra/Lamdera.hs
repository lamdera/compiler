{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera
  ( lamderaVersion
  , inProduction
  , stdoutSetup
  , unsafePerformIO
  , liftIO
  , alternativeImplementation
  , debug_
  , debug_note
  , debug
  , debugT
  , dt
  , debugTrace
  , debugNote
  , debugHaskell
  , debugHaskellWhen
  , PP.sShow
  , T.Text
  , (<>)
  , mconcat
  , (&)
  , first
  , second
  , ppElm
  , isDebug
  , isTypeSnapshot
  , ostype
  , env
  , unsafe
  , lamderaLiveSrc
  , Data.List.Index.imap
  , onlyWhen
  , textContains
  , formatHaskellValue
  , hindentFormatValue
  , readUtf8Text
  , writeUtf8
  , writeUtf8Handle
  , writeUtf8Root
  , Dir.doesFileExist
  , remove
  , rmdir
  , mkdir
  , safeListDirectory
  , copyFile
  , replaceInFile
  , writeLineIfMissing
  , touch
  , lamderaHashesPath
  , lamderaEnvModePath
  , lamderaExternalWarningsPath
  , lamderaBackendDevSnapshotPath
  , getProjectRoot
  , justs
  , lowerFirstLetter
  , pDocLn
  , findElmFiles
  , show_
  , sleep
  , getVersion
  , getEnvMode
  , setEnvMode
  , openUrlInBrowser
  , textSha1
  )
  where

-- A prelude-like thing that contains the commonly used things in elm.
-- Names differ, but semantics are similar.

import qualified Debug.Trace as DT
import qualified Wire.PrettyPrint as PP
import qualified Data.Text as T
import Data.Monoid ((<>), mconcat)
import Data.Function ((&))
import Control.Arrow (first, second)
import qualified System.Environment as Env
import Control.Monad.Except (liftIO, catchError)
import System.IO.Unsafe (unsafePerformIO)
import qualified Debug.Trace as DT
import Data.Text
import qualified Data.Text.Encoding as Text
import qualified Data.Char as Char

-- import CanSer.CanSer (ppElm)

import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Prelude hiding (lookup)
import qualified Data.ByteString as BS
import Data.FileEmbed (bsToExp)
import Language.Haskell.TH (runIO)
import System.FilePath as FP ((</>), joinPath, splitDirectories, takeDirectory)
import qualified System.Directory as Dir
import Control.Monad (unless, filterM)
import System.Info
import System.FilePath.Find (always, directory, extension, fileName, find, (&&?), (/~?), (==?))
import Control.Concurrent (threadDelay)
import Text.Read (readMaybe)

import qualified System.PosixCompat.Files

-- Vendored from File.IO due to recursion errors
import qualified System.IO as IO
import qualified System.IO.Error as Error
import GHC.IO.Exception (IOException, IOErrorType(InvalidArgument))
import qualified Data.ByteString.Internal as BS
import qualified Foreign.ForeignPtr as FPtr
import Control.Exception (catch, throw)
import System.IO.Error (ioeGetErrorType, annotateIOError, modifyIOError)
import Data.List.Index
import Text.Show.Unicode
import qualified System.Process
import qualified Data.Digest.Pure.SHA as SHA

import qualified Reporting.Doc as D


lamderaVersion :: String
lamderaVersion = "0.0.1-alpha10"


stdoutSetup :: IO ()
stdoutSetup = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetEncoding  IO.stdout IO.utf8


inProduction :: IO Bool
inProduction = do
  appNameEnvM <- liftIO $ Env.lookupEnv "LAMDERA_APP_NAME"
  forceNotProd <- liftIO $ Env.lookupEnv "NOTPROD"
  pure $ (appNameEnvM /= Nothing && forceNotProd == Nothing) -- @TODO better isProd check...


-- debug :: String -> Task.Task a
debug str =
  liftIO $ debug_ str


debugT text =
  liftIO $ debug_ (T.unpack text)


alternativeImplementation fn ignored =
  fn


debug_ :: String -> IO ()
debug_ str = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> putStrLn $ "DEBUG: " ++ str
    Nothing -> pure ()


debug_note :: String -> a -> a
debug_note msg value =
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
      Just _ -> do
        let s = show value

        if Prelude.length s > 10000
          then
            pure $ DT.trace (msg ++ ": ❌SKIPPED display, value show > 10,000 chars, here's a clip:\n" <> (Prelude.take 1000 s)) value
          else
            pure $ DT.trace (msg ++ ":" ++ show value) value
      Nothing -> pure value


debugTrace :: Show a => String -> a -> a
debugTrace = dt


debugNote :: Show a => Text -> a -> a
debugNote msg value =
  unsafePerformIO $ do
    debugM <- Env.lookupEnv "LDEBUG"
    case debugM of
      Just _ -> pure $ DT.trace (T.unpack msg) value
      Nothing -> pure value


debugHaskell :: Show a => Text -> a -> a
debugHaskell label value =
  unsafePerformIO $ do
    debugM <- Env.lookupEnv "LDEBUG"
    case debugM of
      Just _ -> do
        hindentPrintValue label value
        pure value

      Nothing ->
        pure value


debugHaskellWhen :: Show a => Bool -> Text -> a -> a
debugHaskellWhen cond label value =
  unsafePerformIO $ do
    debugM <- Env.lookupEnv "LDEBUG"
    case debugM of
      Just _ -> do
        if cond
          then do
            hindentPrintValue label value
            pure value
          else
            pure value
      Nothing ->
        pure value


isDebug :: IO Bool
isDebug = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> pure True
    Nothing -> pure False


isTypeSnapshot :: IO Bool
isTypeSnapshot = do
  debugM <- Env.lookupEnv "LTYPESNAPSHOT"
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
readUtf8 filePath = do
  debug $ "readUtf8:" <> filePath
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
textContains needle haystack = T.isInfixOf needle haystack



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
    hindentPrintValue label v
    pure $ pure ()


hindentPrintValue :: Show a => Text -> a -> IO a
hindentPrintValue label v = do
  let
    str = Text.Show.Unicode.ushow v
    input =
      if Prelude.length str > 10000 then
        "err \"hindentPrintValue value was > 10,000 - skipping\" "
      else
        str

  (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "hindent" [] input
  _ <- putStrLn $
    "----------------------------------------------------------------------------------------------------------------"
      <> T.unpack label
      <> "\n"
      <> stdout

  pure v


hindentFormatValue :: Show a => a -> Text
hindentFormatValue v =
  unsafePerformIO $ do
    (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "hindent" [] (Text.Show.Unicode.ushow v)
    pure $ T.pack stdout


-- Copied from File.IO due to cyclic imports and adjusted for Text
writeUtf8 :: FilePath -> Text -> IO ()
writeUtf8 filePath content = do
  createDirIfMissing filePath
  debug_ $ "writeUtf8: " ++ show filePath
  withUtf8 filePath IO.WriteMode $ \handle ->
    BS.hPut handle (Text.encodeUtf8 content)


-- Write directly to handle
writeUtf8Handle :: IO.Handle -> Text -> IO ()
writeUtf8Handle handle content = do
  IO.hSetEncoding handle IO.utf8
  BS.hPut handle (Text.encodeUtf8 content)


-- Copied from File.IO due to cyclic imports and adjusted for Text
writeUtf8Root :: FilePath -> Text -> IO ()
writeUtf8Root filePath content = do
  root <- getProjectRoot
  writeUtf8 (root </> filePath) content


remove :: FilePath -> IO ()
remove filepath =
  do  debug_ $ "remove: " ++ show filepath
      exists_ <- Dir.doesFileExist filepath
      if exists_
        then Dir.removeFile filepath
        else do
          debug_ $ "[FAILED:DOES NOT EXIST] remove: " ++ show filepath
          return ()


rmdir :: FilePath -> IO ()
rmdir filepath = do
  exists_ <- Dir.doesDirectoryExist filepath
  if exists_
    then Dir.removeDirectoryRecursive filepath
    else pure ()


mkdir :: FilePath -> IO ()
mkdir dir =
  Dir.createDirectoryIfMissing True dir


safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory dir = do
  Dir.listDirectory dir `catchError`
    (\err -> do
      if Error.isDoesNotExistError err
        then
          pure []
        else
          throw err
    )


createDirIfMissing :: FilePath -> IO ()
createDirIfMissing filepath =
  mkdir $ FP.takeDirectory filepath


copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
  debug_ $ "copy: " ++ show source ++ " -> " ++ show dest
  createDirIfMissing dest
  Dir.copyFileWithMetadata source dest


replaceInFile :: Text -> Text -> FilePath -> IO ()
replaceInFile find replace filename = do
  textM <- readUtf8Text filename
  case textM of
    Just text ->
      writeUtf8 filename $ T.replace find replace text

    Nothing ->
      pure ()


writeLineIfMissing :: Text -> FilePath -> IO ()
writeLineIfMissing line filename = do
  exists_ <- Dir.doesFileExist filename
  onlyWhen (not exists_) $ writeUtf8 filename ""

  textM <- readUtf8Text filename
  case textM of
    Just text ->
      onlyWhen (not $ textContains line text) $
        writeUtf8 filename $ text <> "\n" <> line <> "\n"

    Nothing ->
      pure ()


touch :: FilePath -> IO ()
touch filepath = do
  System.PosixCompat.Files.touchFile filepath


lamderaHashesPath :: FilePath -> FilePath
lamderaHashesPath root =
  root </> "lamdera-stuff" </> ".lamdera-hashes"


lamderaEnvModePath :: FilePath -> FilePath
lamderaEnvModePath root =
  root </> "lamdera-stuff" </> ".lamdera-mode"


lamderaExternalWarningsPath :: FilePath -> FilePath
lamderaExternalWarningsPath root =
  root </> "lamdera-stuff" </> ".lamdera-external-warnings"


lamderaBackendDevSnapshotPath :: IO FilePath
lamderaBackendDevSnapshotPath = do
  root <- getProjectRoot
  pure $ root </> "lamdera-stuff" </> ".lamdera-bem-dev"


-- Copy of combined internals of Project.getRoot as it seems to notoriously cause cyclic wherever imported
getProjectRoot :: IO FilePath
getProjectRoot = do
  subDir <- Dir.getCurrentDirectory
  res <- findHelp "elm.json" (FP.splitDirectories subDir)
  case res of
    Just filepath -> pure filepath
    Nothing -> error "Error: could not determine project root while looking for elm.json. Please report this issue."


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
    D.toAnsi IO.stdout doc
    putStrLn ""


findElmFiles :: FilePath -> IO [FilePath]
findElmFiles fp = System.FilePath.Find.find isVisible (isElmFile &&? isVisible &&? isntEvergreen) fp
    where
      isElmFile = extension ==? ".elm"
      isVisible = fileName /~? ".?*"
      isntEvergreen = directory /~? "src/Evergreen/*"


show_ :: Show a => a -> Text
show_ = T.pack . show


sleep milliseconds =
  -- 50 milliseconds
  threadDelay $ milliseconds * 1000


-- For turning "V8.elm" into Just 8
getVersion :: FilePath -> Maybe Int
getVersion filename =
  filename
    & Prelude.drop 1 -- Drop the 'V'
    & Prelude.takeWhile (\i -> i /= '.')
    & Text.Read.readMaybe


data Env = Production | Development
  deriving (Show)

-- Used for Env.mode value injection
getEnvMode :: IO Env
getEnvMode = do
  inProduction <- Lamdera.inProduction
  root <- getProjectRoot
  if inProduction
    then do
      debug "[mode] inProduction"
      pure Production

    else do
      modeString <- readUtf8Text (lamderaEnvModePath root)
      debug $ show $ "[mode] " <> show_ modeString
      pure $
        case modeString of
          Just "Production" -> Production
          -- Just "Review" -> Review
          Just "Development" -> Development
          _ -> Development


setEnvMode :: FilePath -> Text -> IO ()
setEnvMode root mode = do
  writeUtf8 (lamderaEnvModePath root) $ mode


openUrlInBrowser :: Text -> IO ()
openUrlInBrowser url = do
  case ostype of
    "mingw32" -> do
      System.Process.callCommand $ "start " <> T.unpack url

    "darwin" -> do
      System.Process.callCommand $ "open " <> T.unpack url

    "linux" -> do
      System.Process.callCommand $ "xdg-open " <> T.unpack url

    _ -> do
      -- We have an unexpected system...
      putStrLn $ "ERROR: please report: skipping url open on unknown OSTYPE: " <> show ostype
      pure ()


textSha1 :: Text -> Text
textSha1 input =
  T.pack $ SHA.showDigest $ SHA.sha1 $ TLE.encodeUtf8 $ TL.fromStrict $ input
