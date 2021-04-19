{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lamdera
  ( lamderaVersion
  , inProduction
  , getLamderaPkgPath
  , stdoutSetup
  , unsafePerformIO
  , liftIO
  , alternativeImplementation
  , alternativeImplementationWhen
  , alternativeImplementationPassthrough
  , atomicPutStrLn
  , debug_
  , debug_note
  , debug
  , debugT
  , dt
  , debugTrace
  , debugNote
  , debugPass
  , debugPassText
  , debugHaskell
  , debugHaskellPass
  , debugHaskellWhen
  -- , PP.sShow
  , T.Text
  , (<>)
  , mconcat
  , (&)
  , first
  , second
  -- , ppElm
  , isDebug
  -- , isTypeSnapshot
  , isTest
  , ostype
  , env
  , unsafe
  , onlyWhen
  , onlyWhen_
  , textContains
  , textHasPrefix
  , stringContains
  , formatHaskellValue
  , hindent
  , hindent_
  , hindentPrintValue
  , hindentFormatValue
  , readUtf8
  , readUtf8Text
  , writeUtf8
  , writeUtf8Handle
  , writeUtf8Root
  , Dir.doesFileExist
  , Dir.doesDirectoryExist
  , remove
  , rmdir
  , mkdir
  , withCurrentDirectory
  , safeListDirectory
  , copyFile
  , replaceInFile
  , writeLineIfMissing
  , touch
  , lamderaCache
  , lamderaCache_
  , lamderaHashesPath
  , lamderaEnvModePath
  , lamderaExternalWarningsPath
  , lamderaBackendDevSnapshotPath
  , Ext.Common.getProjectRoot
  , Ext.Common.getProjectRootFor
  , Ext.Common.getProjectRootMaybe
  , Ext.Common.justs
  , lowerFirstLetter
  , findElmFiles
  , show_
  , sleep
  , getVersion
  , Env.setEnv
  , Env.unsetEnv
  , getEnvMode
  , setEnvMode
  , openUrlInBrowser
  , textSha1
  , (!!!)
  , toName
  , nameToText
  , utf8ToText
  , imap
  , imapM
  , filterMap
  , withDefault
  , listUpsert
  , bsToStrict
  , bsToLazy
  , bsReadFile
  , callCommand
  , icdiff
  , withStdinYesAll
  , launchAppZero
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
import System.Exit (exitFailure)

import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.List as List

import Prelude hiding (lookup)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed (bsToExp)
import Language.Haskell.TH (runIO)
import System.FilePath as FP ((</>), joinPath, splitDirectories, takeDirectory)
import qualified System.Directory as Dir
import Control.Monad (unless, filterM)
import System.Info
import System.FilePath.Find (always, directory, extension, fileName, find, (&&?), (/~?), (==?))
import Control.Concurrent
import Control.Concurrent.MVar
import Text.Read (readMaybe)

import qualified System.PosixCompat.Files

-- Vendored from File.IO due to recursion errors
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout, hClose, openTempFile)
import qualified System.IO as IO
import qualified System.IO.Error as Error
import GHC.IO.Exception (IOException, IOErrorType(InvalidArgument))
import qualified Data.ByteString.Internal as BS
import qualified Foreign.ForeignPtr as FPtr
import Control.Exception (catch, throw)
import System.IO.Error (ioeGetErrorType, annotateIOError, modifyIOError)
import Test.Main (withStdin)
import Text.Show.Unicode
import qualified System.Process
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Name as N
import qualified Data.Utf8 as Utf8

import qualified Ext.Common
import Ext.Common (getProjectRoot, getProjectRootFor, getProjectRootMaybe)

-- import CanSer.CanSer (ppElm)

lamderaVersion :: String
lamderaVersion = "0.0.1-alpha12"


stdoutSetup :: IO ()
stdoutSetup = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetEncoding  IO.stdout IO.utf8


inProduction :: IO Bool
inProduction = do
  appNameEnvM <- liftIO $ Env.lookupEnv "LAMDERA_APP_NAME"
  forceNotProd <- liftIO $ Env.lookupEnv "NOTPROD"
  pure $ (appNameEnvM /= Nothing && forceNotProd == Nothing) -- @TODO better isProd check...


getLamderaPkgPath :: IO (Maybe String)
getLamderaPkgPath = Env.lookupEnv "LOVR"


-- https://stackoverflow.com/questions/16811376/simulate-global-variable trick
{-# NOINLINE printLock #-}
printLock :: MVar ()
printLock = unsafePerformIO $ newMVar ()


atomicPutStrLn :: String -> IO ()
atomicPutStrLn str =
  withMVar printLock (\_ -> hPutStr stdout (str <> "\n") >> hFlush stdout)


-- debug :: String -> Task.Task a
debug str =
  liftIO $ debug_ str


debugT text =
  liftIO $ debug_ (T.unpack text)

alternativeImplementation fn ignored =
  fn

alternativeImplementationWhen cond fn original =
  if cond
    then fn
    else original

alternativeImplementationPassthrough fn original =
  fn original

debug_ :: String -> IO ()
debug_ str = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> atomicPutStrLn $ "DEBUG: " ++ str ++ "\n"
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


debugPass :: Show a => Text -> a -> b -> b
debugPass label value pass =
  unsafePerformIO $ do
    debugM <- Env.lookupEnv "LDEBUG"
    case debugM of
      Just _ -> do
        atomicPutStrLn $
          "\n🔶--------------------------------------------------------------------------------"
            <> T.unpack label
            <> "\n"
            <> show value
        pure pass

      Nothing ->
        pure pass

debugPassText :: Text -> Text -> b -> b
debugPassText label value pass =
  unsafePerformIO $ do
    debugM <- Env.lookupEnv "LDEBUG"
    case debugM of
      Just _ -> do
        atomicPutStrLn $
          "\n🔶--------------------------------------------------------------------------------"
            <> T.unpack label
            <> "\n"
            <> T.unpack value
        pure pass

      Nothing ->
        pure pass


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


debugHaskellPass :: Show a => Text -> a -> b -> b
debugHaskellPass label value pass =
  unsafePerformIO $ do
    debugM <- Env.lookupEnv "LDEBUG"
    case debugM of
      Just _ -> do
        hindentPrintValue label value
        pure pass

      Nothing ->
        pure pass


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


isTest :: IO Bool
isTest = do
  debugM <- Env.lookupEnv "LTEST"
  case debugM of
    Just _ -> pure True
    Nothing -> pure False


ostype :: String
ostype = do
  dt "OSTYPE:" System.Info.os


env =
  Env.getEnvironment


unsafe = unsafePerformIO


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
onlyWhen :: Monad f => Bool -> f () -> f ()
onlyWhen condition io =
  unless (not condition) io

-- Same but evaluates the IO
onlyWhen_ :: Monad f => f Bool -> f () -> f ()
onlyWhen_ condition io = do
  res <- condition
  unless (not res) io


textContains :: Text -> Text -> Bool
textContains needle haystack = T.isInfixOf needle haystack

textHasPrefix :: Text -> Text -> Bool
textHasPrefix needle haystack = T.isPrefixOf needle haystack

stringContains :: String -> String -> Bool
stringContains needle haystack = List.isInfixOf needle haystack



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
    input = Text.Show.Unicode.ushow v

  -- if Prelude.length input > 10000
  --   then
  --     atomicPutStrLn $ "❌SKIPPED display, value show > 10,000 chars, here's a clip:\n" <> Prelude.take 1000 input
  --   else do
  (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "hindent" ["--line-length","150"] input
  if Prelude.length stderr > 0
    then
      atomicPutStrLn $
        "\n🔶 "
          <> T.unpack label
          <> "\n->"
          <> stderr
          <> "\n📥 for input: \n"
          <> input

    else
      atomicPutStrLn $
        "\n🔶 "
          <> T.unpack label
          <> "\n->"
          <> stdout

  pure v


hindent :: Show a => a -> IO Text
hindent v =
  hindent_ (Text.Show.Unicode.ushow v)


hindent_ :: String -> IO Text
hindent_ s = do
  (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "hindent" ["--line-length","150"] s
  if Prelude.length stderr > 0
    then
      pure $ T.pack stderr
    else
      pure $ T.pack stdout


hindentFormatValue :: Show a => a -> Text
hindentFormatValue v =
  unsafePerformIO $ do
    t <- hindent v
    pure t


-- Copied from File.IO due to cyclic imports and adjusted for Text
writeUtf8 :: FilePath -> Text -> IO ()
writeUtf8 filePath content = do
  createDirIfMissing filePath
  debug_ $ "✍️  writeUtf8: " ++ show filePath
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
  do  debug_ $ "🗑  remove: " ++ show filepath
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
    then do
      debug_ $ "🗑  rmdir: " ++ show filepath
      Dir.removeDirectoryRecursive filepath
    else pure ()


mkdir :: FilePath -> IO ()
mkdir dir =
  Dir.createDirectoryIfMissing True dir


withCurrentDirectory =
  Dir.withCurrentDirectory


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
  debug_ $ "✂️  copy: " ++ show source ++ " -> " ++ show dest
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
  debug_ $ "👆🏻  touch: " ++ show filepath
  exists_ <- Dir.doesFileExist filepath
  if exists_
    then System.PosixCompat.Files.touchFile filepath
    else do
      writeUtf8 filepath ""
      System.PosixCompat.Files.touchFile filepath


lamderaCache :: FilePath -> FilePath
lamderaCache root =
  root </> "elm-stuff" </> "lamdera"


lamderaCache_ :: IO FilePath
lamderaCache_ =
  lamderaCache <$> getProjectRoot


lamderaHashesPath :: FilePath -> FilePath
lamderaHashesPath root =
  lamderaCache root </> ".lamdera-hashes"


lamderaEnvModePath :: FilePath -> FilePath
lamderaEnvModePath root =
  lamderaCache root </> ".lamdera-mode"


lamderaExternalWarningsPath :: FilePath -> FilePath
lamderaExternalWarningsPath root =
  lamderaCache root </> ".lamdera-external-warnings"


lamderaBackendDevSnapshotPath :: IO FilePath
lamderaBackendDevSnapshotPath = do
  root <- getProjectRoot
  pure $ lamderaCache root </> ".lamdera-bem-dev"







lowerFirstLetter :: String -> Text
lowerFirstLetter text =
  case text of
    first:rest -> T.pack $ [Char.toLower first] <> rest


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
      atomicPutStrLn $ "ERROR: please report: skipping url open on unknown OSTYPE: " <> show ostype
      pure ()


textSha1 :: Text -> Text
textSha1 input =
  T.pack $ SHA.showDigest $ SHA.sha1 $ TLE.encodeUtf8 $ TL.fromStrict $ input


-- Safe version of !!
(!!!) :: [a] -> Int -> Maybe a
(!!!) l i =
  if Prelude.length l >= (i + 1)
  then Just (l !! i)
  else Nothing


toName =
  N.fromChars . T.unpack


nameToText =
  T.pack . N.toChars


utf8ToText =
  T.pack . Utf8.toChars


imap :: (Int -> a -> b) -> [a] -> [b]
imap f l = Prelude.zipWith (\v i -> f i v) l [0..]


imapM :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
imapM f as = ifoldr k (return []) as
  where
    k i a r = do
      x <- f i a
      xs <- r
      return (x:xs)

ifoldr :: (Int -> a -> b -> b) -> b -> [a] -> b
ifoldr f z xs = Prelude.foldr (\x g i -> f i x (g (i+1))) (const z) xs 0

type List a = [a]

filterMap :: (a -> Maybe b) -> List a -> List b
filterMap f xs =
  Prelude.foldr (maybeCons f) [] xs

maybeCons :: (a -> Maybe b) -> a -> List b -> List b
maybeCons f mx xs =
  case f mx of
    Just x ->
      x : xs

    Nothing ->
      xs


withDefault :: a -> Maybe a -> a
withDefault default_ m =
  case m of
    Just v -> v
    Nothing -> default_


listUpsert :: (a -> Bool) -> a -> [a] -> [a]
listUpsert check item collection =
  if Prelude.any check collection then
    collection
      & fmap (\v ->
          if check v then
            item
          else
            v
        )
  else
    collection ++ [item]


bsToStrict =
  BSL.toStrict

bsToLazy =
  BSL.fromStrict

bsReadFile =
  BS.readFile

callCommand c = do
  debug_ $ "🤖  callCommand: " <> c
  System.Process.callCommand c


icdiff realExpected realActual = do
  (path1, handle1) <- openTempFile "/tmp" "expected.txt"
  (path2, handle2) <- openTempFile "/tmp" "actual.txt"

  writeUtf8Handle handle1 realExpected
  writeUtf8Handle handle2 realActual

  hClose handle1
  hClose handle2

  sleep 100

  -- putStrLn $ "diff -y --suppress-common-lines --width=160 " <> path1 <> " " <> path2
  -- (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "diff" ["-y", "--suppress-common-lines", "--width=160", path1, path2] ""
  -- (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "diff" ["--width=200", path1, path2] ""

  atomicPutStrLn $ "icdiff -N " <> path1 <> " " <> path2
  (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "icdiff" ["--cols=150", "--show-all-spaces", path1, path2] ""
  -- (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "icdiff" ["-N", "--cols=200", path1, path2] ""

  pure stdout


withStdinYesAll :: IO a -> IO a
withStdinYesAll action =
  -- This doesn't work as `withStdIn` actually writes to a file to emulate stdin
  -- withStdin (BSL.cycle "\n") action

  -- So instead, expect that our CLI will be reasonable and never ask the user to confirm more than this many times...
  withStdin
    "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    action


launchAppZero :: Text -> IO ()
launchAppZero appId = do
  callCommand $ "~/lamdera/scripts/launchAppZero.sh " <> unpack appId
  atomicPutStrLn $ "✨ Called launchAppZero.sh"
