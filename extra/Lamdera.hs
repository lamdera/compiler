{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera
  ( inProduction
  , getLamderaPkgPath
  , stdoutSetup
  , unsafePerformIO
  , liftIO
  , alternativeImplementation
  , alternativeImplementationWhen
  , alternativeImplementationPassthrough
  , Ext.Common.atomicPutStrLn
  , Ext.File.writeBinaryValueStrict
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
  , debugHaskellWhen
  , debugHaskellPass
  , debugHaskellPassWhen
  , debugHaskellPassDiffWhen
  -- , PP.sShow
  , T.Text
  , (<>)
  , mconcat
  , (&)
  -- , ppElm
  , Ext.Common.isDebug
  , Ext.Common.isDebug_
  , isExperimental
  , isExperimental_
  -- , isTypeSnapshot
  , isLamdera
  , isLamdera_
  , disableWire
  , isWireEnabled
  , isWireEnabled_
  , isTest
  , isLiveMode
  , setLiveMode
  , isCheckMode
  , setCheckMode
  , Ext.Common.ostype
  , Ext.Common.OSType(..)
  , env
  , unsafe
  , onlyWhen
  , onlyWhen_
  , textContains
  , textHasPrefix
  , stringContains
  , fileContains
  , formatHaskellValue
  , hindent
  , hindent_
  , hindentPrintLabelled
  , hindentPrint
  , hindentFormatValue
  -- , readUtf8
  , readUtf8Text
  , writeUtf8
  , writeUtf8Handle
  , writeUtf8Root
  , writeIfDifferent
  , writeBinary
  , Dir.doesFileExist
  , Dir.doesDirectoryExist
  , remove
  , rmdir
  , mkdir
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
  , Ext.Common.setProjectRoot
  , Ext.Common.getProjectRoot
  , Ext.Common.getProjectRootFor
  , Ext.Common.getProjectRootMaybe
  , Ext.Common.justs
  , lowerFirstLetter
  , lowerFirstLetter_
  , findElmFiles
  , show_
  , sleep
  , getVersion
  , getEnvMode
  , setEnvMode
  , setEnv
  , unsetEnv
  , lookupEnv
  , requireEnv
  , openUrlInBrowser
  , textSha1
  , (!!!)
  , toName
  , nameToText
  , utf8ToText
  , imap
  , imapM
  , filterMap
  , zipFull
  , withDefault
  , listUpsert
  , bsToStrict
  , bsToLazy
  , bsReadFile
  , callCommand
  , icdiff
  , withStdinYesAll
  , getGitBranch
  , launchAppZero
  , killAppZero
  , head_
  , last_
  , Text.Read.readMaybe
  , readMaybeText
  )
  where

-- A prelude-like thing that contains the commonly used things in elm.
-- Names differ, but semantics are similar.

import Control.DeepSeq (force, deepseq, NFData)
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

import qualified Safe

import Data.Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.IO
import qualified Data.Char as Char
import qualified Data.List as List

import qualified System.Exit (ExitCode(..))

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

import System.IO (hFlush, hPutStr, stdout, hClose, openTempFile)
import qualified System.IO as IO
import qualified System.IO.Error as Error
import Control.Exception (catch, throw)
import Test.Main (withStdin)
import Text.Show.Unicode

import qualified System.Process
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Name as N
import qualified Data.Utf8 as Utf8

import qualified Ext.Common
import qualified Ext.File
import Ext.Common (getProjectRoot, getProjectRootFor, getProjectRootMaybe, OSType(..), ostype, atomicPutStrLn)

-- import CanSer.CanSer (ppElm)


stdoutSetup :: IO ()
stdoutSetup = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetEncoding  IO.stdout IO.utf8


inProduction :: IO Bool
inProduction = do
  appNameEnvM <- liftIO $ lookupEnv "LAMDERA_APP_NAME"
  forceNotProd <- liftIO $ lookupEnv "NOTPROD"
  pure $ (appNameEnvM /= Nothing && forceNotProd == Nothing) -- @TODO better isProd check...


getLamderaPkgPath :: IO (Maybe String)
getLamderaPkgPath = lookupEnv "LOVR"


-- debug :: String -> Task.Task a
debug str =
  liftIO $ debug_ str


debugT text =
  liftIO $ debug_ (T.unpack text)

alternativeImplementation :: a -> a -> a
alternativeImplementation fn ignored =
  fn

alternativeImplementationWhen :: Bool -> a -> a -> a
alternativeImplementationWhen cond fn original =
  if cond
    then fn
    else original

alternativeImplementationPassthrough :: (a -> b) -> a -> b
alternativeImplementationPassthrough fn original =
  fn original

debug_ :: String -> IO ()
debug_ str = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> atomicPutStrLn $ "DEBUG: " ++ str -- ++ "\n"
    Nothing -> pure ()


debug_note :: String -> a -> a
debug_note msg value =
  unsafePerformIO $ do
    debugM <- Env.lookupEnv "LDEBUG"
    case debugM of
      Just _ -> pure $ DT.trace ("DEBUG: " ++ msg) value
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
          "\n🔶->"
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
          "\n🔶->"
            <> T.unpack label
            <> "\n\n"
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
        !x <- hindentPrintLabelled label value
        pure value

      Nothing ->
        pure value


debugHaskellPass :: (Show a, Show b) => Text -> a -> b -> b
debugHaskellPass label value pass =
  unsafePerformIO $ do
    debugM <- Env.lookupEnv "LDEBUG"
    case debugM of
      Just _ -> do
        !x <- hindentPrintLabelled label (value, pass)
        pure pass

      Nothing ->
        pure pass


debugHaskellPassWhen :: (Show a, Show b) => Bool -> Text -> a -> b -> b
debugHaskellPassWhen condition label value pass =
  if not condition
    then pass
    else
      unsafePerformIO $ do
        debugM <- Env.lookupEnv "LDEBUG"
        case debugM of
          Just _ -> do
            !x <- hindentPrintLabelled label (value, pass)
            pure pass

          Nothing ->
            pure pass


debugHaskellPassDiffWhen :: (Show a, Show b, Show c) => Bool -> Text -> (a, b) -> c -> c
debugHaskellPassDiffWhen condition label (v1, v2) pass =
  if not condition
    then pass
    else
      unsafePerformIO $ do
        debugM <- Env.lookupEnv "LDEBUG"
        case debugM of
          Just _ -> do
            hindentPrintLabelled label ((v1,v2), pass)
            v1_ <- hindent v1
            v2_ <- hindent v2
            icdiff v1_ v2_
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
            hindentPrintLabelled label value
            pure value
          else
            pure value
      Nothing ->
        pure value


isExperimental :: IO Bool
isExperimental = do
  experimentalM <- lookupEnv "EXPERIMENTAL"
  case experimentalM of
    Just _ -> pure True
    Nothing -> pure False


{-# NOINLINE isExperimental_ #-}
isExperimental_ :: Bool
isExperimental_ = unsafePerformIO $ isExperimental


isLamdera :: IO Bool
isLamdera = do
  root <- getProjectRoot "Lamdera.isLamdera"
  fileContains (root </> "elm.json") "lamdera/core"


{-# NOINLINE isLamdera_ #-}
isLamdera_ :: Bool
isLamdera_ = unsafePerformIO $ isLamdera


{-# NOINLINE useWire_ #-}
useWire_ :: MVar Bool
useWire_ = unsafePerformIO $ newMVar True

disableWire :: IO ()
disableWire = do
  debug $ "⚡️ disableWire"
  modifyMVar_ useWire_ (\_ -> pure False)

{-# NOINLINE isWireEnabled #-}
isWireEnabled :: IO Bool
isWireEnabled = do
  readMVar useWire_

{-# NOINLINE isWireEnabled_ #-}
isWireEnabled_ :: Bool
isWireEnabled_ = unsafePerformIO $ isWireEnabled



isTest :: IO Bool
isTest = do
  debugM <- lookupEnv "LTEST"
  case debugM of
    Just _ -> pure True
    Nothing -> pure False


{-# NOINLINE isLive_ #-}
isLive_ :: MVar Bool
isLive_ = unsafePerformIO $ newMVar False

setLiveMode :: Bool -> IO ()
setLiveMode b = do
  debug $ "⚡️ set mode live: " <> show b
  modifyMVar_ isLive_ (\_ -> pure b)

{-# NOINLINE isLiveMode #-}
isLiveMode :: IO Bool
isLiveMode = readMVar isLive_


{-# NOINLINE isCheck_ #-}
isCheck_ :: MVar Bool
isCheck_ = unsafePerformIO $ newMVar False

setCheckMode :: Bool -> IO ()
setCheckMode b = do
  debug $ "⚡️ set mode check: " <> show b
  modifyMVar_ isCheck_ (\_ -> pure $! b)

{-# NOINLINE isCheckMode #-}
isCheckMode :: IO Bool
isCheckMode = readMVar isCheck_


env =
  Env.getEnvironment


unsafe = unsafePerformIO


readUtf8Text :: FilePath -> IO (Maybe Text)
readUtf8Text filePath =
  do  exists_ <- Dir.doesFileExist filePath
      if exists_
        then
          Just <$> Data.Text.IO.readFile filePath
        else
          pure Nothing





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

fileContains :: FilePath -> Text -> IO Bool
fileContains filename needle = do
  current <- Dir.getCurrentDirectory
  debug_ $ "🔎 fileContains: " <> show current <> " " <> show filename <> " : " <> show needle
  textM <- readUtf8Text filename
  case textM of
    Just haystack ->
      pure $ textContains needle haystack

    Nothing ->
      pure False


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
    hindentPrintLabelled label v
    pure $ pure ()


hindentPrintLabelled :: Show a => Text -> a -> IO a
hindentPrintLabelled label v = do
  let input = Text.Show.Unicode.ushow v
  formatted <- hindent_ input
  atomicPutStrLn $
    "\n🔶 "
      <> T.unpack label
      <> "\n->"
      <> T.unpack formatted
  pure v


hindentPrint :: Show a => a -> IO a
hindentPrint v = do
  let input = Text.Show.Unicode.ushow v
  formatted <- hindent_ input
  Ext.Common.atomicPutStrLn_ formatted
  pure v


hindent :: Show a => a -> IO Text
hindent v =
  hindent_ (Text.Show.Unicode.ushow v)


hindent_ :: String -> IO Text
hindent_ s = do
  pathM <- Dir.findExecutable "hindent"
  case pathM of
    Just _ -> do
      (exit, stdout, stderr) <-
        System.Process.readProcessWithExitCode "hindent" ["--line-length","150"] s
        `catchError` (\err -> pure (System.Exit.ExitFailure 123, s, "hindent failed on input"))

      if exit /= System.Exit.ExitSuccess
        then
          pure $ T.pack stderr
        else
          if Prelude.length stderr > 0
            then
              pure $ T.pack $
                "\nerrors: \n"
                  <> stderr
                  <> "\n📥 for input: \n"
                  <> s
                  <> "\nwith output: \n"
                  <> stdout

            else
              pure $ T.pack stdout

    Nothing -> do
      debug_ "🔥  hindent missing, returning without formatting"
      pure $ T.pack s


hindentFormatValue :: Show a => a -> Text
hindentFormatValue v =
  unsafePerformIO $ do
    t <- hindent v `catchError` (\_ -> pure "hindent failed to format")
    pure t


writeUtf8 :: FilePath -> Text -> IO ()
writeUtf8 filePath content = do
  createDirIfMissing filePath
  debug_ $ "✍️  writeUtf8: " ++ show filePath
  Data.Text.IO.writeFile filePath content


-- Write directly to handle
writeUtf8Handle :: IO.Handle -> Text -> IO ()
writeUtf8Handle handle content = do
  Data.Text.IO.hPutStr handle content


-- Copied from File.IO due to cyclic imports and adjusted for Text
writeUtf8Root :: FilePath -> Text -> IO ()
writeUtf8Root filePath content = do
  root <- getProjectRoot ("writeUtf8Root:" <> filePath)
  writeUtf8 (root </> filePath) content


writeIfDifferent :: FilePath -> Text -> IO ()
writeIfDifferent filepath newContent = do
  currentM <- readUtf8Text filepath
  case currentM of
    Just currentContent ->
      if currentContent /= newContent
        then do
          debug_ $ "✅  writeIfDifferent: " ++ show filepath
          -- putStrLn $ show (T.length currentContent) <> "-" <> show (T.length newContent)
          -- putStrLn <$> icdiff currentContent newContent
          writeUtf8 filepath newContent
        else
          debug_ $ "⏩  writeIfDifferent skipped unchanged: " ++ show filepath

    Nothing ->
      -- File missing, write
      writeUtf8 filepath newContent


writeBinary :: FilePath -> BSL.ByteString -> IO ()
writeBinary filePath content = do
  createDirIfMissing filePath
  debug_ $ "✍️  writeBinary: " ++ show filePath
  BSL.writeFile filePath content


remove :: FilePath -> IO ()
remove filepath =
  do  debug_ $ "🗑  remove: " ++ show filepath
      exists_ <- Dir.doesFileExist filepath
      if exists_
        then Dir.removeFile filepath
        else do
          debug_ $ "🗑❌  does not exist: " ++ show filepath
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
  lamderaCache <$> getProjectRoot ("lamderaCache_")


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
  root <- getProjectRoot ("lamderaBackendDevSnapshotPath")
  pure $ lamderaCache root </> ".lamdera-bem-dev"







lowerFirstLetter :: String -> Text
lowerFirstLetter string =
  case string of
    first:rest -> T.pack $ [Char.toLower first] <> rest
    [] -> T.pack string

lowerFirstLetter_ :: Text -> Text
lowerFirstLetter_ text =
  case T.unpack text of
    first:rest -> T.pack $ [Char.toLower first] <> rest
    [] -> text


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
  root <- getProjectRoot "getEnvMode"
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


setEnv :: String -> String -> IO ()
setEnv name value = do
  debug $ Prelude.concat ["🌏✍️  ENV ", name, ":", value]
  Env.setEnv name value


unsetEnv :: String -> IO ()
unsetEnv name = do
  debug $ Prelude.concat ["🌏❌  ENV ", name]
  Env.unsetEnv name


lookupEnv :: String -> IO (Maybe String)
lookupEnv name = do
  debug $ Prelude.concat ["🌏👀  ENV ", name]
  Env.lookupEnv name


requireEnv :: String -> IO String
requireEnv name = do
  val <- lookupEnv name
  case val of
    Nothing ->
      error $ Prelude.concat ["🌏👀  ENV var `", name, "` is required but was not found"]
    Just "" ->
      error $ Prelude.concat ["🌏👀  ENV var `", name, "` is required but was empty"]
    Just v -> pure v


openUrlInBrowser :: Text -> IO ()
openUrlInBrowser url = do
  case ostype of
    MacOS -> do
      callCommand $ "open " <> T.unpack url

    Linux -> do
      xdgPath <- Ext.Common.bash "command -v xdg-open"
      if (xdgPath /= "")
        then do
          callCommand $ "xdg-open " <> T.unpack url
        else do
          atomicPutStrLn $ "Oops! I couldn't find a way to open the URL for you. Please open it manually in a browser."

    Windows -> do
      callCommand $ "start " <> T.unpack url

    UnknownOS name -> do
      -- We have an unexpected system...
      atomicPutStrLn $ "Oops, I couldn't find a way to open the URL for you. Please report: for unknown OSTYPE: " <> show name
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


toName :: Text -> N.Name
toName =
  N.fromChars . T.unpack


nameToText :: N.Name -> Text
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

zipFull :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipFull l1 l2 =
  let lx = Prelude.maximum [Prelude.length l1, Prelude.length l2]
  in
  Prelude.replicate lx ()
    & imap (\i _ -> (Safe.atMay l1 i, Safe.atMay l2 i) )

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

  icdiffPath_ <- Dir.findExecutable "icdiff"
  case icdiffPath_ of
    Just icdiffPath -> do
      atomicPutStrLn $ "icdiff -N " <> path1 <> " " <> path2
      -- (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "icdiff" ["--cols=150", "--show-all-spaces", path1, path2] ""
      (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "icdiff" ["--cols=180", path1, path2] ""
      -- (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "icdiff" ["-N", "--cols=200", path1, path2] ""
      pure stdout

    Nothing -> do
      atomicPutStrLn $ "No icdiff found, skipping, try manually with: diff " <> path1 <> " " <> path2
      pure ""


withStdinYesAll :: IO a -> IO a
withStdinYesAll action =
  -- This doesn't work as `withStdIn` actually writes to a file to emulate stdin
  -- withStdin (BSL.cycle "\n") action

  -- So instead, expect that our CLI will be reasonable and never ask the user to confirm more than this many times...
  withStdin
    "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    action


getGitBranch :: IO Text
getGitBranch = do
  -- This invocation doesn't appear to work on older git versions, left for posterity
  -- (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "git" ["branch","--show-current"] ""
  (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "git" ["symbolic-ref", "--short", "-q", "HEAD"] ""
  stdout & pack & strip & pure


launchAppZero :: Text -> IO ()
launchAppZero appId = do
  callCommand $ "~/lamdera/scripts/launchAppZero.sh " <> unpack appId
  atomicPutStrLn $ "✨ Called launchAppZero.sh"

killAppZero :: String -> IO ()
killAppZero appId = do
  adminToken <- requireEnv "TOKEN"
  Ext.Common.bash $ "curl -s -d \"{\"token\":\"" <> adminToken <> "\",\"appId\":\"" <> appId <> "\"}\" -H \"Content-Type: application/json\" -X POST -o /dev/null localhost:8080/v1/admin/appZeroRemove || true >> $LOG 2>&1"
  pure ()


head_ :: [a] -> a -> a
head_ list default_ =
  Safe.headMay list & withDefault default_

last_ :: [a] -> a -> a
last_ list default_ =
  Safe.lastMay list & withDefault default_

readMaybeText :: Read a => Text -> Maybe a
readMaybeText t =
  t & T.unpack & readMaybe
