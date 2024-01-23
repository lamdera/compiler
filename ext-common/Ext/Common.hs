{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Ext.Common where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (unless)
import Control.Arrow ((>>>))
import qualified GHC.IO.Exception

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TLB

import System.Command (command)
import System.Exit (exitFailure)
import System.FilePath as FP ((</>), joinPath, splitDirectories, takeDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout, hClose, openTempFile)
import System.Info
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Mem as Mem
import System.Process
import System.Process.Internals
import Data.Char

import Control.Exception ()
import Formatting (fprint, (%), int, string, formatToString)
import Formatting.Clock (timeSpecs)
import System.Clock (Clock(..), getTime)
import Control.DeepSeq (force, deepseq, NFData)

import Data.Utf8


-- Re-exports
import qualified Data.Function


-- Environment

data OSType = Windows | MacOS | Linux | UnknownOS String deriving (Eq, Show)

ostype :: OSType
ostype = do
  -- case dt "OSTYPE:" System.Info.os of
  case System.Info.os of
    "darwin" -> MacOS
    "linux" -> Linux
    "mingw32" -> Windows
    _ -> UnknownOS System.Info.os

os :: String
os = System.Info.os

arch :: String
arch = do
  System.Info.arch


isDebug :: IO Bool
isDebug = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> pure True
    Nothing -> pure False


{-# NOINLINE isDebug_ #-}
isDebug_ :: Bool
isDebug_ = unsafePerformIO $ isDebug


isProdEnv =
  case ostype of
    MacOS -> False
    Linux -> True
    Windows -> False
    UnknownOS name ->
      -- We have an unexpected system...
      error $ "ERROR: please report: skipping url open on unknown OSTYPE: " <> show name


-- Copy of combined internals of Project.getRoot as it seems to notoriously cause cyclic wherever imported
-- Further modified for more explicit current directory setting compared to flakey Dir.setCurrentDirectory

data ProjectRoot = ProjectRootUnset | ProjectRootSet FilePath | ProjectRootContextual FilePath

-- https://stackoverflow.com/questions/16811376/simulate-global-variable trick
{-# NOINLINE projectRootMvar #-}
projectRootMvar :: MVar ProjectRoot
projectRootMvar = unsafePerformIO $ do
  rootM <- getProjectRootMaybe
  newMVar $
    case rootM of
      Just root ->
        ProjectRootContextual root
      Nothing ->
        ProjectRootUnset

setProjectRoot :: FilePath -> IO ()
setProjectRoot root = do
  debug $ "➡️ 🏠  set project root: " <> root
  modifyMVar_ projectRootMvar (\v -> pure $ ProjectRootSet root)


getProjectRoot :: String -> IO FilePath
getProjectRoot tag = do
  root <- readMVar projectRootMvar
  case root of
    ProjectRootUnset -> do
      -- debug $ "🏠 read project root [" <> tag <> "]: " <> "invalid project root"
      -- @TODO this doesn't seem right... we have creations of this path in prod root...
      -- should we error out instead? Or just return the current directory?
      -- pure "<unset-project-root>"
      subDir <- Dir.getCurrentDirectory
      error $ "Fatal: I don't know where the project root is! This should generally be impossible, unless you've accidentally run lamdera in a folder that has no parent folders with an elm.json. My current working directory is " <> subDir <> "."
    ProjectRootSet root -> do
      -- debug $ "🏠 read project root [" <> tag <> "]: " <> root
      pure root
    ProjectRootContextual root -> do
      -- debug $ "🏠 read project root [" <> tag <> "]: " <> root
      pure root

getProjectRoot_ :: String -> IO (Maybe FilePath)
getProjectRoot_ tag = do
  root <- readMVar projectRootMvar
  case root of
    ProjectRootUnset -> pure Nothing
    ProjectRootSet root -> do
      pure $ Just root
    ProjectRootContextual root -> do
      pure $ Just root


getProjectRootMaybe :: IO (Maybe FilePath)
getProjectRootMaybe = do
  subDir <- Dir.getCurrentDirectory
  findHelp "elm.json" (FP.splitDirectories subDir)


withProjectRoot :: FilePath -> IO a -> IO a
withProjectRoot root io = do
  originalRootM <- getProjectRoot_ "withProjectRoot"
  setProjectRoot root
  !res <- Dir.withCurrentDirectory root io
  case originalRootM of
    Just originalRoot -> setProjectRoot originalRoot
    Nothing -> pure ()
  pure res


findHelp :: FilePath -> [String] -> IO (Maybe FilePath)
findHelp name dirs =
  if Prelude.null dirs then
    return Nothing

  else
    do  exists_ <- Dir.doesFileExist (FP.joinPath dirs </> name)
        if exists_
          then return (Just (FP.joinPath dirs))
          else findHelp name (Prelude.init dirs)


-- Find the project root from an arbitrary fle path
getProjectRootFor :: FilePath -> IO FilePath
getProjectRootFor path = do
  res <- findHelp "elm.json" (FP.splitDirectories $ takeDirectory path)
  case res of
    Just filepath -> pure filepath
    Nothing -> do
      binName <- Env.getProgName
      putStrLn $ "Cannot find an elm.json! Make sure you're in a project folder, or run `" <> binName <> " init` to start a new one. (I started searching from " <> path <> ")"
      exitFailure


{- Helpers -}

justs :: [Maybe a] -> [a]
justs xs = [ x | Just x <- xs ]


{- Debugging
-}


debug :: String -> IO ()
debug str = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> atomicPutStrLn $ "DEBUG: " ++ str -- ++ "\n"
    Nothing -> pure ()


whenDebug :: IO () -> IO ()
whenDebug io = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> io
    Nothing -> pure ()


-- Inversion of `unless` that runs IO only when condition is True
onlyWhen :: Monad f => Bool -> f () -> f ()
onlyWhen condition io =
  unless (not condition) io


-- Same but evaluates the IO
onlyWhen_ :: Monad f => f Bool -> f () -> f ()
onlyWhen_ condition io = do
  res <- condition
  unless (not res) io



-- https://stackoverflow.com/questions/16811376/simulate-global-variable trick
{-# NOINLINE printLock #-}
printLock :: MVar ()
printLock = unsafePerformIO $ newMVar ()


{- Print debugging in a concurrent setting can be painful sometimes due to output
becoming interpolated. This `putStrLn` alternative uses an MVar to ensure all
printouts are atomic and un-garbled.
-}
atomicPutStrLn :: String -> IO ()
atomicPutStrLn str =
  withMVar printLock (\_ -> hPutStr stdout (str <> "\n") >> hFlush stdout)

atomicPutStrLn_ :: Text -> IO ()
atomicPutStrLn_ text =
  atomicPutStrLn $ T.unpack text

atomicPutStrLnDebug :: String -> IO ()
atomicPutStrLnDebug s = do
  onlyWhen (isDebug_) $ atomicPutStrLn s


{- Wrap an IO in basic runtime information
   Note: this is a very naive implementation and may not always work right,
   i.e. if the IO value is not fully evaluated
-}
-- track :: _ -> IO a -> IO a
track label io = do
  -- pid <- getPid_
  -- m1 <- getPidMem pid
  -- a1 <- Mem.getAllocationCounter
  m <- getTime Monotonic
  p <- getTime ProcessCPUTime
  t <- getTime ThreadCPUTime
  !res <- io
  m_ <- getTime Monotonic
  p_ <- getTime ProcessCPUTime
  t_ <- getTime ThreadCPUTime
  -- a2 <- Mem.getAllocationCounter
  -- m2 <- getPidMem pid

  -- fprint ("⏱  " % label % ": " % timeSpecs % " " % timeSpecs % " " % timeSpecs % " (" % string % ", " % string % ", " % string % ")\n") m m_ p p_ t t_ m1 m2 (show pid)
  whenDebug $ fprint ("⏱  " % label % ": " % timeSpecs % " " % timeSpecs % " " % timeSpecs % "\n") m m_ p p_ t t_

  pure res


{-| Experimental: "pure" version of track that attempts to calculate the time to fully evaluate a lazy value

This may be completely misguided, I don't understand Haskell laziness deeply enough yet, so this is more
and exploration than something that can be relied on.

The problem this is trying to solve is having more detailed breakdowns of timing within Eval test suite,
given that `track` is for top level IO currently.

What is unclear though is whether given all the laziness involved, whether this procedural _looking_ code
actually _behaves_ with the implied semantics of how its written, or not.

-}
track_ :: (NFData a) => String -> a -> (String, a)
track_ label value = do
  unsafePerformIO $ do
    m <- getTime Monotonic
    p <- getTime ProcessCPUTime
    t <- getTime ThreadCPUTime
    !x <- deepseq value (pure 1)
    m_ <- getTime Monotonic
    p_ <- getTime ProcessCPUTime
    t_ <- getTime ThreadCPUTime
    let s = formatToString ("(" % timeSpecs % " " % timeSpecs % " " % timeSpecs % ")\n") m m_ p p_ t t_
    pure (s, value)


-- | returns Just pid or Nothing if process has already exited
-- https://stackoverflow.com/questions/27388099/how-to-get-the-process-id-of-a-created-process-in-haskell
getPid_ = do
  -- (_,_,_,ph) <- createProcess $ shell "echo $$"
  (_,_,_,ph) <- createProcess $ shell "echo $$"
  getPid ph

  -- withProcessHandle ph go
  -- where
  --   go ph_ = case ph_ of
  --              OpenHandle x   -> return $ Just x
  --              ClosedHandle _ -> return Nothing


getPidMem pid =
  case pid of
    Just pid_ -> do
      (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "ps" ["-o", "rss=", "-o", "vsz=", "-o", "pid=", show pid_] ""
      pure $ trim $ remdups stdout

    Nothing ->
      pure "x"


remdups :: String -> String
remdups [] = []
remdups [x] = [x]
remdups (x1:x2:xs)
        | x1==x2 = remdups (x2:xs)
        | otherwise = x1:remdups (x2:xs)

trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs


{- GHCI thread management

Developing threaded processes in GHCI can be rather tricky, as threads are directly
invoked from the main GHCI thread, so they don't die unless you kill GHCI and reload,
a rather slow process.

In order to get closer to the holy grail of "":r + kill + reload threads", useful
when working on and testing a daemon, the `trackedForkIO` function is a drop-in
replacement for `forkIO`, which paired with `killTrackedThreads` lets us cleanup
after a `:r` and avoid issues like a socket port already being in use!

-}

trackedForkIO :: String -> IO () -> IO ()
trackedForkIO label io = do
  threadId <- forkIO io
  trackGhciThread label threadId


trackGhciThread :: String -> ThreadId -> IO ()
trackGhciThread label threadId =
  modifyMVar_ ghciThreads
    (\threads -> do
      debug $ "👀  Tracking GHCI thread '" ++ label ++ "':" ++ show threadId
      pure $ threadId:threads
    )


killTrackedThreads :: IO ()
killTrackedThreads = do
  modifyMVar_ ghciThreads
    (\threads -> do
      case threads of
        [] -> do
          debug $ "No tracked GHCI threads to kill."
          pure []
        threads -> do
          debug $ "🔪  Killing tracked GHCI threads: " ++ show threads
          mapM killThread threads
          pure []
    )


-- https://stackoverflow.com/questions/16811376/simulate-global-variable trick
{-# NOINLINE ghciThreads #-}
ghciThreads :: MVar [ThreadId]
ghciThreads = unsafePerformIO $ newMVar []


-- System

bash :: String -> IO String
bash command = do
  (exit, stdOut, stdErr) <- c_ "bash" ["-c", command] ""
  pure $ show $ stdErr <> stdOut

bashq :: String -> IO String
bashq command = do
  (exit, stdOut, stdErr) <- cq_ "bash" ["-c", command] ""
  pure $ show $ stdErr <> stdOut


-- Call executable and print output in debug mode
c_ :: String -> [String] -> String -> IO (GHC.IO.Exception.ExitCode, String, String)
c_ bin args input = do
  atomicPutStrLnDebug $ "🤖  " <> bin <> " " <> show args <> " " <> input
  (exit, stdOut, stdErr) <- System.Process.readProcessWithExitCode bin args input
  -- This doesn't quite work for debugging because bash will honour the sequence codes of the output
  -- which gives us confusing results. I.e. Elm compilation clears the buffer during operation
  -- onlyWhen (Prelude.length stdErr > 0) $ atomicPutStrLnDebug $ "└── stdErr: " <> stdErr
  -- onlyWhen (Prelude.length stdOut > 0) $ atomicPutStrLnDebug $ "└── stdOut: " <> stdOut
  -- This doesn't happen with the show instance as it escapes sequences inside the string
  atomicPutStrLnDebug $ "└── " <> show (exit, stdOut, stdErr)
  pure $ (exit, stdOut, stdErr)


-- Call quiet, don't print output in debug mode
cq_ :: String -> [String] -> String -> IO (GHC.IO.Exception.ExitCode, String, String)
cq_ bin args input = do
  atomicPutStrLnDebug $ "🤖  " <> bin <> " " <> show args <> " " <> input
  (exit, stdOut, stdErr) <- System.Process.readProcessWithExitCode bin args input
  pure $ (exit, stdOut, stdErr)


execCombineStdOutErr :: String -> [String] -> String -> IO String
execCombineStdOutErr bin args input = do
  (exit, stdOut, stdErr) <- c_ bin args input
  pure $ stdErr <> stdOut


requireBinary :: String -> IO FilePath
requireBinary name = do
  x <- Dir.findExecutable name
  case x of
    Just path -> pure path
    _ -> do
      let msg = concat ["💥💥💥 please install the following required binary: ", name]
      atomicPutStrLn msg
      error msg


-- Re-exports

(&) = (Data.Function.&)


-- Strings

type Text = T.Text
type TextLazy = TL.Text
type TextBuilder = TLB.Builder
type Bs = BS.ByteString
type BsLazy = BSL.ByteString
type Builder = B.Builder


-- Helpers for Haskell ecosystem string types chaos

stringToText :: String -> Text
stringToText = T.pack
stringToTextLazy :: String -> TextLazy
stringToTextLazy = TL.pack
stringToBs :: String -> Bs
stringToBs = T.pack >>> textToBs
stringToBsLazy :: String -> BsLazy
stringToBsLazy = T.pack >>> textToBsLazy
stringToBuilder :: String -> Builder
stringToBuilder = T.pack >>> textToBuilder
stringToTextBuilder :: String -> TextBuilder
stringToTextBuilder = TLB.fromString

textToString :: Text -> String
textToString = T.unpack
textToTextLazy :: Text -> TextLazy
textToTextLazy = TL.fromStrict
textToBs :: Text -> Bs
textToBs = T.encodeUtf8
textToBsLazy :: Text -> BsLazy
textToBsLazy = TL.fromStrict >>> TL.encodeUtf8
textToBuilder :: Text -> Builder
textToBuilder = textToBs >>> bsToBuilder
textToTextBuilder :: Text -> TextBuilder
textToTextBuilder = TLB.fromText

textLazyToString :: TextLazy -> String
textLazyToString = TL.unpack
textLazyToText :: TextLazy -> Text
textLazyToText = TL.toStrict
textLazyToBs :: TextLazy -> Bs
textLazyToBs = TL.toStrict >>> textToBs
textLazyToBsLazy :: TextLazy -> BsLazy
textLazyToBsLazy = TL.encodeUtf8
textLazyToBuilder :: TextLazy -> Builder
textLazyToBuilder = TL.encodeUtf8 >>> B.lazyByteString
textLazyToTextBuilder :: TextLazy -> TextBuilder
textLazyToTextBuilder = TLB.fromLazyText

textBuilderToString :: TextBuilder -> String
textBuilderToString = error "todo:textBuilderToString" -- @TODO
textBuilderToText :: TextBuilder -> Text
textBuilderToText = error "todo:textBuilderToText" -- @TODO
textBuilderToTextLazy :: TextBuilder -> TextLazy
textBuilderToTextLazy = TLB.toLazyText
textBuilderToBs :: TextBuilder -> Bs
textBuilderToBs = error "todo:textBuilderToBs" -- @TODO
textBuilderToBsLazy :: TextBuilder -> BsLazy
textBuilderToBsLazy = error "todo:textBuilderToBsLazy" -- @TODO
textBuilderToBuilder :: TextBuilder -> Builder
textBuilderToBuilder = error "todo:textBuilderToBuilder" -- @TODO

bsToString :: Bs -> String
bsToString = T.decodeUtf8 >>> T.unpack
bsToText :: Bs -> Text
bsToText = T.decodeUtf8
bsToTextLazy :: Bs -> TextLazy
bsToTextLazy = BSL.fromStrict >>> TL.decodeUtf8
bsToBsLazy :: Bs -> BsLazy
bsToBsLazy = BSL.fromStrict
bsToBuilder :: Bs -> Builder
bsToBuilder = B.byteString
bsToUtf8 :: Bs -> Data.Utf8.Utf8 a
bsToUtf8 = T.decodeUtf8 >>> T.unpack >>> Data.Utf8.fromChars

bsLazyToString :: BsLazy -> String
bsLazyToString = TL.decodeUtf8 >>> TL.unpack
bsLazyToText :: BsLazy -> Text
bsLazyToText = TL.decodeUtf8 >>> TL.toStrict
bsLazyToTextLazy :: BsLazy -> TextLazy
bsLazyToTextLazy = TL.decodeUtf8
bsLazyToBs :: BsLazy -> Bs
bsLazyToBs = BSL.toStrict
bsLazyToBuilder :: BsLazy -> Builder
bsLazyToBuilder = B.lazyByteString

builderToString :: Builder -> String
builderToString = B.toLazyByteString >>> bsLazyToString
builderToText :: Builder -> Text
builderToText = builderToTextLazy >>> TL.toStrict
builderToTextLazy :: Builder -> TextLazy
builderToTextLazy = B.toLazyByteString >>> TL.decodeUtf8
builderToBs :: Builder -> Bs
builderToBs = B.toLazyByteString >>> bsLazyToBs
builderToBsLazy :: Builder -> BsLazy
builderToBsLazy = B.toLazyByteString
