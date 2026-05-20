{-# Language BangPatterns #-}
{-# Language FunctionalDependencies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module EasyTest where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List
import Data.Map (Map)
import Data.Word
import Data.IORef
import GHC.Stack
import System.Random (Random)
import qualified Control.Concurrent.Async as A
import qualified Data.Map as Map
import qualified System.Random as Random
import qualified Text.PrettyPrint.ANSI.Leijen as P
import System.IO (Handle, hFlush, hPutStr, stdout, stderr, hClose, hSetBuffering, BufferMode(..))
import GHC.IO.Handle (hDuplicate, hDuplicateTo)

import Data.Function ((&))
import qualified Data.Text as T
import Data.TreeDiff
import System.IO (openTempFile)
import System.Process (readProcessWithExitCode)
import qualified System.Directory as Dir
import Lamdera
import qualified Ext.Common

data Status = Failed | Passed !Int | Skipped | Pending

combineStatus :: Status -> Status -> Status
combineStatus Skipped s = s
combineStatus s Skipped = s
combineStatus _ Pending = Pending
combineStatus Pending _ = Pending
combineStatus Failed _ = Failed
combineStatus _ Failed = Failed
combineStatus (Passed n) (Passed m) = Passed (n + m)

data Env =
  Env { rng :: TVar Random.StdGen
      , messages :: String
      , results :: TBQueue (Maybe (TMVar (String, Status)))
      , note_ :: String -> IO ()
      , allow :: String
      , outputBuffer :: IORef [String] }

newtype Test a = Test (ReaderT Env IO (Maybe a))

io :: IO a -> Test a
io = liftIO

ioSilenced :: IO a -> Test a
ioSilenced action = do
  buf <- asks outputBuffer
  result <- liftIO $ withCapturedOutput action
  case result of
    Left (captured, err) -> do
      liftIO $ modifyIORef buf ((captured ++ "\n" ++ show err) :)
      Test $ do
        putResult Failed
        pure Nothing
    Right (a, captured) -> do
      liftIO $ modifyIORef buf (captured :)
      pure a

withCapturedOutput :: IO a -> IO (Either (String, SomeException) (a, String))
withCapturedOutput action = do
  tmpDir <- Dir.getTemporaryDirectory
  (tmpPath, tmpHandle) <- openTempFile tmpDir "test-output-"
  hSetBuffering tmpHandle LineBuffering
  oldStdout <- hDuplicate stdout
  oldStderr <- hDuplicate stderr
  hFlush stdout
  hFlush stderr
  hDuplicateTo tmpHandle stdout
  hDuplicateTo tmpHandle stderr
  hClose tmpHandle
  result <- try action
  hFlush stdout
  hFlush stderr
  hDuplicateTo oldStdout stdout
  hDuplicateTo oldStderr stderr
  hClose oldStdout
  hClose oldStderr
  captured <- readFile tmpPath
  length captured `seq` pure ()
  Dir.removeFile tmpPath
  case result of
    Left err -> pure $ Left (captured, err)
    Right a -> pure $ Right (a, captured)

atomicLogger :: IO (String -> IO ())
atomicLogger = do
  pure atomicPutStrLn

expect' :: HasCallStack => Bool -> Test ()
expect' False = crash "unexpected"
expect' True = pure ()

expect :: HasCallStack => Bool -> Test ()
expect False = crash "unexpected"
expect True = ok

expectEqual :: (Eq a, Show a) => a -> a -> Test ()
expectEqual expected actual =
  if expected == actual
    then ok
    else crash $ unlines ["", (show actual), "** did not equal expected value **", (show expected)]

expectTextContains :: T.Text -> T.Text -> Test ()
expectTextContains haystack needle =
  if textContains needle haystack
    then ok
    else crash $ unlines
      [ ""
      , "💥💥💥"
      , "Inside this haystack:"
      , "▶️"
      , (T.unpack haystack)
      , "◀️"
      , "I could not find this needle:"
      , "▶️"
      , (T.unpack needle)
      , "◀️"
      ]

expectTextContainsAll :: T.Text -> [T.Text] -> Test ()
expectTextContainsAll haystack needles =
  let
    missingNeedles = filter (\needle -> not $ textContains needle haystack) needles
  in
  if null missingNeedles
    then ok
    else crash $ unlines
      [ ""
      , "💥💥💥"
      , "Inside this haystack:"
      , "▶️"
      , (T.unpack haystack)
      , "◀️"
      , "I could not find these needles:"
      , "▶️"
      , (T.unpack $ T.intercalate "◀️\n▶️" missingNeedles)
      , "◀️"
      ]

expectTextDoesNotContain :: T.Text -> T.Text -> Test ()
expectTextDoesNotContain haystack needle =
  if textContains needle haystack
    then crash $ unlines
      [ ""
      , "💥💥💥"
      , "Inside this haystack:"
      , "▶️"
      , (T.unpack haystack)
      , "◀️"
      , "I found this needle:"
      , "▶️"
      , (T.unpack needle)
      , "◀️"
      ]
    else ok


textStripped :: T.Text -> T.Text
textStripped t =
  t
    & T.lines
    & fmap T.stripEnd
    & T.unlines


ensureBinaryIcdiff :: String -> Test ()
ensureBinaryIcdiff t = do
  exists <- liftIO $ Ext.Common.bash "command -v icdiff"
  if exists /= ""
    then ok
    else
      crash $ "I could not find a binary tool used in a test: " ++ t


expectEqualTextTrimmed :: T.Text -> T.Text -> Test ()
expectEqualTextTrimmed expected actual =
  let
    realExpected = textStripped expected
    realActual = textStripped actual
  in
  if realExpected == realActual
    then
      ok
    else do
      _ <- ensureBinaryIcdiff "icdiff"
      diff <- liftIO $ do
        icdiff realExpected realActual

      crash $
        T.unpack $
        T.unlines
          [ ""
          -- , "➡️  the result:"
          -- , (realActual)
          -- , "⬅️  did not equal expected value:"
          -- , (realExpected)
          , "💥💥💥"
          , ""
          , T.pack diff
          -- , T.pack $ show $ prettyEditExpr $ ediff (realExpected) (realActual)
          ]

expectEqualFormat :: (Eq a, Show a) => a -> a -> Test ()
expectEqualFormat expected actual =
  if expected == actual
    then
      ok
    else do
      _ <- ensureBinaryIcdiff "icdiff"
      diff <- liftIO $ do
        icdiff (hindentFormatValue expected) (hindentFormatValue actual)

      crash $
        T.unpack $
        T.unlines
          [ ""
          -- , "➡️  the result:"
          -- , (realActual)
          -- , "⬅️  did not equal expected value:"
          -- , (realExpected)
          , "💥💥💥"
          , ""
          , T.pack diff
          -- , T.pack $ show $ prettyEditExpr $ ediff (realExpected) (realActual)
          ]

expectNotEqual :: (Eq a, Show a) => a -> a -> Test ()
expectNotEqual forbidden actual =
  if forbidden /= actual then ok
  else crash $ unlines ["", show actual, "** did equal the forbidden value **", show forbidden]

expectJust :: HasCallStack => Maybe a -> Test a
expectJust Nothing = crash "expected Just, got Nothing"
expectJust (Just a) = ok >> pure a

expectRight :: HasCallStack => Either e a -> Test a
expectRight (Left _) = crash "expected Right, got Left"
expectRight (Right a) = ok >> pure a

expectLeft :: HasCallStack => Either e a -> Test e
expectLeft (Left e) = ok >> pure e
expectLeft (Right _) = crash "expected Left, got Right"

tests :: [Test ()] -> Test ()
tests = msum

-- | Run all tests whose scope starts with the given prefix
runOnly :: String -> Test a -> IO ()
runOnly prefix t = do
  logger <- atomicLogger
  seed <- abs <$> Random.randomIO :: IO Int
  run' seed logger prefix t

-- | Run all tests with the given seed and whose scope starts with the given prefix
rerunOnly :: Int -> String -> Test a -> IO ()
rerunOnly seed prefix t = do
  logger <- atomicLogger
  run' seed logger prefix t

run :: Test a -> IO ()
run = runOnly ""

rerun :: Int -> Test a -> IO ()
rerun seed = rerunOnly seed []

spinnerFrames :: [Char]
spinnerFrames = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

showProgress :: Handle -> Int -> IORef Int -> IORef String -> String -> IO ()
showProgress tty cols spinnerRef lastMsgRef msg = do
  writeIORef lastMsgRef msg
  renderSpinner tty cols spinnerRef msg

getTerminalWidth :: IO Int
getTerminalWidth = do
  (_, out, _) <- readProcessWithExitCode "sh" ["-c", "stty size < /dev/tty"] ""
  pure $ case words out of
    [_, cols] -> maybe 80 id (readMaybe cols)
    _         -> 80
  where
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _         -> Nothing

renderSpinner :: Handle -> Int -> IORef Int -> String -> IO ()
renderSpinner tty cols spinnerRef msg = do
  idx <- readIORef spinnerRef
  let spinner = spinnerFrames !! (idx `mod` length spinnerFrames)
      prefix = " " ++ [spinner] ++ "  "
      maxMsg = cols - length prefix - 3
      truncated = if length msg > maxMsg then take maxMsg msg ++ "..." else msg
  hPutStr tty $ "\r\ESC[K\ESC[35m" ++ prefix ++ truncated ++ "\ESC[0m"
  hFlush tty
  writeIORef spinnerRef (idx + 1)

clearProgress :: Handle -> IO ()
clearProgress tty = do
  hPutStr tty "\r\ESC[K"
  hFlush tty

run' :: Int -> (String -> IO ()) -> String -> Test a -> IO ()
run' seed note allow (Test t) = do
  let !rng = Random.mkStdGen seed
  resultsQ <- atomically (newTBQueue 50)
  rngVar <- newTVarIO rng
  results <- atomically $ newTVar Map.empty
  failedOutputMap <- newIORef (Map.empty :: Map String [String])
  spinnerRef <- newIORef (0 :: Int)
  lastMsgRef <- newIORef ""
  spinnerActive <- newIORef True
  tty <- hDuplicate stdout
  cols <- getTerminalWidth
  buf <- newIORef ([] :: [String])
  ticker <- A.async $ do
    let tick = do
          active <- readIORef spinnerActive
          when active $ do
            msg <- readIORef lastMsgRef
            when (not $ null msg) $ renderSpinner tty cols spinnerRef msg
          threadDelay 120000
          tick
    tick
  rs <- A.async . forever $ do
    -- note, totally fine if this bombs once queue is empty
    Just result <- atomically $ readTBQueue resultsQ
    (msgs, passed) <- atomically $ takeTMVar result
    atomically $ modifyTVar results (Map.insertWith combineStatus msgs passed)
    resultsMap <- readTVarIO results
    case Map.findWithDefault Skipped msgs resultsMap of
      Skipped -> pure ()
      Pending -> do
        clearProgress tty
        note $ "🚧  " ++ msgs
      Passed _ -> do
        showProgress tty cols spinnerRef lastMsgRef msgs
      Failed -> do
        clearProgress tty
        -- Capture buffered output for this failure
        buffered <- atomicModifyIORef buf (\b -> ([], b))
        modifyIORef failedOutputMap (Map.insertWith (++) msgs (reverse buffered))
        note $ "💥  " ++ msgs
  let bufNote msg = modifyIORef buf (msg :)
  e <- try (runReaderT (void t) (Env rngVar [] resultsQ bufNote allow buf)) :: IO (Either SomeException ())
  case e of
    Left e -> do
      clearProgress tty
      note $ "Exception while running tests: " ++ show e
    Right () -> pure ()
  atomically $ writeTBQueue resultsQ Nothing
  _ <- A.waitCatch rs
  writeIORef spinnerActive False
  A.cancel ticker
  clearProgress tty
  hClose tty
  resultsMap <- readTVarIO results
  failedOutputs <- readIORef failedOutputMap
  let
    resultsList = Map.toList resultsMap
    succeededList = [ n | (_, Passed n) <- resultsList ]
    succeeded = length succeededList
    failures = [ a | (a, Failed) <- resultsList ]
    failed = length failures
    pendings = [ a | (a, Pending) <- resultsList ]
    pending = length pendings
    pendingSuffix = if pending == 0 then "👍 🎉" else ""
    testsPlural n = show n ++ " " ++ if n == 1 then "test" else "tests"
    line = "------------------------------------------------------------"
  note line
  note ""
  when (pending > 0) $ do
    note $ "🚧  " ++ testsPlural pending ++ " still pending (pending scopes below):"
    note $ "    " ++ intercalate "\n    " (map (show . takeWhile (/= '\n')) pendings)
  case failures of
    [] ->
      case succeeded of
        0 -> do
          note "😶  hmm ... no test results recorded"
          note "Tip: use `ok`, `expect`, or `crash` to record results"
          note "Tip: if running via `runOnly` or `rerunOnly`, check for typos"
        n -> note $ "✅  " ++ testsPlural n ++ " passed, no failures! " ++ pendingSuffix
    (hd:_) -> do
      note $ "  " ++ show succeeded ++ (if failed == 0 then " PASSED" else " passed")
      note $ "  " ++ show (length failures) ++ (if failed == 0 then " failed" else " FAILED (failed scopes below)")
      note $ "    " ++ intercalate "\n    " (map (show . takeWhile (/= '\n')) failures)
      -- Print captured output for each failure
      forM_ failures $ \failScope -> do
        case Map.lookup failScope failedOutputs of
          Just output | not (null output) -> do
            note ""
            note $ "  ┌─ " ++ failScope
            mapM_ (\l -> note $ "  │ " ++ l) output
            note $ "  └─"
          _ -> pure ()
      note ""
      note "  To rerun with same random seed:\n"
      note $ "    Test.rerun " ++ show seed
      note $ "    Test.rerunOnly " ++ show seed ++ " " ++ "\"" ++ hd ++ "\""
      note ""
      note line
      note "❌"
      fail "test failures"

-- | Label a test. Can be nested. A `'.'` is placed between nested
-- scopes, so `scope "foo" . scope "bar"` is equivalent to `scope "foo.bar"`
scope :: String -> Test a -> Test a
scope msg (Test t) = wrap . Test $ do
  env <- ask
  let messages' = case messages env of [] -> msg; ms -> ms ++ ('.':msg)
  case (null (allow env) || take (length (allow env)) msg `isPrefixOf` allow env) of
    False -> putResult Skipped >> pure Nothing
    True -> liftIO $
      runReaderT t (env { messages = messages', allow = drop (length msg + 1) (allow env) })

-- | Log a message
note :: String -> Test ()
note msg = do
  note_ <- asks note_
  liftIO $ note_ msg
  pure ()

-- | Log a showable value
note' :: Show s => s -> Test ()
note' = note . show

-- | Generate a random value
random :: Random a => Test a
random = do
  rng <- asks rng
  liftIO . atomically $ do
    rng0 <- readTVar rng
    let (a, rng1) = Random.random rng0
    writeTVar rng rng1
    pure a

-- | Generate a bounded random value. Inclusive on both sides.
random' :: Random a => a -> a -> Test a
random' lower upper = do
  rng <- asks rng
  liftIO . atomically $ do
    rng0 <- readTVar rng
    let (a, rng1) = Random.randomR (lower,upper) rng0
    writeTVar rng rng1
    pure a

bool :: Test Bool
bool = random

word8 :: Test Word8
word8 = random

-- | Generate a random `Char`
char :: Test Char
char = random

-- | Generate a random `Int`
int :: Test Int
int = random

-- | Generate a random `Double`
double :: Test Double
double = random

-- | Generate a random `Word`
word :: Test Word
word = random

-- | Generate a random `Int` in the given range
-- Note: `int' 0 5` includes both `0` and `5`
int' :: Int -> Int -> Test Int
int' = random'

-- | Generate a random `Char` in the given range
-- Note: `char' 'a' 'z'` includes both `'a'` and `'z'`.
char' :: Char -> Char -> Test Char
char' = random'

-- | Generate a random `Double` in the given range
-- Note: `double' 0 1` includes both `0` and `1`.
double' :: Double -> Double -> Test Double
double' = random'

-- | Generate a random `Double` in the given range
-- Note: `word' 0 10` includes both `0` and `10`.
word' :: Word -> Word -> Test Word
word' = random'

-- | Generate a random `Double` in the given range
-- Note: `word8' 0 10` includes both `0` and `10`.
word8' :: Word8 -> Word8 -> Test Word8
word8' = random'

-- | Sample uniformly from the given list of possibilities
pick :: [a] -> Test a
pick as = let n = length as; ind = picker n as in do
  i <- int' 0 (n - 1)
  Just a <- pure (ind i)
  pure a

picker :: Int -> [a] -> (Int -> Maybe a)
picker _ [] = const Nothing
picker _ [a] = \i -> if i == 0 then Just a else Nothing
picker size as = go where
  lsize = size `div` 2
  rsize = size - lsize
  (l,r) = splitAt lsize as
  lpicker = picker lsize l
  rpicker = picker rsize r
  go i = if i < lsize then lpicker i else rpicker (i - lsize)

-- | Alias for `replicateM`
listOf :: Int -> Test a -> Test [a]
listOf = replicateM

-- | Generate a list of lists of the given sizes,
-- an alias for `sizes `forM` \n -> listOf n gen`
listsOf :: [Int] -> Test a -> Test [[a]]
listsOf sizes gen = sizes `forM` \n -> listOf n gen

-- | Alias for `liftA2 (,)`.
pair :: Test a -> Test b -> Test (a,b)
pair = liftA2 (,)

-- | Generate a `Data.Map k v` of the given size.
mapOf :: Ord k => Int -> Test k -> Test v -> Test (Map k v)
mapOf n k v = Map.fromList <$> listOf n (pair k v)

-- | Generate a `[Data.Map k v]` of the given sizes.
mapsOf :: Ord k => [Int] -> Test k -> Test v -> Test [Map k v]
mapsOf sizes k v = sizes `forM` \n -> mapOf n k v

-- | Catch all exceptions that could occur in the given `Test`
wrap :: Test a -> Test a
wrap (Test t) = Test $ do
  env <- ask
  lift $ runWrap env t

runWrap :: Env -> ReaderT Env IO (Maybe a) -> IO (Maybe a)
runWrap env t = do
  e <- try $ runReaderT t env
  case e of
    Left e -> do
      note_ env (
        "\n💥💥💥 EXCEPTION!!!: 💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥\n\n"
        -- Why doesn't this colouring work?
        ++ P.displayS (P.renderPretty 1 80 ( (P.yellow $ P.text (messages env)))) ""
        ++ ":\n" ++ show (e :: SomeException)
        ++ "\n💥💥💥💥💥💥💥💥💥💥💥💥💥💥💥")
      runReaderT (putResult Failed) env
      pure Nothing
    Right a -> pure a

-- | A test with a setup and teardown
using :: IO r -> (r -> IO ()) -> (r -> Test a) -> Test a
using r cleanup use = Test $ do
  r <- liftIO r
  env <- ask
  let Test t = use r
  a <- liftIO (runWrap env t)
  liftIO (cleanup r)
  pure a

-- | The current scope
currentScope :: Test String
currentScope = asks messages

-- | Prepend the current scope to a logging message
noteScoped :: String -> Test ()
noteScoped msg = do
  s <- currentScope
  note (s ++ (if null s then "" else " ") ++ msg)

-- | Record a successful test at the current scope
ok :: Test ()
ok = Test (Just <$> putResult (Passed 1))

-- | Skip any tests depending on the return value.
done :: Test a
done = Test (pure Nothing)

-- | Explicitly skip this test
skip :: Test ()
skip = Test (Nothing <$ putResult Skipped)

-- | Record a failure at the current scope
crash :: HasCallStack => String -> Test a
crash msg = do
  let trace = callStack
      msg' = msg ++ " " ++ prettyCallStack trace
  Test (Just <$> putResult Failed) >> noteScoped ("FAILURE " ++ msg') >> Test (pure Nothing)

-- skips the test but makes a note of this fact
pending :: Test a -> Test a
pending _ = Test (Nothing <$ putResult Pending)

putResult :: Status -> ReaderT Env IO ()
putResult passed = do
  msgs <- asks messages
  allow <- asks (null . allow)
  r <- liftIO . atomically $ newTMVar (msgs, if allow then passed else Skipped)
  q <- asks results
  lift . atomically $ writeTBQueue q (Just r)

instance MonadReader Env Test where
  ask = Test $ do
    allow <- asks (null . allow)
    case allow of
      True -> Just <$> ask
      False -> pure Nothing
  local f (Test t) = Test (local f t)
  reader f = Test (Just <$> reader f)

instance Monad Test where
  return a = Test $ do
    allow <- asks (null . allow)
    pure $ case allow of
      True -> Just a
      False -> Nothing
  Test a >>= f = Test $ do
    a <- a
    case a of
      Nothing -> pure Nothing
      Just a -> let Test t = f a in t

instance MonadFail Test where
  fail = crash

instance Functor Test where
  fmap = liftM

instance Applicative Test where
  pure = return
  (<*>) = ap

instance MonadIO Test where
  liftIO io = do
    s <- asks (null . allow)
    case s of
      True -> wrap $ Test (Just <$> liftIO io)
      False -> Test (pure Nothing)

instance Alternative Test where
  empty = Test (pure Nothing)
  Test t1 <|> Test t2 = Test $ do
    env <- ask
    (rng1, rng2) <- liftIO . atomically $ do
      currentRng <- readTVar (rng env)
      let (rng1, rng2) = Random.split currentRng
      (,) <$> newTVar rng1 <*> newTVar rng2
    lift $ do
      r1 <- runWrap (env { rng = rng1 }) t1
      (<|> r1) <$> runWrap (env { rng = rng2 }) t2

instance MonadPlus Test where
  mzero = empty
  mplus = (<|>)

-- | Run a test in a separate thread, not blocking for its result.
fork :: Test a -> Test ()
fork t = void (fork' t)

-- | Run a test in a separate thread, return a future which can be used
-- to block on its result.
fork' :: Test a -> Test (Test a)
fork' (Test t) = do
  env <- ask
  tmvar <- liftIO newEmptyTMVarIO
  liftIO . atomically $ writeTBQueue (results env) (Just tmvar)
  r <- liftIO . A.async $ runWrap env t
  waiter <- liftIO . A.async $ do
    e <- A.waitCatch r
    _ <- atomically $ tryPutTMVar tmvar (messages env, Skipped)
    case e of
      Left _ -> pure Nothing
      Right a -> pure a
  pure $ do
    a <- liftIO (A.wait waiter)
    case a of Nothing -> empty
              Just a -> pure a
