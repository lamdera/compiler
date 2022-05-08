{-# LANGUAGE OverloadedStrings #-}

module Test.Helpers where

import System.Environment (setEnv, unsetEnv, lookupEnv)
import System.FilePath ((</>))
import Data.Text as T
import qualified Data.Text.Encoding as T

import EasyTest

import Lamdera
import Test.Main (captureProcessResult)
import qualified Test.Main


aggressiveCacheClear :: FilePath -> IO ()
aggressiveCacheClear project = do
  rmdir $ project </> "elm-stuff"
  rmdir "/Users/mario/.elm"
  rmdir "/Users/mario/elm-home-elmx-test" -- @TODO test without clean cache as well


withDebug :: IO a -> IO a
withDebug io = do
  setEnv "LDEBUG" "1"
  res <- io
  unsetEnv "LDEBUG"
  pure res


withDebugPkg :: IO a -> IO a
withDebugPkg io = do
  setEnv "LDEBUG" "1"
  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  res <- io
  unsetEnv "LDEBUG"
  unsetEnv "LOVR"
  pure res


withProdMode :: IO a -> IO a
withProdMode io = do
  setEnv "LDEBUG" "1"
  setEnv "LAMDERA_APP_NAME" "test-local"
  res <- io
  unsetEnv "LDEBUG"
  unsetEnv "LAMDERA_APP_NAME"
  pure res


cp :: String -> String -> IO ()
cp = Lamdera.copyFile


rm :: String -> IO ()
rm path = Lamdera.remove path


catchOutput :: IO () -> Test Text
catchOutput action = do
  -- https://hackage.haskell.org/package/main-tester-0.2.0.1/docs/Test-Main.html
  pr <- io $ captureProcessResult action
  -- @TODO improve this to actually pull out values
  pure $ T.decodeUtf8 $ "stdout:\n" <> Test.Main.prStdout pr <> "\nstderr:\n" <> Test.Main.prStderr pr


catchOutputStdErr :: IO () -> Test Text
catchOutputStdErr action = do
  -- https://hackage.haskell.org/package/main-tester-0.2.0.1/docs/Test-Main.html
  pr <- io $ captureProcessResult action
  -- @TODO improve this to actually pull out values
  pure $ T.decodeUtf8 $ Test.Main.prStderr pr
