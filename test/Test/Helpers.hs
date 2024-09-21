{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Test.Helpers where

import System.FilePath ((</>))
import Data.Text as T
import qualified Data.Text.Encoding as T

import qualified EasyTest

import Lamdera
import Test.Main (captureProcessResult)
import qualified Test.Main
import qualified Ext.Common
import qualified Lamdera.Relative


aggressiveCacheClear :: FilePath -> IO ()
aggressiveCacheClear project = do
  rmdir $ project </> "elm-stuff"
  rmdir "~/.elm"
  rmdir "~/elm-home-elmx-test" -- @TODO test without clean cache as well


withDebug :: IO a -> IO a
withDebug io = do
  withEnvVars [ ("LDEBUG", "1") ] io


withDebugPkg :: IO a -> IO a
withDebugPkg io = do
  overrides <- Lamdera.Relative.requireDir "~/lamdera/overrides"
  withEnvVars [ ("LDEBUG", "1"), ("LOVR", overrides) ] io


withElmHome :: String -> IO a -> IO a
withElmHome elmHomePath io = do
  withEnvVars [ ("ELM_HOME", elmHomePath) ] io


withTestEnv :: EasyTest.Test a -> EasyTest.Test a
withTestEnv test = do
  EasyTest.io $ do
    setEnv "LDEBUG" "1"
    setEnv "LTEST" "1"
    overrides <- Lamdera.Relative.requireDir "~/lamdera/overrides"
    setEnv "LOVR" overrides
  res <- test
  EasyTest.io $ do
    unsetEnv "LDEBUG"
    unsetEnv "LTEST"
    unsetEnv "LOVR"
  pure res


withProdMode :: IO a -> IO a
withProdMode io =
  -- The presence of LAMDERA_APP_NAME causes lamdera check to decide we're in production mode
  withEnvVars [ ("LDEBUG", "1"), ("LAMDERA_APP_NAME", "test-local") ] io


withEnvVars :: [(String, String)] -> IO a -> IO a
withEnvVars vars io = do
  originalVars <- traverse (\(k, _) -> (k,) <$> lookupEnv k) vars
  traverse (uncurry setEnv) vars
  res <- io
  traverse (uncurry forceEnv) originalVars
  pure res


cp :: String -> String -> IO ()
cp src_ dest = do
  src <- Lamdera.Relative.requireFile "cp" src_
  Lamdera.copyFile src dest


rm :: String -> IO ()
rm path_ = do
  path <- Lamdera.Relative.requireFile "rm" path_
  Lamdera.remove path


catchOutput :: IO () -> EasyTest.Test Text
catchOutput action = do
  -- https://hackage.haskell.org/package/main-tester-0.2.0.1/docs/Test-Main.html
  pr <- EasyTest.io $ captureProcessResult action
  -- EasyTest.io $ hindent pr
  pure $ T.pack $ show pr


catchOutputStdErr :: IO () -> EasyTest.Test Text
catchOutputStdErr action = do
  -- https://hackage.haskell.org/package/main-tester-0.2.0.1/docs/Test-Main.html
  pr <- EasyTest.io $ captureProcessResult action
  -- @TODO improve this to actually pull out values
  pure $ T.decodeUtf8 $ Test.Main.prStderr pr


captureProcessOutput action = do
    pr <- captureProcessResult action
    -- hindent pr
    pure $ T.pack $ show pr

