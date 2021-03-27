module Test.Helpers where


import Lamdera
import System.Environment (setEnv, unsetEnv, lookupEnv)
import System.FilePath ((</>))
import Data.Text as T


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
