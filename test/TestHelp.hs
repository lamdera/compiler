module TestHelp where

import Lamdera
import System.Environment (setEnv, unsetEnv, lookupEnv)
import System.FilePath ((</>))
import Data.Text as T

aggressiveCacheClear project = do
  rmdir $ project </> "elm-stuff"
  rmdir "/Users/mario/.elm"
  rmdir "/Users/mario/elm-home-elmx-test" -- @TODO test without clean cache as well


withDebug io = do
  setEnv "LDEBUG" "1"
  io
  unsetEnv "LAMDERA_APP_NAME"

withDebugPkg io = do
  setEnv "LDEBUG" "1"
  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  io
  unsetEnv "LAMDERA_APP_NAME"
  unsetEnv "LOVR"
