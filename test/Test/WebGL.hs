{-# LANGUAGE OverloadedStrings #-}

module Test.WebGL where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import Data.Text as T

import Lamdera
import EasyTest

import qualified Init
import qualified Lamdera.CLI.Login
import qualified Lamdera.AppConfig
import qualified Lamdera.Update
import qualified Lamdera.Compile
import qualified Lamdera.Evergreen.Snapshot
import qualified Lamdera.Relative
import Test.Helpers
import Test.Check

-- import qualified Lamdera.CLI.Check
-- import qualified Lamdera.CLI.Reset
-- import qualified Lamdera.CLI.Live
-- import qualified Lamdera.ReverseProxy
-- import Test.Wire
import qualified Ext.Common


suite :: Test ()
suite = tests
  [ scope "make Elm app containing extension directive in shader" $ do
        project <- io $ Lamdera.Relative.requireDir "test/scenario-webgl-extensions"

        _ <- io $ rmdir (project </> "elm-stuff")

        actual <- catchOutput $ Lamdera.Compile.makeDev project [ "src/Triangle.elm" ]

        expectTextContains actual "Success! Compiled 1 module."
  ]
