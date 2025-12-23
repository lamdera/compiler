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


  -- @TODO currently the type restriction checks only happen in lamdera check
  -- We should probably move them to lamdera make now that we have the isLamdera detection
  -- but that's a fair bit of work – so revisit these tests when that's done

  , pending $ scope "compile Elm app with WebGL.Texture in FrontendModel" $ do
        project <- io $ Lamdera.Relative.requireDir "test/scenario-webgl-texture"

        _ <- io $ rmdir (project </> "elm-stuff")

        actual <- catchOutput $ Lamdera.Compile.makeDev project [ "src/Frontend.elm", "src/Types.elm" ]

        expectTextContains actual "Success! Compiled 3 modules."

  , pending $ scope "compilation should fail with WebGL.Texture in backend contexts" $ do
        project <- io $ Lamdera.Relative.requireDir "test/scenario-webgl-texture"

        _ <- io $ rmdir (project </> "elm-stuff")

        actual <- catchOutput $ Lamdera.Compile.makeDev project [ "src/Backend.elm" ]

        -- Should fail with kernel error for browser-only types
        expectTextContains actual "can only be used in the frontend"
  ]
