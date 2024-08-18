{-# LANGUAGE OverloadedStrings #-}

module Test.JsOutput where

import Lamdera
import EasyTest
import Test.Helpers

import qualified Init
import Make (Flags(..))
import qualified Make
import qualified Ext.Common
import qualified Lamdera.Relative

suite :: Test ()
suite =
  tests
    [ scope "a tail-call function will not re-assign values to itself unnecessarily" $ do
      project <- io $ Lamdera.Relative.requireDir "test/generated-javascript"
      let
        elmHome = project ++ "/elm-home"
        elmStuff = project ++ "/elm-stuff"

      maybeJsOutput <- io $ do
        rmdir elmHome
        rmdir elmStuff

        Test.Helpers.withElmHome elmHome $
          Ext.Common.withProjectRoot project $
            Make.run ["src/Main.elm"] $
              Make.Flags
                { _debug = False
                , _optimize = True
                , _output = Just (Make.JS "elm-stuff/tmp.js")
                , _report = Nothing
                , _docs = Nothing
                , _noWire = True
                , _optimizeLegible = False
                }

        fileContents <- readUtf8Text $ elmStuff ++ "/tmp.js"

        rmdir elmHome
        rmdir elmStuff

        pure fileContents

      case maybeJsOutput of
        Just jsOutput ->
          do
            expectTextContains jsOutput "$temp$list = xs"
            expectTextDoesNotContain jsOutput "$temp$fn = fn"

        Nothing ->
          crash "JS output could not be read."
    ]
