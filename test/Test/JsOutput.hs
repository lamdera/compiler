{-# LANGUAGE OverloadedStrings #-}

module Test.JsOutput where

import Lamdera
import EasyTest
import Test.Helpers

import qualified Init
import Make (Flags(..))
import qualified Make
import qualified Ext.Common

suite :: Test ()
suite =
  tests
    [ scope "a tail-call function will not re-assign values to itself unnecessarily" $ do
      let
        project = "./test/generated-javascript"

        elmHome = project ++ "/elm-home"
        elmStuff = project ++ "/elm-stuff"

      io $ rmdir elmHome
      io $ rmdir elmStuff

      io $
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

      textM <- io $ readUtf8Text $ elmStuff ++ "/tmp.js"

      io $ rmdir elmHome
      io $ rmdir elmStuff

      case textM of
        Just jsOutput ->
          do
            expectTextContains jsOutput "$temp$list = xs"
            expectTextDoesNotContain jsOutput "$temp$fn = fn"

        Nothing ->
          crash "JS output could not be read."
    ]
