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

      maybeJsOutput <- ioSilenced $ do
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
                , _esm = False
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
    , scope "direct function calls (lamdera/compiler PR #41)" $ do
      project <- io $ Lamdera.Relative.requireDir "test/direct-fn-calls"
      let
        elmHome = project ++ "/elm-home"
        elmStuff = project ++ "/elm-stuff"

      maybeJsOutput <- ioSilenced $ do
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
                , _esm = False
                }

        fileContents <- readUtf8Text $ elmStuff ++ "/tmp.js"

        rmdir elmHome
        rmdir elmStuff

        pure fileContents

      case maybeJsOutput of
        Just jsOutput ->
          do
            expectTextContains jsOutput "$fn2$ = function (x, y) {"
            expectTextContains jsOutput "$fn3$ = function (x, y, z) {"
            expectTextContains jsOutput "$Ctor2$ = function (a, b) {"
            expectTextContains jsOutput "$Ctor3$ = function (a, b, c) {"
            expectTextContains jsOutput "$N2$ = function (a, b) {"
            expectTextContains jsOutput "x1 = $author$project$Main$fn1(0)"
            expectTextContains jsOutput "x2 = $author$project$Main$fn2$(0, 0)"
            expectTextContains jsOutput "x3 = $author$project$Main$fn3$(0, 0, 0)"
            expectTextContains jsOutput "c1 = $author$project$Main$Ctor1("
            expectTextContains jsOutput "c2 = $author$project$Main$Ctor2$("
            expectTextContains jsOutput "c3 = $author$project$Main$Ctor3$("
            expectTextContains jsOutput "n1 = 0"
            expectTextContains jsOutput "n2 = $author$project$Main$N2$(0, 0)"

        Nothing ->
          crash "JS output could not be read."
    , scope "direct function calls - mutual recursion" $ do
      project <- io $ Lamdera.Relative.requireDir "test/direct-fn-calls-mutual-recursion"
      let
        elmHome = project ++ "/elm-home"
        elmStuff = project ++ "/elm-stuff"

      maybeJsOutput <- ioSilenced $ do
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
                , _esm = False
                }

        fileContents <- readUtf8Text $ elmStuff ++ "/tmp.js"

        rmdir elmHome
        rmdir elmStuff

        pure fileContents

      case maybeJsOutput of
        Just jsOutput ->
          do
            expectTextContains jsOutput "$Main$a2 = function (n) {"
            expectTextContains jsOutput "$Main$cyclic$a1() {"
            expectTextContains jsOutput "$Main$a1 ="
            expectTextContains jsOutput "$Main$cyclic$a1 = function () {"
            expectTextContains jsOutput "$Main$a1(1)"
            expectTextContains jsOutput "$Main$a2(1)"

            expectTextContains jsOutput "$Main$b2$ = function (m, n) {"
            expectTextContains jsOutput "$Main$cyclic$b1()"
            expectTextContains jsOutput "$Main$b2 = F2("
            expectTextContains jsOutput "$Main$cyclic$b1() {"
            expectTextContains jsOutput "$Main$b1 ="
            expectTextContains jsOutput "$Main$b1$ = function ("
            expectTextContains jsOutput "$Main$cyclic$b1 = function () {"
            expectTextContains jsOutput "$Main$b1$(1, 1)"
            expectTextContains jsOutput "$Main$b2$(1, 1)"

        Nothing ->
          crash "JS output could not be read."
    , scope "direct function calls - mutual recursion with partial application" $ do
      project <- io $ Lamdera.Relative.requireDir "test/direct-fn-calls-mutual-recursion-partial-application"
      let
        elmHome = project ++ "/elm-home"
        elmStuff = project ++ "/elm-stuff"

      maybeJsOutput <- ioSilenced $ do
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
                , _esm = False
                }

        fileContents <- readUtf8Text $ elmStuff ++ "/tmp.js"

        rmdir elmHome
        rmdir elmStuff

        pure fileContents

      case maybeJsOutput of
        Just jsOutput ->
          do
            expectTextContains jsOutput "$Main$a2$ = function (x1, x2, x3, x4, x5) {"
            expectTextContains jsOutput "$Main$a2 = F5"
            expectTextContains jsOutput "$Main$cyclic$a1() {"
            expectTextContains jsOutput "$Main$a2, 1, 2);"
            expectTextContains jsOutput "$Main$a1 ="
            expectTextContains jsOutput "$Main$cyclic$a1 = function () {"
            expectTextContains jsOutput "$Main$a1$ = function ("
            expectTextContains jsOutput "$Main$a2(1, 2, "
            expectTextContains jsOutput "$Main$a1$(3, 4, 5)"
            expectTextContains jsOutput "$Main$a2$(1, 2, 3, 4, 5)"

        Nothing ->
          crash "JS output could not be read."
    ]
