{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Caching where

import EasyTest
import NeatInterpolation

import Test.Helpers
import Lamdera hiding ((&))
import Ext.Common

import Lamdera.Compile


all = EasyTest.run suite

suite :: Test ()
suite = tests $
  let
    project = "./test/scenario-empty-elm-init"

    bashInScenario c = bash $ "cd " ++ project ++ " && " ++ c
  in
  [ scope "a package compiled with Elm first will gracefully recompile with Lamdera" $ do
      io $ do
        rmdir $ project ++ "/elm-home"
        setEnv "ELM_HOME" $ project ++ "/elm-home"
        -- @TODO remove this?
        setEnv "LDEBUG" "1"

      elmCompilation <- io $ do
        rmdir $ project ++ "/elm-stuff"
        writeUtf8 (project ++ "/src/Main.elm") [text|
            module Main exposing (main)

            import Html


            main =
                Html.text "hello!"
        |]
        bashInScenario "ELM_HOME=./elm-home ~/.local/bin/elm-0.19.1-official make src/Main.elm --output=/dev/null"

      expect $ elmCompilation & stringContains "Success!"

      lamderaCompilation <- io $ do
        rmdir $ project ++ "/elm-stuff"
        writeUtf8 (project ++ "/src/Main.elm") [text|
            module Main exposing (main)

            import Html


            main =
                Html.text "hello!"


            will_fail_on_elm_caches =
                Html.w3_encode_Html
        |]

        withDebug $ captureProcessOutput $ Lamdera.Compile.makeDev project ["src/Main.elm"]

      io $ atomicPutStrLn_ lamderaCompilation
      expect $ lamderaCompilation & textContains "Success!"

      io $ bashInScenario "git checkout src/Main.elm"

      pure ()
  ]
