{-# LANGUAGE OverloadedStrings #-}

module Test.BackwardsCompat where

import EasyTest
import Ext.Common
import Lamdera hiding ((&))
import Test.Helpers
import qualified Lamdera.Compile

suite :: Test ()
suite = requireOverrides $ tests $
  [ scope "an vanilla elm project compiled with lamdera should not inject lamdera html modifications" $
    let
      project = "./test/scenario-empty-elm-init"
      bashInScenario c = bash $ "cd " ++ project ++ " && " ++ c

      setup = do
        rmdir $ project ++ "/elm-home"
        rmdir $ project ++ "/elm-stuff"

      cleanup _ = do
        pure ()

      test _ = do
        io $ do
          withEnvVars [("ELM_HOME", project ++ "/elm-home")] $
            Lamdera.Compile.makeDevHtml project ["src/Main.elm"]

        htmlM <- io $ readUtf8Text $ project ++ "/index.html"

        case htmlM of
          Nothing -> crash "index.html not found"
          Just html -> do
            expectTextDoesNotContain html "apple-mobile-web-app-capable"

    in
    using setup cleanup test
  ]
