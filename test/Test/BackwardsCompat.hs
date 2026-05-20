{-# LANGUAGE OverloadedStrings #-}

module Test.BackwardsCompat where

import EasyTest
import Ext.Common
import Lamdera hiding ((&))
import Test.Helpers
import qualified Lamdera.Compile

suite :: Test ()
suite = tests $
  [ scope "an vanilla elm project compiled with lamdera should not inject lamdera html modifications" $
    let
      project = "./test/scenario-empty-elm-init"
      bashInScenario c = bash $ "cd " ++ project ++ " && " ++ c

      setup = do
        rmdir $ project ++ "/elm-stuff"

      cleanup _ = do
        pure ()

      test _ = do
        ioSilenced $ do
          -- Clear env vars that may leak from other tests (e.g. Wire tests set LOVR/LTEST
          -- via withEnvVars which is not exception-safe, so they persist on failure)
          Lamdera.unsetEnv "LOVR"
          Lamdera.unsetEnv "LTEST"
          Lamdera.unsetEnv "LDEBUG"
          Lamdera.unsetEnv "ELM_HOME"
          Lamdera.Compile.makeDevHtml project ["src/Main.elm"]

        htmlM <- io $ readUtf8Text $ project ++ "/index.html"

        case htmlM of
          Nothing -> crash "index.html not found"
          Just html -> do
            expectTextDoesNotContain html "apple-mobile-web-app-capable"

    in
    using setup cleanup test
  ]
