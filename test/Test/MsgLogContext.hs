{-# LANGUAGE OverloadedStrings #-}

module Test.MsgLogContext where

{-| The backend log context (extra/Lamdera/Injection.hs, LamderaBackend): compiles the
backend in test/msg-log-context in dev and --optimize modes and runs it under node with
stand-ins for the runtime globals (test/msg-log-context/harness.js), checking which
global.lamderaCtx update, Debug.log, port Cmds, Http requests and async Task chains see.
-}

import qualified System.Exit as Exit

import Lamdera
import EasyTest
import Test.Helpers

import Make (Flags(..))
import qualified Make
import qualified Generate.Mode as Mode
import qualified Ext.Common
import qualified Lamdera.Injection as Injection
import qualified Lamdera.Relative


suite :: Test ()
suite =
  tests
    [ scope "backend log context, dev build" $ requireBinaryOnPath "node" $ runScenarios "dev" False
    , scope "backend log context, --optimize build" $ requireBinaryOnPath "node" $ runScenarios "prod" True
    , scope "only the backend injection carries the log context" $ do
        let mode = Mode.Dev Nothing
        expectTextContains (Injection.injections Injection.LamderaBackend mode) "lamderaMsgBegin"
        expectTextDoesNotContain (Injection.injections Injection.LamderaFrontend mode) "lamderaCtx"
        expectTextDoesNotContain (Injection.injections Injection.LamderaLive mode) "lamderaCtx"
        expectTextDoesNotContain (Injection.injections Injection.NotLamdera mode) "lamderaCtx"
    ]


runScenarios :: String -> Bool -> Test ()
runScenarios variant optimize = do
  fixture <- io $ Lamdera.Relative.requireDir "test/msg-log-context"
  let
    project = fixture ++ "/" ++ variant
    elmHome = project ++ "/elm-home"
    elmStuff = project ++ "/elm-stuff"
    output = elmStuff ++ "/lbr.js"

  _ <- ioSilenced $ do
    rmdir elmHome
    rmdir elmStuff
    Test.Helpers.withElmHome elmHome $
      Ext.Common.withProjectRoot project $
        Make.run ["../src/LBR.elm"] $
          Make.Flags
            { _debug = False
            , _optimize = optimize
            , _output = Just (Make.JS "elm-stuff/lbr.js")
            , _report = Nothing
            , _docs = Nothing
            , _noWire = True
            , _optimizeLegible = False
            , _esm = False
            }

  results <- io $ mapM (\scenario -> do
      (exit, out, err) <- Ext.Common.cq_ "node" [fixture ++ "/harness.js", output, scenario, variant] ""
      pure (scenario, exit, out <> err)
    ) ["hooks", "hooks-sets", "nohooks", "notruntime"]

  io $ rmdir elmHome
  io $ rmdir elmStuff

  mapM_ (\(scenario, exit, out) ->
      scope scenario $
        if exit == Exit.ExitSuccess
          then ok
          else crash $ "harness failed for " ++ variant ++ " " ++ scenario ++ ":\n" ++ take 4000 out
    ) results
