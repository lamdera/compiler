{-# LANGUAGE OverloadedStrings #-}

module Test where

import EasyTest
import Lamdera
import qualified Test.LamderaGenerated
import qualified Test.Snapshot
import qualified Test.Lamdera
import qualified Test.Check
import qualified Test.Wire
import Test.Helpers

import qualified Lamdera.Compile
import qualified Lamdera.Evaluate
import qualified Lamdera.CLI.Check
import qualified Ext.Query.Canonical

import Develop

import Ext.Common (trackedForkIO)
import qualified System.Directory as Dir
import System.FilePath ((</>))

{-

1. Put the following into ~/.ghci

:set -fbyte-code
:set -fobject-code
:def rr const $ return $ unlines [":r","Test.target"]
:set prompt "\ESC[34mλ: \ESC[m"

Last line is optional, but it's cool! Lambda prompt!

2. Run `stack ghci`

3. Then a feedback loop goes as follows;

  - Make changes to Haskell Wire code
  - Run `:rr` to recompile + typecheck and auto-run Test.target
  - fix any issues, then :rr again
  - if you want to recompile without running, do :r

Easier to change the target definition than constantly adjust the :def!

Press up arrow to get history of prior commands.

-}

-- Current target for ghci :rr command. See ~/.ghci config file, which should contain
-- something like `:def rr const $ return $ unlines [":r","Test.target"]`

-- target = buildTestHarnessToProductionJs
target = do
  Dir.withCurrentDirectory "/Users/mario/dev/projects/lamdera-dashboard" $ Lamdera.CLI.Check.run () ()



checkProjectCompiles = do
  setEnv "LDEBUG" "1"
  Lamdera.CLI.Check.checkUserProjectCompiles "/Users/mario/dev/test/lamdera-init"


-- target = Test.all
-- target = Test.Wire.all
-- target = checkUserConfig
-- target = Test.Wire.buildAllPackages
-- target = Lamdera.CLI.Login.run () ()
-- target = Dir.withCurrentDirectory "/Users/mario/dev/projects/lamdera-test" $ Lamdera.CLI.Reset.run () ()
-- target = Lamdera.Diff.run
-- target = Lamdera.ReverseProxy.start

-- target = Test.Check.mockBuildSh "/Users/mario/lamdera-deploys/test-local-v1" "test-local"
-- target = Test.Check.mockBuildSh "/Users/mario/dev/test/lamdera-init" "test-local"

-- target = do
--   setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
--   setEnv "LDEBUG" "1"
--   setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"
--   let project = "/Users/mario/lamdera/test/v1"
--   -- let project = "/Users/mario/dev/test/elm-color"
--   -- let project = "/Users/mario/dev/test/elm-regex"
--   -- Bust Elm's caching with this one weird trick!
--   Dir.removeDirectoryRecursive $ project </> "/Users/mario/elm-home-elmx-test"
--   Dir.removeDirectoryRecursive $ project </> "elm-stuff"
--   -- touch $ project </> "src/Frontend.elm"
--   -- touch $ project </> "src/Backend.elm"
--   -- Lamdera.Compile.make_ project
--   Lamdera.Compile.makeDev project "src/Frontend.elm"
--   Lamdera.Compile.makeDev project "src/Backend.elm"
--   unsetEnv "LOVR"
--   unsetEnv "LDEBUG"
--   unsetEnv "ELM_HOME"


buildTestHarnessToProductionJs = do
  -- setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "LDEBUG" "1"

  -- let p = "/Users/mario/lamdera/test/v1"
  -- let p = "/Users/mario/dev/projects/lamdera-dashboard"
  let p = "/Users/mario/dev/test/lamdera-init"

  Lamdera.Compile.makeHarnessDevJs p


{- Dynamic testing of lamdera live with managed thread kill + reload -}
target2 = do

  setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "LDEBUG" "1"

  let p = "/Users/mario/lamdera/test/v1"
  -- let p = "/Users/mario/dev/test/lamdera-init"
  -- let p = "/Users/mario/dev/test/nu-ashworld-lamdera"
  -- let p = "/Users/mario/dev/projects/otstats"

  -- rmdir "/Users/mario/.elm"
  -- rmdir $ p <> "/elm-stuff"


  Dir.setCurrentDirectory p
  withCurrentDirectory p $ trackedForkIO "Test.liveReloadLive" $ withCurrentDirectory p $ Develop.run () (Develop.Flags Nothing)

  -- Doing this actually makes no sense in the :rr context, as the thread is long-running so it's the same as
  -- disabling the ENV vars mid-run! But leaving it here as a reminder, because it _does_ pollute the ENV
  -- if we don't cleanup and then move on to use the same ghci session for other things!
  -- unsetEnv "LOVR"
  -- unsetEnv "LDEBUG"


{- WIP interpreter -}
target1 = do
  withDebug $
    Lamdera.Evaluate.exec
      "/Users/mario/dev/projects/lamdera-compiler/test/scenario-interpreter/src/Test/Basic.elm"
      "suite"

    -- Ext.Query.Canonical.loadFileSourceValue
    --   "/Users/mario/dev/projects/lamdera-compiler/test/scenario-interpreter/src/Test/Basic.elm"
    --   "suite"


-- target = do
--   let project = "/Users/mario/dev/projects/lamdera/overrides/packages/elm/bytes/1.0.8"
--   Lamdera.Compile.make_ project
  -- Lamdera.Canonical.showDef
  --   project
  --   "src/Bytes/Encode.elm"
  --   "withDebug"




all =
  EasyTest.run allTests


single = do

  -- rmdir "/Users/mario/.elm"
  -- rmdir "/Users/mario/elm-home-elmx-test" -- @TODO test without clean cache as well
  -- @TODO later when we've restored
  -- rmdir "/Users/mario/.lamdera"

  rmdir "/Users/mario/lamdera/test/v1/elm-stuff"

  EasyTest.rerunOnly 2433968847666733451 "evergreen -> type snapshots -> .alltypes e2e to disk for lamdera/test/v1/"
    allTests


allTests =
  tests
    [ scope "Test.Lamdera -> " $ Test.Lamdera.suite
    , scope "Test.Snapshot -> " $ Test.Snapshot.suite
    , scope "Test.Wire -> " $ Test.Wire.suite
    , Test.LamderaGenerated.suite
    ]
