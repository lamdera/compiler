{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import Control.Monad.Except (catchError)
import Control.Exception (catch, SomeException)
import Prelude hiding (catch)

import qualified System.Directory as Dir
import System.FilePath ((</>))

import EasyTest
import Lamdera
import qualified Test.Snapshot
import qualified Test.Lamdera
import qualified Test.Caching
import qualified Test.Check
import qualified Test.Wire
import qualified Test.Ext.ElmPages.Check
import qualified Test.TypeHashes
import qualified Test.WebGL

import qualified Lamdera.Evergreen.TestMigrationHarness
import qualified Lamdera.Evergreen.TestMigrationGenerator

import Test.Helpers

import qualified Make
import Make (Flags(..))

import qualified Lamdera.Compile
import qualified Lamdera.Evaluate
import qualified Lamdera.CLI.Check
import qualified Lamdera.CLI.Deploy

import Ext.Common (trackedForkIO, bash)
import qualified Ext.Common
import qualified Ext.Query.Canonical

import Develop


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


### Debugging


λ: :set -fbreak-on-error    -- break on exceptions
λ: :trace checkProjectCompiles  -- some function to trace
Stopped at <exception thrown>
_exception :: e = _

We've stopped at an exception. Let's try to look at the code with :list.

[<exception thrown>] λ: :list
Unable to list source for <exception thrown>
Try :back then :list
Because the exception happened in Prelude.head, we can't look at the source directly. But as GHCi informs us, we can go :back and try to list what happened before in the trace.

[<exception thrown>] λ: :back
Logged breakpoint at Broken.hs:2:23-42
_result :: [Integer]
[-1: Broken.hs:2:23-42] λ: :list
1
2  main = print $ head $ filter odd [2, 4, 6]
3
In the terminal, the offending expression filter odd [2, 4, 6] is highlighted in bold font. So this is the expression that evaluated to the empty list in this case.

For more information on how to use the GHCi debugger, see the GHC User's Guide.

-}

-- Current target for ghci :rr command. See ~/.ghci config file, which should contain
-- something like `:def rr const $ return $ unlines [":r","Test.target"]`

-- target = Test.all
-- target = checkProject
--target =
--  Test.Check.checkWithParams "/Users/mario/dev/projects/lamdera-dashboard"
  -- Test.Check.checkWithParams "/Users/mario/lamdera/test/sheep-game" "sheep-game"

-- target = buildTestHarnessToProductionJs
-- target = checkProjectCompiles
-- target = previewProject
target = liveReloadLive
-- target = Test.all
-- target = rerunJust "Lamdera.Evergreen.TestMigrationHarness -> .full first - (WithMigrations 2)"
-- target = checkUserConfig
-- target = Test.Wire.buildAllPackages
-- target = Lamdera.CLI.Login.run () ()
-- target = Dir.withCurrentDirectory "/Users/mario/dev/projects/lamdera-test" $ Lamdera.CLI.Reset.run () ()
-- target = Lamdera.Diff.run
-- target = Lamdera.ReverseProxy.start
-- target = Test.Check.mockBuildSh "/Users/mario/lamdera-deploys/test-local-v1" "test-local"
-- target = Test.Check.mockBuildSh "/Users/mario/dev/test/lamdera-init" "test-local"
-- target = Test.Caching.all

-- target = EasyTest.run Test.WebGL.suite


checkProject = do
  -- let (p, v) = ("/Users/mario/dev/projects/bento-life", "45")
  -- let (p, v) = ("/Users/mario/dev/projects/lamdera-dashboard", "126")
  let (p, v) = ("/Users/mario/dev/test/best-web-vitals", "9")

  Lamdera.remove (p </> "src/Evergreen/Migrate/V" ++ v ++ ".elm")
  Lamdera.rmdir (p </> "src/Evergreen/V" ++ v)

  Ext.Common.setProjectRoot p
  -- Dir.withCurrentDirectory p $ Lamdera.CLI.Check.run () (Lamdera.CLI.Check.Flags { Lamdera.CLI.Check._destructiveMigration = True })
  Dir.withCurrentDirectory p $ Lamdera.CLI.Check.run_ `catch` (\(err :: SomeException) -> pure ())

  Lamdera.Compile.makeDev p ["src/Evergreen/Migrate/V" ++ v ++ ".elm"]


previewProject = do
  let p = "/Users/mario/dev/projects/lamdera-test"
  Dir.withCurrentDirectory p $ Lamdera.CLI.Deploy.run () ()



checkProjectCompiles = do
  setEnv "LDEBUG" "1"
  let
  -- Lamdera.CLI.Check.checkUserProjectCompiles "/Users/mario/dev/test/lamdera-init"
  -- Lamdera.CLI.Check.checkUserProjectCompiles "/Users/mario/dev/projects/lamdera-dashboard"

  --  Lamdera.CLI.Check.checkUserProjectCompiles runs in async so we don't get the trace
      -- root = "/Users/mario/dev/test/style-elements"
      -- root = "/Users/mario/dev/test/lamdera-init"
      -- root = "/Users/mario/dev/test/cdvienne-lamdera-fail/xbus-gui"
      -- scaffold = "src/Main.elm"
      root = "/Users/mario/dev/test/cdvienne-lamdera-fail/xbus-gui/elm-protoc-types"
      scaffold = "src/Proto/Google/Protobuf.elm"

  -- let root = "/Users/mario/dev/test/realia/staging"
  --     scaffold = "src/LFR.elm"
  -- let root = "/Users/mario/dev/projects/elmcraft"
  --     scaffold = "elm-stuff/elm-pages/.elm-pages/Main.elm"
      tmp = lamderaCache root <> "/tmp.js"

  Ext.Common.setProjectRoot root
  Lamdera.Compile.makeDev root [scaffold]


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
liveReloadLive = do

  -- setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
  setEnv "LDEBUG" "1"
  setEnv "EXPERIMENTAL" "1"

  -- let p = "/Users/mario/lamdera/test/v1"
  -- let p = "/Users/mario/dev/test/lamdera-init"
  -- let p = "/Users/mario/dev/projects/refinable"

  let p = withRuntimeRoot "test/scenario-migrations1"

  -- let p = "/Users/mario/lamdera/overrides/packages/elm/bytes/1.0.8/benchmarks"

  -- rmdir "/Users/mario/.elm"
  -- rmdir $ p <> "/elm-stuff"

  -- Dir.setCurrentDirectory p
  -- withCurrentDirectory p $
  trackedForkIO "Test.liveReloadLive" $ Develop.runWithRoot p (Develop.Flags Nothing)

  -- Doing this actually makes no sense in the :rr context, as the thread is long-running so it's the same as
  -- disabling the ENV vars mid-run! But leaving it here as a reminder, because it _does_ pollute the ENV
  -- if we don't cleanup and then move on to use the same ghci session for other things!
  -- unsetEnv "LOVR"
  -- unsetEnv "LDEBUG"


{- WIP interpreter -}
-- target = do
--   withDebug $
--     Lamdera.Evaluate.exec
--       "/Users/mario/dev/projects/lamdera-compiler/test/scenario-interpreter/"
--       "src/Test/Basic.elm"
--       "suite"
--
--     -- Ext.Query.Canonical.loadFileSourceValue
--     --   "/Users/mario/dev/projects/lamdera-compiler/test/scenario-interpreter/src/Test/Basic.elm"
--     --   "suite"


-- target = do
--   let project = "/Users/mario/dev/projects/lamdera/overrides/packages/elm/bytes/1.0.8"
--   Lamdera.Compile.make_ project
  -- Lamdera.Canonical.showDef
  --   project
  --   "src/Bytes/Encode.elm"
  --   "withDebug"

all =
  EasyTest.run allTests

rerun seed =
  EasyTest.rerun seed allTests

rerunJust label =
  EasyTest.rerunOnly 0 label allTests

rerunOnly seed label =
  EasyTest.rerunOnly seed label allTests


allTests =
  tests
    [ tests []
    , scope "Test.Lamdera -> " $ Test.Lamdera.suite
    , scope "Test.Snapshot -> " $ Test.Snapshot.suite
    , scope "Test.Wire -> " $ Test.Wire.suite
    -- Disable temporarily as the cache busting is crazy aggressive meaning 100mb redownload each run :|
    -- , scope "Test.Ext.ElmPages.Check -> " $ Test.Ext.ElmPages.Check.suite
    , scope "Test.TypeHashes -> " $ Test.TypeHashes.suite
    , scope "Test.Check -> " $ Test.Check.suite
    , scope "Lamdera.Evergreen.TestMigrationHarness -> " $ Lamdera.Evergreen.TestMigrationHarness.suite
    , scope "Lamdera.Evergreen.TestMigrationGenerator -> " $ Lamdera.Evergreen.TestMigrationGenerator.suite
    , scope "Test.WebGL -> " $ Test.WebGL.suite
    ]
