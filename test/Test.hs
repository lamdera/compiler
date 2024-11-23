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
import qualified Test.JsOutput
import qualified Test.WebGL

import qualified Test.Lamdera.Evergreen.TestMigrationHarness
import qualified Test.Lamdera.Evergreen.TestMigrationGenerator

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

target = Test.all

checkProject = do
  let (p, v) = ("~/dev/test/lamdera-init", "1")

  Lamdera.remove (p </> "src/Evergreen/Migrate/V" ++ v ++ ".elm")
  Lamdera.rmdir (p </> "src/Evergreen/V" ++ v)

  Ext.Common.setProjectRoot p
  -- Dir.withCurrentDirectory p $ Lamdera.CLI.Check.run () (Lamdera.CLI.Check.Flags { Lamdera.CLI.Check._destructiveMigration = True })
  Dir.withCurrentDirectory p $ Lamdera.CLI.Check.run_ `catch` (\(err :: SomeException) -> pure ())


checkProjectCompiles = do
  setEnv "LDEBUG" "1"
  let
    root = "~/dev/test/lamdera-init"
    scaffold = "src/Frontend.elm"
  Ext.Common.setProjectRoot root
  Lamdera.Compile.makeDev root [scaffold]


{- Dynamic testing of lamdera live with managed thread kill + reload -}
liveReloadLive = do
  setEnv "LDEBUG" "1"
  setEnv "EXPERIMENTAL" "1"
  let p = "test/scenario-alltypes"
  trackedForkIO "Test.liveReloadLive" $ Develop.runWithRoot p (Develop.Flags Nothing)

  -- Doing this actually makes no sense in the :rr context, as the thread is long-running so it's the same as
  -- disabling the ENV vars mid-run! But leaving it here as a reminder, because it _does_ pollute the ENV
  -- if we don't cleanup and then move on to use the same ghci session for other things!
  -- unsetEnv "LOVR"
  -- unsetEnv "LDEBUG"

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
    -- , scope "Test.Snapshot -> " $ Test.Snapshot.suite
    , scope "Test.Wire -> " $ Test.Wire.suite
    -- Disable temporarily as the cache busting is crazy aggressive meaning 100mb redownload each run :|
    , scope "Test.Ext.ElmPages.Check -> " $ Test.Ext.ElmPages.Check.suite
    , scope "Test.TypeHashes -> " $ Test.TypeHashes.suite
    , scope "Test.Check -> " $ Test.Check.suite
    -- , scope "Lamdera.Evergreen.TestMigrationHarness -> " $ Test.Lamdera.Evergreen.TestMigrationHarness.suite
    , scope "Lamdera.Evergreen.TestMigrationGenerator -> " $ Test.Lamdera.Evergreen.TestMigrationGenerator.suite
    , scope "Test.WebGL -> " $ Test.WebGL.suite
    , scope "Test.JsOutput -> " $ Test.JsOutput.suite
    ]
