module Test where

import EasyTest
import Lamdera
import qualified Test.LamderaGenerated
import qualified Test.Snapshot
import qualified Test.Lamdera
import qualified Test.Check
import qualified Test.Wire

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
  - Run `:rr` to recompile + typecheck and auto-run TestLamdera.target
  - fix any issues, then :rr again
  - if you want to recompile without running, do :r

Easier to change the target definition than constantly adjust the :def!

Press up arrow to get history of prior commands.

-}

-- Current target for ghci :rr command. See ~/.ghci config file, which should contain
-- something like `:def rr const $ return $ unlines [":r","Test.target"]`
-- target = Test.Wire.wire
-- target = checkUserConfig
-- target = TestWire.buildAllPackages
-- target = Lamdera.CLI.Login.run () ()
-- target = Dir.withCurrentDirectory "/Users/mario/dev/projects/lamdera-test" $ Lamdera.CLI.Reset.run () ()
-- target = Lamdera.Diff.run
-- target = Lamdera.ReverseProxy.start
target = Test.Check.asUser "/Users/mario/dev/projects/lamdera-dashboard" "dashboard"


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
