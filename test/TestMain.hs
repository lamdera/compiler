module Main where

import System.Environment (getArgs)

import qualified EasyTest
import qualified Test
import qualified Test.WireGolden

{-| With no arguments, runs every suite. With one argument, runs only the suites whose
scope starts with it (e.g. `stack test --test-arguments Test.WireGolden`).
`golden-project` runs Test.WireGolden.captureProject (GOLDEN_PROJECT / GOLDEN_OUT).
-}
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["golden-project"] -> Test.WireGolden.captureProject
    [prefix] -> EasyTest.runOnly prefix Test.allTests
    _ -> Test.all
