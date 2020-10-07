module Test where

import EasyTest
import Lamdera
import qualified TestLamderaGenerated
import qualified TestSnapshot
import qualified TestLamdera
import qualified TestWire

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
    [ TestLamderaGenerated.suite
    , scope "evergreen -> type snapshots -> " $ TestSnapshot.suite
    , scope "lamdera -> " $ TestLamdera.suite
    , scope "wire -> " $ TestWire.suite
    ]
