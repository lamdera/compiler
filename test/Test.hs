module Test where

import EasyTest
import TestLamderaGenerated
import TestEvergreen
import TestLamdera

all =
  EasyTest.run $
    tests
      [ TestLamderaGenerated.suite
      , scope "evergreen -> type snapshots -> " $ TestEvergreen.suite
      , scope "lamdera -> " $ TestLamdera.suite
      ]
