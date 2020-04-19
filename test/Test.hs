module Test where

import EasyTest
import TestLamderaGenerated
import TestEvergreen

all =
  EasyTest.run $
    tests
      [ TestLamderaGenerated.suite
      , scope "evergreen -> type snapshots -> " $ TestEvergreen.suite      
      ]
