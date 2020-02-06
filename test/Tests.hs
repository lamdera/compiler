module Tests where

import EasyTest
import TestLamderaGenerated

run =
  EasyTest.run TestLamderaGenerated.suite
