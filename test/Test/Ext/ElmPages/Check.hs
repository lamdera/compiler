module Test.Ext.ElmPages.Check where

import EasyTest
import Test.Helpers


all = EasyTest.run suite


suite :: Test ()
suite = tests $
  [ scope "isWireCompatible" $ do
      expectEqual 1 1


  ]
