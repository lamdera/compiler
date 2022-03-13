{-# LANGUAGE OverloadedStrings #-}

module Test.Ext.ElmPages.Check where

import EasyTest
import Test.Helpers

import Lamdera
import Lamdera.Compile

all = EasyTest.run suite


suite :: Test ()
suite = tests $
  [ scope "isWireCompatible" $ do
      actual <- catchOutput $
        Lamdera.Compile.makeDev "./test/scenario-elm-pages-incompatible-wire/.elm-pages" "Main.elm"

      expectTextContains actual
        "PageData:\\n\\n- must not contain functions"
  ]
