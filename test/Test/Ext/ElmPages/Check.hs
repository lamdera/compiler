{-# LANGUAGE OverloadedStrings #-}

module Test.Ext.ElmPages.Check where

import qualified Data.Text as T

import EasyTest
import Test.Helpers

import Lamdera
import Lamdera.Compile

all = EasyTest.run suite


suite :: Test ()
suite = tests $
  [ scope "isWireCompatible" $ do
      io $ setEnv "LDEBUG" "1"

      actual <- catchOutputStdErr $
        Lamdera.Compile.makeDev "./test/scenario-elm-pages-incompatible-wire/.elm-pages" "Main.elm"

      io $ atomicPutStrLn $ T.unpack actual

      expectTextContains actual
        "Route.Index.Data.unserialisableValue must not contain functions"
  ]
