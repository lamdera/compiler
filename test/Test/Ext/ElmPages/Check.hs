{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Test.Ext.ElmPages.Check where

import qualified Data.Text as T

import EasyTest
import Test.Helpers
import qualified System.Directory as Dir

import Lamdera hiding (atomicPutStrLn)
import Lamdera.Compile
import Ext.Common

all = EasyTest.run suite


suite :: Test ()
suite = tests $
  [ scope "isWireCompatible" $ do
      io $ do
        let p = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-elm-pages-incompatible-wire"

        Dir.withCurrentDirectory p $ do
          bash $ "git submodule init"
          bash $ "git submodule update"
          bash $ "npm ci"
          bash $ "npm run build"

      actual <- catchOutput $
        Lamdera.Compile.makeDev ".elm-pages" ["Main.elm"]

      let !x = debugHaskell "actual output" actual

      expectTextContains actual
        "Route.Index.Data.unserialisableValue must not contain functions"
  ]
