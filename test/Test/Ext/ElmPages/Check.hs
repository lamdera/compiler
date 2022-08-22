{-# LANGUAGE OverloadedStrings #-}

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
        let p = "./test/scenario-elm-pages-incompatible-wire"
        Dir.setCurrentDirectory p
        setEnv "LDEBUG" "1"
        bash $ "npm i"
        bash $ "npm run build"

      actual <- catchOutput $
        Lamdera.Compile.makeDev "./" ".elm-pages/Main.elm"

      io $ Dir.setCurrentDirectory "../.."

      expectTextContains actual
        "Route.Index.Data.unserialisableValue must not contain functions"
  ]
