{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Test.Ext.ElmPages.Check where

import qualified Data.Text as T

import EasyTest
import Test.Helpers
import qualified System.Directory as Dir

import Ext.Common
import Lamdera hiding (atomicPutStrLn)
import Lamdera.Compile
import qualified Lamdera.Relative

all = EasyTest.run suite


suite :: Test ()
suite = tests $
  [ scope "isWireCompatible" $ do
      p <- io $ Lamdera.Relative.findDir "test/scenario-elm-pages-incompatible-wire"
      actual <- io $ do

        atomicPutStrLn $ "project dir is" <> p

        Dir.withCurrentDirectory p $ do
          bash $ "git submodule init"
          bash $ "git submodule update"
          bash $ "npm i"
          bash $ "npm run build"

      expectTextContains (stringToText actual) "Route.Index.Data.unserialisableValue must not contain functions"
  ]
