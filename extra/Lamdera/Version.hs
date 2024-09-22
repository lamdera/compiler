{-# LANGUAGE TemplateHaskell #-}

module Lamdera.Version where

import GitHash

import qualified Ext.Common
import qualified Elm.Version as V


type Version = (Int, Int, Int)

raw :: (Int, Int, Int)
raw = (1,2,2)


rawToString :: (Int, Int, Int) -> String
rawToString (m,mi,p) =
  show m <> "." <> show mi <> "." <> show p


short :: String
short = rawToString raw


full :: String
full =
  let
    gi = $$tGitInfoCwd
    dirty | giDirty gi = "-dirty"
          | otherwise  = ""
  in
  concat
    [ "lamdera-", short, "-", elm, "-", Ext.Common.os, "-", Ext.Common.arch, "-", giHash gi, dirty
    , " (", giCommitDate gi, ")"
    , " (branch:", giBranch gi, ")"
    ]


elm :: String
elm =
  V.toChars V.compiler
