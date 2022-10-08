{-# LANGUAGE TemplateHaskell #-}

module Lamdera.Version where

import GitHash

import qualified Elm.Version as V


raw :: (Int, Int, Int)
raw = (1,0,2)


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
    [ "lamdera-", short, "-", giHash gi, dirty
    , " (", giCommitDate gi, ")"
    , " (branch:", giBranch gi, ")"
    ]


elm :: String
elm =
  V.toChars V.compiler