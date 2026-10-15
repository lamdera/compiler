{-# LANGUAGE TemplateHaskell #-}

module Lamdera.Version where

import GitHash

import qualified Ext.Common
import qualified Elm.Version as V


type Version = (Int, Int, Int)

raw :: (Int, Int, Int)
raw = (1,5,0)


rawToString :: (Int, Int, Int) -> String
rawToString (m,mi,p) =
  show m <> "." <> show mi <> "." <> show p


short :: String
short = rawToString raw


medium :: String
medium = "lamdera-" <> short <> "-" <> elm


full :: String
full =
  let
    gi = $$tGitInfoCwd
    dirty | giDirty gi = "-dirty"
          | otherwise  = ""
  in
  concat
    [ medium, "-", Ext.Common.os, "-", Ext.Common.arch, "-", giHash gi, dirty
    , " (", giCommitDate gi, ")"
    , " (branch:", giBranch gi, ")"
    ]


elm :: String
elm =
  V.toChars V.compiler


{-| Package artifacts in ELM_HOME (`artifacts.<medium>.dat`) and the project's
`elm-stuff/<medium>/` are keyed by the version string alone.

Every module's objects also carry the Wire3 codec IR (Lamdera.Wire3.Direct). A cache
written by a compiler that stores no IR, or a different IR format, must not be reused
under the same key: its types would silently fall back to the slow Elm codecs. The
version string is the cache buster, so a change to the stored IR format needs a new
version, or caches built by the previous binary must be cleared.
-}
artifacts :: String
artifacts =
  "artifacts." <> medium <> ".dat"


resetPatch :: V.Version -> V.Version
resetPatch version =
  V.fromVersion version $ \major minor _patch ->
    V.toVersion major minor 0
