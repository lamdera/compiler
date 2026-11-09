{-# LANGUAGE TemplateHaskell #-}

module Lamdera.Version where

import GitHash

import qualified Ext.Common
import qualified Elm.Version as V


type Version = (Int, Int, Int)

raw :: (Int, Int, Int)
raw = (1,6,0)


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


{-| The first 8 characters of the commit this compiler was built from. Sent with every
request to an app's /_i (see Lamdera.Http.compilerHeaders), with `short` and
`hashScheme`, so the server can tell exactly which compiler is asking. -}
shaShort :: String
shaShort =
  take 8 $ giHash $$tGitInfoCwd


{-| How this compiler hashes types (Lamdera.TypeHash), as a number that changes whenever
the same types would hash differently. Stored next to the hashes a build writes
(.lamdera-hash-scheme), and sent to /_i, so a compiler and a server that hash types
differently say so, rather than asking for a migration of types that did not change.

It is the date (yyyymmdd) of the commit that changed the hashing, so newer is bigger:

  (none)    up to 1.5.0, which neither stores nor sends a scheme
  20261028  1.5.1, Vec2/Vec3/Vec4/Mat4 hash by their size and WebGL Shader/Entity like Texture

Bump it with ANY change to type hashing, deliberate or not, to the date of that change.
The "hash scheme" tests in test/Test/TypeHashes.hs pin hashes of known types per scheme,
so a change that forgets to bump this fails there.
-}
hashScheme :: Int
hashScheme = 20261028


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
