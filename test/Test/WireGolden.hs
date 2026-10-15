{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.WireGolden where

{- Goldens for everything Lamdera derives from type and field names, recorded with
Lamdera 1.5.0 on Elm 0.19.2 for the golden app in test/wire3-golden/app:

  - Wire3 type hashes and the hashed type text (Lamdera.TypeHash), for every type of the
    golden app and the six Lamdera core types;
  - the Evergreen type snapshot that `lamdera check` writes (Lamdera.Evergreen.Snapshot).

Wire3 orders record fields and constructors by name, and so do the hashes and snapshots.
The golden types mix name lengths, case, digits and underscores, declared out of order,
so any change in how names are ordered changes these outputs. The bytes the codecs write
are pinned by test/wire3-golden/golden.js.

GOLDEN_ACCEPT=1 rewrites the expected files from the current compiler instead of
comparing (only ever do that on the reference compiler).

GOLDEN_PROJECT=<dir> GOLDEN_OUT=<file> runs the same two captures on another Lamdera app
(no expectations; for comparing real apps between compilers outside the repo). The
snapshot is written to <dir>/src/Evergreen/V999 and removed again.
-}

import Control.Monad (forM, when)
import qualified Data.List as List
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath ((</>))

import EasyTest
import Lamdera
import qualified Lamdera.Evergreen.Snapshot
import qualified Lamdera.Relative
import qualified Lamdera.TypeHash
import qualified Lamdera.Types
import qualified Ext.Common
import Test.Helpers
import qualified Data.Name


all = EasyTest.run suite


suite :: Test ()
suite = tests
  [ scope "type hashes match 1.5.0" $ do
      project <- io $ Lamdera.Relative.requireDir "test/wire3-golden/app"
      actual <- ioSilenced $ captureTypeHashes project goldenTypes
      expectGolden "typehashes.txt" actual

  , scope "evergreen snapshot matches 1.5.0" $ do
      project <- io $ Lamdera.Relative.requireDir "test/wire3-golden/app"
      actual <- ioSilenced $ captureSnapshot project 1
      expectGolden "evergreen-v1.txt" actual
  ]


{-| The types of the golden app without type parameters, by module file, module and type
name. Listed rather than read from the interfaces so that the output order never depends
on how names compare. (Hashing a parameterised type on its own never happens for real
and does not terminate for some of them; Tree, Choice, Wrapped, Named and Ext are
covered through the types that use them.) -}
goldenTypes :: [(FilePath, String, String)]
goldenTypes =
  [ ("src/Types.elm", "Types", t)
  | t <- [ "Rec", "Ctors", "Nested", "Forest", "Containers", "UsesExt", "Cross"
         , "FrontendModel", "BackendModel", "FrontendMsg", "ToBackend", "BackendMsg"
         , "ToFrontend"
         ]
  ] ++
  [ ("src/Other.elm", "Other", t)
  | t <- [ "Person", "OtherCtor", "OtherRec", "OtherRec2", "Opaque" ]
  ]


{-| The core type hashes as `lamdera check` computes them, then hash and type text of each
listed type. -}
captureTypeHashes :: FilePath -> [(FilePath, String, String)] -> IO T.Text
captureTypeHashes project types =
  Ext.Common.withProjectRoot project $ do
    core <- Lamdera.TypeHash.calculateLamderaHashes
    let coreLines =
          case core of
            Right (hashes, _) ->
              zipWith (\n h -> "core " <> T.pack (nameString n) <> " " <> h) Lamdera.Types.core hashes
            Left _ ->
              [ "core: calculateLamderaHashes failed" ]
    typeLines <- forM types $ \(path, moduleName, typeName) -> do
      (thash, ttext) <- Lamdera.TypeHash.calculateHashPair path (nameFromString moduleName) (nameFromString typeName)
      pure $ T.pack (moduleName <> "." <> typeName) <> " " <> thash <> " " <> ttext
    pure $ T.unlines (coreLines ++ typeLines)


{-| The Evergreen snapshot files for the given version, concatenated in path order. -}
captureSnapshot :: FilePath -> Int -> IO T.Text
captureSnapshot project version = do
  let dir = project </> "src" </> "Evergreen" </> ("V" <> show version)
  rmdir dir
  Ext.Common.withProjectRoot project $ Lamdera.Evergreen.Snapshot.run version
  files <- listFilesRecursive dir
  contents <- forM (List.sort files) $ \file -> do
    text <- readUtf8Text (dir </> file)
    pure $ "-- FILE " <> T.pack file <> "\n" <> withDefault "<unreadable>" text <> "\n"
  rmdir dir
  pure $ T.concat contents


listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive dir = do
  entries <- Dir.listDirectory dir
  fmap concat $ forM entries $ \entry -> do
    isDir <- Dir.doesDirectoryExist (dir </> entry)
    if isDir
      then fmap (entry </>) <$> listFilesRecursive (dir </> entry)
      else pure [entry]


expectGolden :: FilePath -> T.Text -> Test ()
expectGolden file actual = do
  expectedDir <- io $ Lamdera.Relative.requireDir "test/wire3-golden/expected"
  let path = expectedDir </> file
  acceptM <- io $ lookupEnv "GOLDEN_ACCEPT"
  if acceptM == Just "1"
    then do
      io $ writeUtf8 path actual
      ok
    else do
      expected <- io $ readUtf8Text path
      expectEqualTextTrimmed (withDefault "<missing golden file>" expected) actual


nameString :: Data.Name.Name -> String
nameString =
  Data.Name.toChars


nameFromString :: String -> Data.Name.Name
nameFromString =
  Data.Name.fromChars


{-| GOLDEN_PROJECT / GOLDEN_OUT: capture a real app's core type hashes and snapshot. -}
captureProject :: IO ()
captureProject = do
  projectM <- lookupEnv "GOLDEN_PROJECT"
  outM <- lookupEnv "GOLDEN_OUT"
  case (projectM, outM) of
    (Just project, Just out) -> do
      hashes <- captureTypeHashes project []
      snapshot <- captureSnapshot project 999
      writeUtf8 out (hashes <> "\n" <> snapshot)

    _ ->
      error "set GOLDEN_PROJECT and GOLDEN_OUT"
