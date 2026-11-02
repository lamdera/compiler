{-# LANGUAGE OverloadedStrings #-}

module Test.MultiVersion where

{- Elm 0.19.1, 0.19.2, 0.19.3 and 0.19.4 are interchangeable for Lamdera on both sides of
elm.json: the application's own elm-version (Elm.Details.isCompatibleAppElmVersion)
and every package's elm-version bound (Elm.Constraint.goodElm, via
interchangeableElm). This checks the two predicates directly, and builds the empty
plain Elm and Lamdera applications with their elm.json saying 0.19.1 and 0.19.4;
test/multi-version/run.sh builds real applications at every version against
rewritten package bounds.
-}

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath ((</>))

import EasyTest
import Lamdera hiding ((&))
import Test.Helpers

import qualified Elm.Constraint as Con
import qualified Elm.Details as Details
import qualified Elm.Version as V
import qualified Json.Decode as D
import qualified Lamdera.Compile


suite :: Test ()
suite = tests
  [ scope "the compiler is Elm 0.19.4" $
      expectEqual "0.19.4" (V.toChars V.compiler)

  , scope "applications at 0.19.1, 0.19.2, 0.19.3 and 0.19.4 are accepted" $
      expectEqual [True, True, True, True] $
        fmap Details.isCompatibleAppElmVersion [V.toVersion 0 19 1, V.toVersion 0 19 2, V.toVersion 0 19 3, V.toVersion 0 19 4]

  , scope "applications past the compiler, or outside 0.19, are refused" $
      expectEqual [False, False, False] $
        fmap Details.isCompatibleAppElmVersion [V.toVersion 0 19 5, V.toVersion 0 20 0, V.toVersion 1 0 0]

  , scope "package bounds starting at 0.19.0, 0.19.1, 0.19.2, 0.19.3 and 0.19.4 are accepted" $
      goodElm True
        [ "0.19.0 <= v < 0.20.0"
        , "0.19.1 <= v < 0.20.0"
        , "0.19.2 <= v < 0.20.0"
        , "0.19.3 <= v < 0.20.0"
        , "0.19.4 <= v < 0.20.0"
        ]

  , scope "package bounds that exclude the compiler but admit 0.19.1, 0.19.2 or 0.19.3 are accepted" $
      goodElm True
        [ "0.19.0 <= v < 0.19.2" -- bburdette/toop 1.2.0
        , "0.19.0 <= v < 0.19.3"
        , "0.19.0 <= v < 0.19.4"
        , "0.19.1 <= v < 0.19.2"
        , "0.19.2 <= v < 0.19.3"
        , "0.19.3 <= v < 0.19.4"
        ]

  , scope "package bounds that admit none of 0.19.1, 0.19.2, 0.19.3 and 0.19.4 are refused" $
      goodElm False
        [ "0.19.0 <= v < 0.19.1"
        , "0.18.0 <= v < 0.19.0"
        , "0.19.5 <= v < 0.20.0"
        , "0.20.0 <= v < 0.21.0"
        ]

  , scope "an application saying 0.19.1 builds" $
      tests
        [ build "plain" "./test/scenario-empty-elm-init" "0.19.1" ["src/Main.elm"]
        , build "lamdera" "./test/scenario-empty-lamdera-init" "0.19.1" ["src/Frontend.elm", "src/Backend.elm"]
        ]

  , scope "an application saying 0.19.4 builds" $
      tests
        [ build "plain" "./test/scenario-empty-elm-init" "0.19.4" ["src/Main.elm"]
        , build "lamdera" "./test/scenario-empty-lamdera-init" "0.19.4" ["src/Frontend.elm", "src/Backend.elm"]
        ]

  , scope "an application saying 0.19.5 is refused" $
      buildFails "plain" "./test/scenario-empty-elm-init" "0.19.5" ["src/Main.elm"] "ELM VERSION MISMATCH"
  ]


goodElm :: Bool -> [String] -> Test ()
goodElm expected bounds =
  do  results <- traverse (\bound -> (,) bound . Con.goodElm <$> constraint bound) bounds
      expectEqual (fmap (\bound -> (bound, expected)) bounds) results


constraint :: String -> Test Con.Constraint
constraint bound =
  do  result <- io $ D.fromByteString Con.decoder (BS.pack (show bound))
      case result of
        Right c -> pure c
        Left _ -> crash $ "could not parse the constraint " ++ show bound



-- REAL BUILDS


-- A copy of the fixture with its elm.json saying `version`, built from scratch.
withFixture :: String -> FilePath -> String -> (FilePath -> Test a) -> Test a
withFixture label fixture version test =
  let
    project = "./test/scenario-multi-version-" ++ label ++ "-" ++ version

    setup = do
      rmdir project
      Dir.createDirectoryIfMissing True project
      copyTree (fixture </> "src") (project </> "src")
      elmJson <- maybe (error "no elm.json in the fixture") id <$> readUtf8Text (fixture </> "elm.json")
      writeUtf8 (project </> "elm.json") (setElmVersion version elmJson)
  in
  using setup (\_ -> rmdir project) (\_ -> test project)


build :: String -> FilePath -> String -> [FilePath] -> Test ()
build label fixture version files =
  scope label $
    withFixture label fixture version $ \project -> do
      io $ do
        Lamdera.unsetEnv "LOVR"
        Lamdera.unsetEnv "LTEST"
        Lamdera.unsetEnv "LDEBUG"
      elmJson <- io $ readUtf8Text (project </> "elm.json")
      expect (maybe False (T.isInfixOf (T.pack ("\"elm-version\": \"" ++ version ++ "\""))) elmJson)
      actual <- catchOutput $ Lamdera.Compile.makeDev project files
      expectTextContains actual "Success!"


buildFails :: String -> FilePath -> String -> [FilePath] -> T.Text -> Test ()
buildFails label fixture version files problem =
  scope label $
    withFixture label fixture version $ \project -> do
      io $ do
        Lamdera.unsetEnv "LOVR"
        Lamdera.unsetEnv "LTEST"
        Lamdera.unsetEnv "LDEBUG"
      actual <- catchOutput $ Lamdera.Compile.makeDev project files
      expectTextContains actual problem


setElmVersion :: String -> T.Text -> T.Text
setElmVersion version elmJson =
  T.unlines $ fmap
    (\line ->
        if "\"elm-version\"" `T.isInfixOf` line
          then T.takeWhile (/= '"') line <> "\"elm-version\": \"" <> T.pack version <> "\","
          else line
    )
    (T.lines elmJson)


copyTree :: FilePath -> FilePath -> IO ()
copyTree from to = do
  Dir.createDirectoryIfMissing True to
  entries <- Dir.listDirectory from
  mapM_
    (\entry -> do
        isDir <- Dir.doesDirectoryExist (from </> entry)
        if isDir
          then copyTree (from </> entry) (to </> entry)
          else Dir.copyFile (from </> entry) (to </> entry)
    )
    entries
