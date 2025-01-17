{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Check where

import System.FilePath ((</>))
import qualified System.Directory as Dir
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

import EasyTest
import NeatInterpolation

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

import Test.Helpers
import Lamdera
import Lamdera.Evergreen.Snapshot
import qualified Lamdera.CLI.Check
import qualified Lamdera.Offline
import qualified Ext.Common
import qualified Lamdera.Compile
import qualified Lamdera.Relative


all = EasyTest.run suite


suite :: Test ()
suite = tests $
  [ scope "Lamdera.Nitpick.DebugLog - lamdera check" $ do

      project <- io $ Lamdera.Relative.requireDir "test/project-scenarios/blank-injectable"

      io $ Ext.Common.bash $ "cd " <> project <> " && git init"
      io $ Ext.Common.bash $ "cd " <> project <> " && git remote add lamdera git@apps.lamdera.com:always-v0.git"

      let path = (project </> "src" </> "Frontend.elm")

      withFileModifications (project </> "src" </> "Frontend.elm")
        [ ("{- viewFunctionBodyPlaceholder -}",   "_ = Debug.log \"hello\" \"world\"")
        , ("{- updateFunctionBodyPlaceholder -}", "_ = Debug.log \"hello\" \"world\"")
        ] $ do

        scope "lamdera check should succeed, ignoring the Debug.log usages" $ do
          actual <- catchOutput $ checkWithParamsProduction project "always-v0"
          expectTextContains actual
            "It appears you're all set to deploy the first version of 'always-v0'!"

        scope "lamdera make --optimized should continue to fail" $ do
          actual <- catchOutput $ Lamdera.Compile.makeOptimizedWithCleanup (pure ()) project "src/Frontend.elm"
          expectTextContains actual
            "There are uses of the `Debug` module in the following modules:"


  , scope "production check -> AppConfig usages & injection" $ do
      project <- io $ Lamdera.Relative.requireDir "test/scenario-always-v0"
      let
          expectContains needle file = (project </> file) & expectFileContains needle

      io $ Ext.Common.bash $ "cd " <> project <> " && git init"
      io $ Ext.Common.bash $ "cd " <> project <> " && git remote add lamdera git@apps.lamdera.com:always-v0.git"

      _ <- io $ checkWithParamsProduction project "always-v0"

      "elm-stuff/lamdera/.lamdera-fe-config" & expectContains "frontendOnly"
      "elm-stuff/lamdera/.lamdera-fe-config" & expectContains "both"
      "elm-stuff/lamdera/.lamdera-fe-config" & expectContains "external"

      "elm-stuff/lamdera/.lamdera-be-config" & expectContains "backendOnly"
      "elm-stuff/lamdera/.lamdera-be-config" & expectContains "both"
      "elm-stuff/lamdera/.lamdera-be-config" & expectContains "rpc"

      "frontend-app.js" & expectContains "fe-only-from-dashboard"
      "backend-app.js" & expectContains "be-only-from-dashboard"

      "frontend-app.js" & expectContains "both-from-dashboard"
      "backend-app.js" & expectContains "both-from-dashboard"

      io $ rmdir (project </> ".git")
      io $ cleanupCheckGen project

  , scope "detects false migration claims for changed types" $ do
      project <- io $ Lamdera.Relative.requireDir "test/project-scenarios/blank-injectable"

      let
        migration = [text|
          module Evergreen.Migrate.V2 exposing (backendModel, backendMsg, frontendModel, frontendMsg, toBackend, toFrontend)

          {-| This migration file was automatically generated by the lamdera compiler.

          It includes:

            - A migration for each of the 6 Lamdera core types that has changed
            - A function named `migrate_ModuleName_TypeName` for each changed/custom type

          Expect to see:

            - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
            - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
              value mappings from the old type by default

          You can edit this file however you wish! It won't be generated again.

          See <https://dashboard.lamdera.app/docs/evergreen> for more info.

          -}

          import Evergreen.V1.Types
          import Evergreen.V2.Types
          import Lamdera.Migrations exposing (..)


          frontendModel : Evergreen.V1.Types.FrontendModel -> ModelMigration Evergreen.V2.Types.FrontendModel Evergreen.V2.Types.FrontendMsg
          frontendModel old =
              ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


          backendModel : Evergreen.V1.Types.BackendModel -> ModelMigration Evergreen.V2.Types.BackendModel Evergreen.V2.Types.BackendMsg
          backendModel old =
              ModelUnchanged


          frontendMsg : Evergreen.V1.Types.FrontendMsg -> MsgMigration Evergreen.V2.Types.FrontendMsg Evergreen.V2.Types.FrontendMsg
          frontendMsg old =
              MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


          toBackend : Evergreen.V1.Types.ToBackend -> MsgMigration Evergreen.V2.Types.ToBackend Evergreen.V2.Types.BackendMsg
          toBackend old =
              MsgUnchanged


          backendMsg : Evergreen.V1.Types.BackendMsg -> MsgMigration Evergreen.V2.Types.BackendMsg Evergreen.V2.Types.BackendMsg
          backendMsg old =
              MsgUnchanged


          toFrontend : Evergreen.V1.Types.ToFrontend -> MsgMigration Evergreen.V2.Types.ToFrontend Evergreen.V2.Types.FrontendMsg
          toFrontend old =
              MsgUnchanged

        |]

        changedTypes =
          [ ("FrontendModel", "hash-x", "hash-y")
          , ("BackendModel", "hash-x", "hash-y")
          , ("FrontendMsg", "hash-x", "hash-y")
          , ("ToFrontend", "hash-x", "hash-y")
          ]

      result <- catchOutput $ Lamdera.CLI.Check.ensureNoFalseClaims "src/Evergreen/Migration/VX.elm" migration changedTypes
      expectTextContainsAll result
        -- NB: catchOutput escapes so we have to double-escape in our matchers
        [ "BackendModel type has changed since last deploy, but the \\\"backendModel\\\"\\nmigration claims it has not"
        , "ToFrontend type has changed since last deploy, but the \\\"toFrontend\\\"\\nmigration claims it has not"
        ]

  , scope "extractTopLevelExpressions" $ do
      let input = [text|
        module Main exposing (..)

        import Debug exposing (log)
        import Html exposing (Html, text)

        main : Html msg
        main =
            text "Hello, World!"

        foo =
          this should be caught, despite no type signature
          and this

          all in foo

          still in foo

        bar : something
        bar =
          and bar too, including it's type sig
      |]

      expectEqual
        ["module Main exposing (..)\n\n"
        ,"import Debug exposing (log)"
        ,"import Html exposing (Html, text)\n\n"
        ,"main : Html msg"
        ,"main =\n    text \"Hello, World!\"\n\n"
        ,"foo =\n  this should be caught, despite no type signature\n  and this\n  all in foo\n  still in foo\n\n"
        ,"bar : something"
        ,"bar =\n  and bar too, including it's type sig\n"]
        (Lamdera.CLI.Check.extractTopLevelExpressions input)
  ]




withFileModifications :: FilePath -> [(Text, Text)] -> Test a -> Test a
withFileModifications path mods action =
    EasyTest.using
      (mods & mapM_ (\(before, after) -> replaceInFile before after path))
      (\_ -> mods & mapM_ (\(before, after) -> replaceInFile after before path))
      (\_ -> action)


expectFileContains :: Text -> FilePath -> Test ()
expectFileContains needle file =
  scope ("-> '" ++ file ++ "' contains '" ++ T.unpack needle ++ "'") $ do
    exists_ <- io $ Dir.doesFileExist file
    onlyWhen (not exists_) $
      crash $ "❌  expectFileContains: file not found: " ++ file

    textM <- io $ readUtf8Text file
    case textM of
      Just t ->
        if (not $ textContains needle t)
          then
            crash $ "❌  expectFileContains: file '" ++ file ++ "' does not contain '" ++ T.unpack needle ++ "'\n📖 file contents were:\n" ++ T.unpack t
          else
            ok

      Nothing ->
        ok


{-| For quick and general local development testing via `stack ghci` as Test.Check.check -}
check = do
  checkWithParamsProduction "~/lamdera/test/v1" "always-v0"


{-| Run the `lamdera check` pipeline with specific params -}
checkWithParams projectPath = do
  overrides <- Lamdera.Relative.requireDir "~/lamdera/overrides"
  elmHome <- Lamdera.Relative.requireDir "~/elm-home-elmx-test"

  withEnvVars [("LDEBUG", "1"), ("LOVR", overrides), ("ELM_HOME", elmHome)] $ do
    Ext.Common.withProjectRoot projectPath $ Lamdera.CLI.Check.run_


{-| Run the `lamdera check` pipeline as if it were run in production to invoke prod behaviour -}
checkWithParamsProduction projectPath appName = do
  requireEnv "TOKEN"

  overrides <- Lamdera.Relative.requireDir "~/lamdera/overrides"
  elmHome <- Lamdera.Relative.requireDir "~/elm-home-elmx-test"
  project <- Lamdera.Relative.requireDir projectPath

  withEnvVars
    [ ("LAMDERA_APP_NAME", appName)
    , ("LOVR", overrides)
    , ("ELM_HOME", elmHome)
    ] $ do
    cp "~/lamdera/runtime/src/LBR.elm" (project ++ "/src/LBR.elm")
    cp "~/lamdera/runtime/src/LFR.elm" (project ++ "/src/LFR.elm")
    cp "~/lamdera/runtime/src/LamderaRPC.elm" (project ++ "/src/LamderaRPC.elm")

    rpcExists <- doesFileExist (project ++ "/src/RPC.elm")
    onlyWhen (not rpcExists) $
      cp "~/lamdera/runtime/src/RPC_Empty.elm" (project ++ "/src/RPC.elm")
    cp "~/lamdera/runtime/src/LamderaHelpers.elm" (project ++ "/src/LamderaHelpers.elm")

    checkWithParams project


{-| Run the `lamdera check` pipeline with specific params -}
checkWithParamsNoDebug version projectPath appName = do
  project <- Lamdera.Relative.requireDir projectPath
  overrides <- Lamdera.Relative.requireDir "~/lamdera/overrides"
  elmHome <- Lamdera.Relative.requireDir "~/elm-home-elmx-test"

  withEnvVars [("LAMDERA_APP_NAME", appName), ("VERSION", show version), ("LOVR", overrides), ("ELM_HOME", elmHome), ("NOTPROD", "1")] $ do
    debug $ "project is " ++ project

    cp "~/lamdera/runtime/src/LBR.elm" (project ++ "/src/LBR.elm")
    cp "~/lamdera/runtime/src/LFR.elm" (project ++ "/src/LFR.elm")
    cp "~/lamdera/runtime/src/RPC_Empty.elm" (project ++ "/src/RPC.elm")
    cp "~/lamdera/runtime/src/LamderaHelpers.elm" (project ++ "/src/LamderaHelpers.elm")

    Ext.Common.withProjectRoot project $ do
      callCommand "git init"
      Lamdera.CLI.Check.run_

    rm (project ++ "/src/LBR.elm")
    rm (project ++ "/src/LFR.elm")
    rm (project ++ "/src/RPC.elm")
    rm (project ++ "/src/LamderaHelpers.elm")


cleanupCheckGen project = do
  rm (project ++ "/src/LBR.elm")
  rm (project ++ "/src/LFR.elm")
  -- rm (project ++ "/src/RPC.elm")
  rm (project ++ "/src/LamderaGenerated.elm")
  rm (project ++ "/src/LamderaHelpers.elm")
  rm (project ++ "/src/LamderaRPC.elm")
  rm (project ++ "/backend-app.js")
  rm (project ++ "/frontend-app.js")
