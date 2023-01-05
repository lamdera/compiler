{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lamdera.Evergreen.TestMigrationGenerator where

import EasyTest
import Control.Exception (SomeException, AsyncException(UserInterrupt), catch, fromException, throw)
import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import NeatInterpolation
import qualified Data.Text as T
import System.FilePath ((</>))
import qualified Data.Map as Map
import qualified System.FilePath as FP

import AST.Canonical
import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as Module
import Elm.Package (Name(..))
import qualified Elm.Package as Pkg
import qualified Elm.ModuleName as ModuleName
import qualified Data.Name as N

import Test.Helpers
import qualified Ext.Common

import Lamdera
import qualified Lamdera.Compile
import qualified Lamdera.Types
import qualified Lamdera.Evergreen.MigrationGenerator as MigrationGenerator
import qualified Lamdera.Evergreen.MigrationGeneratorHelpers as Helpers
import qualified Lamdera.Make

import qualified Ext.Query.Canonical
import qualified Ext.Query.Optimized
import qualified Ext.ElmFormat

import StandaloneInstances


all = do
  run suite


suite :: Test ()
suite = tests
  [ scope "testExamples" testExamples
  -- , scope "e2e migration: 1 -> 2" $ testMigrationGeneration "scenario-migration-generate" 1 2
  -- , scope "containsUserTypes" testContainsUserTypes
  ]


testMigrationGeneration scenario oldVersion newVersion = do

  io $ atomicPutStrLn <$> Ext.Common.requireBinary "elm-format"

  let root = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-migration-generate"
      typeCompares = zipWith3
        (\label local prod -> (label, T.unpack local, T.unpack prod))
        (Lamdera.Types.core)
        ["o","o","o","o","o","o"]
        ["x","x","x","o","o","o"]


  mock <- io $ readUtf8Text $ "test/scenario-migration-generate/src/Evergreen/Migrate/V" <> show newVersion <> ".elm"
  result <- io $ MigrationGenerator.betweenVersions typeCompares oldVersion newVersion root

  _ <- io $ writeUtf8 ("test/scenario-migration-generate/src/Evergreen/Migrate/VX" <> show newVersion <> ".elm") result

  expectEqualTextTrimmed (mock & withDefault "failed to load file") result

  let filenames =
        [ "src/Evergreen/V" <> show oldVersion <> "/Types.elm"
        , "src/Evergreen/V" <> show newVersion <> "/Types.elm"
        , "src/Evergreen/Migrate/V" <> show newVersion <> ".elm"
        ]

  actual <- catchOutput $
    Lamdera.Compile.makeDev "/Users/mario/dev/projects/lamdera-compiler/test/scenario-migration-generate" filenames

  expectTextContains actual
    "This `Unimplemented` value is a:\n\n    UnimplementedMigration"


testContainsUserTypes = do

  let tipe = TType
        (Module.Canonical (Name "elm" "core") "Result")
        "Result"
        [ TType (Module.Canonical (Name "MartinSStewart" "elm-audio") "Audio") "LoadError" []
        , TType (Module.Canonical (Name "MartinSStewart" "elm-audio") "Audio") "Source" []
        ]
      tvarMap = [("userMsg", TVar "userMsg"), ("userModel", TVar "userModel")]

  expect $ Helpers.containsUserTypes tvarMap tipe == False




testExamples :: Test ()
testExamples = do

  failuresM <- io $ newMVar []

  let project = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-migration-generate"

  io $ do
    setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
    setEnv "LTEST" "1"
    setEnv "LDEBUG" "1"
    setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"

  let testFiles =
        [
          -- "src/Test/Migrate_Record.elm"
        -- , "src/Test/Migrate_External_Wrap.elm"
         "src/Test/Migrate_External_Paramed.elm"
        -- , ""

        ]

  let
    catchTestException :: FilePath -> SomeException -> IO a
    catchTestException filename e = do
      modifyMVar_ failuresM (\failures -> pure $ failures ++ filename)
      putStrLn "🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥"
      throw e

  testFiles & mapM (\filename -> do
    io $ putStrLn $ "testing: " <> show filename

    -- Bust Elm's caching with this one weird trick!
    io $ touch $ project </> filename
    interfacesE <- io $ Lamdera.Make.compileToInterfaces project filename `catch` catchTestException filename

    let
      oldVersion = 1
      newVersion = 2
      typeName = "TypeNew"
      moduleName = N.fromChars $ "Test." <> FP.takeBaseName filename

      moduleCan :: ModuleName.Canonical
      moduleCan = ModuleName.Canonical (Pkg.Name "author" "project") moduleName

    case interfacesE of
      Left err -> error $ "🏗  error, please compile manually:\n\ncd " <> project <> " && lamdera make " <> filename <> "\n\n"
      Right interfaces -> do
        let
          typeOldM = Helpers.findDef moduleCan "TypeOld" interfaces
          typeNewM = Helpers.findDef moduleCan "TypeNew" interfaces

        expectationM <- io $ Ext.Query.Optimized.findString "expected" project filename

        case (typeOldM, typeNewM, expectationM) of
          (Just typeDefOld, Just typeDefNew, Just expectation) -> do
            let
              migrationNested = MigrationGenerator.migrateTypeDef typeDefOld typeDefNew oldVersion newVersion interfaces [] []

              (Helpers.MigrationNested migration _ migrationDefs) = migrationNested

              migrationDefM = migrationDefs & Map.lookup (moduleCan, typeName) & fmap Helpers.migrations

            -- x <- io $ hindentPrintValue "typeOldM" typeOldM
            -- x <- io $ hindentPrintValue "typeNewM" typeNewM
            -- x <- io $ hindentPrintValue "out" (expectation & T.replace "\\n" "\n")
            -- x <- io $ hindentPrintValue "migrationDefM" migrationDefM
            -- x <- io $ hindentPrintValue "migrationDefs" migrationDefs
            -- x <- io $ hindentPrintValue "out" thing

            case migrationDefM of
              Just migrationDef -> do
                expected <- io $ Ext.ElmFormat.formatOrPassthrough (expectation & T.replace "\\n" "\n" & T.strip)
                actual <- io $ Ext.ElmFormat.formatOrPassthrough (migrationDef & T.replace "\\n" "\n" & T.strip)

                expectEqualTextTrimmed expected actual


                pure migrationNested
              Nothing ->
                pure migrationNested

          (_, _, Nothing) ->            error $ "Could not find `expectation` string in test: " <> filename
          (Nothing, Just typeNew, _) -> error $ "Could not find TypeOld in test: " <> filename
          (Just typeOld, Nothing, _) -> error $ "Could not find TypeNew in test: " <> filename
          (Nothing, Nothing, _) ->      error $ "Could not find TypeOld or TypeNew in test: " <> filename
    )

  io $ do
    unsetEnv "LOVR"
    unsetEnv "LTEST"
    unsetEnv "LDEBUG"
    unsetEnv "ELM_HOME"

  failures <- io $ readMVar failuresM
  if length failures > 0
    then
      crash failures
    else
      scope "senarios-alltypes no exceptions" $ ok

