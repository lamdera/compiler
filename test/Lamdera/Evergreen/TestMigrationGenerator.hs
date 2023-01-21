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
import qualified Reporting.Exit

import Test.Helpers
import Ext.Common

import Lamdera hiding ((&))
import qualified Lamdera.Compile
import qualified Lamdera.Types
import qualified Lamdera.Evergreen.MigrationGenerator as MigrationGenerator
import qualified Lamdera.Evergreen.MigrationGeneratorHelpers as Helpers
import qualified Lamdera.Make

import qualified Ext.Query.Canonical
import qualified Ext.Query.Optimized
import qualified Ext.ElmFormat

import qualified Lamdera.Progress
import StandaloneInstances


all = do
  run suite


suite :: Test ()
suite = tests
  [
    scope "testExamples" testExamples
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

  compilationStdout <- catchOutput $
    Lamdera.Compile.makeDev "/Users/mario/dev/projects/lamdera-compiler/test/scenario-migration-generate" filenames

  compilationStdout `expectTextContains`
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
testExamples = withTestEnv $ do
  failuresM <- io $ newMVar []
  let
    project = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-migration-generate"
    testFiles =
        [
          -- "src/Test/Migrate_Record.elm"
        -- , "src/Test/Migrate_External_Wrap.elm"
          "src/Migrate_External_Paramed"
        , "src/Migrate_All"
        -- , ""
        ]

    catchTestException :: FilePath -> SomeException -> IO a
    catchTestException filename e = do
      modifyMVar_ failuresM (\failures -> pure $ failures ++ filename)
      putStrLn "🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥"
      throw e

  testFiles & mapM (\folder -> do
    io $ putStrLn $ "testing: " <> show folder
    let
      oldVersion = 1
      newVersion = 2
      filenameOld = folder </> "Old.elm"
      filenameNew = folder </> "New.elm"
      filenameExpected = folder </> "Expected.elm"
      filenameActual = folder </> "Actual.elm"
      typeName = "Target"
      scenarioName = FP.takeBaseName folder
      moduleNameOld = N.fromChars $ scenarioName <> ".Old"
      moduleNameNew = N.fromChars $ scenarioName <> ".New"
      moduleNameActual = scenarioName <> ".Actual"
      moduleNameExpected = scenarioName <> ".Expected"
      moduleOld = ModuleName.Canonical (Pkg.Name "author" "project") moduleNameOld
      moduleNew = ModuleName.Canonical (Pkg.Name "author" "project") moduleNameNew

    interfacesE <- io $ Lamdera.Make.compileToInterfaces project filenameOld [filenameNew] `catch` catchTestException filenameNew

    case interfacesE of
      Left err -> io $ Lamdera.Progress.throw $ Reporting.Exit.makeToReport err
      -- "🏗  error, please compile manually:\n\ncd " <> project <> " && lamdera make " <> filename <> "\n\n"
      Right interfaces -> do
        let
          typeOldM = Helpers.findDef moduleOld typeName interfaces
          typeNewM = Helpers.findDef moduleNew typeName interfaces

        expectationM <- io $ readUtf8Text (project </> filenameExpected)

        case (typeOldM, typeNewM, expectationM) of
          (Just typeDefOld, Just typeDefNew, Just expectation) -> do
              let
                migrationNested = MigrationGenerator.migrateTypeDef typeDefOld typeDefNew oldVersion newVersion interfaces [] []
                (Helpers.MigrationNested migration migrationImports migrationDefs) = migrationNested
                migrationDefM = migrationDefs & Map.lookup (moduleNew, typeName) & fmap Helpers.migrations

              case migrationDefM of
                Just migrationDef -> do
                  let final = MigrationGenerator.migrationsToFile (stringToText moduleNameActual) oldVersion newVersion [(typeName, (migrationNested { Helpers.migrationDef = "target = " <> migration  }))] moduleNew
                      expected = Ext.ElmFormat.formatOrPassthrough (expectation & T.replace "\\n" "\n" & T.strip)
                      -- actual = Ext.ElmFormat.formatOrPassthrough (migrationDef & T.replace "\\n" "\n" & T.strip)
                      actual = Ext.ElmFormat.formatOrPassthrough (final)
                  _ <- io $ writeUtf8 (project </> filenameActual) actual
                  expectEqualTextTrimmed (expected & T.replace "Expected exposing (..)" "Actual exposing (..)") actual
                  pure migrationNested
                Nothing ->
                  pure migrationNested
          (_, _, Nothing) ->                  error $ "Could not find expectation file: " <> filenameExpected
          (Nothing, Just typeNew, _) ->       error $ "Could not find type `Target` in test: " <> N.toChars moduleNameNew
          (Just typeOld, Nothing, _) -> error $ "Could not find type `Target` in test: " <> N.toChars moduleNameOld
          (Nothing, Nothing, _) ->      error $ "Could not find type `Target` in test: " <> N.toChars moduleNameNew
    )

  failures <- io $ readMVar failuresM
  if length failures > 0
    then
      crash failures
    else
      scope "senarios-alltypes no exceptions" $ ok



-- Old setup for reference with expectation inside an Elm string expression
-- expectationM <- io $ Ext.Query.Optimized.findString "expected" project filename
-- case (typeOldM, typeNewM <|> typeNewM2, expectationM) of
--   (Just typeDefOld, Just typeDefNew, Just expectation) -> do
--       let
--         migrationNested = MigrationGenerator.migrateTypeDef typeDefOld typeDefNew oldVersion newVersion interfaces [] []
--         (Helpers.MigrationNested migration _ migrationDefs) = migrationNested
--         migrationDefM = migrationDefs & Map.lookup (moduleCan, typeName) & fmap Helpers.migrations

--       case migrationDefM of
--         Just migrationDef -> do
--           let final = MigrationGenerator.migrationsToFile oldVersion newVersion [(typeName, migrationNested)] moduleCan
--           expected <- io $ Ext.ElmFormat.formatOrPassthrough (expectation & T.replace "\\n" "\n" & T.strip)
--           actual <- io $ Ext.ElmFormat.formatOrPassthrough (migrationDef & T.replace "\\n" "\n" & T.strip)
--           expectEqualTextTrimmed expected final
--           pure migrationNested
--         Nothing ->
--           pure migrationNested
--   (_, _, Nothing) ->                  error $ "Could not find `expectation` string in test: " <> N.toChars moduleName
--   (Nothing, Just typeNew, _) ->       error $ "Could not find TypeOld in test: " <> N.toChars moduleName
--   (Just typeOld, Nothing, _) -> error $ "Could not find TypeNew in test: " <> N.toChars moduleName <> " or " <> N.toChars moduleName2
--   (Nothing, Nothing, _) ->      error $ "Could not find TypeOld or TypeNew in test: " <> N.toChars moduleName