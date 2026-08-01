{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Wire where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Elm.ModuleName as Module
import qualified Elm.Package as Pkg

import Control.Concurrent.MVar
import Control.Exception (SomeException, AsyncException(UserInterrupt), catch, fromException, throw)
import System.FilePath ((</>))

import EasyTest
import Test.Helpers
import Lamdera
import qualified Lamdera.Compile
import qualified Lamdera.Relative

-- tests
import qualified Lamdera.Wire3.Core
import AST.Canonical

all = EasyTest.run suite

suite :: Test ()
suite = tests $
  [ scope "compile all Elm wire expectations" wire
  , scope "wire codegen has no Debug remnants under --optimize" wireOptimized
  , scope "function tests" functions
  ]

functions :: Test ()
functions = do
  let before =
        TType
            (Module.Canonical (Pkg.Name "elm" "core") "Maybe")
            "Maybe"
            [ TAlias
                (Module.Canonical (Pkg.Name "author" "project") "Test.Wire_Record_Extensible5_ElmCss")
                "Length"
                [("compatible", TVar "compatibleB"), ("units", TVar "unit")]
                (Holey
                  (TAlias
                      (Module.Canonical (Pkg.Name "author" "project") "Test.Wire_Record_Extensible5_ElmCss_External")
                      "Length"
                      [("compatible", TVar "compatible"), ("units", TVar "units")]
                      (Holey
                        (TRecord
                            (Map.fromList
                              [ ( "length"
                                , FieldType
                                    1
                                    (TType (Module.Canonical (Pkg.Name "author" "project") "Test.Wire_Record_Extensible5_ElmCss_External") "Compatible" []))
                              , ("numericValue", FieldType 2 (TType (Module.Canonical (Pkg.Name "elm" "core") "Basics") "Float" []))
                              , ("unitLabel", FieldType 4 (TType (Module.Canonical (Pkg.Name "elm" "core") "String") "String" []))
                              , ("units", FieldType 3 (TVar "units"))
                              , ("value", FieldType 0 (TType (Module.Canonical (Pkg.Name "elm" "core") "String") "String" []))
                              ])
                            (Just "compatible")))))
            ]

      expected =
        TType
            (Module.Canonical (Pkg.Name "elm" "core") "Maybe")
            "Maybe"
            [ TAlias
                (Module.Canonical (Pkg.Name "author" "project") "Test.Wire_Record_Extensible5_ElmCss")
                "Length"
                [("compatibleB", TVar "compatibleB"), ("unit", TVar "unit")]
                (Holey
                  (TAlias
                      (Module.Canonical (Pkg.Name "author" "project") "Test.Wire_Record_Extensible5_ElmCss_External")
                      "Length"
                      [("compatibleB", TVar "compatibleB"), ("unit", TVar "unit")]
                      (Holey
                        (TRecord
                            (Map.fromList
                              [ ( "length"
                                , FieldType
                                    1
                                    (TType (Module.Canonical (Pkg.Name "author" "project") "Test.Wire_Record_Extensible5_ElmCss_External") "Compatible" []))
                              , ("numericValue", FieldType 2 (TType (Module.Canonical (Pkg.Name "elm" "core") "Basics") "Float" []))
                              , ("unitLabel", FieldType 4 (TType (Module.Canonical (Pkg.Name "elm" "core") "String") "String" []))
                              , ("units", FieldType 3 (TVar "unit"))
                              , ("value", FieldType 0 (TType (Module.Canonical (Pkg.Name "elm" "core") "String") "String" []))
                              ])
                            (Just "compatibleB")))))
            ]

  expectEqualFormat expected (Lamdera.Wire3.Core.normaliseTvarNames Map.empty before)


wireTestFiles :: [FilePath]
wireTestFiles =
  [ "src/Test/Wire_Union_1_Basic.elm"
  , "src/Test/Wire_Union_2_Basic.elm"
  , "src/Test/External.elm"
  , "src/Test/Wire_Union_3_Params.elm"
  , "src/Test/Wire_Union_4_Tricky.elm"
  , "src/Test/Wire_Union_5_Massive.elm"
  , "src/Test/Wire_Alias_1_Basic.elm"
  , "src/Test/Wire_Alias_2_Record.elm"
  , "src/Test/Wire_Alias_3_SubAlias.elm"
  , "src/Test/Wire_Alias_4_TvarRename.elm"
  , "src/Test/Wire_Alias_5_ParamChain.elm"
  , "src/Test/Wire_Tvar_Ambiguous.elm"
  , "src/Test/Wire_Core_Types.elm"
  , "src/Test/Wire_Package_Types.elm"
  , "src/Test/Wire_Recursive.elm"
  , "src/Test/Wire_Record_Extensible1_Basic.elm"
  , "src/Test/Wire_Record_Extensible2_MultiParam.elm"
  , "src/Test/Wire_Record_Extensible3_Tricky.elm"
  , "src/Test/Wire_Record_Extensible4_DB.elm"
  , "src/Test/Wire_Record_Extensible5_ElmCss.elm"
  , "src/Test/Wire_Phantom.elm"
  , "src/Test/Wire_Tvar_Deep.elm"
  , "src/Test/Wire_Tvar_Deep2.elm"
  , "src/Test/Wire_Tvar_Recursive_Reference.elm"
  , "src/Test/Wire_Unsupported.elm"
  , "src/Test/Wire_Unconstructable.elm"
  , "src/Test/Wire_Union_ForeignRecordAlias.elm"
  ]


wire :: Test ()
wire = do

  failuresM <- io $ newMVar []

  ioSilenced $ do
    let project = "./test/scenario-alltypes"

    overrides <- Lamdera.Relative.requireDir "~/lamdera/overrides"
    elmHome <- Lamdera.Relative.requireDir "~/elm-home-elmx-test"

    withEnvVars [("LDEBUG", "1"), ("LTEST", "1"), ("LOVR", overrides), ("ELM_HOME", elmHome)] $ do

      let
        catchTestException :: FilePath -> SomeException -> IO a
        catchTestException filename e = do
          modifyMVar_ failuresM (\failures -> pure $ failures ++ filename)
          throw e

      wireTestFiles & mapM (\filename -> do
          -- Bust Elm's caching with this one weird trick!
          touch $ project </> filename
          Lamdera.Compile.makeDev project [filename] `catch` catchTestException filename
        )

  failures <- io $ readMVar failuresM
  if length failures > 0
    then
      crash failures
    else
      scope "scenario-alltypes no exceptions" $ ok


wireOptimized :: Test ()
wireOptimized = do
  ioSilenced $ do
    let project = "./test/scenario-alltypes"

    overrides <- Lamdera.Relative.requireDir "~/lamdera/overrides"
    elmHome <- Lamdera.Relative.requireDir "~/elm-home-elmx-test"

    withEnvVars [("LDEBUG", "1"), ("LOVR", overrides), ("ELM_HOME", elmHome)] $ do
      let
        toModuleName fp =
          -- "src/Test/Wire_Unsupported.elm" -> "Test.Wire_Unsupported"
          T.pack $ map (\c -> if c == '/' then '.' else c) $ drop 4 $ take (length fp - 4) fp

        imports = wireTestFiles
          & map (\f -> "import " <> toModuleName f)
          & T.intercalate "\n"

        scaffold =
          "module WireOptimizeCheck exposing (..)\n\n"
          <> imports
          <> "\nimport Html\n\nmain = Html.text \"\"\n"

        scaffoldPath = project </> "src/WireOptimizeCheck.elm"

      writeUtf8 scaffoldPath scaffold
      Lamdera.Compile.makeOptimized project ("src" </> "WireOptimizeCheck.elm")
      remove scaffoldPath

  scope "scenario-alltypes --optimize no exceptions" $ ok
