{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Wire where

import qualified Data.Map as Map
import qualified Elm.ModuleName as Module
import qualified Elm.Package as Pkg

import Control.Concurrent.MVar
import Control.Exception (SomeException, AsyncException(UserInterrupt), catch, fromException, throw)
import System.FilePath ((</>))

import EasyTest
import Lamdera
import qualified Lamdera.Compile

-- tests
import qualified Lamdera.Wire3.Core
import AST.Canonical

all = EasyTest.run suite

suite :: Test ()
suite = tests $
  [ scope "compile all Elm wire expectations" wire
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


wire :: Test ()
wire = do

  failuresM <- io $ newMVar []

  io $ do
    let project = "./test/scenario-alltypes"

    setEnv "LOVR" "/Users/mario/dev/projects/lamdera/overrides"
    setEnv "LTEST" "1"
    setEnv "LDEBUG" "1"
    setEnv "ELM_HOME" "/Users/mario/elm-home-elmx-test"

    let testFiles =
          [ ""
          , "src/Test/Wire_Union_1_Basic.elm"
          , "src/Test/Wire_Union_2_Basic.elm"
          , "src/Test/External.elm"
          , "src/Test/Wire_Union_3_Params.elm"
          , "src/Test/Wire_Union_4_Tricky.elm"
          , "src/Test/Wire_Union_5_Massive.elm"
          , "src/Test/Wire_Alias_1_Basic.elm"
          , "src/Test/Wire_Alias_2_Record.elm"
          , "src/Test/Wire_Alias_3_SubAlias.elm"
          , "src/Test/Wire_Alias_4_TvarRename.elm"
          , "src/Test/Wire_Tvar_Ambiguous.elm"
          , "src/Test/Wire_Core_Types.elm"
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
          ]

    let
      catchTestException :: FilePath -> SomeException -> IO a
      catchTestException filename e = do
        modifyMVar_ failuresM (\failures -> pure $ failures ++ filename)
        putStrLn "🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥"
        throw e


    testFiles & filter ((/=) "") & mapM (\filename -> do
        putStrLn $ "testing: " <> show filename
        -- Bust Elm's caching with this one weird trick!
        touch $ project </> filename
        Lamdera.Compile.makeDev project [filename] `catch` catchTestException filename
      )

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
