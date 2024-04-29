{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lamdera.Evergreen.TestMigrationHarness where

import EasyTest
import Control.Applicative
import Control.Monad
import NeatInterpolation
import qualified Data.Text as T

import Test.Helpers

import Lamdera
import Lamdera.Evergreen.MigrationHarness (VersionInfo(..))
import qualified Lamdera.Evergreen.MigrationHarness
import qualified Lamdera.Compile


all = do
  run suite
  -- runOnly "migration 62 after migrate 57" suite


suite :: Test ()
suite = tests
  [ scope "new 3 after migrate 2" $ do
      migrations <- io $ Lamdera.Evergreen.MigrationHarness.getMigrationsSequence ["V2.elm", "V3.elm"] (WithoutMigrations 4) 3
      expectEqualTextTrimmed (show migrations & T.pack) "[(2,[WithMigrations 2,WithMigrations 3,WithoutMigrations 4]),(3,[WithMigrations 3,WithoutMigrations 4]),(4,[WithoutMigrations 4])]"

  , scope "deploy 3 after migrate 2" $ do
      migrations <- io $ Lamdera.Evergreen.MigrationHarness.getMigrationsSequence ["V2.elm"] (WithoutMigrations 3) 3
      show migrations
        & expectEqual "[(1,[WithMigrations 1,WithMigrations 2,WithoutMigrations 3]),(2,[WithMigrations 2,WithoutMigrations 3]),(3,[WithoutMigrations 3])]"

  , scope "deploy 3 after no migrations" $ do
      migrations <- io $ Lamdera.Evergreen.MigrationHarness.getMigrationsSequence ["V3.elm"] (WithMigrations 3) 3
      show migrations
        & expectEqual "[(1,[WithMigrations 1,WithMigrations 3]),(2,[WithMigrations 1,WithMigrations 3]),(3,[WithMigrations 3])]"

  , scope "historicMigrations: no deploys" $ do
      let nextVersion = (WithoutMigrations 1)
      migrations <- io $ Lamdera.Evergreen.MigrationHarness.getMigrationsSequence [] nextVersion 3
      expectEqualTextTrimmed (Lamdera.Evergreen.MigrationHarness.historicMigrations migrations nextVersion "BackendModel") ""

  , scope "historicMigrations: 1 deploy" $ do
      let nextVersion = (WithoutMigrations 2)
      migrations <- io $ Lamdera.Evergreen.MigrationHarness.getMigrationsSequence [] nextVersion 3
      expectEqualTextTrimmed (Lamdera.Evergreen.MigrationHarness.historicMigrations migrations nextVersion "BackendModel") [text|
        1 ->
            decodeType 5 2 bytes T2.w3_decode_BackendModel
                |> upgradeSucceeds
                |> otherwiseError
      |]

  , scope "historicMigrations: 3rd deploy after migrations" $ do
      let nextVersion = (WithoutMigrations 3)
      migrations <- io $ Lamdera.Evergreen.MigrationHarness.getMigrationsSequence ["V2.elm"] nextVersion 3
      expectEqualTextTrimmed (Lamdera.Evergreen.MigrationHarness.historicMigrations migrations nextVersion "BackendModel") [text|
        1 ->
            decodeType 5 1 bytes T1.w3_decode_BackendModel
                |> thenMigrateModel 5 M2.backendModel T1.w3_encode_BackendModel T2.w3_decode_BackendModel 2
                |> thenMigrateModel 5 (always ModelUnchanged) T2.w3_encode_BackendModel T3.w3_decode_BackendModel 3
                |> upgradeSucceeds
                |> otherwiseError
        2 ->
            decodeType 5 3 bytes T3.w3_decode_BackendModel
                |> upgradeSucceeds
                |> otherwiseError
      |]


  , scope "unchanged 62 after migrate 57" $ do

      let nextVersion = (WithoutMigrations 62)
      migrations <- io $ Lamdera.Evergreen.MigrationHarness.getMigrationsSequence
        ["V17.elm","V28.elm","V13.elm","V40.elm","V42.elm","V56.elm","V57.elm","V43.elm","V52.elm","V45.elm","V22.elm","V3.elm","V2.elm","V34.elm","V20.elm","V18.elm","V6.elm","V26.elm"]
        nextVersion
        3

      scope "migrations" $
        expectEqualTextTrimmed (show migrations & T.pack) "[(60,[WithMigrations 57,WithoutMigrations 62]),(61,[WithMigrations 57,WithoutMigrations 62]),(62,[WithoutMigrations 62])]"

      scope "type imports" $
        expectEqualTextTrimmed (Lamdera.Evergreen.MigrationHarness.typeImports migrations nextVersion & T.intercalate "\n") "import Evergreen.V57.Types as T57"

      scope "historic migrations" $
        expectEqualTextTrimmed (Lamdera.Evergreen.MigrationHarness.historicMigrations migrations nextVersion "BackendModel")
        [text|
          60 ->
              decodeType 5 62 bytes T62.w3_decode_BackendModel
                  |> upgradeSucceeds
                  |> otherwiseError
          61 ->
              decodeType 5 62 bytes T62.w3_decode_BackendModel
                  |> upgradeSucceeds
                  |> otherwiseError
        |]



  , scope "migration 62 after migrate 57" $ do

      let
        nextVersion = (WithMigrations 62)
        migrationsFilenames = ["V17.elm","V28.elm","V13.elm","V40.elm","V42.elm","V56.elm","V57.elm","V43.elm","V52.elm","V45.elm","V22.elm","V3.elm","V2.elm","V34.elm","V20.elm","V18.elm","V6.elm","V26.elm", "V62.elm"]
      migrations <- io $ Lamdera.Evergreen.MigrationHarness.getMigrationsSequence migrationsFilenames nextVersion 3

      scope "migrations" $
        expectEqualTextTrimmed (show migrations & T.pack) "[(60,[WithMigrations 57,WithMigrations 62]),(61,[WithMigrations 57,WithMigrations 62]),(62,[WithMigrations 62])]"

      scope "type imports" $
        expectEqualTextTrimmed (Lamdera.Evergreen.MigrationHarness.typeImports migrations nextVersion & T.intercalate "\n") "import Evergreen.V57.Types as T57"

      scope "historic migrations" $
        expectEqualTextTrimmed (Lamdera.Evergreen.MigrationHarness.historicMigrations migrations nextVersion "BackendModel")
        [text|
          60 ->
              decodeType 5 57 bytes T57.w3_decode_BackendModel
                  |> thenMigrateModel 5 M62.backendModel T57.w3_encode_BackendModel T62.w3_decode_BackendModel 62
                  |> upgradeSucceeds
                  |> otherwiseError
          61 ->
              decodeType 5 57 bytes T57.w3_decode_BackendModel
                  |> thenMigrateModel 5 M62.backendModel T57.w3_encode_BackendModel T62.w3_decode_BackendModel 62
                  |> upgradeSucceeds
                  |> otherwiseError
        |]


  , scope "full first - (WithoutMigrations 1)" $ do

      let
        nextVersion = (WithoutMigrations 1)
        migrationsFilenames = []
      migrations <- io $ Lamdera.Evergreen.MigrationHarness.getMigrationsSequence migrationsFilenames nextVersion 1
      result <- io $ withProdMode $ Lamdera.Evergreen.MigrationHarness.generateFor nextVersion migrationsFilenames

      scope "full" $
        expectEqualTextTrimmed result
        [text|

          module LamderaGenerated exposing (..)

          import Lamdera.Migrations exposing (..)
          import Lamdera.Wire3 exposing (Bytes, Decoder, Encoder, bytesDecode, bytesEncode)
          import LamderaHelpers exposing (..)
          import Types as T1


          currentVersion : Int
          currentVersion =
              1


          upgradeBackendModelPrevious =
              ()


          decodeAndUpgradeBackendModel : Int -> Bytes -> UpgradeResult T1.BackendModel T1.BackendMsg
          decodeAndUpgradeBackendModel version bytes =
              case version of
                  1 ->
                      decodeType 5 version bytes T1.w3_decode_BackendModel
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "BackendModel", bytes )


          decodeAndUpgradeFrontendModel : Int -> Bytes -> UpgradeResult T1.FrontendModel T1.FrontendMsg
          decodeAndUpgradeFrontendModel version bytes =
              case version of
                  1 ->
                      decodeType 4 version bytes T1.w3_decode_FrontendModel
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "FrontendModel", bytes )


          decodeAndUpgradeFrontendMsg : Int -> Bytes -> UpgradeResult T1.FrontendMsg T1.FrontendMsg
          decodeAndUpgradeFrontendMsg version bytes =
              case version of
                  1 ->
                      decodeType 0 version bytes T1.w3_decode_FrontendMsg
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "FrontendMsg", bytes )


          decodeAndUpgradeToBackend : Int -> Bytes -> UpgradeResult T1.ToBackend T1.BackendMsg
          decodeAndUpgradeToBackend version bytes =
              case version of
                  1 ->
                      decodeType 1 version bytes T1.w3_decode_ToBackend
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "ToBackend", bytes )


          decodeAndUpgradeBackendMsg : Int -> Bytes -> UpgradeResult T1.BackendMsg T1.BackendMsg
          decodeAndUpgradeBackendMsg version bytes =
              case version of
                  1 ->
                      decodeType 2 version bytes T1.w3_decode_BackendMsg
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "BackendMsg", bytes )


          decodeAndUpgradeToFrontend : Int -> Bytes -> UpgradeResult T1.ToFrontend T1.FrontendMsg
          decodeAndUpgradeToFrontend version bytes =
              case version of
                  1 ->
                      decodeType 3 version bytes T1.w3_decode_ToFrontend
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "ToFrontend", bytes )

        |]


  , scope "full first - (WithMigrations 2)" $ do

      let
        nextVersion = (WithMigrations 2)
        migrationsFilenames = ["V2.elm"]
      migrations <- io $ Lamdera.Evergreen.MigrationHarness.getMigrationsSequence migrationsFilenames nextVersion 2
      result <- io $ withProdMode $ Lamdera.Evergreen.MigrationHarness.generateFor nextVersion migrationsFilenames

      scope "full" $
        expectEqualTextTrimmed result
        [text|

          module LamderaGenerated exposing (..)

          import Evergreen.Migrate.V2 as M2
          import Evergreen.V1.Types as T1
          import Lamdera.Migrations exposing (..)
          import Lamdera.Wire3 exposing (Bytes, Decoder, Encoder, bytesDecode, bytesEncode)
          import LamderaHelpers exposing (..)
          import Types as T2


          currentVersion : Int
          currentVersion =
              2


          upgradeBackendModelPrevious : T1.BackendModel -> UpgradeResult T2.BackendModel T2.BackendMsg
          upgradeBackendModelPrevious model_v1 =
              model_v1
                  |> M2.backendModel


          decodeAndUpgradeBackendModel : Int -> Bytes -> UpgradeResult T2.BackendModel T2.BackendMsg
          decodeAndUpgradeBackendModel version bytes =
              case version of
                  1 ->
                      decodeType 5 1 bytes T1.w3_decode_BackendModel
                          |> thenMigrateModel 5 M2.backendModel T1.w3_encode_BackendModel T2.w3_decode_BackendModel 2
                          |> upgradeSucceeds
                          |> otherwiseError

                  2 ->
                      decodeType 5 version bytes T2.w3_decode_BackendModel
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "BackendModel", bytes )


          decodeAndUpgradeFrontendModel : Int -> Bytes -> UpgradeResult T2.FrontendModel T2.FrontendMsg
          decodeAndUpgradeFrontendModel version bytes =
              case version of
                  1 ->
                      decodeType 4 1 bytes T1.w3_decode_FrontendModel
                          |> thenMigrateModel 4 M2.frontendModel T1.w3_encode_FrontendModel T2.w3_decode_FrontendModel 2
                          |> upgradeSucceeds
                          |> otherwiseError

                  2 ->
                      decodeType 4 version bytes T2.w3_decode_FrontendModel
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "FrontendModel", bytes )


          decodeAndUpgradeFrontendMsg : Int -> Bytes -> UpgradeResult T2.FrontendMsg T2.FrontendMsg
          decodeAndUpgradeFrontendMsg version bytes =
              case version of
                  1 ->
                      decodeType 0 1 bytes T1.w3_decode_FrontendMsg
                          |> thenMigrateMsg 0 M2.frontendMsg T1.w3_encode_FrontendMsg T2.w3_decode_FrontendMsg 2
                          |> upgradeSucceeds
                          |> otherwiseError

                  2 ->
                      decodeType 0 version bytes T2.w3_decode_FrontendMsg
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "FrontendMsg", bytes )


          decodeAndUpgradeToBackend : Int -> Bytes -> UpgradeResult T2.ToBackend T2.BackendMsg
          decodeAndUpgradeToBackend version bytes =
              case version of
                  1 ->
                      decodeType 1 1 bytes T1.w3_decode_ToBackend
                          |> thenMigrateMsg 1 M2.toBackend T1.w3_encode_ToBackend T2.w3_decode_ToBackend 2
                          |> upgradeSucceeds
                          |> otherwiseError

                  2 ->
                      decodeType 1 version bytes T2.w3_decode_ToBackend
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "ToBackend", bytes )


          decodeAndUpgradeBackendMsg : Int -> Bytes -> UpgradeResult T2.BackendMsg T2.BackendMsg
          decodeAndUpgradeBackendMsg version bytes =
              case version of
                  1 ->
                      decodeType 2 1 bytes T1.w3_decode_BackendMsg
                          |> thenMigrateMsg 2 M2.backendMsg T1.w3_encode_BackendMsg T2.w3_decode_BackendMsg 2
                          |> upgradeSucceeds
                          |> otherwiseError

                  2 ->
                      decodeType 2 version bytes T2.w3_decode_BackendMsg
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "BackendMsg", bytes )


          decodeAndUpgradeToFrontend : Int -> Bytes -> UpgradeResult T2.ToFrontend T2.FrontendMsg
          decodeAndUpgradeToFrontend version bytes =
              case version of
                  1 ->
                      decodeType 3 1 bytes T1.w3_decode_ToFrontend
                          |> thenMigrateMsg 3 M2.toFrontend T1.w3_encode_ToFrontend T2.w3_decode_ToFrontend 2
                          |> upgradeSucceeds
                          |> otherwiseError

                  2 ->
                      decodeType 3 version bytes T2.w3_decode_ToFrontend
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "ToFrontend", bytes )
        |]

      scope "compile and run" $ do
        let
          project = "/Users/mario/dev/projects/lamdera-compiler/test/scenario-migration-generate/"
          helpers = "src/LamderaHelpers.elm"
          target = "src/LamderaGenerated.elm"
          filenames = [target]

          setup = do
            writeUtf8 (project <> target) result
            cp (withRuntimeRoot ("runtime/" <> helpers)) (project <> helpers)

          cleanup _ = do
            rm (project <> target)
            rm (project <> helpers)

          test _ = do

            compilationStdout <- catchOutput $
              Lamdera.Compile.makeDev (withCompilerRoot "test/scenario-migration-generate") filenames

            compilationStdout `expectTextDoesNotContain` "I cannot find a `unsafeCoerce` variable"

        using setup cleanup test


  , scope "full first - (WithoutMigrations 2)" $ do

      let
        nextVersion = (WithoutMigrations 2)
        migrationsFilenames = []
      migrations <- io $ Lamdera.Evergreen.MigrationHarness.getMigrationsSequence migrationsFilenames nextVersion 2
      result <- io $ withProdMode $ Lamdera.Evergreen.MigrationHarness.generateFor nextVersion migrationsFilenames

      scope "full" $
        expectEqualTextTrimmed result
        [text|

          module LamderaGenerated exposing (..)

          import Evergreen.V1.Types as T1
          import Lamdera.Migrations exposing (..)
          import Lamdera.Wire3 exposing (Bytes, Decoder, Encoder, bytesDecode, bytesEncode)
          import LamderaHelpers exposing (..)
          import Types as T2


          currentVersion : Int
          currentVersion =
              2


          upgradeBackendModelPrevious : T2.BackendModel -> UpgradeResult T2.BackendModel T2.BackendMsg
          upgradeBackendModelPrevious model_v1 =
              unchanged model_v1


          decodeAndUpgradeBackendModel : Int -> Bytes -> UpgradeResult T2.BackendModel T2.BackendMsg
          decodeAndUpgradeBackendModel version bytes =
              case version of
                  1 ->
                      decodeType 5 2 bytes T2.w3_decode_BackendModel
                          |> upgradeSucceeds
                          |> otherwiseError

                  2 ->
                      decodeType 5 version bytes T2.w3_decode_BackendModel
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "BackendModel", bytes )


          decodeAndUpgradeFrontendModel : Int -> Bytes -> UpgradeResult T2.FrontendModel T2.FrontendMsg
          decodeAndUpgradeFrontendModel version bytes =
              case version of
                  1 ->
                      decodeType 4 2 bytes T2.w3_decode_FrontendModel
                          |> upgradeSucceeds
                          |> otherwiseError

                  2 ->
                      decodeType 4 version bytes T2.w3_decode_FrontendModel
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "FrontendModel", bytes )


          decodeAndUpgradeFrontendMsg : Int -> Bytes -> UpgradeResult T2.FrontendMsg T2.FrontendMsg
          decodeAndUpgradeFrontendMsg version bytes =
              case version of
                  1 ->
                      decodeType 0 2 bytes T2.w3_decode_FrontendMsg
                          |> upgradeSucceeds
                          |> otherwiseError

                  2 ->
                      decodeType 0 version bytes T2.w3_decode_FrontendMsg
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "FrontendMsg", bytes )


          decodeAndUpgradeToBackend : Int -> Bytes -> UpgradeResult T2.ToBackend T2.BackendMsg
          decodeAndUpgradeToBackend version bytes =
              case version of
                  1 ->
                      decodeType 1 2 bytes T2.w3_decode_ToBackend
                          |> upgradeSucceeds
                          |> otherwiseError

                  2 ->
                      decodeType 1 version bytes T2.w3_decode_ToBackend
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "ToBackend", bytes )


          decodeAndUpgradeBackendMsg : Int -> Bytes -> UpgradeResult T2.BackendMsg T2.BackendMsg
          decodeAndUpgradeBackendMsg version bytes =
              case version of
                  1 ->
                      decodeType 2 2 bytes T2.w3_decode_BackendMsg
                          |> upgradeSucceeds
                          |> otherwiseError

                  2 ->
                      decodeType 2 version bytes T2.w3_decode_BackendMsg
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "BackendMsg", bytes )


          decodeAndUpgradeToFrontend : Int -> Bytes -> UpgradeResult T2.ToFrontend T2.FrontendMsg
          decodeAndUpgradeToFrontend version bytes =
              case version of
                  1 ->
                      decodeType 3 2 bytes T2.w3_decode_ToFrontend
                          |> upgradeSucceeds
                          |> otherwiseError

                  2 ->
                      decodeType 3 version bytes T2.w3_decode_ToFrontend
                          |> upgradeIsCurrent
                          |> otherwiseError

                  _ ->
                      UnknownVersion ( version, "ToFrontend", bytes )
        |]
  ]


-- suite :: Test ()
-- suite = tests
--   [ scope "addition.ex1" $ expect (1 + 1 == 2)
--   , scope "addition.ex2" $ expect (2 + 3 == 5)
--   , scope "list.reversal" . fork $ do
--       -- generate lists from size 0 to 10, of Ints in (0,43)
--       -- shorthand: listsOf [0..10] (int' 0 43)
--       ns <- [0..10] `forM` \n -> replicateM n (int' 0 43)
--       ns `forM_` \ns -> expect (reverse (reverse ns) == ns)
--   -- equivalent to `scope "addition.ex3"`
--   , scope "addition" . scope "ex3" $ expect (3 + 3 == 6)
--   , scope "always passes" $ do
--       note "I'm running this test, even though it always passes!"
--       ok -- like `pure ()`, but records a success result
--   , scope "failing test" $ crash "oh noes!!" ]
--

-- NB: `run suite` would run all tests, but we only run
-- tests whose scopes are prefixed by "addition"
-- main = runOnly "addition" suite
