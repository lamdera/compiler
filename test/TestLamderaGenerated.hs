{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TestLamderaGenerated where

import EasyTest
import Control.Applicative
import Control.Monad
import NeatInterpolation
import qualified Data.Text as T
import Lamdera
import Lamdera.Generated
import System.Environment (setEnv, unsetEnv)

all = do
  -- setEnv "LDEBUG" "1"
  run suite
  -- runOnly "migration 62 after migrate 57" suite
  -- unsetEnv "LDEBUG"


suite :: Test ()
suite = tests
  [ scope "new 3 after migrate 2" $ do
      migrations <- io $ getMigrationsSequence ["V2.elm", "V3.elm"] (WithoutMigrations 4) 3
      expectEqualTextTrimmed (show migrations & T.pack) "[(2,[WithMigrations 2,WithMigrations 3,WithoutMigrations 4]),(3,[WithMigrations 3,WithoutMigrations 4]),(4,[WithoutMigrations 4])]"

  , scope "deploy 3 after migrate 2" $ do
      migrations <- io $ getMigrationsSequence ["V2.elm"] (WithoutMigrations 3) 3
      show migrations
        & expectEqual "[(1,[WithMigrations 1,WithMigrations 2,WithoutMigrations 3]),(2,[WithMigrations 2,WithoutMigrations 3]),(3,[WithoutMigrations 3])]"

  , scope "deploy 3 after no migrations" $ do
      migrations <- io $ getMigrationsSequence ["V3.elm"] (WithMigrations 3) 3
      show migrations
        & expectEqual "[(1,[WithMigrations 1,WithMigrations 3]),(2,[WithMigrations 1,WithMigrations 3]),(3,[WithMigrations 3])]"

  , scope "historicMigrations: no deploys" $ do
      let nextVersion = (WithoutMigrations 1)
      migrations <- io $ getMigrationsSequence [] nextVersion 3
      expectEqualTextTrimmed (historicMigrations migrations nextVersion) ""

  , scope "historicMigrations: 1 deploy" $ do
      let nextVersion = (WithoutMigrations 2)
      migrations <- io $ getMigrationsSequence [] nextVersion 3
      expectEqualTextTrimmed (historicMigrations migrations nextVersion) [text|
        1 ->
            case tipe of
                "BackendModel" ->
                    decodeType "BackendModel" 2 intList T2.w2_decode_BackendModel
                        |> upgradeSucceeds CurrentBackendModel
                        |> otherwiseError

                "FrontendModel" ->
                    decodeType "FrontendModel" 2 intList T2.w2_decode_FrontendModel
                        |> upgradeSucceeds CurrentFrontendModel
                        |> otherwiseError

                "FrontendMsg" ->
                    decodeType "FrontendMsg" 2 intList T2.w2_decode_FrontendMsg
                        |> upgradeSucceeds CurrentFrontendMsg
                        |> otherwiseError

                "ToBackend" ->
                    decodeType "ToBackend" 2 intList T2.w2_decode_ToBackend
                        |> upgradeSucceeds CurrentToBackend
                        |> otherwiseError

                "BackendMsg" ->
                    decodeType "BackendMsg" 2 intList T2.w2_decode_BackendMsg
                        |> upgradeSucceeds CurrentBackendMsg
                        |> otherwiseError

                "ToFrontend" ->
                    decodeType "ToFrontend" 2 intList T2.w2_decode_ToFrontend
                        |> upgradeSucceeds CurrentToFrontend
                        |> otherwiseError

                _ ->
                    UnknownType tipe

      |]

  , scope "historicMigrations: 3rd deploy after migrations" $ do
      let nextVersion = (WithoutMigrations 3)
      migrations <- io $ getMigrationsSequence ["V2.elm"] nextVersion 3
      expectEqualTextTrimmed (historicMigrations migrations nextVersion) [text|
        1 ->
            case tipe of
                "BackendModel" ->
                    decodeType "BackendModel" 1 intList T1.w2_decode_BackendModel
                        |> thenMigrateModel "BackendModel" M2.backendModel T1.w2_encode_BackendModel T2.w2_decode_BackendModel 2
                        |> thenMigrateModel "BackendModel" (always ModelUnchanged) T2.w2_encode_BackendModel T3.w2_decode_BackendModel 3
                        |> upgradeSucceeds CurrentBackendModel
                        |> otherwiseError

                "FrontendModel" ->
                    decodeType "FrontendModel" 1 intList T1.w2_decode_FrontendModel
                        |> thenMigrateModel "FrontendModel" M2.frontendModel T1.w2_encode_FrontendModel T2.w2_decode_FrontendModel 2
                        |> thenMigrateModel "FrontendModel" (always ModelUnchanged) T2.w2_encode_FrontendModel T3.w2_decode_FrontendModel 3
                        |> upgradeSucceeds CurrentFrontendModel
                        |> otherwiseError

                "FrontendMsg" ->
                    decodeType "FrontendMsg" 1 intList T1.w2_decode_FrontendMsg
                        |> thenMigrateMsg "FrontendMsg" M2.frontendMsg T1.w2_encode_FrontendMsg T2.w2_decode_FrontendMsg 2
                        |> thenMigrateMsg "FrontendMsg" (always MsgUnchanged) T2.w2_encode_FrontendMsg T3.w2_decode_FrontendMsg 3
                        |> upgradeSucceeds CurrentFrontendMsg
                        |> otherwiseError

                "ToBackend" ->
                    decodeType "ToBackend" 1 intList T1.w2_decode_ToBackend
                        |> thenMigrateMsg "ToBackend" M2.toBackend T1.w2_encode_ToBackend T2.w2_decode_ToBackend 2
                        |> thenMigrateMsg "ToBackend" (always MsgUnchanged) T2.w2_encode_ToBackend T3.w2_decode_ToBackend 3
                        |> upgradeSucceeds CurrentToBackend
                        |> otherwiseError

                "BackendMsg" ->
                    decodeType "BackendMsg" 1 intList T1.w2_decode_BackendMsg
                        |> thenMigrateMsg "BackendMsg" M2.backendMsg T1.w2_encode_BackendMsg T2.w2_decode_BackendMsg 2
                        |> thenMigrateMsg "BackendMsg" (always MsgUnchanged) T2.w2_encode_BackendMsg T3.w2_decode_BackendMsg 3
                        |> upgradeSucceeds CurrentBackendMsg
                        |> otherwiseError

                "ToFrontend" ->
                    decodeType "ToFrontend" 1 intList T1.w2_decode_ToFrontend
                        |> thenMigrateMsg "ToFrontend" M2.toFrontend T1.w2_encode_ToFrontend T2.w2_decode_ToFrontend 2
                        |> thenMigrateMsg "ToFrontend" (always MsgUnchanged) T2.w2_encode_ToFrontend T3.w2_decode_ToFrontend 3
                        |> upgradeSucceeds CurrentToFrontend
                        |> otherwiseError

                _ ->
                    UnknownType tipe

        2 ->
            case tipe of
                "BackendModel" ->
                    decodeType "BackendModel" 3 intList T3.w2_decode_BackendModel
                        |> upgradeSucceeds CurrentBackendModel
                        |> otherwiseError

                "FrontendModel" ->
                    decodeType "FrontendModel" 3 intList T3.w2_decode_FrontendModel
                        |> upgradeSucceeds CurrentFrontendModel
                        |> otherwiseError

                "FrontendMsg" ->
                    decodeType "FrontendMsg" 3 intList T3.w2_decode_FrontendMsg
                        |> upgradeSucceeds CurrentFrontendMsg
                        |> otherwiseError

                "ToBackend" ->
                    decodeType "ToBackend" 3 intList T3.w2_decode_ToBackend
                        |> upgradeSucceeds CurrentToBackend
                        |> otherwiseError

                "BackendMsg" ->
                    decodeType "BackendMsg" 3 intList T3.w2_decode_BackendMsg
                        |> upgradeSucceeds CurrentBackendMsg
                        |> otherwiseError

                "ToFrontend" ->
                    decodeType "ToFrontend" 3 intList T3.w2_decode_ToFrontend
                        |> upgradeSucceeds CurrentToFrontend
                        |> otherwiseError

                _ ->
                    UnknownType tipe
      |]


  , scope "unchanged 62 after migrate 57" $ do

      let nextVersion = (WithoutMigrations 62)
      migrations <- io $ getMigrationsSequence
        ["V17.elm","V28.elm","V13.elm","V40.elm","V42.elm","V56.elm","V57.elm","V43.elm","V52.elm","V45.elm","V22.elm","V3.elm","V2.elm","V34.elm","V20.elm","V18.elm","V6.elm","V26.elm"]
        nextVersion
        3

      scope "migrations" $
        expectEqualTextTrimmed (show migrations & T.pack) "[(60,[WithMigrations 57,WithoutMigrations 62]),(61,[WithMigrations 57,WithoutMigrations 62]),(62,[WithoutMigrations 62])]"

      scope "type imports" $
        expectEqualTextTrimmed (importTypes migrations nextVersion) "import Evergreen.V57.Types as T57"

      scope "historic migrations" $
        expectEqualTextTrimmed (historicMigrations migrations nextVersion)
        [text|
          60 ->
              case tipe of
                  "BackendModel" ->
                      decodeType "BackendModel" 62 intList T62.w2_decode_BackendModel
                          |> upgradeSucceeds CurrentBackendModel
                          |> otherwiseError

                  "FrontendModel" ->
                      decodeType "FrontendModel" 62 intList T62.w2_decode_FrontendModel
                          |> upgradeSucceeds CurrentFrontendModel
                          |> otherwiseError

                  "FrontendMsg" ->
                      decodeType "FrontendMsg" 62 intList T62.w2_decode_FrontendMsg
                          |> upgradeSucceeds CurrentFrontendMsg
                          |> otherwiseError

                  "ToBackend" ->
                      decodeType "ToBackend" 62 intList T62.w2_decode_ToBackend
                          |> upgradeSucceeds CurrentToBackend
                          |> otherwiseError

                  "BackendMsg" ->
                      decodeType "BackendMsg" 62 intList T62.w2_decode_BackendMsg
                          |> upgradeSucceeds CurrentBackendMsg
                          |> otherwiseError

                  "ToFrontend" ->
                      decodeType "ToFrontend" 62 intList T62.w2_decode_ToFrontend
                          |> upgradeSucceeds CurrentToFrontend
                          |> otherwiseError

                  _ ->
                      UnknownType tipe

          61 ->
              case tipe of
                  "BackendModel" ->
                      decodeType "BackendModel" 62 intList T62.w2_decode_BackendModel
                          |> upgradeSucceeds CurrentBackendModel
                          |> otherwiseError

                  "FrontendModel" ->
                      decodeType "FrontendModel" 62 intList T62.w2_decode_FrontendModel
                          |> upgradeSucceeds CurrentFrontendModel
                          |> otherwiseError

                  "FrontendMsg" ->
                      decodeType "FrontendMsg" 62 intList T62.w2_decode_FrontendMsg
                          |> upgradeSucceeds CurrentFrontendMsg
                          |> otherwiseError

                  "ToBackend" ->
                      decodeType "ToBackend" 62 intList T62.w2_decode_ToBackend
                          |> upgradeSucceeds CurrentToBackend
                          |> otherwiseError

                  "BackendMsg" ->
                      decodeType "BackendMsg" 62 intList T62.w2_decode_BackendMsg
                          |> upgradeSucceeds CurrentBackendMsg
                          |> otherwiseError

                  "ToFrontend" ->
                      decodeType "ToFrontend" 62 intList T62.w2_decode_ToFrontend
                          |> upgradeSucceeds CurrentToFrontend
                          |> otherwiseError

                  _ ->
                      UnknownType tipe
        |]



  , scope "migration 62 after migrate 57" $ do

      let nextVersion = (WithMigrations 62)
      migrations <- io $ getMigrationsSequence
        ["V17.elm","V28.elm","V13.elm","V40.elm","V42.elm","V56.elm","V57.elm","V43.elm","V52.elm","V45.elm","V22.elm","V3.elm","V2.elm","V34.elm","V20.elm","V18.elm","V6.elm","V26.elm", "V62.elm"]
        nextVersion
        3

      scope "migrations" $
        expectEqualTextTrimmed (show migrations & T.pack) "[(60,[WithMigrations 57,WithMigrations 62]),(61,[WithMigrations 57,WithMigrations 62]),(62,[WithMigrations 62])]"

      scope "type imports" $
        expectEqualTextTrimmed (importTypes migrations nextVersion) "import Evergreen.V57.Types as T57"

      scope "historic migrations" $
        expectEqualTextTrimmed (historicMigrations migrations nextVersion)
        [text|
          60 ->
              case tipe of
                  "BackendModel" ->
                      decodeType "BackendModel" 57 intList T57.w2_decode_BackendModel
                          |> thenMigrateModel "BackendModel" M62.backendModel T57.w2_encode_BackendModel T62.w2_decode_BackendModel 62
                          |> upgradeSucceeds CurrentBackendModel
                          |> otherwiseError

                  "FrontendModel" ->
                      decodeType "FrontendModel" 57 intList T57.w2_decode_FrontendModel
                          |> thenMigrateModel "FrontendModel" M62.frontendModel T57.w2_encode_FrontendModel T62.w2_decode_FrontendModel 62
                          |> upgradeSucceeds CurrentFrontendModel
                          |> otherwiseError

                  "FrontendMsg" ->
                      decodeType "FrontendMsg" 57 intList T57.w2_decode_FrontendMsg
                          |> thenMigrateMsg "FrontendMsg" M62.frontendMsg T57.w2_encode_FrontendMsg T62.w2_decode_FrontendMsg 62
                          |> upgradeSucceeds CurrentFrontendMsg
                          |> otherwiseError

                  "ToBackend" ->
                      decodeType "ToBackend" 57 intList T57.w2_decode_ToBackend
                          |> thenMigrateMsg "ToBackend" M62.toBackend T57.w2_encode_ToBackend T62.w2_decode_ToBackend 62
                          |> upgradeSucceeds CurrentToBackend
                          |> otherwiseError

                  "BackendMsg" ->
                      decodeType "BackendMsg" 57 intList T57.w2_decode_BackendMsg
                          |> thenMigrateMsg "BackendMsg" M62.backendMsg T57.w2_encode_BackendMsg T62.w2_decode_BackendMsg 62
                          |> upgradeSucceeds CurrentBackendMsg
                          |> otherwiseError

                  "ToFrontend" ->
                      decodeType "ToFrontend" 57 intList T57.w2_decode_ToFrontend
                          |> thenMigrateMsg "ToFrontend" M62.toFrontend T57.w2_encode_ToFrontend T62.w2_decode_ToFrontend 62
                          |> upgradeSucceeds CurrentToFrontend
                          |> otherwiseError

                  _ ->
                      UnknownType tipe

          61 ->
              case tipe of
                  "BackendModel" ->
                      decodeType "BackendModel" 57 intList T57.w2_decode_BackendModel
                          |> thenMigrateModel "BackendModel" M62.backendModel T57.w2_encode_BackendModel T62.w2_decode_BackendModel 62
                          |> upgradeSucceeds CurrentBackendModel
                          |> otherwiseError

                  "FrontendModel" ->
                      decodeType "FrontendModel" 57 intList T57.w2_decode_FrontendModel
                          |> thenMigrateModel "FrontendModel" M62.frontendModel T57.w2_encode_FrontendModel T62.w2_decode_FrontendModel 62
                          |> upgradeSucceeds CurrentFrontendModel
                          |> otherwiseError

                  "FrontendMsg" ->
                      decodeType "FrontendMsg" 57 intList T57.w2_decode_FrontendMsg
                          |> thenMigrateMsg "FrontendMsg" M62.frontendMsg T57.w2_encode_FrontendMsg T62.w2_decode_FrontendMsg 62
                          |> upgradeSucceeds CurrentFrontendMsg
                          |> otherwiseError

                  "ToBackend" ->
                      decodeType "ToBackend" 57 intList T57.w2_decode_ToBackend
                          |> thenMigrateMsg "ToBackend" M62.toBackend T57.w2_encode_ToBackend T62.w2_decode_ToBackend 62
                          |> upgradeSucceeds CurrentToBackend
                          |> otherwiseError

                  "BackendMsg" ->
                      decodeType "BackendMsg" 57 intList T57.w2_decode_BackendMsg
                          |> thenMigrateMsg "BackendMsg" M62.backendMsg T57.w2_encode_BackendMsg T62.w2_decode_BackendMsg 62
                          |> upgradeSucceeds CurrentBackendMsg
                          |> otherwiseError

                  "ToFrontend" ->
                      decodeType "ToFrontend" 57 intList T57.w2_decode_ToFrontend
                          |> thenMigrateMsg "ToFrontend" M62.toFrontend T57.w2_encode_ToFrontend T62.w2_decode_ToFrontend 62
                          |> upgradeSucceeds CurrentToFrontend
                          |> otherwiseError

                  _ ->
                      UnknownType tipe
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
