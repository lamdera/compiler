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

all = run suite


suite :: Test ()
suite = tests
  [ scope "deploy 3 after migrate 2" $ do
      migrations <- io $ getMigrationsSequence ["V2.elm"] (WithoutMigrations 3)
      show migrations
        & expectEqual "[[WithMigrations 1,WithMigrations 2,WithoutMigrations 3],[WithMigrations 2,WithoutMigrations 3],[WithoutMigrations 3]]"

  , scope "deploy 3 after no migrations" $ do
      migrations <- io $ getMigrationsSequence ["V3.elm"] (WithMigrations 3)
      show migrations
        & expectEqual "[[WithMigrations 1,WithMigrations 3],[WithMigrations 1,WithMigrations 3],[WithMigrations 3]]"

  , scope "historicMigrations: no deploys" $ do
      migrations <- io $ getMigrationsSequence [] (WithoutMigrations 1)
      expectEqualTextTrimmed (historicMigrations migrations) ""
  , scope "historicMigrations: 1 deploy" $ do
      migrations <- io $ getMigrationsSequence [] (WithoutMigrations 2)
      expectEqualTextTrimmed (historicMigrations migrations) [text|
        1 ->
            case tipe of
                "BackendModel" ->
                    decodeType "BackendModel" version intList T1.w2_decode_BackendModel
                        |> thenMigrateModel "BackendModel" (always ModelUnchanged) T1.w2_encode_BackendModel T2.w2_decode_BackendModel 2
                        |> upgradeSucceeds CurrentBackendModel
                        |> otherwiseError

                "FrontendModel" ->
                    decodeType "FrontendModel" version intList T1.w2_decode_FrontendModel
                        |> thenMigrateModel "FrontendModel" (always ModelUnchanged) T1.w2_encode_FrontendModel T2.w2_decode_FrontendModel 2
                        |> upgradeSucceeds CurrentFrontendModel
                        |> otherwiseError

                "FrontendMsg" ->
                    decodeType "FrontendMsg" version intList T1.w2_decode_FrontendMsg
                        |> thenMigrateMsg "FrontendMsg" (always MsgUnchanged) T1.w2_encode_FrontendMsg T2.w2_decode_FrontendMsg 2
                        |> upgradeSucceeds CurrentFrontendMsg
                        |> otherwiseError

                "ToBackend" ->
                    decodeType "ToBackend" version intList T1.w2_decode_ToBackend
                        |> thenMigrateMsg "ToBackend" (always MsgUnchanged) T1.w2_encode_ToBackend T2.w2_decode_ToBackend 2
                        |> upgradeSucceeds CurrentToBackend
                        |> otherwiseError

                "BackendMsg" ->
                    decodeType "BackendMsg" version intList T1.w2_decode_BackendMsg
                        |> thenMigrateMsg "BackendMsg" (always MsgUnchanged) T1.w2_encode_BackendMsg T2.w2_decode_BackendMsg 2
                        |> upgradeSucceeds CurrentBackendMsg
                        |> otherwiseError

                "ToFrontend" ->
                    decodeType "ToFrontend" version intList T1.w2_decode_ToFrontend
                        |> thenMigrateMsg "ToFrontend" (always MsgUnchanged) T1.w2_encode_ToFrontend T2.w2_decode_ToFrontend 2
                        |> upgradeSucceeds CurrentToFrontend
                        |> otherwiseError

                _ ->
                    UnknownType tipe

      |]
  ]


-- import Data.TreeDiff
-- prettyEditExpr $ ediff ("test1") ("test2")
--
-- t1 = "test\n\ntest"
-- t2 = "test\n\nbest"
-- prettyEditExpr $ ediff t1 t2






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
