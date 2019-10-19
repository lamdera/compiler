{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Check (run) where


import Prelude hiding (init)
import Control.Monad.Except (catchError, liftIO)
import qualified Data.Map as Map
import qualified System.Directory as Dir

import qualified Deps.Cache as Cache
import qualified Deps.Explorer as Explorer
import qualified Deps.Solver as Solver
import qualified Elm.Compiler.Version as Compiler
import qualified Elm.Package as Pkg
import qualified Elm.Project.Constraint as Con
import qualified Elm.Project.Json as Project
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Init as E
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal

-- HTTP...


import Prelude hiding (zip)
import qualified Codec.Archive.Zip as Zip
import Control.Monad (void)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multi
import qualified Network.HTTP.Types as Http
import qualified System.Directory as Dir
import System.FilePath ((</>), splitFileName)
import qualified System.Environment as Env

import Elm.Package (Name(Name), Version)
import qualified Elm.Package as Pkg
import qualified Json.Decode as D

import qualified Reporting.Doc as D
import qualified Reporting.Exit.Http as E
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Reporting.Task.Http as Http
import qualified Stuff.Paths as Path


-- Compilation

import qualified Elm.Project as Project
import qualified Generate.Output as Output
import qualified Reporting.Task as Task
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Progress.Terminal as Terminal


-- This file itself

import qualified File.IO as IO
import qualified Data.List as List
import Elm
import Control.Concurrent (threadDelay)
import NeatInterpolation
import Algorithms.NaturalSort
import Text.Read
import Data.Maybe
import qualified Data.Text.Encoding as Text
import qualified Data.Char as Char
import Data.Text.Internal.Search


run :: () -> () -> IO ()
run () () =
  do  reporter <- Terminal.create

      -- @TODO do we need to check we're in the Elm root?
      hashesExist <- Dir.doesFileExist ".lamdera-hashes"

      Task.run reporter $ do
        liftIO $ putStrLn "Checking project compiles..."

        checkUserProjectCompiles

        (prodVersion, productionTypes) <-
            fetchProductionTypes `catchError` (\err -> pure (0, []))

        let nextVersion = (prodVersion + 1)

        if prodVersion == 0 then
          Task.report Progress.LamderaCannotCheckRemote

        else if nextVersion == 1 then
          -- This is the first version, we don't need any migration checking.
          allSet 1 nextVersion

        else do
          localTypes <- fetchLocalTypes

          if productionTypes /= localTypes then do

            let lamderaTypes =
                  [ "FrontendModel"
                  , "BackendModel"
                  , "FrontendMsg"
                  , "ToBackend"
                  , "BackendMsg"
                  , "ToFrontend"
                  ]

                typeCompares = zipWith3 (\label local prod -> (label,local,prod)) lamderaTypes localTypes productionTypes

                comparison =
                  typeCompares & filter (\(label, local, prod) -> local /= prod)

                formattedChangedTypes =
                  comparison
                    & fmap (\(label, local, prod) -> D.indent 4 (D.dullyellow (D.fromString label)))

                nextMigrationPath = "src/Evergreen/Migrate/V" ++ (show nextVersion) ++ ".elm"

            migrationExists <- liftIO $ Dir.doesFileExist $ nextMigrationPath

            if migrationExists then do

              -- @TODO check migrations match changes. I.e. user might have changed one thing, we generated
              -- a migration, and then later changed another thing. Basically grep for `toBackend old = Unchanged` for each label?
              -- so we don't acidentally run a migration saying nothing changed when it's not type safe? is that even possible or will compiler catch incorrect "unchanged" declarations?

              migrationSource <- liftIO $ Text.decodeUtf8 <$> IO.readUtf8 nextMigrationPath

              if textContains "Unimplemented" migrationSource then
                Task.throw $ Exit.Lamdera
                  $ Help.report "UNIMPLEMENTED MIGRATION" (Just nextMigrationPath)
                    ("The following types have changed since v" ++ show prodVersion ++ " and require migrations:")
                    (formattedChangedTypes ++
                      [ D.reflow $ "The migration file at " ++ nextMigrationPath ++ " has migrations that still haven't been implemented."
                      , D.reflow "See <https://lamdera.com/evergreen-migrations> more info."
                      ]
                    )
              else do
                migrationCheck nextVersion

                -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?
                liftIO $ threadDelay 50000 -- 50 milliseconds

                allSet 2 nextVersion


            else do
              lastTypeChangeVersion <- liftIO getLastTypeChangeVersion

              let m = defaultMigrationFile lastTypeChangeVersion nextVersion typeCompares

              liftIO $ IO.writeUtf8 nextMigrationPath $ Text.encodeUtf8 m

              Task.throw $ Exit.Lamdera
                $ Help.report "UNIMPLEMENTED MIGRATION" (Just nextMigrationPath)
                  ("The following types have changed since v" ++ show prodVersion ++ " and require migrations:")
                  (formattedChangedTypes ++
                    [ D.reflow $ "I've generated a placeholder migration file in " ++ nextMigrationPath ++ " to help you get started."
                    , D.reflow "See <https://lamdera.com/evergreen-migrations> more info."
                    ]
                  )


              -- liftIO $ putStrLn $ T.unpack m

              liftIO $ putStrLn $ "\nI've created a placeholder migration file in " ++ nextMigrationPath

          else
            -- Types are the same.
            -- @TODO If we have a version with no type changes do we still write migrations?
            -- If not, what happens when we get to the next version and we can't refer to the types from the previous one?
            allSet 3 nextVersion


getLastTypeChangeVersion = do

  migrations <- Dir.listDirectory "src/Evergreen/Migrate"
  if migrations == [] then
    pure 1
  else do
    migrations
      & List.sortBy Algorithms.NaturalSort.compare
      & List.last
      & drop 1
      & takeWhile (\i -> i /= '.')
      & readMaybe
      & fromMaybe 1 -- If there are no migrations or it failed, it must be version 1?
      & pure


allSet identifier nextVersion =
  liftIO $ putStrLn $ "\nIt appears you're all set to deploy version " ++ (show nextVersion) ++ ". (" ++ show identifier ++ ")"


fetchProductionTypes :: Task.Task (Int, [String])
fetchProductionTypes =
  let
    endpoint =
      "http://localhost:3030/_i"

    headers =
      [ ( Http.hUserAgent, "lamdera-cli" )
      , ( Http.hAccept, "application/json" )
      ]

    decoder =
      D.at ["h"] (D.list D.string)
       & D.andThen
           (\hashes ->
             D.at ["v"] D.int
              & D.andThen (\version ->
                D.succeed (version, hashes)
              )
           )
  in
    Http.run $ Http.anything endpoint $ \request manager ->
      do  response <- Client.httpLbs (request { Client.requestHeaders = headers }) manager
          let bytes = LBS.toStrict (Client.responseBody response)
          case D.parse "lamdera-hashes" id decoder bytes of
            Right value ->
              return $ Right value

            Left jsonProblem ->
              -- @TODO fix this
              return $ Left $ E.BadJson "github.json" jsonProblem


fetchLocalTypes :: Task.Task [String]
fetchLocalTypes = do
  -- This could fail normally but we're using this function after
  -- we've already checked it exists
  hashString <- liftIO $ IO.readUtf8 ".lamdera-hashes"

  let
    decoder =
      (D.list D.string)

  case D.parse "lamdera-hashes" id decoder hashString of
    Right value ->
      return $ value

    Left jsonProblem ->
      -- @TODO fix this
      return $ error $ show jsonProblem --Left $ E.BadJson "github.json" jsonProblem


checkUserProjectCompiles =
  -- Dir.withCurrentDirectory ("/Users/mario/dev/projects/lamdera/test/v2") $
    -- do  reporter <- Terminal.create
        -- Task.run reporter $
          do  summary <- Project.getRoot
              let jsOutput = Just (Output.Html Nothing "/dev/null")
              Project.compile Output.Dev Output.Client jsOutput Nothing summary [ "src" </> "Frontend.elm" ]
              Project.compile Output.Dev Output.Client jsOutput Nothing summary [ "src" </> "Backend.elm" ]

        -- _ <- BS.readFile tempFileName
        -- seq (BS.length result) (Dir.removeFile tempFileName)
        -- return ()


migrationCheck version =
  -- Dir.withCurrentDirectory ("/Users/mario/dev/projects/lamdera/test/v2") $
    -- do  reporter <- Terminal.create
        -- Task.run reporter $
          do  summary <- Project.getRoot
              let jsOutput = Just (Output.Html Nothing "/dev/null")
              Project.compile Output.Dev Output.Client jsOutput Nothing summary [ "src" </> "Evergreen" </> "Migrate" </> "V" ++ show version ++ ".elm" ]


        -- _ <- BS.readFile tempFileName
        -- seq (BS.length result) (Dir.removeFile tempFileName)
        -- return ()



-- exampleInitRun :: () -> () -> IO ()
-- exampleInitRun () () =
--   do  reporter <- Terminal.create
--       exists <- Dir.doesFileExist ".lamdera-hash"
--       Task.run reporter $
--         if exists then
--           Task.throw (Exit.Init E.AlreadyStarted)
--         else
--           do  approved <- Task.getApproval question
--               if approved
--                 then
--                   do  init
--                       liftIO $ putStrLn "Okay, I created it. Now read that link!"
--                 else
--                   liftIO $ putStrLn "Okay, I did not make any changes!"


defaultMigrationFile :: Int -> Int -> [(String, String, String)] -> Text
defaultMigrationFile oldVersion newVersion typeCompares = do
  let old = T.pack $ show oldVersion
      new = T.pack $ show newVersion

      typeCompareMigration :: (String, String, String) -> Text
      typeCompareMigration (typename, oldhash, newhash) = do
        let implementation =
              if oldhash == newhash then
                "Unchanged"
              else
                "Unimplemented"
            msgType = msgForType typename

            typenameCamel =
              case typename of
                first:rest -> T.pack $ [Char.toLower first] ++ rest

            typenameT = T.pack typename

        [text|
          $typenameCamel : Old.$typenameT -> Migration New.$typenameT New.$msgType
          $typenameCamel old =
              $implementation
        |]

      msgForType t =
        case t of
          "BackendModel" ->
            "BackendMsg"
          "FrontendModel" ->
            "FrontendMsg"
          "FrontendMsg" ->
            "FrontendMsg"
          "ToBackend" ->
            "BackendMsg"
          "BackendMsg" ->
            "BackendMsg"
          "ToFrontend" ->
            "FrontendMsg"


  let header = [text|

    module Evergreen.Migrate.V$new exposing (..)

    import Evergreen.Migrate exposing (..)
    import Evergreen.Type.V$old as Old
    import Evergreen.Type.V$new as New


  |]

  typeCompares
    & fmap typeCompareMigration
    & (++) [header]
    & T.intercalate "\n\n"



textContains :: Text -> Text -> Bool
textContains needle haystack = indices needle haystack /= []
