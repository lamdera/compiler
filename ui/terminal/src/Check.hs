{-# LANGUAGE OverloadedStrings #-}
module Check
  ( run
  )
  where


import Prelude hiding (init)
import Control.Monad.Trans (liftIO)
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
import qualified Reporting.Progress.Terminal as Terminal

-- This file itself

import qualified File.IO as IO
import qualified Data.List as List
import Elm
import Control.Concurrent (threadDelay)



run :: () -> () -> IO ()
run () () =
  do  reporter <- Terminal.create

      currentDir <- Dir.getCurrentDirectory
      -- liftIO $ putStrLn $ show currentDir

      -- @TODO make sure we're in the root directory
      -- See if we can re-use a check for elm.json and throw Elm's not a project error?
      hashesExist <- Dir.doesFileExist ".lamdera-hashes"
      Task.run reporter $ do
        if hashesExist then do
          pure ()
        else
          compileCheck

        (prodVersion, productionTypes) <- fetchProductionTypes

        let nextVersion = (prodVersion + 1)

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

              comparison =
                zipWith3 (\label local prod -> (label,local,prod)) lamderaTypes localTypes productionTypes
                & filter (\(label, local, prod) -> local /= prod)

          let nextMigrationPath = "src/Evergreen/Migrate/V" ++ (show nextVersion) ++ ".elm"

          migrationExists <- liftIO $ Dir.doesFileExist $ nextMigrationPath

          if migrationExists then do

            liftIO $ putStrLn "Compiling Evergreen migrations...\n"

            migrationCheck nextVersion

            liftIO $ threadDelay 50000 -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?

            liftIO $ putStrLn $ "\nIt appears you're all set to deploy version " ++ (show nextVersion) ++ "."


          else do
            liftIO $ putStrLn $ "The following types have changed since v" ++ show prodVersion ++ " and require migrations:\n"

            comparison
              & fmap (\(label, local, prod) -> "- " ++ label)
              & List.intersperse "\n"
              & List.concat
              & putStrLn
              & liftIO

            liftIO $ putStrLn $ "\nI've created a placeholder migration file in " ++ nextMigrationPath

        else
          liftIO $ putStrLn "Types are the same!"



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



compileCheck =
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
