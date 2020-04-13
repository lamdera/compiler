{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Check where


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
import qualified Stuff.Paths as Paths

-- Compilation
import qualified Elm.Project as Project
import qualified Generate.Output as Output
import qualified Reporting.Task as Task
import qualified Reporting.Exit.Help as Help
import qualified Reporting.Progress.Terminal as Terminal
import qualified Reporting.Report as Report

-- This file itself
import Lamdera
import qualified Lamdera.Evergreen
import Lamdera.Generated (VersionInfo(..), createLamderaGenerated, vinfoVersion, getLastLocalTypeChangeVersion)
import qualified File.IO as IO
import qualified Data.List as List
import qualified Data.List.Safe as SafeList
import NeatInterpolation
import Text.Read
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as Text
import Data.Text.Internal.Search (indices)
import System.Process (readProcess, callCommand)
import Control.Monad (unless, filterM)
import qualified System.IO as IO
import qualified Elm.Project.Summary as Summary
import qualified Text.Read




run :: () -> () -> IO ()
run () () = do
  debug_ "Starting check..."

  reporter <- Terminal.create

  appNameEnvM <- liftIO $ Env.lookupEnv "LAMDERA_APP_NAME"
  hoistRebuild <- liftIO $ Env.lookupEnv "HOIST_REBUILD"
  forceVersionM <- liftIO $ Env.lookupEnv "VERSION"
  forceNotProd <- liftIO $ Env.lookupEnv "NOTPROD"
  inDebug <- Lamdera.isDebug

  let inProduction = (appNameEnvM /= Nothing && forceNotProd == Nothing) -- @TODO better isProd check...
      isHoistRebuild = (hoistRebuild /= Nothing)
      forceVersion = fromMaybe (-1) $
        case forceVersionM of
          Just fv -> Text.Read.readMaybe fv
          Nothing -> Nothing

  debug $ "production:" ++ show inProduction
  debug $ "hoist rebuild:" ++ show isHoistRebuild
  debug $ "force version:" ++ show forceVersion



  Task.run reporter $ do

    summary <- Project.getRoot
    let root = Summary._root summary

    temporaryCheckOldTypesNeedingMigration inProduction root

    liftIO $ putStrLn "───> Checking project compiles..."
    checkUserProjectCompiles summary root

    liftIO $ putStrLn "───> Checking Evergreen migrations..."

    lamderaRemotes <- liftIO getLamderaRemotes
    onlyWhen (lamderaRemotes == [] && appNameEnvM == Nothing) lamderaThrowUnknownApp

    -- Prior `onlyWhen` guards against situation where no name is determinable
    let appName = certainAppName lamderaRemotes appNameEnvM

    debug $ "app name:" ++ show appName

    localTypes <- fetchLocalTypes root

    (prodVersion, productionTypes) <-
      if isHoistRebuild
        then do
          if (forceVersion == (-1))
            then
              genericExit "ERROR: Hoist rebuild got -1 for version, check your usage."

            else do
              liftIO $ putStrLn $ "❗️Gen with forced version: " <> show forceVersion
              pure (forceVersion, localTypes)

        else do
          (pv, pt) <- fetchProductionInfo appName inDebug `catchError` (\err -> pure ((-1), []))
          if (pv /= (-1))
            then
              -- Everything is as it should be
              pure (pv, pt)
            else do
              if (inProduction)
                then
                  genericExit "FATAL: application info could not be obtained. Please report this to support."

                else do
                  -- See commented out code at bottom re prior "assume version when offline"
                  Task.throw $ Exit.Lamdera
                    $ Help.report "ERROR" Nothing
                      ("I normally check for production info here but I wasn't able to.")
                      [ D.reflow $ "Please check your connection and try again, or contact support."]

    let
      localTypesChangedFromProduction =
        productionTypes /= localTypes

      nextVersion =
        if isHoistRebuild then
          -- No version bump, the stated version will be the prod version
          prodVersion
        else
          (prodVersion + 1)

    nextVersionInfo <-
      if prodVersion == 0 then
        pure $ WithoutMigrations 1
      else
        if isHoistRebuild
          then do
            -- Hoist case
            hoistMigrationExists <- liftIO $ Dir.doesFileExist $ "src/Evergreen/Migrate/V" <> show prodVersion <> ".elm"
            if hoistMigrationExists
              then
                pure $ WithMigrations prodVersion
              else
                pure $ WithoutMigrations prodVersion

          else
            -- Normal case
            if localTypesChangedFromProduction then
              pure $ WithMigrations nextVersion
            else
              pure $ WithoutMigrations nextVersion

    debug $ "Continuing with (prodV,nextV,nextVInfo) " ++ show (prodVersion, nextVersion, nextVersionInfo)

    -- onlyWhen isHoistRebuild $ do
    --   approveHoist <- Task.getApproval $
    --     D.stack
    --       [ D.fillSep [ D.yellow "WARNING:","Confirm","hoist!" ]
    --       , D.reflow $ "Proceed with hoist as v" <> show nextVersion <> "? [Y/n]: "
    --       ]
    --   onlyWhen (not approveHoist) $ genericExit "Quitting hoist"


    if nextVersion == 1
      then do
        -- Always snapshot types for the first version, we need a starting point
        _ <- liftIO $ snapshotCurrentTypesTo summary root nextVersion

        -- This is the first version, we don't need any migration checking.
        onlyWhen (not inProduction) $ committedCheck root nextVersionInfo

        writeLamderaGenerated root inProduction nextVersionInfo
        buildProductionJsFiles root inProduction nextVersionInfo

        pDocLn $ D.green (D.reflow $ "It appears you're all set to deploy the first version of '" <> T.unpack appName <> "'!")
        liftIO $ putStrLn ""

      else do
        writeLamderaGenerated root inProduction nextVersionInfo

        let
          nextMigrationPathBare = ("src/Evergreen/Migrate/V") <> (show nextVersion) <> ".elm"
          nextMigrationPath = root </> nextMigrationPathBare
          -- lastTypesPath = (root </> "src/Evergreen/Type/V") <> (show prodVersion) <> ".elm"

        migrationExists <- liftIO $ Dir.doesFileExist $ nextMigrationPath

        if localTypesChangedFromProduction
          then do
            debug $ "Local and production types differ"

            _ <- liftIO $ snapshotCurrentTypesTo summary root nextVersion

            let
              typeCompares = zipWith3
                (\label local prod -> (label,local,prod))
                [ "FrontendModel", "BackendModel", "FrontendMsg", "ToBackend", "BackendMsg", "ToFrontend" ]
                localTypes
                productionTypes

              changedTypes =
                typeCompares & filter (\(label, local, prod) -> local /= prod)

              formattedChangedTypes =
                changedTypes
                  & fmap (\(label, local, prod) -> D.indent 4 (D.dullyellow (D.fromString label)))
                  & D.vcat

            if migrationExists
              then do
                debug $ "Migration file already exists"

                -- @TODO check migrations match changes. I.e. user might have changed one thing, we generated
                -- a migration, and then later changed another thing. Basically grep for `toBackend old = Unchanged` for each label?
                -- so we don't acidentally run a migration saying nothing changed when it's not type safe?
                -- is that even possible or will compiler catch incorrect "unchanged" declarations?

                debug $ "Reading migration source..."
                migrationSource <- liftIO $ Text.decodeUtf8 <$> IO.readUtf8 nextMigrationPath

                if textContains "Unimplemented" migrationSource
                  then do
                    lamderaThrowUnimplementedMigration nextMigrationPath formattedChangedTypes prodVersion nextMigrationPathBare

                  else do
                    migrationCheck root nextVersion
                    onlyWhen (not inProduction) $ committedCheck root nextVersionInfo

                    pDocLn $ D.green $ D.reflow $ "\nIt appears you're all set to deploy v" <> (show nextVersion) <> " of '" <> T.unpack appName <> "'."

                    mapM pDocLn $
                      ([ D.reflow "Evergreen migrations will be applied to the following types:"
                       , formattedChangedTypes
                       , D.reflow "See <https://dashboard.lamdera.app/docs/evergreen> for more info." ]
                      )

                    possiblyShowExternalTypeWarnings

                    buildProductionJsFiles root inProduction nextVersionInfo

              else do
                debug $ "Migration does not exist"

                _ <- liftIO $ mkdir $ root </> "src/Evergreen/Migrate"

                lastLocalTypeChangeVersion <- liftIO $ Lamdera.Generated.getLastLocalTypeChangeVersion root

                let defaultMigrations = defaultMigrationFile lastLocalTypeChangeVersion nextVersion typeCompares

                liftIO $ writeUtf8 nextMigrationPath defaultMigrations

                possiblyShowExternalTypeWarnings

                Task.throw $ Exit.Lamdera
                  $ Help.report "UNIMPLEMENTED MIGRATION" (Just nextMigrationPathBare)
                    ("The following types have changed since v" <> show prodVersion <> " and require migrations:")
                    [ formattedChangedTypes
                    , D.reflow $ "I've generated a placeholder migration file to help you get started:"
                    , D.reflow $ nextMigrationPath
                    , D.reflow "See <https://dashboard.lamdera.app/docs/evergreen> for more info."
                    ]


          else do
            -- Types are the same.
            debug "Local and production types are identical"

            -- @TODO probably need to hint to the user here if types have become
            -- the same but snapshots / migrations exist? They'll need to be removed!
            -- onlyWhen (not inProduction) $ committedCheck root nextVersionInfo

            onlyWhen (migrationExists && not isHoistRebuild) $
              Task.throw $ Exit.Lamdera
                $ Help.report "UNEXPECTED MIGRATION" (Just nextMigrationPathBare)
                  ("There appears to be a migration file when I wasn't expecting one.")
                  [ D.reflow $ "It appears local types have not changed compared to production, however I'm seeing a migration at " <> nextMigrationPathBare <> "."
                  , D.reflow "Perhaps it needs to be removed?"
                  , D.reflow "See <https://dashboard.lamdera.app/docs/evergreen> for more info."
                  ]

            pDocLn $ D.green $ D.reflow $ "\nIt appears you're all set to deploy v" <> (show nextVersion) <> " of '" <> T.unpack appName <> "'."
            pDocLn $ D.reflow $ "\nThere are no Evergreen type changes for this version."

            possiblyShowExternalTypeWarnings

            buildProductionJsFiles root inProduction nextVersionInfo





buildProductionJsFiles :: FilePath -> Bool -> VersionInfo -> Task.Task ()
buildProductionJsFiles root inProduction versionInfo = do
  let version = vinfoVersion versionInfo

  onlyWhen inProduction $ do
    summary <- Project.getRoot

    debug $ "Compiling JS for production v" <> show (vinfoVersion versionInfo)

    onlyWhen (version /= 1 && versionInfo == WithMigrations version) $ do -- Version 1 has no migrations for rewrite

      let migrationPath = (root </> "src/Evergreen/Migrate/V") <> show version <> ".elm"
      debug $ "Rewriting " <> migrationPath <> " type import to point to Types"

      -- Temporarily point migration to current types in order to type-check
      -- osReplace ("s/import Evergreen.Type.V" <> show version <> "/import Types/g") migrationPath
      liftIO $ replaceInFile ("import Evergreen.V" <> tshow version <> ".Types") "import Types" migrationPath

    -- debug $ "Injecting BACKENDINJECTION " <> (root </> "elm-backend-overrides.js")
    -- liftIO $ Env.setEnv "BACKENDINJECTION" (root </> "elm-backend-overrides.js")
    -- _ <- liftIO $ readProcess "touch" [root </> "src" </> "Types.elm"] ""

    -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?
    -- liftIO $ sleep 50 -- 50 milliseconds

    -- liftIO $ callCommand $ "cp ~/lamdera/runtime/src/LamderaHelpers.elm " ++ root ++ "/src"
    -- liftIO $ callCommand $ "cp ~/lamdera/runtime/src/LFR.elm " ++ root ++ "/src"
    -- liftIO $ callCommand $ "cp ~/lamdera/runtime/src/LBR.elm " ++ root ++ "/src"

    Project.compile
      Output.Prod
      Output.Client
      (Just (Output.JavaScript Nothing "backend-app.js"))
      Nothing
      summary
      [ "src" </> "LBR.elm" ]

    -- debug $ "Unsetting BACKENDINJECTION"
    -- liftIO $ Env.unsetEnv "BACKENDINJECTION"

    -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?
    liftIO $ sleep 50 -- 50 milliseconds

    Project.compile
      Output.Prod
      Output.Client
      (Just (Output.JavaScript Nothing "frontend-app.js"))
      Nothing
      summary
      [ "src" </> "LFR.elm" ]


    -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?
    liftIO $ sleep 50 -- 50 milliseconds

    onlyWhen (version /= 1 && versionInfo == WithMigrations version) $ do -- Version 1 has no migrations for rewrite

      let migrationPath = (root </> "src/Evergreen/Migrate/V") <> show version <> ".elm"
      debug $ "Rewriting " <> migrationPath <> " type import back to VX"

      -- Temporarily point migration to current types in order to type-check
      liftIO $ replaceInFile "import Types" ("import Evergreen.V" <> tshow version <> ".Types") migrationPath

    -- NOTE: Could do this, but assuming it'll be good to have the original evidence of
    -- state for situation where things go wrong in production and you can poke around
    -- Restore the type back to what it was
    -- osReplace ("s/import Types/import Evergreen.Type.V" <> show version <> "/g") migrationPath



-- @TODO drop this entire type down to Task then we don't need the wrapping of IO
snapshotCurrentTypesTo :: Summary.Summary -> FilePath -> Int -> IO String
snapshotCurrentTypesTo summary root version = do
  -- Snapshot the current types, and rename the module for the snapshot

  debug_ "Executing in type snapshot mode..."

  Env.setEnv "LTYPESNAPSHOT" (show version)

  -- Elm's caches will mean Types.elm won't get recompiled without 'changes', so we touch it
  _ <- liftIO $ Lamdera.touch $ root </> "src" </> "Types.elm"

  -- Invoke compiler in snapshot mode for src/Types.elm
  Dir.withCurrentDirectory root $
    do  reporter <- Terminal.create
        Task.run reporter $
          do  --summary <- Project.getRoot
              Project.compile
                Output.Prod
                Output.Client
                (Just (Output.JavaScript Nothing "/dev/null"))
                Nothing
                summary
                [ "src" </> "Types.elm" ]

  Env.unsetEnv "LTYPESNAPSHOT"

  pure ""


getLamderaRemotes = do
  gitRemotes <- readProcess "git" ["remote", "-v"] ""
  gitRemotes
    & T.pack
    & T.splitOn "\n"
    & filter (textContains "apps.lamdera.com")
    & filter (textContains "(push)")
    & pure


lamderaThrowUnknownApp =
  Task.throw $ Exit.Lamdera
    $ Help.report "UNKNOWN APP" (Just "git remote -v")
      ("I cannot figure out which Lamdera app this repository belongs to!")
      ([ D.reflow "I normally look for a git remote called 'lamdera' but did not find one."
       , D.reflow "Did you maybe forget to add the lamdera remote for your app as listed on the Dashboard?"
       , D.reflow "See <https://dashboard.lamdera.app/docs/deploying> for more info."
       ]
      )


lamderaThrowUnimplementedMigration nextMigrationPath formattedChangedTypes prodVersion nextMigrationPathBare = do
  Task.throw $ Exit.Lamdera
    $ Help.report "UNIMPLEMENTED MIGRATION" (Just nextMigrationPathBare)
      ("The following types have changed since v" <> show prodVersion <> " and require migrations:")
      [ formattedChangedTypes
      , D.reflow $ "The migration file has migrations that still haven't been implemented:"
      , D.reflow $ nextMigrationPath
      , D.fillSep ["See",D.cyan ("<https://dashboard.lamdera.app/docs/evergreen>"),"for more info."]
      ]



certainAppName :: [Text] -> Maybe String -> Text
certainAppName lamderaRemotes appNameEnvM =
  case appNameEnvM of
    Just appNameEnv -> T.pack appNameEnv
    _ ->
      List.head lamderaRemotes
        & T.splitOn ":"
        & (\l ->
            case l of
              f:second:_ -> second
          )
        & T.splitOn "."
        & List.head



fetchProductionInfo :: Text -> Bool -> Task.Task (Int, [String])
fetchProductionInfo appName inDebug =
  let
    endpoint =
      if textContains "-local" appName && inDebug
        then
          "https://" <> T.unpack appName <> ".lamdera.test/_i"

        else
          "https://" <> T.unpack appName <> ".lamdera.app/_i"

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
      do  debug $ "HTTP fetching " <> endpoint
          response <- Client.httpLbs (request { Client.requestHeaders = headers }) manager
          let bytes = LBS.toStrict (Client.responseBody response)
          case D.parse "lamdera-info" id decoder bytes of
            Right value ->
              return $ Right value

            Left jsonProblem ->
              -- @TODO fix this
              return $ Left $ E.BadJson "github.json" jsonProblem


fetchLocalTypes :: FilePath -> Task.Task [String]
fetchLocalTypes root = do

  debug $ "Reading local types..."

  -- This could fail normally but we're using this function after
  -- we've already checked it exists
  hashString <- liftIO $ IO.readUtf8 (lamderaHashesPath root)

  let
    decoder =
      (D.list D.string)

  case D.parse "lamdera-hashes" id decoder hashString of
    Right value ->
      return $ value

    Left jsonProblem ->
      -- @TODO fix this
      return $ error $ show jsonProblem --Left $ E.BadJson "github.json" jsonProblem


checkUserProjectCompiles :: Summary.Summary -> FilePath -> Task.Task ()
checkUserProjectCompiles summary root = do
  -- Why do we sometimes not get .lamdera-hashes generated?
  --
  -- $ DEBUG=1 lamdera check
  --   Checking project compiles...
  --   Success!
  --   Success!
  --   DEBUG: Continuing with (prodV,nextV) (1,2)
  --   DEBUG: Reading local types...
  --   lamdera: /Users/mario/dev/projects/lamdera/test/v2/lamdera-stuff/.lamdera-hashes: openFile: does not exist (No such file or directory)
  --
  -- Because of caches? But how? First non-cached build should always generate it?
  --
  -- touch src/Types.elm fixed the problem, so something to look into here... maybe we always touch it for now?
  --
  -- But need to have a better error message for this probably... watch out for it in future.

  _ <- liftIO $ Lamdera.touch $ root </> "src" </> "Types.elm"

  let jsOutput = Just (Output.Html Nothing "/dev/null")
  Project.compile Output.Prod Output.Client jsOutput Nothing summary [ "src" </> "Frontend.elm" ]
  Project.compile Output.Prod Output.Client jsOutput Nothing summary [ "src" </> "Backend.elm" ]

  -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?
  liftIO $ sleep 50 -- 50 milliseconds


migrationCheck :: FilePath -> Int -> Task.Task ()
migrationCheck root version =
  -- Dir.withCurrentDirectory ("/Users/mario/dev/projects/lamdera/test/v2") $
    -- do  reporter <- Terminal.create
        -- Task.run reporter $
          do  summary <- Project.getRoot

              -- Temporarily point migration to current types in order to type-check
              debug "Type-checking Evergreen migrations..."

              let migrationPath = (root </> "src/Evergreen/Migrate/V") <> show version <> ".elm"

              debug "Replacing type reference"
              -- osReplace ("s/import Evergreen.Type.V" <> show version <> "/import Types/g") migrationPath
              liftIO $ replaceInFile ("import Evergreen.V" <> tshow version <> ".Types") "import Types" migrationPath

              -- @TODO
              -- This is now cleaner for local checks, but we still need a full e2e check in production before we deploy!
              -- frontendRuntimeExists <- liftIO $ Dir.doesFileExist $ root </> "src/LFR.elm"
              -- backendRuntimeExists <- liftIO $ Dir.doesFileExist $ root </> "src/LBR.elm"

              -- debug "Creating build scaffold files"
              -- liftIO $ unless frontendRuntimeExists $ writeUtf8 frontendRuntimeLocalContent $ root </> "src/LFR.elm"
              -- liftIO $ unless backendRuntimeExists $ writeUtf8 backendRuntimeLocalContent $ root </> "src/LBR.elm"

              liftIO $ mkdir $ root </> "lamdera-stuff/alpha"

              let lamderaCheckBothPath = "lamdera-stuff/alpha/LamderaCheckBoth.elm"
              liftIO $ writeUtf8 (root </> lamderaCheckBothPath) (lamderaCheckBothFileContents version)
              let jsOutput = Just (Output.Html Nothing "/dev/null")
              Project.compile Output.Dev Output.Client jsOutput Nothing summary [ lamderaCheckBothPath ]

              -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?
              liftIO $ sleep 50 -- 50 milliseconds

              debug "Cleaning up build scaffold"
              -- Remove our temporarily checker file
              liftIO $ remove $ root </> lamderaCheckBothPath

              -- Restore the type back to what it was
              liftIO $ replaceInFile "import Types" ("import Evergreen.V" <> tshow version <> ".Types") migrationPath

              -- Cleanup dummy runtime files if we added them
              -- liftIO $ unless frontendRuntimeExists $ callCommand $ "rm " <> root </> "src/LFR.elm"
              -- liftIO $ unless backendRuntimeExists $ callCommand $ "rm " <> root </> "src/LBR.elm"


committedCheck :: FilePath -> VersionInfo -> Task.Task ()
committedCheck root versionInfo = do

  let version = vinfoVersion versionInfo

  debug $ "Commit-checking migration and types files"

  let migrationPath = (root </> "src/Evergreen/Migrate/V") <> show version <> ".elm"
  migrations <- liftIO $ gitStatus migrationPath

  let typesPath = (root </> "src/Evergreen/V") <> show version <> "/*.elm"
  types <- liftIO $ gitStatus typesPath

  let missingPaths =
        [ if (migrations /= Committed && version > 1 && versionInfo == WithMigrations version) then
            -- Bare non-root path intentional otherwise UI is pretty ugly...
            Just $ "src/Evergreen/Migrate/V" <> show version <> ".elm"
          else
            Nothing
        , if types /= Committed then
            -- Bare non-root path intentional otherwise UI is pretty ugly...
            Just $ "src/Evergreen/V" <> show version <> "/*"
          else
            Nothing
        ]
        & justs

  -- On first version, we have no migrations
  onlyWhen (missingPaths /= []) $ do

    -- pDocLn $ D.green (D.reflow $ "It appears you're all set to deploy the first version of ''!")
    Task.report $
      Progress.LamderaProgress $
        Help.reportToDoc $
        Help.report "UNCOMMITTED FILES" (Just "src/Evergreen/")
          ("I need type and migration files to be comitted otherwise I cannot deploy!")
          ([ D.reflow "Here is a shortcut:"
           , D.dullyellow (D.reflow $ "git add " <> (List.intercalate " " missingPaths))
           , D.dullyellow (D.reflow $ "git commit -m \"Preparing for v" <> show version <> "\"")
           ]
          )

    addToGitApproved <- Task.getApproval $
      D.stack
        [ D.reflow $ "Shall I `git add` for you? [Y/n]: "
        ]

    if addToGitApproved
      then do
        liftIO $ mapM (\path -> callCommand $ "git add " <> path) missingPaths
        commitApproved <- Task.getApproval $
          D.stack
            [ D.reflow $ "Shall I `git commit` for you? [Y/n]: "
            ]

        if commitApproved
          then
            liftIO $ callCommand $ "git commit -m \"Preparing for v" <> show version <> "\""

          else
            genericExit "Okay, I did not commit it."

      else
        genericExit "Okay, I did not add it."


defaultMigrationFile :: Int -> Int -> [(String, String, String)] -> Text
defaultMigrationFile oldVersion newVersion typeCompares = do
  let old = T.pack $ show oldVersion
      new = T.pack $ show newVersion

      typeCompareMigration :: (String, String, String) -> Text
      typeCompareMigration (typename, oldhash, newhash) = do
        let implementation =
              if oldhash == newhash then
                unchangedForType typename
              else
                "Unimplemented"
                -- @TODO when working on more intelligent auto-generations...
                -- if typename == "BackendModel" || typename == "FrontendModel" then
                --   [text|
                --     ModelMigrated
                --         ( Unimplemented
                --         , Cmd.none
                --         )
                --   |]
                -- else
                --   "Unimplemented"

            msgType = msgForType typename

            typenameCamel = lowerFirstLetter typename

            typenameT = T.pack typename

            migrationType = migrationWrapperForType typename

        [text|
          $typenameCamel : Old.$typenameT -> $migrationType New.$typenameT New.$msgType
          $typenameCamel old =
              $implementation
        |]

      migrationWrapperForType t =
        case t of
          "BackendModel" ->
            "ModelMigration"
          "FrontendModel" ->
            "ModelMigration"
          "FrontendMsg" ->
            "MsgMigration"
          "ToBackend" ->
            "MsgMigration"
          "BackendMsg" ->
            "MsgMigration"
          "ToFrontend" ->
            "MsgMigration"

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

      unchangedForType t =
        case t of
          "BackendModel" ->
            "ModelUnchanged"
          "FrontendModel" ->
            "ModelUnchanged"
          "FrontendMsg" ->
            "MsgUnchanged"
          "ToBackend" ->
            "MsgUnchanged"
          "BackendMsg" ->
            "MsgUnchanged"
          "ToFrontend" ->
            "MsgUnchanged"

  let header = [text|

    module Evergreen.Migrate.V$new exposing (..)

    import Evergreen.V$old.Types as Old
    import Evergreen.V$new.Types as New
    import Lamdera.Migrations exposing (..)


  |]

  typeCompares
    & fmap typeCompareMigration
    & (<>) [header]
    & T.intercalate "\n\n"


lamderaCheckBothFileContents :: Int -> Text
lamderaCheckBothFileContents version =
  -- Source for this is in lamdera/runtime/src/LamderaCheckBoth.elm
  let version_ = T.pack $ show version
  in
  [text|
    module LamderaCheckBoth exposing (..)

    import Backend
    import Browser exposing (Document, UrlRequest)
    import Browser.Navigation exposing (Key)
    import Evergreen.Migrate.V$version_
    import Frontend
    import Lamdera exposing (Url, SessionId, ClientId)


    checkFrontendTypes :
        { init : Url -> Key -> ( frontendModel, Cmd frontendMsg )
        , view : frontendModel -> Document frontendMsg
        , update : frontendMsg -> frontendModel -> ( frontendModel, Cmd frontendMsg )
        , updateFromBackend : toFrontend -> frontendModel -> ( frontendModel, Cmd frontendMsg )
        , subscriptions : frontendModel -> Sub frontendMsg
        , onUrlRequest : UrlRequest -> frontendMsg
        , onUrlChange : Url -> frontendMsg
        }
        -> Bool
    checkFrontendTypes app = True

    checkBackendTypes :
            { init : ( backendModel, Cmd backendMsg )
            , update : backendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
            , updateFromFrontend : SessionId -> ClientId -> toBackend -> backendModel -> ( backendModel, Cmd backendMsg )
            , subscriptions : backendModel -> Sub backendMsg
            }
        -> Bool
    checkBackendTypes app = True

    checkBoth :
        { init : Url -> Key -> ( frontendModel, Cmd frontendMsg )
        , view : frontendModel -> Document frontendMsg
        , update : frontendMsg -> frontendModel -> ( frontendModel, Cmd frontendMsg )
        , updateFromBackend : toFrontend -> frontendModel -> ( frontendModel, Cmd frontendMsg )
        , subscriptions : frontendModel -> Sub frontendMsg
        , onUrlRequest : UrlRequest -> frontendMsg
        , onUrlChange : Url -> frontendMsg
        }
        ->
            { init : ( backendModel, Cmd backendMsg )
            , update : backendMsg -> backendModel -> ( backendModel, Cmd backendMsg )
            , updateFromFrontend : SessionId -> ClientId -> toBackend -> backendModel -> ( backendModel, Cmd backendMsg )
            , subscriptions : backendModel -> Sub backendMsg
            }
        -> Bool
    checkBoth frontend backend =
      let
        checkFrontend = checkFrontendTypes frontend
        checkBackend = checkBackendTypes backend
      in
      True

    check =
        checkBoth Frontend.app Backend.app
  |]


-- Dummy files for when checking things locally. Remote has full generated file.
frontendRuntimeLocalContent :: Text
frontendRuntimeLocalContent =
  [text|
    module LFR exposing (..)

    import Frontend

    lamderaFrontendRuntime = ""
  |]


backendRuntimeLocalContent :: Text
backendRuntimeLocalContent =
  [text|
    module LBR exposing (..)

    import Backend

    lamderaBackendRuntime = ""
  |]


gitStatus :: String -> IO GitStatus
gitStatus filepath = do
  gsPorcelain <- readProcess "git" ["status", "--porcelain", filepath] ""
  -- print $ firstTwoChars gsPorcelain
  case firstTwoChars gsPorcelain of
    ('_', '_') -> do
      -- `git status` is empty, so we need to check if the file is tracked (thus clean) or non-existent
      gitFiles <- readProcess "git" ["ls-files", filepath] ""
      case gitFiles of
        "" -> pure Uninitiated
        _  -> pure Committed

    ('?', _) ->
      pure ChangesPending -- Untracked, but exists
    ('A', _) ->
      pure ChangesPending -- Added, so not yet committed
    (' ', 'M') ->
      pure ChangesPending -- Modified
    ('M', ' ') ->
      pure ChangesPending -- Added, so not yet committed
    (' ', 'D') ->
      pure Deleted -- Added, so not yet committed


    -- @TODO other statuses other than Uninitiatied
    -- https://git-scm.com/docs/git-status#_short_format
    _ -> pure UnexpectedGitStatus


data GitStatus
  = Uninitiated
  | ChangesPending
  | Committed
  | Staged
  | Deleted
  | UnexpectedGitStatus
  deriving (Show, Eq)


firstTwoChars :: String -> (Char, Char)
firstTwoChars str =
  case str of
    first:second:_ -> (first, second)
    _ -> ('_','_')


writeLamderaGenerated :: FilePath -> Bool -> VersionInfo -> Task.Task ()
writeLamderaGenerated root inProduction nextVersion =
  onlyWhen inProduction $ do
    gen <- liftIO $ Lamdera.Generated.createLamderaGenerated root nextVersion
    -- liftIO $ putStrLn $ T.unpack gen
    liftIO $ writeUtf8 (root </> "src/LamderaGenerated.elm") gen


possiblyShowExternalTypeWarnings :: Task.Task ()
possiblyShowExternalTypeWarnings = do

  root <- liftIO getProjectRoot
  warnings_ <- liftIO $ maybeReadUtf8 $ lamderaExternalWarningsPath root

  case warnings_ of
    Just warnings -> do

      debug "Printing out external type warnings"

      pDocLn $
        D.stack
          (
          [ D.red $ D.reflow $ "WARNING: note the following Alpha limitation!"
          , D.reflow $ "You are referencing some types outside your project:"
          , D.vcat [ D.fromText warnings ]
          , D.red $ D.reflow $ "Package upgrades that change these types won't get covered by Evergreen migrations currently!"
          , D.reflow "See <https://dashboard.lamdera.app/docs/evergreen> for more info."
          ]
          )

    Nothing -> do
      debug "No external type warnings found."

      pure ()


maybeReadUtf8 :: FilePath -> IO (Maybe Text)
maybeReadUtf8 filePath =
  do  exists_ <- Dir.doesFileExist filePath
      if exists_
        then
          Just <$> Text.decodeUtf8 <$> IO.readUtf8 filePath
          -- pure $ Just $
        else
          pure Nothing


genericExit :: String -> Task.Task a
genericExit str =
  Task.throw $ Exit.Lamdera
    $ Help.report "ERROR" Nothing
      (str)
      []


temporaryCheckOldTypesNeedingMigration :: Bool -> FilePath -> Task.Task ()
temporaryCheckOldTypesNeedingMigration inProduction root = do

  exists_ <- liftIO $ Dir.doesDirectoryExist $ root </> "src" </> "Evergreen" </> "Type"

  onlyWhen exists_ $ do

    if inProduction
      then
        Task.throw $ Exit.Lamdera
          $ Help.report "Evergreen API changes" (Just "src/Evergreen/")
              ("The Evergreen API changed in alpha5. It appears you've not migrated yet!")
              ([ D.dullyellow $ D.reflow "Please download the latest binary and run `lamdera check` again."
               , D.reflow $ "https://dashboard.lamdera.app/docs/download"
               , D.reflow $ "See the full release here: https://dashboard.lamdera.app/releases/alpha5"
               ]
              )
      else
        Task.report $
          Progress.LamderaProgress $
            Help.reportToDoc $
            Help.report "Evergreen API changes" (Just "src/Evergreen/")
              ("The Evergreen API changed in alpha5. It appears you've not migrated yet!")
              ([ D.reflow "The following changes were introduced:"
               , D.vcat
                   [ D.reflow $ "- Type snapshots now extract from your entire project, not just Types.elm"
                   , D.reflow $ "- Snapshots now live in src/Evergreen/V*/ folders for each version"
                   ]
               , D.reflow $ "See the full release here: https://dashboard.lamdera.app/releases/alpha5"
               , D.dullyellow $ D.reflow $ "I can help you migrate by doing the following:"
               , D.vcat
                   [ D.reflow $ "- Moving src/Evergreen/Type/V*.elm to src/Evergreen/V*/Types.elm"
                   , D.reflow $ "- Renaming all the moved module names"
                   , D.reflow $ "- Renaming all Type imports in migrations"
                   , D.reflow $ "- Removing the src/Evergreen/Type/ folder"
                   , D.reflow $ "- Staging the changes for git commit"
                   ]
               ]
              )

    migrationApproved <- Task.getApproval $
      D.stack
        [ D.reflow $ "Shall I attempt to migrate for you? [Y/n]: "
        ]

    if migrationApproved
      then do

        liftIO $ do

          let oldTypeSnapshotFolder = root </> "src/Evergreen/Type"

          typeFilepaths <- safeListDirectory $ oldTypeSnapshotFolder

          typeFilepaths
            & mapM (\filepath -> do

              case getVersion filepath of
                Just version -> do
                  let dest = (root </> "src" </> "Evergreen" </> ("V" <> show version) </> "Types.elm")

                  putStrLn $ "Moving " <> filepath <> " -> " <> "src/Evergreen/V" <> show version <> "/Types.elm"
                  copyFile (oldTypeSnapshotFolder </> filepath) dest

                  putStrLn $ "Renaming '" <> ("module Evergreen.Type.V" <> show version) <> "' to '" <> ("module Evergreen.V" <> show version <> ".Types") <> "'"
                  replaceInFile ("module Evergreen.Type.V" <> (T.pack $ show version)) ("module Evergreen.V" <> (T.pack $ show version) <> ".Types") dest


                  callCommand $ "git add " <> dest

                Nothing ->
                  -- Skip any incorrectly named files...
                  pure ()
            )

          putStrLn $ "Removing " <> (oldTypeSnapshotFolder) <> "..."
          rmdir $ root </> "src/Evergreen/Type"


          putStrLn $ "Renaming migration imports..."

          migrationFilepaths <- safeListDirectory $ root </> "src/Evergreen/Migrate"

          migrationFilepaths
            & mapM (\filepath -> do

              case getVersion filepath of
                Just version -> do

                  -- import Evergreen.Type.V18 as Old
                  -- import Lamdera.Migrations exposing (..)
                  -- import Evergreen.Type.V20 as New
                  let migrationPath = (root </> "src/Evergreen/Migrate" </> filepath)

                  putStrLn $ "Renaming imports in " <> migrationPath


                  replaceInFile ("import Evergreen.Type.") ("import Evergreen.") migrationPath
                  replaceInFile (" as Old") (".Types as Old") migrationPath
                  replaceInFile (" as New") (".Types as New") migrationPath

                Nothing ->
                  -- Skip any incorrectly named files...
                  pure ()
            )

          putStrLn $ "Staging the changes for git commit..."
          callCommand $ "git add -u " <> oldTypeSnapshotFolder <> " || true"

          putStrLn $ "\n\nDone! If you encounter issues with this helper, please drop a note in Discord.\n\n"

        pure ()

      else
        genericExit "Okay, I did not migrate."
