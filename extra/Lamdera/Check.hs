{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.Check where

import Control.Monad.Except (catchError, throwError)
import Data.Maybe (fromMaybe)
import NeatInterpolation
import qualified Data.List as List
import qualified Data.Text.Encoding as T
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Process as System
import qualified Text.Read
import System.FilePath ((</>))

import qualified Data.NonEmptyList as NE
import qualified Data.Text as T
import qualified File as IO
import qualified Json.Decode as D
import qualified Reporting
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help
import Make (Flags(..))
import qualified Make

import Lamdera
import Lamdera.Generated (VersionInfo(..), createLamderaGenerated, vinfoVersion, getLastLocalTypeChangeVersion)
import qualified Lamdera.AppConfig
import qualified Lamdera.Http
import qualified Lamdera.Progress as Progress
import qualified Lamdera.Project
import qualified Lamdera.Update
import qualified Lamdera.Compile


progressPointer t = do
  Progress.report $ D.fillSep [ D.fromChars "───>", D.blue $ t <> "\n" ]

progress t = do
  Progress.report $ D.stack [D.reflow t, ""]

progressDoc d =
  Progress.report $ D.stack [d, ""]


run :: () -> () -> IO ()
run () () = do
  debug_ "Starting check..."

  -- reporter <- Terminal.create

  appNameEnvM <- Env.lookupEnv "LAMDERA_APP_NAME"
  hoistRebuild <- Env.lookupEnv "HOIST_REBUILD"
  forceVersionM <- Env.lookupEnv "VERSION"
  forceNotProd <- Env.lookupEnv "NOTPROD"
  inDebug <- Lamdera.isDebug
  prodTokenM <- Env.lookupEnv "TOKEN"

  inProduction <- Lamdera.inProduction

  let isHoistRebuild = (hoistRebuild /= Nothing)
      forceVersion = fromMaybe (-1) $
        case forceVersionM of
          Just fv -> Text.Read.readMaybe fv
          Nothing -> Nothing

  debug $ "production:" ++ show inProduction
  debug $ "hoist rebuild:" ++ show isHoistRebuild
  debug $ "force version:" ++ show forceVersion

  -- summary <- Project.getRoot
  root <- getProjectRoot

  temporaryCheckOldTypesNeedingMigration inProduction root

  progressPointer "Checking project compiles..."
  checkUserProjectCompiles root

  lamderaRemotes <- Lamdera.Project.getLamderaRemotes
  onlyWhen (lamderaRemotes == [] && appNameEnvM == Nothing) Lamdera.Project.lamderaThrowUnknownApp
  -- Prior `onlyWhen` guards against situation where no name is determinable
  let appName = Lamdera.Project.certainAppName lamderaRemotes appNameEnvM

  progressPointer  "Checking Evergreen migrations...\n"
  debug $ "app name:" ++ show appName

  localTypes <- fetchLocalTypes root

  (prodVersion, productionTypes) <-
    if isHoistRebuild
      then do
        if (forceVersion == (-1))
          then
            genericExit "ERROR: Hoist rebuild got -1 for version, check your usage."

          else do
            progress $ "❗️Gen with forced version: " <> show forceVersion
            pure (forceVersion, localTypes)

      else do

        prodInfo_ <- fetchProductionInfo appName (inDebug || forceNotProd /= Nothing)
        case prodInfo_ of
          Right (pv, pt) ->
            -- Everything is as it should be
            pure (pv, pt)

          Left err ->
            if (inProduction)
              then
                genericExit "FATAL: application info could not be obtained. Please report this to support."

              else do
                -- See commented out code at bottom re prior "assume version when offline"
                Progress.throw $
                  Help.report "ERROR" Nothing
                    ("I normally check for production info here but the request failed.")
                    [ D.reflow $ "Please check your connection and try again, or contact support."
                    , D.fromChars $ Lamdera.Http.errorToString err
                    ]


  let
    localTypesChangedFromProduction =
      productionTypes /= localTypes

    nextVersion =
      if isHoistRebuild then
        -- No version bump, the stated version will be the prod version
        prodVersion
      else if forceVersion /= (-1) then
        forceVersion
      else
        (prodVersion + 1)

  nextVersionInfo <-
    if prodVersion == 0 then
      pure $ WithoutMigrations 1
    else
      if isHoistRebuild
        then do
          -- Hoist case
          hoistMigrationExists <- Dir.doesFileExist $ "src/Evergreen/Migrate/V" <> show prodVersion <> ".elm"
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
  --   approveHoist <- Reporting.ask $
  --     D.stack
  --       [ D.fillSep [ D.yellow "WARNING:","Confirm","hoist!" ]
  --       , D.reflow $ "Proceed with hoist as v" <> show nextVersion <> "? [Y/n]: "
  --       ]
  --   onlyWhen (not approveHoist) $ genericExit "Quitting hoist"


  if nextVersion == 1
    then do
      -- Always snapshot types for the first version, we need a starting point
      _ <- snapshotCurrentTypesTo root nextVersion

      writeLamderaGenerated root inProduction nextVersionInfo
      buildProductionJsFiles root inProduction nextVersionInfo

      onlyWhen (not inProduction) $ possiblyShowExternalTypeWarnings

      progressDoc $ D.green (D.reflow $ "It appears you're all set to deploy the first version of '" <> T.unpack appName <> "'!")

      -- This is the first version, we don't need any migration checking.
      onlyWhen (not inProduction) $ committedCheck root nextVersionInfo

      putStrLn ""

    else do
      writeLamderaGenerated root inProduction nextVersionInfo

      let
        nextMigrationPathBare = ("src/Evergreen/Migrate/V") <> (show nextVersion) <> ".elm"
        nextMigrationPath = root </> nextMigrationPathBare
        -- lastTypesPath = (root </> "src/Evergreen/Type/V") <> (show prodVersion) <> ".elm"

      migrationExists <- Dir.doesFileExist $ nextMigrationPath

      if localTypesChangedFromProduction
        then do
          debug $ "Local and production types differ"

          _ <- snapshotCurrentTypesTo root nextVersion

          let
            typeCompares = zipWith3
              (\label local prod -> (label, T.unpack local, T.unpack prod))
              [ "FrontendModel", "BackendModel", "FrontendMsg", "ToBackend", "BackendMsg", "ToFrontend" ]
              localTypes
              productionTypes

            changedTypes =
              typeCompares & filter (\(label, local, prod) -> local /= prod)

            formattedChangedTypes =
              changedTypes
                & fmap (\(label, local, prod) -> D.indent 4 (D.dullyellow (D.fromChars label)))
                & D.vcat

          if migrationExists
            then do
              debug $ "Migration file already exists"

              -- @TODO check migrations match changes. I.e. user might have changed one thing, we generated
              -- a migration, and then later changed another thing. Basically grep for `toBackend old = Unchanged` for each label?
              -- so we don't acidentally run a migration saying nothing changed when it's not type safe?
              -- is that even possible or will compiler catch incorrect "unchanged" declarations?

              debug $ "Reading migration source..."
              migrationSource <- T.decodeUtf8 <$> IO.readUtf8 nextMigrationPath

              if textContains "Unimplemented" migrationSource
                then do
                  lamderaThrowUnimplementedMigration nextMigrationPath formattedChangedTypes prodVersion nextMigrationPathBare

                else do
                  migrationCheck root nextVersionInfo
                  onlyWhen (not inProduction) $ possiblyShowExternalTypeWarnings

                  progressDoc $ D.green $ D.reflow $ "\nIt appears you're all set to deploy v" <> (show nextVersion) <> " of '" <> T.unpack appName <> "'."

                  mapM progressDoc $
                    ([ D.reflow "Evergreen migrations will be applied to the following types:"
                     , formattedChangedTypes
                     , D.reflow "See <https://dashboard.lamdera.app/docs/evergreen> for more info." ]
                    )

                  onlyWhen (not inProduction) $ committedCheck root nextVersionInfo

                  buildProductionJsFiles root inProduction nextVersionInfo

            else do
              debug $ "Migration does not exist"

              _ <- mkdir $ root </> "src/Evergreen/Migrate"

              lastLocalTypeChangeVersion <- Lamdera.Generated.getLastLocalTypeChangeVersion root

              let defaultMigrations = defaultMigrationFile lastLocalTypeChangeVersion nextVersion typeCompares

              writeUtf8 nextMigrationPath defaultMigrations

              onlyWhen (not inProduction) $ possiblyShowExternalTypeWarnings

              Progress.throw $
                Help.report "UNIMPLEMENTED MIGRATION" (Just nextMigrationPathBare)
                  ("The following types have changed since last deploy (v" <> show prodVersion <> ") and require migrations:")
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
            Progress.throw $
              Help.report "UNEXPECTED MIGRATION" (Just nextMigrationPathBare)
                ("There appears to be a migration file when I wasn't expecting one.")
                [ D.reflow $ "It appears local types have not changed compared to production, however I'm seeing a migration at " <> nextMigrationPathBare <> "."
                , D.reflow "Perhaps it needs to be removed?"
                , D.reflow "See <https://dashboard.lamdera.app/docs/evergreen> for more info."
                ]

          migrationCheck root nextVersionInfo
          onlyWhen (not inProduction) $ possiblyShowExternalTypeWarnings

          progressDoc $ D.green $ D.reflow $ "\nIt appears you're all set to deploy v" <> (show nextVersion) <> " of '" <> T.unpack appName <> "'."
          progressDoc $ D.reflow $ "\nThere are no Evergreen type changes for this version."

          buildProductionJsFiles root inProduction nextVersionInfo

  progressPointer "Checking config..."
  Lamdera.AppConfig.checkUserConfig appName (fmap T.pack prodTokenM)

  version_ <- Lamdera.Update.fetchCurrentVersion
  case version_ of
    Right version ->
      onlyWhen (version /= "skip" && (not $ textContains version (T.pack Lamdera.lamderaVersion))) $ do
          progressPointer "Checking version..."
          progressDoc $ D.stack
            [ D.red $ D.reflow $ "NOTE: There is a new alpha version, please upgrade before you deploy."
            , D.reflow $ "Current: " <> Lamdera.lamderaVersion
            , D.reflow $ "New:     " <> T.unpack version
            , D.reflow $ "You can download it here: <https://dashboard.lamdera.app/docs/download>"
            ]
    Left err ->
      debug $ Lamdera.Http.errorToString err



buildProductionJsFiles :: FilePath -> Bool -> VersionInfo -> IO ()
buildProductionJsFiles root inProduction versionInfo = do
  let version = vinfoVersion versionInfo

  onlyWhen inProduction $ do
    -- summary <- Project.getRoot @REMOVE
    root <- getProjectRoot

    debug $ "Compiling JS for production v" <> show (vinfoVersion versionInfo)

    onlyWhen (version /= 1 && versionInfo == WithMigrations version) $ do
      let migrationPath = root </> "src/Evergreen/Migrate/V" <> show version <> ".elm"
      replaceSnapshotTypeReferences migrationPath version

    -- debug $ "Injecting BACKENDINJECTION " <> (root </> "elm-backend-overrides.js")
    -- Env.setEnv "BACKENDINJECTION" (root </> "elm-backend-overrides.js")
    -- _ <- System.readProcess "touch" [root </> "src" </> "Types.elm"] ""

    Make.run ["src" </> "LBR.elm"] $
      Make.Flags
        { _debug = False
        , _optimize = True
        , _output = Just (Make.JS "backend-app.js")
        , _report = Nothing
        , _docs = Nothing
        }

    Make.run ["src" </> "LFR.elm"] $
      Make.Flags
        { _debug = False
        , _optimize = True
        , _output = Just (Make.JS "frontend-app.js")
        , _report = Nothing
        , _docs = Nothing
        }

    -- Project.compile
    --   Output.Prod
    --   Output.Client
    --   (Just (Output.JavaScript Nothing "backend-app.js"))
    --   Nothing
    --   summary
    --   [ "src" </> "LBR.elm" ]

    -- debug $ "Unsetting BACKENDINJECTION"
    -- Env.unsetEnv "BACKENDINJECTION"

    -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?
    -- sleep 50 -- 50 milliseconds

    -- Project.compile
    --   Output.Prod
    --   Output.Client
    --   (Just (Output.JavaScript Nothing "frontend-app.js"))
    --   Nothing
    --   summary
    --   [ "src" </> "LFR.elm" ]


    -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?
    -- sleep 50 -- 50 milliseconds



replaceSnapshotTypeReferences migrationPath version = do
  debug $ "Replacing " <> "Evergreen.V" <> show version <> " type references in " <> migrationPath
  replaceInFile ("Evergreen.V" <> show_ version <> ".") "" migrationPath



-- @TODO drop this entire type down to Task then we don't need the wrapping of IO
snapshotCurrentTypesTo :: FilePath -> Int -> IO String
snapshotCurrentTypesTo root version = do
  -- Snapshot the current types, and rename the module for the snapshot

  debug_ "Executing in type snapshot mode..."

  Env.setEnv "LTYPESNAPSHOT" (show version)

  -- Elm's caches will mean Types.elm won't get recompiled without 'changes', so we touch it
  _ <- Lamdera.touch $ root </> "src" </> "Types.elm"

  -- Invoke compiler in snapshot mode for src/Types.elm
  Lamdera.Compile.makeNull root ("src" </> "Types.elm")

  Env.unsetEnv "LTYPESNAPSHOT"

  pure ""




lamderaThrowUnimplementedMigration nextMigrationPath formattedChangedTypes prodVersion nextMigrationPathBare = do
  Progress.throw
    $ Help.report "UNIMPLEMENTED MIGRATION" (Just nextMigrationPathBare)
      ("The following types have changed since last deploy (v" <> show prodVersion <> ") and require migrations:")
      [ formattedChangedTypes
      , D.reflow $ "There are still migration placeholders that need implementing here:"
      , D.reflow $ nextMigrationPath
      , D.fillSep ["See",D.cyan ("<https://dashboard.lamdera.app/docs/evergreen>"),"for more info."]
      ]



fetchProductionInfo :: Text -> Bool -> IO (Either Lamdera.Http.Error (Int, [Text]))
fetchProductionInfo appName useLocal =
  let
    endpoint =
      if textContains "-local" appName && useLocal
        then
          "https://" <> T.unpack appName <> ".lamdera.test/_i"

        else
          "https://" <> T.unpack appName <> ".lamdera.app/_i"

    decoder =
      D.succeed (,)
        & D.required "v" D.int
        & D.required "h" (D.list D.text)
  in
  Lamdera.Http.normalJson "lamdera-info" endpoint decoder


fetchLocalTypes :: FilePath -> IO [Text]
fetchLocalTypes root = do

  debug $ "Reading local types from " <> lamderaHashesPath root

  -- This could fail normally but we're using this function after
  -- we've already checked it exists
  hashString <- IO.readUtf8 (lamderaHashesPath root)

  let
    decoder =
      (D.list D.text)

  case D.fromByteString decoder hashString of
    Right value ->
      return $ value

    Left jsonProblem ->
      -- @TODO fix this - what should happen here?
      error "fetchLocalTypes decode failed"
      -- return $ error $ show jsonProblem --Left $ E.BadJson "github.json" jsonProblem


checkUserProjectCompiles :: FilePath -> IO ()
checkUserProjectCompiles root = do
  _ <- Lamdera.touch $ root </> "src" </> "Types.elm"

  -- let jsOutput = Just (Output.Html Nothing "/dev/null")
  -- Project.compile Output.Prod Output.Client jsOutput Nothing summary [ "src" </> "Frontend.elm" ]
  -- Project.compile Output.Prod Output.Client jsOutput Nothing summary [ "src" </> "Backend.elm" ]

  Lamdera.Compile.makeNull root ("src" </> "Frontend.elm")
  Lamdera.Compile.makeNull root ("src" </> "Backend.elm")

  -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?
  -- sleep 50 -- 50 milliseconds


migrationCheck :: FilePath -> VersionInfo -> IO ()
migrationCheck root nextVersion = do
  root <- getProjectRoot
  let
    version = vinfoVersion nextVersion
    migrationPath = (root </> "src/Evergreen/Migrate/V") <> show version <> ".elm"
    migrationPathBk = (root </> "src/Evergreen/Migrate/.V") <> show version <> ".elm.bk"

  debug "Type-checking Evergreen migrations..."

  migrationExists <- Dir.doesFileExist migrationPath

  onlyWhen migrationExists $ do
    copyFile migrationPath migrationPathBk
    replaceSnapshotTypeReferences migrationPath version

  mkdir $ root </> "lamdera-stuff/alpha"

  gen <- Lamdera.Generated.createLamderaGenerated root nextVersion

  let lamderaCheckBothPath = "lamdera-stuff/alpha/LamderaCheckBoth.elm"

  writeUtf8 (root </> lamderaCheckBothPath) (gen)
  -- let jsOutput = Just (Output.Html Nothing "/dev/null")
  -- Project.compile Output.Dev Output.Client jsOutput Nothing summary [ lamderaCheckBothPath ]

  (Dir.withCurrentDirectory root $
    Make.run ["src" </> "LFR.elm"] $
        Make.Flags
          { _debug = False
          , _optimize = False
          , _output = Just (Make.DevNull)
          , _report = Nothing
          , _docs = Nothing
          })
    `catchError` (\err -> do
      debug "catchError: Cleaning up build scaffold"
      -- Remove our temporarily checker file
      remove $ root </> lamderaCheckBothPath

      -- Restore backed up unaltered migration file
      onlyWhen migrationExists $ do
        copyFile migrationPathBk migrationPath
        remove migrationPathBk

      -- putStrLn $ show err
      throwError err
    )

  -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?
  sleep 50 -- 50 milliseconds

  debug "Cleaning up build scaffold"
  -- Remove our temporarily checker file
  remove $ root </> lamderaCheckBothPath

  -- Restore backed up unaltered migration file
  onlyWhen migrationExists $ do
    copyFile migrationPathBk migrationPath
    remove migrationPathBk


committedCheck :: FilePath -> VersionInfo -> IO ()
committedCheck root versionInfo = do

  let version = vinfoVersion versionInfo

  debug $ "Commit-checking migration and types files"

  let migrationPath = (root </> "src/Evergreen/Migrate/V") <> show version <> ".elm"
  migrations <- gitStatus migrationPath

  let typesPath = (root </> "src/Evergreen/V") <> show version <> "/*.elm"
  types <- gitStatus typesPath

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
    Progress.report $
      Help.reportToDoc $
      Help.report "UNCOMMITTED FILES" (Just "src/Evergreen/")
        ("I need type and migration files to be comitted otherwise I cannot deploy!")
        ([ D.reflow "Here is a shortcut:"
         , D.dullyellow (D.reflow $ "git add " <> (List.intercalate " " missingPaths))
         , D.dullyellow (D.reflow $ "git commit -m \"Preparing for v" <> show version <> "\"")
         ]
        )

    addToGitApproved <- Reporting.ask $
      D.stack [ D.reflow $ "Shall I `git add` for you? [Y/n]: " ]

    if addToGitApproved
      then do
        mapM (\path -> System.callCommand $ "git add " <> path) missingPaths
        commitApproved <- Reporting.ask $
          D.stack [ D.reflow $ "Shall I `git commit` for you? [Y/n]: " ]

        if commitApproved
          then do
            System.callCommand $ "git commit -m \"Preparing for v" <> show version <> "\""
            progress ""

          else
            progress "Okay, I did not commit it."

      else
        progress "Okay, I did not add it."


defaultMigrationFile :: Int -> Int -> [(String, String, String)] -> Text
defaultMigrationFile oldVersion newVersion typeCompares = do
  let old = show_ oldVersion
      new = show_ newVersion

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
  let version_ = show_ version
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
  gsPorcelain <- System.readProcess "git" ["status", "--porcelain", filepath] ""
  -- print $ firstTwoChars gsPorcelain
  case firstTwoChars gsPorcelain of
    ('_', '_') -> do
      -- `git status` is empty, so we need to check if the file is tracked (thus clean) or non-existent
      gitFiles <- System.readProcess "git" ["ls-files", filepath] ""
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


writeLamderaGenerated :: FilePath -> Bool -> VersionInfo -> IO ()
writeLamderaGenerated root inProduction nextVersion =
  onlyWhen inProduction $ do
    gen <- Lamdera.Generated.createLamderaGenerated root nextVersion
    writeUtf8 (root </> "src/LamderaGenerated.elm") gen


possiblyShowExternalTypeWarnings :: IO ()
possiblyShowExternalTypeWarnings = do

  root <- getProjectRoot
  warnings_ <- maybeReadUtf8 $ lamderaExternalWarningsPath root

  case warnings_ of
    Just warnings -> do

      debug "Printing out external type warnings"

      progressDoc $
        D.stack
          (
          [ D.red $ D.reflow $ "WARNING: Evergreen Alpha does not cover type changes outside your project"
          , D.reflow $ "You are referencing the following in your core types:"
          , D.vcat [ D.fromChars . T.unpack $ warnings ]
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
          Just <$> T.decodeUtf8 <$> IO.readUtf8 filePath
          -- pure $ Just $
        else
          pure Nothing


genericExit :: String -> IO a
genericExit str =
  Progress.throw
    $ Help.report "ERROR" Nothing
      (str)
      []







-- Legacy, to be removed at Beta


temporaryCheckOldTypesNeedingMigration :: Bool -> FilePath -> IO ()
temporaryCheckOldTypesNeedingMigration inProduction root = do

  exists_ <- Dir.doesDirectoryExist $ root </> "src" </> "Evergreen" </> "Type"

  onlyWhen exists_ $ do

    if inProduction
      then
        Progress.throw
          $ Help.report "Evergreen API changes" (Just "src/Evergreen/")
              ("The Evergreen API changed in alpha5. It appears you've not migrated yet!")
              ([ D.dullyellow $ D.reflow "Please download the latest binary and run `lamdera check` again."
               , D.reflow $ "https://dashboard.lamdera.app/docs/download"
               , D.reflow $ "See the full release here: https://dashboard.lamdera.app/releases/alpha5"
               ]
              )
      else
        Progress.report $
            Help.reportToDoc $
            Help.report "Evergreen API changes" (Just "src/Evergreen/")
              ("The Evergreen API changed in alpha5. It appears you've not migrated yet!")
              ([ D.reflow "The following changes were introduced:"
               , D.vcat
                   [ D.reflow $ "- Type snapshots now extract from your entire project, not just Types.elm"
                   , D.reflow $ "- Type snapshots now live in src/Evergreen/V*/ folders for each version"
                   , D.reflow $ "- Only the types referenced by the 6 core types will get extracted (i.e. functions/other types no longer get copied)"
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

    migrationApproved <- Reporting.ask $
      D.stack
        [ D.reflow $ "Shall I attempt to migrate for you? [Y/n]: "
        ]

    if migrationApproved
      then do

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
                replaceInFile ("module Evergreen.Type.V" <> (show_ version)) ("module Evergreen.V" <> (show_ version) <> ".Types") dest

                System.callCommand $ "git add " <> dest

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
        System.callCommand $ "git add -u " <> oldTypeSnapshotFolder <> " || true"

        putStrLn $ "\n\nDone! If you encounter issues with this helper, please drop a note in Discord.\n\n"

        pure ()

      else
        genericExit "Okay, I did not migrate."
