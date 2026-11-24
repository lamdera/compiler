{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.CLI.Check where

import Control.Monad.Except (catchError, throwError)
import Control.Monad (forM_, when)
import qualified Control.Exception
import qualified Data.Char
import Data.Maybe (fromMaybe)
import qualified Data.Maybe
import NeatInterpolation
import qualified Data.List as List
import qualified Data.Text.Encoding as T
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Process as System
import qualified Text.Read
import System.FilePath ((</>))
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)

import qualified Data.NonEmptyList as NE
import qualified Data.Text as T
import qualified File as IO
import qualified Json.Decode as D
import qualified Reporting
import qualified Reporting.Doc as D
import qualified Reporting.Exit
import qualified Reporting.Exit.Help as Help
import Make (Flags(..))
import qualified Make

import qualified Elm.Outline
import qualified Elm.ModuleName
import qualified Root
import qualified AST.Prim.TypeName

import Lamdera
import qualified Lamdera.Version


import qualified Lamdera.AppConfig
import qualified Lamdera.Checks
import qualified Lamdera.CompileMeta
import qualified Lamdera.Compile
import qualified Lamdera.Http
import qualified Lamdera.Legacy
import qualified Lamdera.Progress as Progress
import qualified Lamdera.Project
import qualified Lamdera.TypeHash
import qualified Lamdera.Types
import qualified Lamdera.Update
import qualified Lamdera.Version
import qualified Network.Status

import Lamdera.Evergreen.MigrationHarness (VersionInfo(..), vinfoVersion, getLastLocalTypeChangeVersion)
import qualified Lamdera.Evergreen.MigrationDestructive
import qualified Lamdera.Evergreen.MigrationGenerator
import qualified Lamdera.Evergreen.MigrationHarness
import qualified Lamdera.Evergreen.Snapshot


data Flags =
  Flags
    { _destructiveMigration :: Bool
    , _force :: Bool
    , _destructiveMigrationExceptBackend :: Bool
    }


run_ :: IO ()
run_ = run () (Lamdera.CLI.Check.Flags { _destructiveMigration = False, _force = False, _destructiveMigrationExceptBackend = False })


run :: () -> Lamdera.CLI.Check.Flags -> IO ()
run () flags@(Lamdera.CLI.Check.Flags destructiveMigration force destructiveExceptBackend) = do
  debug_ "Starting check..."

  inProduction_ <- Lamdera.inProduction

  if inProduction_
    then
      -- In production check will only get run in the right context
      -- and we also may not have branch information available due to the checkout mechanism
      -- so assume the invocation is right and allow it to continue
      runHelp () flags
    else do
      branch <- Lamdera.getGitBranch
      if force
        then runHelp () flags
        else
          case branch of
            "main" -> runHelp () flags
            "master" -> runHelp () flags
            _ -> do
              atomicPutStrLn "`lamdera check` is only for main/master branches when preparing for a production deploy."
              atomicPutStrLn "If you're trying to deploy a preview app, use `lamdera deploy` instead."
              pure ()


runHelp :: () -> Lamdera.CLI.Check.Flags -> IO ()
runHelp () flags@(Lamdera.CLI.Check.Flags destructiveMigration force destructiveExceptBackend) = do
  Lamdera.setCheckMode True

  -- appNameEnvM <- Env.lookupEnv "LAMDERA_APP_NAME"

  forceNotProd <- Env.lookupEnv "NOTPROD"
  inDebug <- Lamdera.isDebug
  isHoistRebuild <- isHoistRebuild_
  forceVersion <- forceVersion_
  inProduction_ <- Lamdera.inProduction
  isPreviewBuild <- Env.lookupEnv "PREVIEW"

  debug $ "production:" ++ show inProduction_
  debug $ "hoist rebuild:" ++ show isHoistRebuild
  debug $ "force version:" ++ show forceVersion

  root <- getProjectRoot "Lamdera.CLI.Check.run"
  checkGitInitialised root

  Lamdera.Legacy.temporaryCheckOldTypesNeedingMigration inProduction_ root
  Lamdera.Legacy.temporaryCheckCodecsNeedsUpgrading inProduction_ root

  progressPointer_ "Checking project compiles..."
  -- In production, check optimized up-front for Debug.* usage immediate feedback
  checkUserProjectCompiles root inProduction_

  appName <- Lamdera.Project.appNameOrThrow
  (localTypes, externalTypeWarnings) <- getLocalInfo

  if isPreviewBuild == Just "true"
    then do
      buildFirstDeploy root inProduction_ appName externalTypeWarnings

    else do
      progressPointer "Checking Evergreen migrations..."
      debug $ "app name:" ++ show appName

      ips <- Network.Status.ips

      if ips == [] && not inProduction_
        then offlineCheck root
        else onlineCheck root appName inDebug localTypes externalTypeWarnings isHoistRebuild forceVersion forceNotProd inProduction_ destructiveMigration destructiveExceptBackend

  Lamdera.setCheckMode False


offlineCheck root = do
  progressDoc $ D.stack
    [ D.red $ D.reflow $ "--- OFFLINE MODE ---"
    , D.reflow $ "It appears you are offline, continuing with limited checks."
    , D.red $ D.reflow $ "Note: being offline and continuing can cause serious confusion when returning online, in particular:"
    , D.reflow $ "- I have no idea what's in production, so I can only guess no types have changed."
    , D.reflow $ "- This means I won't make any new snapshots"
    , D.reflow $ "- I will assume the latest current migration on disk is the real latest migration"
    , D.reflow $ "- I will type check that migration under Evergreen"
    , D.reflow $ "I recommend you either skip this check, or run it again when you're online."
    ]

  shouldContinue <- Reporting.ask $ D.stack [ D.reflow $ "Do you want me to continue despite this? [Y/n]: " ]

  if shouldContinue
    then do
      migrationFilePaths <- Lamdera.Evergreen.MigrationHarness.findMigrationFilePaths root

      let
        nextVersionInfo =
          case last_ (debugNote "migrationVersions" $ Lamdera.Evergreen.MigrationHarness.migrationVersions migrationFilePaths) 1 of
            1 -> WithoutMigrations 1
            x -> WithMigrations x

        nextVersion = vinfoVersion nextVersionInfo

        nextMigrationPathBare = ("src/Evergreen/Migrate/V") <> (show nextVersion) <> ".elm"
        nextMigrationPath = root </> nextMigrationPathBare

      debug_ $ "nextVersionInfo" ++ show nextVersionInfo

      debug $ "Reading migration source..."
      migrationSource <- T.decodeUtf8 <$> IO.readUtf8 nextMigrationPath

      if textContains "Unimplemented" migrationSource
        then do
          Progress.throw $
            Help.report "UNIMPLEMENTED MIGRATION" (Just nextMigrationPathBare)
              ("The latest migration still has migration placeholders that need implementing:")
              [ D.reflow $ nextMigrationPathBare
              , D.fillSep ["See",D.cyan ("<https://dashboard.lamdera.app/docs/evergreen>"),"for more info."]
              ]

        else do
          migrationCheck root nextVersionInfo []
    else do
      pure ()

  pure ()


onlineCheck root appName inDebug localTypes externalTypeWarnings isHoistRebuild forceVersion forceNotProd inProduction_ destructiveMigration destructiveExceptBackend = do

  (prodVersion, productionTypes, nextVersion) <- getProdInfo appName inProduction_ forceNotProd forceVersion isHoistRebuild localTypes

  let
    localTypesChangedFromProduction = productionTypes /= localTypes

  nextVersionInfo <- getNextVersionInfo_ nextVersion prodVersion isHoistRebuild localTypesChangedFromProduction

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
      buildFirstDeploy root inProduction_ appName externalTypeWarnings

    else do
      writeLamderaGenerated root inProduction_ nextVersionInfo

      let
        nextMigrationPathBare = ("src/Evergreen/Migrate/V") <> (show nextVersion) <> ".elm"
        nextMigrationPath = root </> nextMigrationPathBare

      migrationExists <- Dir.doesFileExist $ nextMigrationPath

      if localTypesChangedFromProduction
        then do
          debug $ "Local and production types differ"

          Lamdera.Evergreen.Snapshot.run nextVersion

          let
            typeCompares = zipWith3
              (\label local prod -> (label, T.unpack local, T.unpack prod))
              Lamdera.Types.core
              localTypes
              productionTypes

            changedTypes =
              typeCompares & filter (\(label, local, prod) -> local /= prod)

            formattedChangedTypes =
              changedTypes
                & fmap (\(label, local, prod) -> D.indent 4 (D.dullyellow (D.fromChars $ show label)))
                & D.vcat

          lastLocalTypeChangeVersion <- Lamdera.Evergreen.MigrationHarness.getLastLocalTypeChangeVersion root

          if migrationExists
            then do
              debug $ "Migration file already exists"

              -- @TODO check migrations match changes. I.e. user might have changed one thing, we generated
              -- a migration, and then later changed another thing. Basically grep for `toBackend old = Unchanged` for each label?
              -- so we don't acidentally run a migration saying nothing changed when it's not type safe?
              -- is that even possible or will compiler catch incorrect "unchanged" declarations?

              onlyWhen destructiveMigration $ do
                destructive <- Lamdera.Evergreen.MigrationDestructive.generate lastLocalTypeChangeVersion nextVersion typeCompares
                writeUtf8 nextMigrationPath destructive
                pure ()

              -- Unlike --destructive-migration, the BackendModel migration in an existing file may be hand written
              onlyWhen (destructiveExceptBackend && not destructiveMigration) $
                Progress.throw $
                  Help.report "MIGRATION ALREADY EXISTS" (Just nextMigrationPathBare)
                    ("--destructive-migration-except-backend only applies when generating a new migration, and there is already one here:")
                    [ D.dullyellow $ D.reflow $ nextMigrationPath
                    , D.reflow "If you haven't written anything in it you want to keep, delete it and run `lamdera check --destructive-migration-except-backend` again."
                    ]

              debug $ "Reading migration source..."
              migrationSource <- T.decodeUtf8 <$> IO.readUtf8 nextMigrationPath

              let resetWarning =
                    if textContains "ModelReset" migrationSource
                      then [ D.red $ D.reflow "WARNING: This migration contains a `ModelReset` which will reset the model to its `init` value. For BackendModel's this cannot be reversed without rolling back to a previous version and data snapshot." ]
                      else []

              if textContains "Unimplemented" migrationSource
                then do
                  Progress.throw $
                    Help.report "UNIMPLEMENTED MIGRATION" (Just nextMigrationPathBare)
                      ("The following types have changed since last deploy (v" <> show prodVersion <> ") and require migrations:")
                      [ formattedChangedTypes
                      , D.reflow $ "There are still migration placeholders that need implementing here:"
                      , D.dullgreen $ D.reflow $ nextMigrationPath
                      , D.fillSep ["See",D.cyan ("<https://dashboard.lamdera.app/docs/evergreen>"),"for more info."]
                      ]

                else do
                  migrationCheck root nextVersionInfo changedTypes
                  onlyWhen (not inProduction_) $ showExternalTypeWarnings externalTypeWarnings

                  checkUserProjectCompiles root True
                  checkConfig appName

                  progressDoc $
                    D.stack
                      (
                      [ D.dullgreen $ D.reflow $ "\nIt appears you're all set to deploy v" <> (show nextVersion) <> " of '" <> T.unpack appName <> "'."
                      , D.reflow "Evergreen migrations will be applied to the following types:"
                      , formattedChangedTypes
                      , D.reflow "See <https://dashboard.lamdera.app/docs/evergreen> for more info."
                      ] ++ resetWarning
                      )

                  onlyWhen (not inProduction_) $ committedCheck root nextVersionInfo

                  buildProductionJsFiles root inProduction_ nextVersionInfo

            else do
              debug $ "Migration does not exist"

              if inProduction_
                then
                  Progress.throw $
                    Help.report "UNIMPLEMENTED MIGRATION" (Just nextMigrationPathBare)
                      ("The following types have changed since last deploy (v" <> show prodVersion <> ") and require migrations:")
                      [ formattedChangedTypes
                      , D.reflow $ "Please run `lamdera check` locally to get started implementing a migration."
                      , D.reflow "See <https://dashboard.lamdera.app/docs/evergreen> for more info."
                      ]

                else do
                  _ <- mkdir $ root </> "src/Evergreen/Migrate"
                  defaultMigrations <- do
                    if destructiveMigration
                      then
                        Lamdera.Evergreen.MigrationDestructive.generate lastLocalTypeChangeVersion nextVersion typeCompares
                      else
                        Lamdera.Evergreen.MigrationGenerator.betweenVersions destructiveExceptBackend typeCompares lastLocalTypeChangeVersion nextVersion root
                  writeUtf8 nextMigrationPath defaultMigrations

                  Progress.throw $
                    Help.report "UNIMPLEMENTED MIGRATION" (Just nextMigrationPathBare)
                      ("The following types have changed since last deploy (v" <> show prodVersion <> ") and require migrations:")
                      [ formattedChangedTypes
                      , D.reflow $ "I've tried to generate a migration for you here:"
                      , D.dullyellow $ D.reflow $ nextMigrationPath
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

          migrationCheck root nextVersionInfo []
          onlyWhen (not inProduction_) $ showExternalTypeWarnings externalTypeWarnings

          checkUserProjectCompiles root True
          checkConfig appName

          progressDoc $ D.dullgreen $ D.reflow $ "\nIt appears you're all set to deploy v" <> (show nextVersion) <> " of '" <> T.unpack appName <> "'."
          progressDoc $ D.reflow $ "There are no Evergreen type changes for this version."

          buildProductionJsFiles root inProduction_ nextVersionInfo


  onlyWhen (not inProduction_) $ checkForLatestBinaryVersion inDebug

  pure ()


buildFirstDeploy root inProduction_ appName externalTypeWarnings = do
  let nextVersionInfo = WithoutMigrations 1

  -- Always snapshot types for the first version, we need a starting point
  _ <- Lamdera.Evergreen.Snapshot.run 1

  writeLamderaGenerated root inProduction_ nextVersionInfo
  buildProductionJsFiles root inProduction_ nextVersionInfo

  onlyWhen (not inProduction_) $ showExternalTypeWarnings externalTypeWarnings

  checkUserProjectCompiles root True
  checkConfig appName

  progressDoc $ D.dullgreen (D.reflow $ "It appears you're all set to deploy the first version of '" <> T.unpack appName <> "'!")

  -- This is the first version, we don't need any migration checking.
  onlyWhen (not inProduction_) $ committedCheck root nextVersionInfo

  -- @TEMPORARY
  putStrLn ""


checkConfig appName = do
  progressPointer "Checking config..."
  prodTokenM <- Env.lookupEnv "TOKEN"
  Lamdera.AppConfig.checkUserConfig appName (fmap T.pack prodTokenM)


isHoistRebuild_ :: IO Bool
isHoistRebuild_ = do
  hoistRebuild <- Env.lookupEnv "HOIST_REBUILD"
  pure (hoistRebuild /= Nothing)


forceVersion_ :: IO Int
forceVersion_ = do
  forceVersionM <- Env.lookupEnv "VERSION"
  pure $ fromMaybe (-1) $
    case forceVersionM of
      Just fv -> Text.Read.readMaybe fv
      Nothing -> Nothing


-- getNextVersionInfo :: FilePath -> IO VersionInfo
-- getNextVersionInfo root = do
--
--   hoistRebuild <- Env.lookupEnv "HOIST_REBUILD"
--   forceVersionM <- Env.lookupEnv "VERSION"
--   forceNotProd <- Env.lookupEnv "NOTPROD"
--   inProduction <- Lamdera.inProduction
--
--   let isHoistRebuild = (hoistRebuild /= Nothing)
--       forceVersion = fromMaybe (-1) $
--         case forceVersionM of
--           Just fv -> Text.Read.readMaybe fv
--           Nothing -> Nothing
--
--   -- Lamdera.Legacy.temporaryCheckOldTypesNeedingMigration inProduction root
--
--   -- progressPointer_ "Checking project compiles..."
--   -- checkUserProjectCompiles root
--
--   appName <- Lamdera.Project.appNameOrThrow
--
--   (localTypes, externalTypeWarnings) <- getLocalInfo
--   (prodVersion, productionTypes, nextVersion) <- getProdInfo appName inProduction forceNotProd forceVersion isHoistRebuild localTypes
--
--   let
--     localTypesChangedFromProduction = productionTypes /= localTypes
--
--   nextVersionInfo <- getNextVersionInfo_ nextVersion prodVersion isHoistRebuild localTypesChangedFromProduction
--
--   pure nextVersionInfo


getLocalInfo :: IO (([Text], [(Text, [Text], Lamdera.Types.DiffableType)]))
getLocalInfo = do
  hashesResult <- Lamdera.TypeHash.calculateAndWrite
  case hashesResult of
    Left problem ->
      Progress.throw $ Reporting.Exit.reactorToReport $ Reporting.Exit.ReactorBadBuild $ problem
    Right (localTypes, externalTypeWarnings) ->
      pure (localTypes, externalTypeWarnings)


getProdInfo :: Text -> Bool -> Maybe String -> Int -> Bool -> [Text] -> IO (Int, [Text], Int)
getProdInfo appName inProduction_ forceNotProd forceVersion isHoistRebuild localTypes = do
  -- Bootstrap (local test env only, and only while no dashboard is deployed yet)
  -- treats the app as a fresh v0 instead of fetching prior version info.
  -- See Lamdera.Http.bootstrapActive.
  bootstrap <- Lamdera.Http.bootstrapActive
  (prodVersion, productionTypes) <-
    if isHoistRebuild
      then do
        if (forceVersion == (-1))
          then
            genericExit "ERROR: Hoist rebuild got -1 for version, check your usage."

          else do
            debug_ $ "❗️Gen with forced version: " <> show forceVersion
            pure (forceVersion, localTypes)

      else if bootstrap
      then do
        debug_ "❗️[bootstrap] skipping production info fetch, assuming fresh v0 app"
        pure (0, localTypes)

      else do

        -- /_i is answered by the real app once deployed, and by an appzero
        -- placeholder (v0) for a freshly-created app before its first deploy — so a
        -- fresh-v0 build always has a positive source of prod info. A genuine
        -- failure to reach it means the app is misconfigured/crashed, not new.
        prodInfo_ <- fetchProductionInfo appName (forceNotProd /= Nothing)
        case prodInfo_ of
          Right (pv, pt) ->
            -- Everything is as it should be
            pure (pv, pt)

          Left err ->
            if (inProduction_)
              then do
                debug_ $ show err
                genericExit "FATAL: application info could not be obtained. Please report this to support."

              else do
                Lamdera.Http.printHttpError err "I needed to query production application info"
                exitFailure
  let
    nextVersion =
      if isHoistRebuild then
        -- No version bump, the stated version will be the prod version
        prodVersion
      else if forceVersion /= (-1) then
        forceVersion
      else
        (prodVersion + 1)

  pure (prodVersion, productionTypes, nextVersion)


getNextVersionInfo_ nextVersion prodVersion isHoistRebuild localTypesChangedFromProduction =
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


checkForLatestBinaryVersion inDebug = do
  progressPointer "Checking lamdera version..."
  latestVersionText_ <- Lamdera.Update.fetchCurrentVersion
  case latestVersionText_ of
    Right latestVersionText -> do
      let
        toIntCertain :: Text -> Int
        toIntCertain t =
          t & T.unpack
            & Text.Read.readMaybe
            & withDefault 0

        latestVersion =
          latestVersionText
            & T.splitOn "-"
            & (\parts ->
                  case parts of
                    ev:lv:_ ->
                      lv
                        & T.splitOn "."
                        & fmap toIntCertain
                        & (\parts_ ->
                                case parts_ of
                                    v1 : v2 : v3 : [] ->
                                        Just ( v1, v2, v3 )

                                    _ ->
                                        Nothing
                           )

                    _ -> Nothing

              )
            & withDefault ( 0, 0, 0 )

        localVersion =
          Lamdera.Version.raw

      debug $ "comparing remote:" <> show latestVersionText <> " local:" <> show Lamdera.Version.raw
      debug $ "comparing remote:" <> show latestVersion <> " local:" <> show localVersion

      onlyWhen (latestVersionText /= "skip" && latestVersion > localVersion) $ do
          progressDoc $ D.stack
            [ D.red $ D.reflow $ "NOTE: There is a new lamdera version, please upgrade before you deploy."
            , D.reflow $ "Current: " <> Lamdera.Version.short
            , D.reflow $ "New:     " <> T.unpack latestVersionText
            , D.reflow $ "Run `lamdera update`, or download it here: <https://dashboard.lamdera.app/docs/download>"
            ]

      onlyWhen (latestVersion < localVersion) $ do
          progressDoc $ D.stack
            [ D.magenta $ D.reflow $
                "\nWarning: this is a pre-release compiler v" <> Lamdera.Version.rawToString localVersion <>
                " (latest is v" <> Lamdera.Version.rawToString latestVersion <> ")"
            ]

    Left err ->
      onlyWhen (inDebug) $ Lamdera.Http.printHttpError err "I needed to check the release version"



buildProductionJsFiles :: FilePath -> Bool -> VersionInfo -> IO ()
buildProductionJsFiles root inProduction_ versionInfo = do
  onlyWhen inProduction_ $ do
    let version = vinfoVersion versionInfo

    progressPointer "Compiling production code..."
    debug $ "Compiling JS for production v" <> show (vinfoVersion versionInfo)

    onlyWhen (version /= 1 && versionInfo == WithMigrations version) $ do
      let migrationPath = root </> "src/Evergreen/Migrate/V" <> show version <> ".elm"
      replaceSnapshotTypeReferences migrationPath version

    Make.run ["src" </> "LBR.elm"] $
      Make.Flags
        { _debug = False
        , _optimize = True
        , _output = Just (Make.JS "backend-app.js")
        , _report = Nothing
        , _docs = Nothing
        , _noWire = False
        , _optimizeLegible = True
        , _esm = False
        }

    Make.run ["src" </> "LFR.elm"] $
      Make.Flags
        { _debug = False
        , _optimize = True
        , _output = Just (Make.JS "frontend-app.js")
        , _report = Nothing
        , _docs = Nothing
        , _noWire = False
        , _optimizeLegible = False
        , _esm = False
        }

    Lamdera.AppConfig.writeUsage
    Lamdera.CompileMeta.write


replaceSnapshotTypeReferences migrationPath version = do
  debug $ "Replacing " <> "Evergreen.V" <> show version <> " type references in " <> migrationPath
  replaceInFile ("Evergreen.V" <> show_ version <> ".") "" migrationPath


fetchProductionInfo :: Text -> Bool -> IO (Either Lamdera.Http.Error (Int, [Text]))
fetchProductionInfo appName useLocal = do
  -- Prefer a local unix socket when present, otherwise use HTTP.
  appSocket <- Lamdera.Http.socketPathIfExists appName
  let
    endpoint =
      case appSocket of
        Just _ ->
          "http://" <> T.unpack appName <> ".lamdera.app/_i"

        Nothing ->
          if (textContains "-local" appName && ostype == MacOS) || useLocal
            then
              -- "https://" <> T.unpack appName <> ".lamdera.test/_i"
              "http://" <> T.unpack appName <> ".lamdera.test/_i"

            else
              "https://" <> T.unpack appName <> ".lamdera.app/_i"

    decoder =
      D.succeed (,,)
        & D.required "v" D.int
        & D.required "h" (D.list D.text)
        -- The hash scheme the app's hashes were computed with, from runtimes that know it
        & D.optional "hs" (fmap Just D.int) Nothing
  res <- Lamdera.Http.normalJsonVia appSocket "lamdera-info" endpoint decoder
  case res of
    -- Only an OLDER scheme than the app's: the servers have moved on. A newer one is fine,
    -- stored schemes lag behind for apps whose hashes the change didn't touch.
    Right (_, _, Just appScheme) | appScheme > Lamdera.Version.hashScheme ->
      Progress.throw $
        Help.report "LAMDERA UPDATE NEEDED" Nothing
          ("This lamdera (" <> Lamdera.Version.short <> ") hashes types differently from the compiler that built " <> T.unpack appName <> ", so it can't tell whether your types changed.")
          [ D.reflow $ "This compiler uses hash scheme " <> show Lamdera.Version.hashScheme <> ", the app's types were hashed with scheme " <> show appScheme <> "."
          , D.reflow $ "Run `lamdera update`, or download the latest version here: <https://dashboard.lamdera.app/docs/download>"
          ]

    Right (v, h, _) ->
      pure $ Right (v, h)

    Left err ->
      case Lamdera.Http.refusalMessage err of
        Just message ->
          Progress.throw $
            Help.report "LAMDERA UPDATE NEEDED" Nothing
              ("The server refused this lamdera (" <> Lamdera.Version.short <> "):")
              [ D.reflow message ]

        Nothing ->
          pure $ Left err


checkUserProjectCompiles :: FilePath -> Bool -> IO ()
checkUserProjectCompiles root optimized = do

  -- Make sure we catch project outline issues first
  eitherOutline <- Elm.Outline.read (Root.fromFilePath root) True
  case eitherOutline of
    Left problem ->
      Progress.throw
        $ Reporting.Exit.toDetailsReport $
          Reporting.Exit.DetailsBadOutline problem
    Right outline ->
      pure ()

  _ <- Lamdera.touch $ root </> "src" </> "Types.elm"

  if optimized
    then do
      Lamdera.Compile.makeOptimized root ("src" </> "Frontend.elm")
      Lamdera.Compile.makeOptimized root ("src" </> "Backend.elm")
    else do
      Lamdera.Compile.makeDev root ["src" </> "Frontend.elm"]
      Lamdera.Compile.makeDev root ["src" </> "Backend.elm"]


migrationCheck :: FilePath -> VersionInfo -> [(AST.Prim.TypeName.Name, String, String)] -> IO ()
migrationCheck root nextVersion changedTypes = do
  let
    version = vinfoVersion nextVersion
    migrationPath = (root </> "src/Evergreen/Migrate/V") <> show version <> ".elm"
    migrationPathBk = (root </> "src/Evergreen/Migrate/.V") <> show version <> ".elm.bk"

  debug "Type-checking Evergreen migrations..."

  recoverInterruptedCheck migrationPath migrationPathBk version
  ensureSnapshotTypeReferences root

  migrationExists <- Dir.doesFileExist migrationPath

  let
    -- Puts the user's migration file back. Safe to call more than once.
    restoreMigration = do
      bkExists <- Dir.doesFileExist migrationPathBk
      onlyWhen (migrationExists && bkExists) $ do
        copyFile migrationPathBk migrationPath
        remove migrationPathBk

  -- The check rewrites the user's migration file in place: restore it however we leave,
  -- Ctrl-C and exceptions included, or the file is left without its type prefixes.
  flip Control.Exception.onException restoreMigration $ do
    onlyWith migrationPath $ (\migration -> do
        ensureNoFalseClaims migrationPath migration changedTypes
        copyFile migrationPath migrationPathBk
        replaceSnapshotTypeReferences migrationPath version
      )

    let cache = lamderaCache root
    let lamderaCheckBothPath = cache </> "LamderaCheckBoth.elm"
    mkdir cache

    harness <- Lamdera.Evergreen.MigrationHarness.generate root nextVersion
    writeUtf8 lamderaCheckBothPath harness

    let
      cleanup = do
        debug "make_cleanup: Cleaning up build scaffold"
        -- When in debug mode, backup the check file for debugging inspection
        onlyWhen_ Lamdera.isDebug $ copyFile lamderaCheckBothPath (cache </> "LamderaCheckBoth.elm.bk")
        -- Remove our temporary check harness file
        remove lamderaCheckBothPath

        -- Restore backed up unaltered migration file
        restoreMigration

        pure ()

    Dir.withCurrentDirectory root $
      Make.run_cleanup cleanup [ lamderaCheckBothPath ] $
          Make.Flags
            { _debug = False
            , _optimize = False
            , _output = Just (Make.DevNull)
            , _report = Nothing
            , _docs = Nothing
            , _noWire = False
            , _optimizeLegible = False
            , _esm = False
            }

    -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?
    sleep 50 -- 50 milliseconds

    debug "Cleaning up build scaffold"

    -- When in debug mode, backup the check file for debugging inspection
    onlyWhen_ Lamdera.isDebug $ copyFile lamderaCheckBothPath (cache </> "LamderaCheckBoth.elm.bk")
    -- Remove our temporary check harness file
    remove $ root </> lamderaCheckBothPath

    -- Restore backed up unaltered migration file
    restoreMigration


{-| A check killed before it restored the migration file (an older lamdera, or SIGKILL)
leaves the backup behind. Put it back when the file is still exactly the check's rewrite
of it, otherwise the user edited it since: don't guess which one is right.
-}
recoverInterruptedCheck :: FilePath -> FilePath -> Int -> IO ()
recoverInterruptedCheck migrationPath migrationPathBk version = do
  bkM <- readUtf8Text migrationPathBk
  onlyWhen (bkM /= Nothing) $ do
    currentM <- readUtf8Text migrationPath
    case (bkM, currentM) of
      (Just bk, Just current) | current == bk ->
        remove migrationPathBk

      (Just bk, Just current) | current == T.replace ("Evergreen.V" <> show_ version <> ".") "" bk -> do
        debug $ "Restoring " <> migrationPath <> " from an interrupted check"
        copyFile migrationPathBk migrationPath
        remove migrationPathBk

      _ ->
        Progress.throw $
          Help.report "INTERRUPTED CHECK" (Just migrationPath)
            ("An earlier `lamdera check` was interrupted while it had this migration file rewritten, and left the original at " <> migrationPathBk <> ".")
            [ D.reflow "The file has been changed since, so I can't tell which one you want. Compare them, keep the one you want in the migration file, and delete the backup."
            ]


{-| Every type a migration names for its version must be the snapshot's: `Evergreen.V12.Types.BackendModel`,
never the current `Types.BackendModel`. Those are the same types only while that version is the
newest, so the mistake compiles until the next version's chain fails with a confusing type mismatch.
`lamdera check` strips these prefixes while it type-checks, so an editor that saves the file during
a check leaves it this way.
-}
ensureSnapshotTypeReferences :: FilePath -> IO ()
ensureSnapshotTypeReferences root = do
  migrationFiles <- Lamdera.Evergreen.MigrationHarness.findMigrationFilePaths root
  reports <- migrationFiles & mapM (\file -> do
      let
        versionM = file & T.pack & T.stripPrefix "V" >>= T.stripSuffix ".elm" & fmap T.unpack >>= Text.Read.readMaybe
        path = root </> "src/Evergreen/Migrate" </> file
      case versionM of
        Just version -> do
          contentM <- readUtf8Text path
          case contentM of
            Just content -> do
              offending <- currentTypeImports root version content
              pure $ snapshotTypeReferencesReport ("src/Evergreen/Migrate" </> file) version offending
            Nothing -> pure Nothing
        Nothing -> pure Nothing
    )
  Progress.throwMultiple $ Data.Maybe.catMaybes reports


-- The imports of snapshotted modules whose types the migration names without the snapshot prefix,
-- with the references found.
currentTypeImports :: FilePath -> Int -> Text -> IO [(Text, [Text])]
currentTypeImports root version content = do
  let
    -- An import may run over several lines (`exposing` on the next one).
    importDecls =
      content
        & T.lines
        & foldl (\acc line ->
            if "import " `T.isPrefixOf` line then acc ++ [line]
            else if (" " `T.isPrefixOf` line || "\t" `T.isPrefixOf` line) && acc /= [] && "import " `T.isPrefixOf` last acc
              then init acc ++ [last acc <> " " <> line]
            else acc ++ [""]
          ) []
        & filter (T.isPrefixOf "import ")

    parse decl =
      case T.words decl of
        "import" : moduleName : rest ->
          let
            alias = case rest of
              "as" : a : _ -> a
              _ -> moduleName
          in
          Just (moduleName, alias, "exposing" `elem` rest)
        _ -> Nothing

    isRefChar c = Data.Char.isAlphaNum c || c == '_' || c == '.'

    references qualifier =
      T.breakOnAll (qualifier <> ".") content
        & filterMap (\(before, after) ->
            let rest = T.drop (T.length qualifier + 1) after
            in
            if (T.null before || not (isRefChar (T.last before))) && not (T.null rest) && Data.Char.isUpper (T.head rest)
              then Just $ qualifier <> "." <> T.takeWhile (\c -> Data.Char.isAlphaNum c || c == '_') rest
              else Nothing
          )
        & List.nub

  importDecls & filterMap parse & filter (\(m, _, _) -> not ("Evergreen." `T.isPrefixOf` m))
    & mapM (\(moduleName, alias, exposing) -> do
        let snapshotPath = root </> "src/Evergreen" </> ("V" <> show version) </> (T.unpack (T.replace "." "/" moduleName) <> ".elm")
        snapshotted <- Dir.doesFileExist snapshotPath
        let refs = references alias
        pure $
          if snapshotted && (refs /= [] || exposing)
            then Just (moduleName, if refs == [] then ["import " <> moduleName <> " exposing (...)"] else refs)
            else Nothing
      )
    & fmap Data.Maybe.catMaybes


snapshotTypeReferencesReport :: FilePath -> Int -> [(Text, [Text])] -> Maybe Help.Report
snapshotTypeReferencesReport path version offending =
  case offending of
    [] -> Nothing
    (firstModule, _) : _ ->
      let
        v = show version
        snapshot m = "Evergreen.V" <> v <> "." <> T.unpack m
        examples = offending & concatMap snd & take 5 & map (\r -> "    " <> T.unpack r) & List.intercalate "\n"
      in
      Just $ Help.report "MIGRATION USES CURRENT TYPES" (Just path)
        ("This v" <> v <> " migration refers to the current " <> (offending & map (\(m, _) -> "`" <> T.unpack m <> "`") & List.intercalate ", ") <> " instead of the v" <> v <> " snapshot:")
        [ D.vcat [ D.fromChars examples ]
        , D.reflow $ "A migration must name the types snapshotted for v" <> v <> ", e.g. `" <> snapshot firstModule <> ".BackendModel`. The current types only match them while v" <> v <> " is the newest version, so this breaks the next deploy after it."
        , D.reflow $ "Import " <> snapshot firstModule <> " and prefix each of these with `Evergreen.V" <> v <> ".`. If you didn't write them: `lamdera check` drops these prefixes while it runs, so an editor that saved this file during a check can leave it like this."
        ]

ensureNoFalseClaims :: FilePath -> Text -> [(AST.Prim.TypeName.Name, String, String)] -> IO ()
ensureNoFalseClaims migrationPath migration changedTypes = do
  Progress.throwMultiple $ ensureNoFalseClaims_ migrationPath migration changedTypes

ensureNoFalseClaims_ :: FilePath -> Text -> [(AST.Prim.TypeName.Name, String, String)] -> [Help.Report]
ensureNoFalseClaims_ migrationPath migration changedTypes =
  let
    topLevels = extractTopLevelExpressions migration
  in
  -- Ensure that no changed types claim to be ModelUnchanged or MsgUnchanged in the corresponding topLevel migration
  changedTypes & filterMap (\(changedType_, _, _) ->
    let
      changedType = AST.Prim.TypeName.nameToChars changedType_
      name = lowerFirstLetter changedType
      relevantTopLevels = topLevels & filter (T.isPrefixOf (name))
      falseClaims = relevantTopLevels & filter (\topLevel -> "ModelUnchanged" `T.isInfixOf` topLevel || "MsgUnchanged" `T.isInfixOf` topLevel)

      migrationHint =
        if changedType `List.elem` ["BackendModel", "FrontendModel"]
          then "If you're intending to reset, use `ModelReset` to explicitly reset a Model value back to it's `init` value, otherwise implement a migration as normal."
          else "If you're intending to ignore old Msg values, use `MsgOldValueIgnored` to ignore old Msg values during a migration, otherwise implement a migration as normal."

    in
    -- atomicPutStrLn $ show (changedType, name, relevantTopLevels)

    if length falseClaims > 0
      then
        Just $ Help.report "INCORRECT MIGRATION" (Just migrationPath)
          ("The " <> changedType <> " type has changed since last deploy, but the " <> show name <> " migration claims it has not.")
          [ D.reflow migrationHint
          , D.reflow "See <https://dashboard.lamdera.app/docs/evergreen> for more info."
          ]
      else
        Nothing
    )


committedCheck :: FilePath -> VersionInfo -> IO ()
committedCheck root versionInfo = do
  debug $ "Commit-checking migration and types files"

  let
    version = vinfoVersion versionInfo
    migrationPath = (root </> "src/Evergreen/Migrate/V") <> show version <> ".elm"
    typesPath = (root </> "src/Evergreen/V") <> show version <> "/*.elm"

  migrations <- gitStatus migrationPath
  types <- gitStatus typesPath

  let
    missingPaths =
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

    Progress.report $
      Help.reportToDoc $
      Help.report "UNCOMMITTED FILES" (Just "src/Evergreen/")
        ("I need type and migration files to be committed otherwise I cannot deploy!")
        ([ D.reflow "Here is a shortcut:"
         , D.dullyellow (D.reflow $ "git add " <> (List.intercalate " " missingPaths))
         , D.dullyellow (D.reflow $ "git commit -m \"Preparing for v" <> show version <> "\"")
         ]
        )

    addToGitApproved <- Reporting.ask $
      D.stack [ D.reflow $ "Shall I run `git add` for you? [Y/n]: " ]

    if addToGitApproved
      then do
        mapM (\path -> callCommand $ "git add " <> path) missingPaths
        commitApproved <- Reporting.ask $
          D.stack [ D.reflow $ "Shall I run `git commit` for you? [Y/n]: " ]

        if commitApproved
          then do
            callCommand $ "git commit -m \"Preparing for v" <> show version <> "\""
            progress ""

          else
            progress "Okay, I did not commit it."

      else
        progress "Okay, I did not add it."


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


-- Very basic extraction of Elm chunks based on any line
-- that start with a non-space character grouped with any following
-- lines that start with a space character.
extractTopLevelExpressions :: Text -> [Text]
extractTopLevelExpressions input =
    reverse $ foldl groupLines [] (T.lines input)
  where
    groupLines :: [Text] -> Text -> [Text]
    groupLines [] line = [line]
    groupLines acc line
        | isTopLevel line = line : acc
        | otherwise = (T.unlines [T.strip (head acc), line]) : tail acc

    isTopLevel :: Text -> Bool
    isTopLevel line = not (T.null line) && T.head line /= ' '


writeLamderaGenerated :: FilePath -> Bool -> VersionInfo -> IO ()
writeLamderaGenerated root inProduction_ nextVersion =
  onlyWhen inProduction_ $ do
    gen <- Lamdera.Evergreen.MigrationHarness.generate root nextVersion
    writeIfDifferent (root </> "src/LamderaGenerated.elm") gen


showExternalTypeWarnings :: [(Text, [Text], Lamdera.Types.DiffableType)] -> IO ()
showExternalTypeWarnings warnings = do
  let
    textWarnings :: Text
    textWarnings =
      warnings
        & fmap (\(tipe, warnings_, tds) -> warnings_)
        & List.concat
        & List.nub
        & List.sort
        & T.intercalate "\n- "
        & (<>) "- "

  if length warnings > 0
    then do
      progressDoc $
        D.stack
          (
          [ D.dullyellow $ D.reflow $ "WARNING: Evergreen does not cover type changes outside your project yet"
          , D.reflow $ "You are referencing the following in your core types:"
          , D.vcat [ D.fromChars . T.unpack $ textWarnings ]
          , D.dullyellow $ D.reflow $ "Package upgrades that change these types won't get covered by Evergreen migrations currently!"
          , D.reflow "See <https://dashboard.lamdera.app/docs/evergreen> for more info."
          , D.reflow ""
          ]
          )

    else do
      debug "No external type warnings found."
      pure ()


checkGitInitialised :: FilePath -> IO ()
checkGitInitialised root = do
  status <- gitRepoStatus root
  case status of
    GitRepoDir -> pure ()
    GitRepoWorktree _ -> pure ()
    GitRepoWorktreeBroken reason ->
      -- Don't offer `git init` here – that would clobber a worktree's `.git` file.
      Progress.throw
        $ Help.report "BROKEN GIT WORKTREE" (Nothing)
          (reason)
          [ D.reflow "This looks like a git worktree whose `.git` file is malformed or points to a git directory that no longer exists."
          , D.reflow "Try re-creating the worktree, or run `git worktree repair` from the main checkout."
          ]
    GitRepoMissing -> do
      appName <- getInput $
        D.vcat
          [ "It looks like your project is missing a git repository!"
          , "I can initialize it for you."
          , "What is your Lamdera app name? [enter to skip]: "
          ]

      if appName == ""
        then do
          Progress.throw
            $ Help.report "SKIPPING GIT INITIALISATION" (Nothing)
              ("Okay, I'll let you set it up then!")
              [ D.reflow "See <https://dashboard.lamdera.app/docs/building> for more."]
        else do
          progressPointer_ "Initialising git..."
          callCommand $ "cd " <> root <> " && git init"
          let gitAddRemoteCmd = "git remote add lamdera git@apps.lamdera.com:" <> appName <> ".git"
          atomicPutStrLn $ "Adding remote: " <> gitAddRemoteCmd
          callCommand $ "cd " <> root <> " && " <> gitAddRemoteCmd


genericExit :: String -> IO a
genericExit str =
  Progress.throw
    $ Help.report "ERROR" Nothing
      (str)
      []


{-| Derived from Reporting.ask -}
getInput :: D.Doc -> IO String
getInput doc =
  do  Help.toStdout doc
      getInputHelp


getInputHelp :: IO String
getInputHelp =
  do  hFlush stdout
      getLine


progressPointer t = do
  Progress.report $ D.fillSep [ D.fromChars "\n───>", D.blue $ t ]

progressPointer_ t = do
  Progress.report $ D.fillSep [ D.fromChars "───>", D.blue $ t ]

progress t = do
  Progress.report $ D.stack [D.reflow t]

progressDoc d =
  Progress.report $ D.stack [d]
