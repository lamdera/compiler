{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.CLI.Check where

import Control.Monad.Except (catchError, throwError, forM_, when)
import Data.Maybe (fromMaybe)
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

import Lamdera
import qualified Lamdera.Version


import qualified Lamdera.AppConfig
import qualified Lamdera.Checks
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
    }


run_ :: IO ()
run_ = run () (Lamdera.CLI.Check.Flags { _destructiveMigration = False, _force = False })


run :: () -> Lamdera.CLI.Check.Flags -> IO ()
run () flags@(Lamdera.CLI.Check.Flags destructiveMigration force) = do
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
runHelp () flags@(Lamdera.CLI.Check.Flags destructiveMigration force) = do
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
        else onlineCheck root appName inDebug localTypes externalTypeWarnings isHoistRebuild forceVersion forceNotProd inProduction_ destructiveMigration

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


onlineCheck root appName inDebug localTypes externalTypeWarnings isHoistRebuild forceVersion forceNotProd inProduction_ destructiveMigration = do

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
                        Lamdera.Evergreen.MigrationGenerator.betweenVersions typeCompares lastLocalTypeChangeVersion nextVersion root
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
  (prodVersion, productionTypes) <-
    if isHoistRebuild
      then do
        if (forceVersion == (-1))
          then
            genericExit "ERROR: Hoist rebuild got -1 for version, check your usage."

          else do
            debug_ $ "❗️Gen with forced version: " <> show forceVersion
            pure (forceVersion, localTypes)

      else do

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
        }

    Lamdera.AppConfig.writeUsage


replaceSnapshotTypeReferences migrationPath version = do
  debug $ "Replacing " <> "Evergreen.V" <> show version <> " type references in " <> migrationPath
  replaceInFile ("Evergreen.V" <> show_ version <> ".") "" migrationPath


fetchProductionInfo :: Text -> Bool -> IO (Either Lamdera.Http.Error (Int, [Text]))
fetchProductionInfo appName useLocal =
  let
    endpoint =
      if (textContains "-local" appName && ostype == MacOS) || useLocal
        then
          -- "https://" <> T.unpack appName <> ".lamdera.test/_i"
          "http://" <> T.unpack appName <> ".lamdera.test/_i"

        else
          "https://" <> T.unpack appName <> ".lamdera.app/_i"

    decoder =
      D.succeed (,)
        & D.required "v" D.int
        & D.required "h" (D.list D.text)
  in
  Lamdera.Http.normalJson "lamdera-info" endpoint decoder


checkUserProjectCompiles :: FilePath -> Bool -> IO ()
checkUserProjectCompiles root optimized = do

  -- Make sure we catch project outline issues first
  eitherOutline <- Elm.Outline.read root True
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


migrationCheck :: FilePath -> VersionInfo -> [(Elm.ModuleName.Raw, String, String)] -> IO ()
migrationCheck root nextVersion changedTypes = do
  let
    version = vinfoVersion nextVersion
    migrationPath = (root </> "src/Evergreen/Migrate/V") <> show version <> ".elm"
    migrationPathBk = (root </> "src/Evergreen/Migrate/.V") <> show version <> ".elm.bk"

  debug "Type-checking Evergreen migrations..."

  migrationExists <- Dir.doesFileExist migrationPath

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
      onlyWhen migrationExists $ do
        copyFile migrationPathBk migrationPath
        remove migrationPathBk

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
          }

  -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?
  sleep 50 -- 50 milliseconds

  debug "Cleaning up build scaffold"

  -- When in debug mode, backup the check file for debugging inspection
  onlyWhen_ Lamdera.isDebug $ copyFile lamderaCheckBothPath (cache </> "LamderaCheckBoth.elm.bk")
  -- Remove our temporary check harness file
  remove $ root </> lamderaCheckBothPath

  -- Restore backed up unaltered migration file
  onlyWhen migrationExists $ do
    copyFile migrationPathBk migrationPath
    remove migrationPathBk

ensureNoFalseClaims :: FilePath -> Text -> [(Elm.ModuleName.Raw, String, String)] -> IO ()
ensureNoFalseClaims migrationPath migration changedTypes = do
  Progress.throwMultiple $ ensureNoFalseClaims_ migrationPath migration changedTypes

ensureNoFalseClaims_ :: FilePath -> Text -> [(Elm.ModuleName.Raw, String, String)] -> [Help.Report]
ensureNoFalseClaims_ migrationPath migration changedTypes =
  let
    topLevels = extractTopLevelExpressions migration
  in
  -- Ensure that no changed types claim to be ModelUnchanged or MsgUnchanged in the corresponding topLevel migration
  changedTypes & filterMap (\(changedType_, _, _) ->
    let
      changedType = Elm.ModuleName.toChars changedType_
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
        ("I need type and migration files to be comitted otherwise I cannot deploy!")
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
  gitInitialised <- Dir.doesDirectoryExist $ root </> ".git"
  onlyWhen (not gitInitialised) $ do
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
