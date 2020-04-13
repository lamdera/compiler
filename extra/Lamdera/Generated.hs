{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lamdera.Generated where

import Prelude hiding (init)
import qualified System.Directory as Dir
import qualified Data.Text as T
import NeatInterpolation
import Algorithms.NaturalSort
import qualified Data.List as List
import System.FilePath ((</>))
import Control.Monad.Except (catchError, liftIO)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

import Lamdera


createLamderaGenerated :: FilePath -> VersionInfo -> IO Text
createLamderaGenerated root nextVersion = do

  debug_ $ "Reading src/Evergreen/Migrate to determine migrationSequence"

  migrationFilepaths <- safeListDirectory $ root </> "src/Evergreen/Migrate"
  debug_ $ "migrationFilepaths:" <> show migrationFilepaths

  lamderaGenerated nextVersion migrationFilepaths


lamderaGenerated nextVersion migrationFilepaths = do
  migrationSequence <- liftIO $ getMigrationsSequence migrationFilepaths nextVersion `catchError`
    (\err -> do
      debug $ show err
      debug "getMigrationsSequence was empty - that should only be true on v1 deploy"
      pure [[WithMigrations 1]]
    )

  debug_ $ "Migration sequence: " ++ show migrationSequence

  let
    importMigrations = generateImportMigrations migrationSequence

    importTypes :: Text
    importTypes =
      migrationSequence
        & List.head
        & justWithMigrationVersions
        -- & dt "hrm"
        & fmap (\v -> importType v)
        & (\list ->
          case nextVersion of
            WithMigrations _ ->
              case list of
                [] -> []
                _ -> List.init list -- All except last, as we already `import Types as VN`
            WithoutMigrations _ ->
              list
          )
        & T.concat


    importType :: Int -> Text
    importType version =
      let versionT = T.pack $ show version
      in
      [text|import Evergreen.V$versionT.Types as T$versionT|]

    historicMigrations_ = historicMigrations migrationSequence

    nextVersion_ = T.pack $ show $ vinfoVersion nextVersion

  debug_ $ "Generated source for LamderaGenerated"

  pure $ [text|
    module LamderaGenerated exposing (..)

    $importMigrations
    $importTypes
    import Lamdera.Migrations exposing (..)
    import LamderaHelpers exposing (..)
    import Types as T$nextVersion_


    currentVersion : Int
    currentVersion =
        $nextVersion_


    decodeAndUpgrade : Int -> String -> List Int -> UpgradeResult T$nextVersion_.BackendModel T$nextVersion_.FrontendModel T$nextVersion_.FrontendMsg T$nextVersion_.ToBackend T$nextVersion_.BackendMsg T$nextVersion_.ToFrontend
    decodeAndUpgrade version tipe intList =
        case version of
            $historicMigrations_


            $nextVersion_ ->
                case tipe of
                    "BackendModel" ->
                        decodeType "BackendModel" version intList T$nextVersion_.evg_decode_BackendModel
                            |> upgradeIsCurrent CurrentBackendModel
                            |> otherwiseError


                    "FrontendModel" ->
                        decodeType "FrontendModel" version intList T$nextVersion_.evg_decode_FrontendModel
                            |> upgradeIsCurrent CurrentFrontendModel
                            |> otherwiseError


                    "FrontendMsg" ->
                        decodeType "FrontendMsg" version intList T$nextVersion_.evg_decode_FrontendMsg
                            |> upgradeIsCurrent CurrentFrontendMsg
                            |> otherwiseError


                    "ToBackend" ->
                        decodeType "ToBackend" version intList T$nextVersion_.evg_decode_ToBackend
                            |> upgradeIsCurrent CurrentToBackend
                            |> otherwiseError


                    "BackendMsg" ->
                        decodeType "BackendMsg" version intList T$nextVersion_.evg_decode_BackendMsg
                            |> upgradeIsCurrent CurrentBackendMsg
                            |> otherwiseError


                    "ToFrontend" ->
                        decodeType "ToFrontend" version intList T$nextVersion_.evg_decode_ToFrontend
                            |> upgradeIsCurrent CurrentToFrontend
                            |> otherwiseError


                    _ ->
                        UnknownType tipe

            _ ->
                UnknownVersion ( version, tipe, intList )
  |]


generateImportMigrations :: [[VersionInfo]] -> Text
generateImportMigrations migrationSequence =
  case migrationSequence of
    firstMigration:_ ->
      firstMigration
        & (\(_:actualMigrations) ->
            fmap generateImportMigration (justWithMigrationVersions actualMigrations)
          )
        & T.concat

    _ ->
      ""


generateImportMigration :: Int -> Text
generateImportMigration version =
  let versionT = T.pack $ show version
  in
  [text|import Evergreen.Migrate.V$versionT as M$versionT|]






historicMigrations :: [[VersionInfo]] -> Text
historicMigrations migrationSequence =
  migrationSequence
    -- & dt "historicMigratoins:migrationSequence"
    & imap (\i v ->
      historicMigration migrationSequence (i + 1) v
    )
    & (\list ->
        case list of
          [] -> []
          _ -> List.init list -- All except last, as we already `import Types as V[x]`
      )
    & T.intercalate "\n"


historicMigration :: [[VersionInfo]] -> Int -> [VersionInfo] -> Text
historicMigration migrationSequence forVersion migrationsForVersion =
  let
    (startVersion:subsequentVersions) = migrationsForVersion
    types = [ "BackendModel", "FrontendModel", "FrontendMsg", "ToBackend", "BackendMsg", "ToFrontend"]

    typeMigrations =
      types
        & fmap (migrationForType migrationSequence migrationsForVersion startVersion)
        & T.intercalate "\n"

    forVersion_ = T.pack $ show $ forVersion

  in
  [text|
    $forVersion_ ->
        case tipe of
            $typeMigrations

            _ ->
                UnknownType tipe
  |]


migrationForType :: [[VersionInfo]] -> [VersionInfo] -> VersionInfo -> Text -> Text
migrationForType migrationSequence migrationsForVersion startVersion tipe = do

  let
    allMigrations = List.head migrationSequence

    intermediateMigrations =
      migrationsForVersion
        & pairs
        -- & dt "intermediateMigrations:pairs"
        & fmap (\(from,to) -> intermediateMigration allMigrations tipe from to)
        & T.concat

    startVersion_ = T.pack $ show $ vinfoVersion startVersion

  [text|
    "$tipe" ->
        decodeType "$tipe" version intList T$startVersion_.evg_decode_$tipe
            $intermediateMigrations
            |> upgradeSucceeds Current$tipe
            |> otherwiseError
  |]


intermediateMigration :: [VersionInfo] -> Text -> VersionInfo -> VersionInfo -> Text
intermediateMigration allMigrations tipe from to =
  let
    typenameCamel = lowerFirstLetter $ T.unpack tipe

    thenMigrateForType =
      case tipe of
        "BackendModel"  -> "thenMigrateModel"
        "FrontendModel" -> "thenMigrateModel"
        "FrontendMsg"   -> "thenMigrateMsg"
        "ToBackend"     -> "thenMigrateMsg"
        "BackendMsg"    -> "thenMigrateMsg"
        "ToFrontend"    -> "thenMigrateMsg"
    kindForType =
      case tipe of
        "BackendModel"  -> "Model"
        "FrontendModel" -> "Model"
        "FrontendMsg"   -> "Msg"
        "ToBackend"     -> "Msg"
        "BackendMsg"    -> "Msg"
        "ToFrontend"    -> "Msg"

    lastMigrationBefore targetVersion =
      allMigrations
        & List.takeWhile (\v -> vinfoVersion v <= targetVersion)
        -- & dt ("lastMigrationBefore:v" ++ show targetVersion)
        & List.reverse
        & List.find (\v -> case v of
            WithMigrations _ -> True
            WithoutMigrations _ -> False
          )
        & fmap vinfoVersion
        & fromMaybe (-1) -- Should be impossible because v1 is always WithMigrations
  in
  case to of
    WithMigrations v ->
      let from_ = T.pack $ show (vinfoVersion from)
          to_ = T.pack $ show (vinfoVersion to)
          migrationFn = [text|M$to_.$typenameCamel|]
      in
      [text|
        |> $thenMigrateForType "$tipe" $migrationFn T$from_.evg_encode_$tipe T$to_.evg_decode_$tipe $to_
      |]

    WithoutMigrations v ->
      let from_ = T.pack $ show $ lastMigrationBefore (vinfoVersion from)
          to_ = T.pack $ show (vinfoVersion to)
          migrationFn = "(always " <> kindForType <> "Unchanged)"
      in
      [text|
        |> $thenMigrateForType "$tipe" $migrationFn T$from_.evg_encode_$tipe T$to_.evg_decode_$tipe $to_
      |]










data VersionInfo = WithMigrations Int | WithoutMigrations Int deriving (Eq, Show)


justWithMigrationVersions :: [VersionInfo] -> [Int]
justWithMigrationVersions versions =
  versions
    & fmap (\vinfo ->
      case vinfo of
        WithMigrations v -> Just v
        WithoutMigrations v -> Nothing
    )
    & justs


vinfoVersion :: VersionInfo -> Int
vinfoVersion vinfo =
  case vinfo of
    WithMigrations v -> v
    WithoutMigrations v -> v



-- Doesn't need to be in IO, but left so that we can use convenient catchError and debugging
getMigrationsSequence :: [FilePath] -> VersionInfo -> IO [[VersionInfo]]
getMigrationsSequence migrationFilepaths nextVersion =

  -- types <- Dir.listDirectory $ root </> "src/Evergreen/Type"

  let
    toVersionInfo :: Int -> VersionInfo
    toVersionInfo version =
      if List.elem version (migrationVersions migrationFilepaths) then
        WithMigrations version
      else
        WithoutMigrations version

    -- typeVersions :: [Int]
    -- typeVersions =
    --   types
    --     & List.sortBy Algorithms.NaturalSort.compare
    --     & fmap getVersion
    --     & justs -- This is bad? But those files probably aren't VX.elm files?


    sequences :: [[VersionInfo]]
    sequences =
      [1..(vinfoVersion nextVersion)]
        & fmap (\v ->
          -- WithMigrations v
          migrationVersions migrationFilepaths
            & fmap (\v -> WithMigrations v)
            & (\lm ->
                case nextVersion of
                  WithMigrations _ -> lm
                  -- Make sure next version is in the sequence even if it has no migration
                  WithoutMigrations _ -> lm ++ [nextVersion]
              )
            & List.reverse
            & takeWhileInclusive
                (\mv ->
                  (vinfoVersion mv > v)
                  -- || (mv == WithoutMigrations (vinfoVersion mv))
                )
            & List.reverse

              -- & fmap (\l -> [fmap WithMigrations l])

            -- if List.elem v migrationVersions then
            --   WithMigrations v
            -- else
            --   WithoutMigrations v
          )
        -- & scanr (\v acc -> acc ++ [v]) []
        & filter ((/=) [])
        -- & fmap reverse

  in
  pure sequences


migrationVersions :: [FilePath] -> [Int]
migrationVersions migrationFilepaths =
  migrationFilepaths
    -- There will never be a V1 migration but it is always
    -- the first version in our sequence
    & (++) ["V1.elm"]
    & List.sortBy Algorithms.NaturalSort.compare
    & fmap getVersion
    & justs -- This is bad? But those files probably aren't VX.elm files?


takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)


getLastLocalTypeChangeVersion :: FilePath -> IO Int
getLastLocalTypeChangeVersion root = do

  migrationFilepaths <- Dir.listDirectory $ root </> "src/Evergreen/Migrate"
  if migrationFilepaths == []
    then
      pure 1
    else do
      migrationFilepaths
        & List.sortBy Algorithms.NaturalSort.compare
        & List.last
        & getVersion
        & fromMaybe 1 -- If there are no migrations or it failed, it must be version 1?
        & pure
