{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module Lamdera.Evergreen.MigrationHarness where

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
import Data.List

import Lamdera
import qualified Ext.ElmFormat


generate :: FilePath -> VersionInfo -> IO Text
generate root nextVersion = do
  migrationFilepaths <- findMigrationFilePaths root
  generateFor nextVersion migrationFilepaths


findMigrationFilePaths :: FilePath -> IO [FilePath]
findMigrationFilePaths root = do
  debug_ $ "Reading src/Evergreen/Migrate to determine migrationSequence"
  paths <- safeListDirectory $ root </> "src/Evergreen/Migrate"
  let migrationFilePaths = paths & filter (\p -> ".elm" `isSuffixOf` p)
  debug_ $ "migrationFilePaths:" <> show migrationFilePaths
  pure migrationFilePaths


generateFor :: VersionInfo -> [FilePath] -> IO Text
generateFor nextVersion migrationFilepaths = do

  let migrationsLimit = 3

  migrationSequence <- liftIO $ getMigrationsSequence migrationFilepaths nextVersion migrationsLimit `catchError`
    (\err -> do
      debug $ show err
      debug "getMigrationsSequence was empty - that should only be true on v1 deploy"
      pure [(1,[WithMigrations 1])]
    )

  debug_ $ "Migration sequence: " ++ show migrationSequence
  debug_ $ "Next version: " ++ show nextVersion

  let
    decodeAndUpgrades =
      coreTypes
        & fmap (\tipe -> decodeAndUpgradeFor migrationSequence nextVersion tipe)
        & T.intercalate "\n\n"

    nextVersion_ = show_ $ vinfoVersion nextVersion

    imports =
      (migrationImports migrationSequence
        <> typeImports migrationSequence nextVersion
        <> [ "import Lamdera.Migrations exposing (..)"
           , "import Lamdera.Wire3 exposing (Bytes, Decoder, Encoder, bytesDecode, bytesEncode)"
           , "import Types as T" <> nextVersion_
           ]
      )
        & filter ((/=) "")
        & sort
        & T.intercalate "\n"

  supportingCode <- genSupportingCode

  debug_ $ "Generated source for LamderaGenerated"

  let
    final =
      [text|
        module LamderaGenerated exposing (..)

        $imports

        $supportingCode

        currentVersion : Int
        currentVersion =
            $nextVersion_


        $decodeAndUpgrades

      |]

  pure $ Ext.ElmFormat.formatOrPassthrough final



decodeAndUpgradeFor migrationSequence nextVersion valueType = do
  let
    nextVersion_ = show_ $ vinfoVersion nextVersion

    historicMigrations_ = historicMigrations migrationSequence nextVersion valueType

    cmdMsgType =
      case valueType of
        "BackendModel"  -> "BackendMsg"
        "FrontendModel" -> "FrontendMsg"
        "FrontendMsg"   -> "FrontendMsg"
        "ToBackend"     -> "BackendMsg"
        "BackendMsg"    -> "BackendMsg"
        "ToFrontend"    -> "FrontendMsg"
        _               ->
          error $ "Evergreen.Generation: impossible value type: " <> show valueType

    valueTypeInt = show_ $ tipeStringToInt valueType

    caseAll =
      if historicMigrations_ /= "" then
        [ historicMigrations migrationSequence nextVersion valueType
        , caseNext
        ] & T.intercalate "\n\n"
      else
        caseNext

    caseNext =
      if valueType == "BackendModel" then
        [text|
            $nextVersion_ ->
                decodeType $valueTypeInt version bytes T$nextVersion_.w3_decode_$valueType
                    |> fallback (\_ -> decodeType $valueTypeInt version bytes T$nextVersion_.w2_decode_$valueType)
                    |> upgradeIsCurrent
                    |> otherwiseError
        |]
      else
        [text|
            $nextVersion_ ->
                decodeType $valueTypeInt version bytes T$nextVersion_.w3_decode_$valueType
                    |> upgradeIsCurrent
                    |> otherwiseError
        |]


  [text|
    decodeAndUpgrade$valueType : Int -> Bytes -> UpgradeResult T$nextVersion_.$valueType T$nextVersion_.$cmdMsgType
    decodeAndUpgrade$valueType version bytes =
        case version of
            $caseAll

            _ ->
                UnknownVersion ( version, "$valueType", bytes )
  |]


typeImports :: [(a, [VersionInfo])] -> VersionInfo -> [Text]
typeImports migrationSequence nextVersion =
  migrationSequence
    & List.head
    & (\(v,l) -> justWithMigrationVersions l)
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


importType :: Int -> Text
importType version =
  let version_ = show_ version
  in
  "import Evergreen.V" <> version_ <> ".Types as T" <> version_


migrationImports :: [(Int, [VersionInfo])] -> [Text]
migrationImports migrationSequence =
  case migrationSequence of
    firstMigration:_ ->
      firstMigration
        & (\(v, (_:actualMigrations)) ->
            fmap generateImportMigration (justWithMigrationVersions actualMigrations)
          )

    _ ->
      []


generateImportMigration :: Int -> Text
generateImportMigration version =
  let version_ = show_ version
  in
  "import Evergreen.Migrate.V" <> version_ <> " as M" <> version_


-- Generate the migration funnel for the set of consequetive
-- app versions leading up to the current version.
historicMigrations :: [(Int,[VersionInfo])] -> VersionInfo -> Text -> Text
historicMigrations migrationSequence finalVersion tipe =
  migrationSequence
    -- & dt "historicMigratoins:migrationSequence"
    & fmap (\(forVersion, migrationsForVersion) ->
      historicMigration migrationSequence forVersion migrationsForVersion finalVersion tipe
    )
    & (\list ->
        case list of
          [] -> []
          _ -> List.init list -- All except last, as we already `import Types as V[x]`
      )
    & T.intercalate "\n"


coreTypes = [ "BackendModel", "FrontendModel", "FrontendMsg", "ToBackend", "BackendMsg", "ToFrontend"]


-- Generate migration funnel for a single app version entrypoint
historicMigration :: [(Int, [VersionInfo])] -> Int -> [VersionInfo] -> VersionInfo -> Text -> Text
historicMigration migrationSequence forVersion migrationsForVersion finalVersion tipe =
  let
    (startVersion:subsequentVersions) = migrationsForVersion

    typeMigration =
      migrationForType migrationSequence migrationsForVersion startVersion finalVersion tipe

    forVersion_ = show_ $ forVersion

  in
  [text|
    $forVersion_ ->
        $typeMigration
  |]


migrationForType :: [(Int, [VersionInfo])] -> [VersionInfo] -> VersionInfo -> VersionInfo -> Text ->  Text
migrationForType migrationSequence migrationsForVersion startVersion finalVersion tipe = do

  let
    -- !_ = debugHaskell "migrationForType" (migrationSequence, migrationsForVersion, startVersion, tipe)

    (v, allMigrations) = List.head migrationSequence

    intermediateMigrations =
      migrationsForVersion
        & pairs
        -- & dt "intermediateMigrations:pairs"

    intermediateMigrationsFormatted =
      intermediateMigrations
        & fmap (\(from,to) -> intermediateMigration allMigrations tipe from to finalVersion)
        & T.intercalate "\n"

    startVersion_ = show_ $ vinfoVersion startVersion
    finalVersion_ = show_ $ vinfoVersion finalVersion

    valueTypeInt = show_ $ tipeStringToInt tipe

    decodeAsLatest =
      if tipe == "BackendModel"
        then
          [text|
            decodeType $valueTypeInt $finalVersion_ bytes T$finalVersion_.w3_decode_$tipe
                |> fallback (\_ -> decodeType $valueTypeInt $finalVersion_ bytes T$finalVersion_.w2_decode_$tipe)
                |> upgradeSucceeds
                |> otherwiseError
          |]
        else
          [text|
            decodeType $valueTypeInt $finalVersion_ bytes T$finalVersion_.w3_decode_$tipe
                |> upgradeSucceeds
                |> otherwiseError
          |]

    migrateNext =
      if tipe == "BackendModel"
        then
          [text|
            decodeType $valueTypeInt $startVersion_ bytes T$startVersion_.w3_decode_$tipe
                |> fallback (\_ -> decodeType $valueTypeInt $startVersion_ bytes T$startVersion_.w2_decode_$tipe)
                $intermediateMigrationsFormatted
                |> upgradeSucceeds
                |> otherwiseError
          |]
        else
          [text|
            decodeType $valueTypeInt $startVersion_ bytes T$startVersion_.w3_decode_$tipe
                $intermediateMigrationsFormatted
                |> upgradeSucceeds
                |> otherwiseError
          |]

  case intermediateMigrations of
    single:[] ->
      case single of
        (from, WithoutMigrations to) ->
          -- Our migration consists of a prior migration point, to the latest
          -- unchanged point. This means we can skip some work and just decode as
          -- the latest right away.
          decodeAsLatest

        _ ->
          migrateNext

    _ ->
      migrateNext


intermediateMigration :: [VersionInfo] -> Text -> VersionInfo -> VersionInfo -> VersionInfo -> Text
intermediateMigration allMigrations tipe from to finalVersion =
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

    valueTypeInt = show_ $ tipeStringToInt tipe
  in
  case to of
    WithMigrations v ->
      let from_ = show_ (vinfoVersion from)
          to_ = show_ (vinfoVersion to)
          migrationFn = [text|M$to_.$typenameCamel|]
      in
      [text|
        |> $thenMigrateForType $valueTypeInt $migrationFn T$from_.w3_encode_$tipe T$to_.w3_decode_$tipe $to_
      |]

    WithoutMigrations v ->
      {- It might seem like this is uneeded, but it's for when there's a migration in our chain, yet
         the last version has no migrations. I.e.:

         decodeType "BackendModel" 1 intList T1.w3_decode_BackendModel
             |> thenMigrateModel "BackendModel" M2.backendModel T1.w3_encode_BackendModel T2.w3_decode_BackendModel 2
             |> thenMigrateModel "BackendModel" (always ModelUnchanged) T2.w3_encode_BackendModel T3.w3_decode_BackendModel 3
             |> upgradeSucceeds CurrentBackendModel
             |> otherwiseError

      -}
      let from_ = show_ $ lastMigrationBefore (vinfoVersion from)
          to_ = show_ (vinfoVersion to)
          migrationFn = "(always " <> kindForType <> "Unchanged)"
      in
      [text|
        |> $thenMigrateForType $valueTypeInt $migrationFn T$from_.w3_encode_$tipe T$to_.w3_decode_$tipe $to_
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
getMigrationsSequence :: [FilePath] -> VersionInfo -> Int -> IO [(Int,[VersionInfo])]
getMigrationsSequence migrationFilepaths nextVersion depth =

  let
    versions = migrationVersions migrationFilepaths

    toVersionInfo :: Int -> VersionInfo
    toVersionInfo version =
      if List.elem version versions then
        WithMigrations version
      else
        WithoutMigrations version

    sequences :: [(Int,[VersionInfo])]
    sequences =
      [1..(vinfoVersion nextVersion)]
        & fmap (\v ->
          versions
            & fmap (\mv -> WithMigrations mv)
            & (\lm ->
                case nextVersion of
                  WithMigrations _ -> lm
                  -- Make sure next version is in the sequence even if it has no migration
                  WithoutMigrations _ -> lm ++ [nextVersion]
              )
            & List.reverse
            & takeWhileInclusive
                (\mv -> vinfoVersion mv > v)
            & List.reverse
            & (\m -> (v,m))
          )
        & filter (\(v,l) -> l /= [])
        & lastN depth
  in
  pure sequences


lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const . drop 1) xs (drop n xs)


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


tipeStringToInt :: Text -> Int
tipeStringToInt tipe =
  case tipe of
    "FrontendMsg"   -> 0
    "ToBackend"     -> 1
    "BackendMsg"    -> 2
    "ToFrontend"    -> 3
    "FrontendModel" -> 4
    "BackendModel"  -> 5
    _               ->
      error $ "Evergreen.Generation: unsupported type: " <> show tipe


getLastLocalTypeChangeVersion :: FilePath -> IO Int
getLastLocalTypeChangeVersion root = do
  migrationFilepaths <- findMigrationFilePaths root

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



genSupportingCode = do

  inProduction_ <- Lamdera.inProduction

  if inProduction_
    then
      -- In production, use shared injected code that the LBR/LFR runtime harnesses
      -- also reference to share types and type check everything together
      pure "import LamderaHelpers exposing (..)\n"
    else
      -- In development, we aren't building with the harnesses, so rather than an extra
      -- file dependency, just inject the additional helpers we need to type check our migrations
      pure [text|

        type UpgradeResult valueType msgType
            = AlreadyCurrent ( valueType, Cmd msgType )
            | Upgraded ( valueType, Cmd msgType )
            | UnknownVersion ( Int, String, Bytes )
            | UnknownType String
            | DecoderError String


        thenMigrateModel :
            Int
            -> (oldModel -> ModelMigration newModel msgN)
            -> (oldModel -> Encoder)
            -> Decoder newModel
            -> Int
            -> Result String ( oldModel, Cmd msgO )
            -> Result String ( newModel, Cmd msgN )
        thenMigrateModel tipe migrationFn oldEncoder newDecoder newVersion priorResult =
            priorResult
                |> Result.andThen (\( oldValue, _ ) -> migrateModelValue tipe migrationFn oldValue oldEncoder newDecoder newVersion)


        thenMigrateMsg :
            Int
            -> (oldMsg -> MsgMigration newMsg msgN)
            -> (oldMsg -> Encoder)
            -> Decoder newMsg
            -> Int
            -> Result String ( oldMsg, Cmd msgO )
            -> Result String ( newMsg, Cmd msgN )
        thenMigrateMsg tipe migrationFn oldEncoder newDecoder newVersion priorResult =
            priorResult
                |> Result.andThen (\( oldValue, _ ) -> migrateMsgValue tipe migrationFn oldValue oldEncoder newDecoder newVersion)


        upgradeSucceeds : Result String ( newModel, Cmd msg ) -> Result String (UpgradeResult newModel msg)
        upgradeSucceeds priorResult =
            priorResult |> Result.map Upgraded


        upgradeIsCurrent : Result String ( newModel, Cmd msg ) -> Result String (UpgradeResult newModel msg)
        upgradeIsCurrent priorResult =
            priorResult |> Result.map AlreadyCurrent


        fallback : (() -> Result String ( newModel, Cmd msg )) -> Result String ( newModel, Cmd msg ) -> Result String ( newModel, Cmd msg )
        fallback res priorResult =
            case priorResult of
                Ok v ->
                    Ok v

                Err _ ->
                    res ()


        otherwiseError : Result String (UpgradeResult valueType msgType) -> UpgradeResult valueType msgType
        otherwiseError priorResult =
            case priorResult of
                Ok upgradeResult ->
                    upgradeResult

                Err err ->
                    DecoderError err


        migrateModelValue :
            Int
            -> (oldModel -> ModelMigration newModel msg)
            -> oldModel
            -> (oldModel -> Encoder)
            -> Decoder newModel
            -> Int
            -> Result String ( newModel, Cmd msg )
        migrateModelValue tipe migrationFn oldValue oldEncoder newDecoder newVersion =
            case migrationFn oldValue of
                ModelMigrated ( newValue, cmds ) ->
                    Ok ( newValue, cmds )

                ModelUnchanged ->
                    oldValue
                        |> oldEncoder
                        |> bytesEncode
                        |> (\bytes -> decodeType tipe newVersion bytes newDecoder)


        migrateMsgValue :
            Int
            -> (oldMsg -> MsgMigration newMsg msg)
            -> oldMsg
            -> (oldMsg -> Encoder)
            -> Decoder newMsg
            -> Int
            -> Result String ( newMsg, Cmd msg )
        migrateMsgValue tipe migrationFn oldValue oldEncoder newDecoder newVersion =
            case migrationFn oldValue of
                MsgMigrated ( newValue, cmds ) ->
                    Ok ( newValue, cmds )

                MsgUnchanged ->
                    oldValue
                        |> oldEncoder
                        |> bytesEncode
                        |> (\bytes -> decodeType tipe newVersion bytes newDecoder)

                MsgOldValueIgnored ->
                    Err <| "Migration for " ++ tipeIntToString tipe ++ "." ++ String.fromInt (newVersion - 1) ++ "->" ++ String.fromInt newVersion ++ " ignores old values"


        decodeType : Int -> Int -> Bytes -> Decoder oldValue -> Result String ( oldValue, Cmd msg )
        decodeType tipe version bytes decoderOld =
            case bytesDecode decoderOld bytes of
                Just oldValue ->
                    Ok ( oldValue, Cmd.none )

                Nothing ->
                    -- This would mean data has been manipulated or corrupted,
                    -- or we have a bug in our encoder/decoder generation
                    Err <| "[decodeType] the impossible happened; failed to decode " ++ tipeIntToString tipe ++ ".v" ++ String.fromInt version


        tipeIntToString tipe =
            case tipe of
                0 ->
                    "FrontendMsg"

                1 ->
                    "ToBackend"

                2 ->
                    "BackendMsg"

                3 ->
                    "ToFrontend"

                4 ->
                    "FrontendModel"

                5 ->
                    "BackendModel"

                _ ->
                    "tipeIntToString FAILED"

      |]
