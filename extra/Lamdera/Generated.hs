{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

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
import Data.List

import Lamdera


createLamderaGenerated :: FilePath -> VersionInfo -> IO Text
createLamderaGenerated root nextVersion = do

  debug_ $ "Reading src/Evergreen/Migrate to determine migrationSequence"

  migrationFilepaths <- safeListDirectory $ root </> "src/Evergreen/Migrate"
  debug_ $ "migrationFilepaths:" <> show migrationFilepaths

  lamderaGenerated nextVersion migrationFilepaths


lamderaGenerated nextVersion migrationFilepaths = do
  migrationSequence <- liftIO $ getMigrationsSequence migrationFilepaths nextVersion 3 `catchError`
    (\err -> do
      debug $ show err
      debug "getMigrationsSequence was empty - that should only be true on v1 deploy"
      pure [(1,[WithMigrations 1])]
    )

  debug_ $ "Migration sequence: " ++ show migrationSequence
  debug_ $ "Next version: " ++ show nextVersion

  let
    importMigrations = generateImportMigrations migrationSequence

    importTypes_ = importTypes migrationSequence nextVersion

    historicMigrations_ = historicMigrations migrationSequence nextVersion

    nextVersion_ = show_ $ vinfoVersion nextVersion

  debug_ $ "Generated source for LamderaGenerated"

  pure $ [text|
    module LamderaGenerated exposing (..)

    $importMigrations
    $importTypes_
    import Lamdera.Migrations exposing (..)
    import Types as T$nextVersion_
    import Lamdera.Wire2 exposing (Decoder, Encoder, bytesDecode, bytesEncode, intListFromBytes, intListToBytes)


    type CurrentType backendModel frontendModel frontendMsg toBackend backendMsg toFrontend
        = CurrentBackendModel ( backendModel, Cmd backendMsg )
        | CurrentFrontendModel ( frontendModel, Cmd frontendMsg )
        | CurrentFrontendMsg ( frontendMsg, Cmd frontendMsg )
        | CurrentToBackend ( toBackend, Cmd backendMsg )
        | CurrentBackendMsg ( backendMsg, Cmd backendMsg )
        | CurrentToFrontend ( toFrontend, Cmd frontendMsg )


    type UpgradeResult backendModel frontendModel frontendMsg toBackend backendMsg toFrontend
        = AlreadyCurrent (CurrentType backendModel frontendModel frontendMsg toBackend backendMsg toFrontend)
        | Upgraded (CurrentType backendModel frontendModel frontendMsg toBackend backendMsg toFrontend)
        | UnknownVersion ( Int, String, List Int )
        | UnknownType String
        | DecoderError String


    thenMigrateModel :
        String
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
        String
        -> (oldMsg -> MsgMigration newMsg msgN)
        -> (oldMsg -> Encoder)
        -> Decoder newMsg
        -> Int
        -> Result String ( oldMsg, Cmd msgO )
        -> Result String ( newMsg, Cmd msgN )
    thenMigrateMsg tipe migrationFn oldEncoder newDecoder newVersion priorResult =
        priorResult
            |> Result.andThen (\( oldValue, _ ) -> migrateMsgValue tipe migrationFn oldValue oldEncoder newDecoder newVersion)


    upgradeSucceeds tagger priorResult =
        priorResult
            |> Result.andThen (\( value, cmds ) -> Ok <| Upgraded (tagger ( value, cmds )))


    upgradeIsCurrent tagger priorResult =
        priorResult
            |> Result.andThen (\( value, cmds ) -> Ok <| AlreadyCurrent (tagger ( value, cmds )))


    otherwiseError : Result String (UpgradeResult backendModel frontendModel frontendMsg toBackend backendMsg toFrontend) -> UpgradeResult backendModel frontendModel frontendMsg toBackend backendMsg toFrontend
    otherwiseError priorResult =
        case priorResult of
            Ok upgradeResult ->
                upgradeResult

            Err err ->
                DecoderError err


    migrateModelValue :
        String
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
                -- @TODO technically getting here should mean we can unsafeCoerce the value,
                -- however in the spirit of skepticism (as users could potentially screw with values)
                -- we'll instead attempt to decode in the latest version...
                oldValue
                    |> oldEncoder
                    |> bytesEncode
                    |> intListFromBytes
                    |> (\intList -> decodeType tipe newVersion intList newDecoder)


    migrateMsgValue :
        String
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
                -- @TODO technically getting here should mean we can unsafeCoerce the value,
                -- however in the spirit of skepticism (as users could potentially screw with values)
                -- we'll instead attempt to decode in the latest version...
                oldValue
                    |> oldEncoder
                    |> bytesEncode
                    |> intListFromBytes
                    |> (\intList -> decodeType tipe newVersion intList newDecoder)

            MsgOldValueIgnored ->
                Err <| "Migration for " ++ tipe ++ "." ++ String.fromInt (newVersion - 1) ++ "->" ++ String.fromInt newVersion ++ " ignores old values"


    {-| -}
    decodeType : String -> Int -> List Int -> Decoder oldValue -> Result String ( oldValue, Cmd msg )
    decodeType tipe version intList decoderOld =
        case bytesDecode decoderOld (intListToBytes intList) of
            Just oldValue ->
                Ok ( oldValue, Cmd.none )

            Nothing ->
                -- This would mean data has been manipulated or corrupted,
                -- or we have a bug in our encoder/decoder generation
                Err <| "[decodeType] the impossible happened; failed to decode " ++ tipe ++ ".v" ++ String.fromInt version ++ " intlist length " ++ (String.fromInt <| List.length intList)


    intlistToString i =
        i |> List.map String.fromInt |> String.join ","



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
                        decodeType "BackendModel" version intList T$nextVersion_.w2_decode_BackendModel
                            |> upgradeIsCurrent CurrentBackendModel
                            |> otherwiseError


                    "FrontendModel" ->
                        decodeType "FrontendModel" version intList T$nextVersion_.w2_decode_FrontendModel
                            |> upgradeIsCurrent CurrentFrontendModel
                            |> otherwiseError


                    "FrontendMsg" ->
                        decodeType "FrontendMsg" version intList T$nextVersion_.w2_decode_FrontendMsg
                            |> upgradeIsCurrent CurrentFrontendMsg
                            |> otherwiseError


                    "ToBackend" ->
                        decodeType "ToBackend" version intList T$nextVersion_.w2_decode_ToBackend
                            |> upgradeIsCurrent CurrentToBackend
                            |> otherwiseError


                    "BackendMsg" ->
                        decodeType "BackendMsg" version intList T$nextVersion_.w2_decode_BackendMsg
                            |> upgradeIsCurrent CurrentBackendMsg
                            |> otherwiseError


                    "ToFrontend" ->
                        decodeType "ToFrontend" version intList T$nextVersion_.w2_decode_ToFrontend
                            |> upgradeIsCurrent CurrentToFrontend
                            |> otherwiseError


                    _ ->
                        UnknownType tipe

            _ ->
                UnknownVersion ( version, tipe, intList )
  |]


importTypes migrationSequence nextVersion =
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
    & T.concat


importType :: Int -> Text
importType version =
  let versionT = show_ version
  in
  [text|import Evergreen.V$versionT.Types as T$versionT|]

generateImportMigrations :: [(Int, [VersionInfo])] -> Text
generateImportMigrations migrationSequence =
  case migrationSequence of
    firstMigration:_ ->
      firstMigration
        & (\(v, (_:actualMigrations)) ->
            fmap generateImportMigration (justWithMigrationVersions actualMigrations)
          )
        & T.concat

    _ ->
      ""


generateImportMigration :: Int -> Text
generateImportMigration version =
  let versionT = show_ version
  in
  [text|import Evergreen.Migrate.V$versionT as M$versionT|]


-- Generate the migration funnel for the set of consequetive
-- app versions leading up to the current version.
historicMigrations :: [(Int,[VersionInfo])] -> VersionInfo -> Text
historicMigrations migrationSequence finalVersion =
  migrationSequence
    -- & dt "historicMigratoins:migrationSequence"
    & fmap (\(forVersion, migrationsForVersion) ->
      historicMigration migrationSequence forVersion migrationsForVersion finalVersion
    )
    & (\list ->
        case list of
          [] -> []
          _ -> List.init list -- All except last, as we already `import Types as V[x]`
      )
    & T.intercalate "\n"


-- Generate migration funnel for a single app version entrypoint
historicMigration :: [(Int, [VersionInfo])] -> Int -> [VersionInfo] -> VersionInfo -> Text
historicMigration migrationSequence forVersion migrationsForVersion finalVersion =
  let
    (startVersion:subsequentVersions) = migrationsForVersion
    types = [ "BackendModel", "FrontendModel", "FrontendMsg", "ToBackend", "BackendMsg", "ToFrontend"]

    typeMigrations =
      types
        & fmap (migrationForType migrationSequence migrationsForVersion startVersion finalVersion)
        & T.intercalate "\n"

    forVersion_ = show_ $ forVersion

  in
  [text|
    $forVersion_ ->
        case tipe of
            $typeMigrations

            _ ->
                UnknownType tipe
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
        & T.concat

    startVersion_ = show_ $ vinfoVersion startVersion
    finalVersion_ = show_ $ vinfoVersion finalVersion

    decodeAsLatest =
      [text|
        "$tipe" ->
            decodeType "$tipe" $finalVersion_ intList T$finalVersion_.w2_decode_$tipe
                |> upgradeSucceeds Current$tipe
                |> otherwiseError
      |]

    migrateNext =
      [text|
        "$tipe" ->
            decodeType "$tipe" $startVersion_ intList T$startVersion_.w2_decode_$tipe
                $intermediateMigrationsFormatted
                |> upgradeSucceeds Current$tipe
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
  in
  case to of
    WithMigrations v ->
      let from_ = show_ (vinfoVersion from)
          to_ = show_ (vinfoVersion to)
          migrationFn = [text|M$to_.$typenameCamel|]
      in
      [text|
        |> $thenMigrateForType "$tipe" $migrationFn T$from_.w2_encode_$tipe T$to_.w2_decode_$tipe $to_
      |]

    WithoutMigrations v ->
      {- It might seem like this is uneeded, but it's for when there's a migration in our chain, yet
         the last version has no migrations. I.e.:

         decodeType "BackendModel" 1 intList T1.w2_decode_BackendModel
             |> thenMigrateModel "BackendModel" M2.backendModel T1.w2_encode_BackendModel T2.w2_decode_BackendModel 2
             |> thenMigrateModel "BackendModel" (always ModelUnchanged) T2.w2_encode_BackendModel T3.w2_decode_BackendModel 3
             |> upgradeSucceeds CurrentBackendModel
             |> otherwiseError

      -}
      let from_ = show_ $ lastMigrationBefore (vinfoVersion from)
          to_ = show_ (vinfoVersion to)
          migrationFn = "(always " <> kindForType <> "Unchanged)"
      in
      [text|
        |> $thenMigrateForType "$tipe" $migrationFn T$from_.w2_encode_$tipe T$to_.w2_decode_$tipe $to_
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
