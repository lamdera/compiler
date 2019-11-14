{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
import System.Process
import Control.Monad (unless, filterM)
import qualified System.IO as IO



run :: () -> () -> IO ()
run () () =
  do  reporter <- Terminal.create

      Task.run reporter $ do
        liftIO $ putStrLn "Checking project compiles..."

        checkUserProjectCompiles

        gitRemotes <- liftIO $ readProcess "git" ["remote", "-v"] ""

        let lamderaRemotes =
              gitRemotes
                & T.pack
                & T.splitOn "\n"
                & filter (textContains "lamdera")

        unless (lamderaRemotes /= []) $
          Task.throw $ Exit.Lamdera
            $ Help.report "UNKNOWN APP" (Just "git remote -v")
              ("I cannot figure out which Lamdera app this repository belongs to.")
              ([ D.reflow "I normally look for a git remote labelled 'lamdera' but did not find one."
               , D.reflow "See <https://lamdera.com/app-setup> for more info."
               ]
              )

        let appName =
              List.head lamderaRemotes
                & T.splitOn ":"
                & (\l ->
                    case l of
                      f:second:_ -> second
                  )
                & T.splitOn "."
                & List.head

        (prodVersion, productionTypes) <-
            fetchProductionInfo appName `catchError` (\err -> pure (0, []))

        let nextVersion = (prodVersion + 1)

        if nextVersion == 0
          then
            Task.report Progress.LamderaCannotCheckRemote

          else if nextVersion == 1 then do
            -- This is the first version, we don't need any migration checking.
            _ <- liftIO $ snapshotCurrentTypesTo nextVersion

            pDocLn $ D.dullyellow (D.reflow $ "It appears you're all set to deploy the first version of '" <> T.unpack appName <> "'!")

            testWriteLamderaGenerated nextVersion

          else do
            localTypes <- fetchLocalTypes

            if productionTypes /= localTypes
              then do

                lastTypeChangeVersion <- liftIO getLastLocalTypeChangeVersion

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

                    nextMigrationPath = "src/Evergreen/Migrate/V" <> (show nextVersion) <> ".elm"

                    lastTypesPath = "src/Evergreen/Type/V" <> (show prodVersion) <> ".elm"



                migrationExists <- liftIO $ Dir.doesFileExist $ nextMigrationPath

                if migrationExists
                  then do
                    -- @TODO check migrations match changes. I.e. user might have changed one thing, we generated
                    -- a migration, and then later changed another thing. Basically grep for `toBackend old = Unchanged` for each label?
                    -- so we don't acidentally run a migration saying nothing changed when it's not type safe?
                    -- is that even possible or will compiler catch incorrect "unchanged" declarations?

                    -- This also might have implications for ordering of snapshotCurrentTypesTo, which could currently happen at
                    -- the top level of this branching condition, but doens't because you wanted to keep the effects close to the contentious areas
                    -- until the desired behavior was clear

                    _ <- liftIO $ snapshotCurrentTypesTo nextVersion

                    migrationSource <- liftIO $ Text.decodeUtf8 <$> IO.readUtf8 nextMigrationPath

                    if textContains "Unimplemented" migrationSource
                      then do
                        testWriteLamderaGenerated nextVersion

                        Task.throw $ Exit.Lamdera
                          $ Help.report "UNIMPLEMENTED MIGRATION" (Just nextMigrationPath)
                            ("The following types have changed since v" <> show prodVersion <> " and require migrations:")
                            (formattedChangedTypes <>
                              [ D.reflow $ "The migration file at " <> nextMigrationPath <> " has migrations that still haven't been implemented."
                              , D.reflow "See <https://lamdera.com/evergreen-migrations> more info."
                              ]
                            )
                      else do
                        migrationCheck nextVersion

                        committedCheck nextVersion

                        pDocLn $ D.dullyellow $ D.reflow $ "\nIt appears you're all set to deploy v" <> (show nextVersion) <> " of '" <> T.unpack appName <> "'."

                        mapM pDocLn $
                          ([ D.reflow "Evergreen migrations will be applied to the following types:" ]
                           <> formattedChangedTypes <>
                           [ D.reflow "See <https://lamdera.com/evergreen-migrations> more info." ]
                          )

                        testWriteLamderaGenerated nextVersion

                        pure ()

                  else do

                    let defaultMigrations = defaultMigrationFile lastTypeChangeVersion nextVersion typeCompares

                    liftIO $ writeUtf8 defaultMigrations nextMigrationPath

                    -- Text.encodeUtf8 defaultMigrations
                    --   & IO.writeUtf8 nextMigrationPath
                    --   & liftIO

                    _ <- liftIO $ snapshotCurrentTypesTo nextVersion

                    testWriteLamderaGenerated nextVersion

                    Task.throw $ Exit.Lamdera
                      $ Help.report "UNIMPLEMENTED MIGRATION" (Just nextMigrationPath)
                        ("The following types have changed since v" <> show prodVersion <> " and require migrations:")
                        (formattedChangedTypes <>
                          [ D.reflow $ "I've generated a placeholder migration file in " <> nextMigrationPath <> " to help you get started."
                          , D.reflow "See <https://lamdera.com/evergreen-migrations> more info."
                          ]
                        )

              else do
                -- Types are the same.
                -- @TODO If we have a version with no type changes do we still write migrations?
                -- If not, what happens when we get to the next version and we can't refer to the types from the previous one?

                pDocLn $ D.dullyellow $ D.reflow $ "\nIt appears you're all set to deploy v" <> (show nextVersion) <> " of '" <> T.unpack appName <> "'."
                pDocLn $ D.reflow $ "\nThere are no Evergreen type changes for this version."

                -- liftIO $ writeUtf8 defaultMigrations nextMigrationPath
                testWriteLamderaGenerated nextVersion



snapshotCurrentTypesTo :: Int -> IO String
snapshotCurrentTypesTo version = do
  -- Snapshot the current types, and rename the module for the snapshot
  _ <- readProcess "cp" ["src/Types.elm", "src/Evergreen/Type/V" <> show version <> ".elm"] ""
  callCommand $ "sed -i -e 's/module Types exposing/module Evergreen.Type.V" <> show version <> " exposing/g' src/Evergreen/Type/V" <> show version <> ".elm"
  -- How do we get sed to not make these files? Same issue in build.sh...
  callCommand $ "rm src/Evergreen/Type/V" <> show version <> ".elm-e"
  pure ""



getLastLocalTypeChangeVersion :: IO Int
getLastLocalTypeChangeVersion = do

  types <- Dir.listDirectory "src/Evergreen/Type"
  if types == []
    then
      pure 1
    else do
      types
        & List.sortBy Algorithms.NaturalSort.compare
        & List.last
        & drop 1
        & takeWhile (\i -> i /= '.')
        & readMaybe
        & fromMaybe 1 -- If there are no migrations or it failed, it must be version 1?
        & pure


pDocLn doc =
  liftIO $ do
    putStrLn ""
    D.toAnsi IO.stdout doc
    putStrLn ""



fetchProductionInfo :: Text -> Task.Task (Int, [String])
fetchProductionInfo appName =
  let
    endpoint =
      case appName of
        "testapp" ->
          "http://localhost:3030/_i"
        _ ->
          "https://" <> T.unpack appName <> ".apps.lamdera.com/_i"

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
          case D.parse "lamdera-info" id decoder bytes of
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
  --   do  reporter <- Terminal.create
  --       Task.run reporter $
          do  summary <- Project.getRoot
              let jsOutput = Just (Output.Html Nothing "/dev/null")
              Project.compile Output.Dev Output.Client jsOutput Nothing summary [ "src" </> "Frontend.elm" ]
              Project.compile Output.Dev Output.Client jsOutput Nothing summary [ "src" </> "Backend.elm" ]


migrationCheck version =
  -- Dir.withCurrentDirectory ("/Users/mario/dev/projects/lamdera/test/v2") $
    -- do  reporter <- Terminal.create
        -- Task.run reporter $
          do  summary <- Project.getRoot

              -- Temporarily point migration to current types in order to type-check
              liftIO $ putStrLn "Checking Evergreen migrations..."

              liftIO $ callCommand $ "sed -i -e 's/import Evergreen.Type.V" <> show version <> "/import Types/g' src/Evergreen/Migrate/V" <> show version <> ".elm"


              frontendRuntimeExists <- liftIO $ Dir.doesFileExist $ "src/LamderaFrontendRuntime.elm"
              liftIO $ unless frontendRuntimeExists $ writeUtf8 frontendRuntimeLocalContent "src/LamderaFrontendRuntime.elm"

              backendRuntimeExists <- liftIO $ Dir.doesFileExist $ "src/LamderaBackendRuntime.elm"
              liftIO $ unless backendRuntimeExists $ writeUtf8 backendRuntimeLocalContent "src/LamderaBackendRuntime.elm"

              -- Compile check it
              liftIO $ writeUtf8 lamderaCheckBothFileContents "src/LamderaCheckBoth.elm"
              let jsOutput = Just (Output.Html Nothing "/dev/null")
              Project.compile Output.Dev Output.Client jsOutput Nothing summary [ "src" </> "LamderaCheckBoth.elm" ]

              -- @TODO this is because the migrationCheck does weird terminal stuff that mangles the display... how to fix this?
              liftIO $ threadDelay 50000 -- 50 milliseconds

              -- Remove our temporarily checker file
              liftIO $ callCommand $ "rm src/LamderaCheckBoth.elm"

              -- Restore the type back to what it was
              liftIO $ callCommand $ "sed -i -e 's/import Types/import Evergreen.Type.V" <> show version <> "/g' src/Evergreen/Migrate/V" <> show version <> ".elm"
              liftIO $ callCommand $ "rm src/Evergreen/Migrate/V" <> show version <> ".elm-e"

              -- Cleanup dummy runtime files if we added them
              liftIO $ unless frontendRuntimeExists $ callCommand $ "rm src/LamderaFrontendRuntime.elm"
              liftIO $ unless backendRuntimeExists $ callCommand $ "rm src/LamderaBackendRuntime.elm"



committedCheck version = do

  let migrationPath = "src/Evergreen/Migrate/V" <> show version <> ".elm"
  migrations <- liftIO $ gitStatus migrationPath

  unless (migrations == Committed) $
    Task.throw $ Exit.Lamdera
      $ Help.report "UNCOMMITTED MIGRATION" (Just migrationPath)
        ("I need migration files to be comitted otherwise I cannot deploy!")
        ([ D.reflow "Here is a shortcut to add this file to git:"
         , D.dullyellow (D.reflow $ "git add " <> migrationPath)
         , D.reflow "You will then need to commit it."
         ]
        )

  let typesPath = "src/Evergreen/Type/V" <> show version <> ".elm"
  types <- liftIO $ gitStatus typesPath

  unless (types == Committed) $
    Task.throw $ Exit.Lamdera
    $ Help.report "UNCOMMITTED TYPES" (Just typesPath)
      ("I need type snapshot files to be comitted otherwise I cannot deploy!")
      ([ D.reflow "Here is a shortcut to add this file to git:"
       , D.dullyellow (D.reflow $ "git add " <> typesPath)
       , D.reflow "You will then need to commit it."
       ]
      )


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

            typenameCamel = lowerFirstLetter typename

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

    import Evergreen.Type.V$old as Old
    import Evergreen.Type.V$new as New
    import Lamdera.Migrations exposing (..)


  |]

  typeCompares
    & fmap typeCompareMigration
    & (<>) [header]
    & T.intercalate "\n\n"


lamderaCheckBothFileContents :: Text
lamderaCheckBothFileContents =
  [text|
    module LamderaCheckBoth exposing (..)

    import LamderaBackendRuntime
    import LamderaFrontendRuntime


    lamdera =
        "both"
  |]


-- Dummy files for when checking things locally. Remote has full generated file.
frontendRuntimeLocalContent :: Text
frontendRuntimeLocalContent =
  [text|
    module LamderaFrontendRuntime exposing (..)

    import Frontend

    lamderaFrontendRuntime = ""
  |]


backendRuntimeLocalContent :: Text
backendRuntimeLocalContent =
  [text|
    module LamderaBackendRuntime exposing (..)

    import Backend

    lamderaBackendRuntime = ""
  |]


textContains :: Text -> Text -> Bool
textContains needle haystack = indices needle haystack /= []


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


writeUtf8 :: Text -> FilePath -> IO ()
writeUtf8 textContent path =
  Text.encodeUtf8 textContent
    & IO.writeUtf8 path


lowerFirstLetter :: String -> Text
lowerFirstLetter text =
  case text of
    first:rest -> T.pack $ [Char.toLower first] <> rest


createLamderaGenerated :: Int -> IO Text
createLamderaGenerated currentVersion = do

  migrationSequence <- liftIO $ getMigrationsSequence

  let
    migrationImports :: Text
    migrationImports =
      migrationSequence
        & List.head
        & (\(_:actualMigrations) ->
            fmap migrationImport actualMigrations
          )
        & T.concat


    migrationImport :: Int -> Text
    migrationImport version =
      let versionT = T.pack $ show version
      in
      [text|import Evergreen.Migrate.V$versionT as M$versionT|]


    typeImports :: Text
    typeImports =
      migrationSequence
        & fmap (\(version:_) -> typeImport version)
        & T.concat


    typeImport :: Int -> Text
    typeImport version =
      let versionT = T.pack $ show version
      in
      [text|import Evergreen.Type.V$versionT as T$versionT|]


    historicMigrations :: Text
    historicMigrations =
      migrationSequence
        & fmap historicMigration
        & T.intercalate "\n"
        


    historicMigration :: [Int] -> Text
    historicMigration (startVersion:subsequentVersions) =
      let
        types = [ "BackendModel", "FrontendModel", "FrontendMsg", "ToBackend", "BackendMsg", "ToFrontend"]

        typeMigrations =
          types
            & fmap (migrationForType startVersion subsequentVersions)
            & T.intercalate "\n"

        startVersion_ = T.pack $ show startVersion

      in
      [text|
        $startVersion_ ->
            case tipe of
                $typeMigrations

                _ ->
                    UnknownType tipe
      |]


    migrationForType :: Int -> [Int] -> Text -> Text
    migrationForType startVersion intermediateVersions tipe = do

      let
        typenameCamel = lowerFirstLetter $ T.unpack tipe

        intermediateMigration from to =
          let from_ = T.pack $ show from
              to_ = T.pack $ show to
          in
          [text|
            |> thenMigrate "$tipe" M$to_.$typenameCamel T$from_.evg_encode_$tipe T$to_.evg_decode_$tipe $to_
          |]

        intermediateMigrations =
          intermediateVersions
            & fmap (\to -> intermediateMigration (to - 1) to) -- @TODO fix (-1) when list of versions is proper
            & T.concat


        startVersion_ = T.pack $ show startVersion

      [text|
        "$tipe" ->
            decodeType version intList T$startVersion_.evg_decode_$tipe
                $intermediateMigrations
                |> upgradeSucceeds Current$tipe
                |> otherwiseError
      |]

    currentVersion_ = T.pack $ show currentVersion

  pure $ [text|
    module LamderaGenerated exposing (..)

    $migrationImports
    $typeImports
    import Lamdera.Migrations exposing (..)
    import LamderaHelpers exposing (..)
    import Types as T$currentVersion_


    currentVersion : Int
    currentVersion =
        $currentVersion_


    decodeAndUpgrade : Int -> String -> List Int -> UpgradeResult T$currentVersion_.BackendModel T$currentVersion_.FrontendModel T$currentVersion_.FrontendMsg T$currentVersion_.ToBackend T$currentVersion_.BackendMsg T$currentVersion_.ToFrontend
    decodeAndUpgrade version tipe intList =
        case version of
            $historicMigrations


            $currentVersion_ ->
                case tipe of
                    "BackendModel" ->
                        decodeType version intList T$currentVersion_.evg_decode_BackendModel
                            |> upgradeIsCurrent CurrentBackendModel
                            |> otherwiseError


                    "FrontendModel" ->
                        decodeType version intList T$currentVersion_.evg_decode_FrontendModel
                            |> upgradeIsCurrent CurrentBackendModel
                            |> otherwiseError


                    "FrontendMsg" ->
                        decodeType version intList T$currentVersion_.evg_decode_FrontendMsg
                            |> upgradeIsCurrent CurrentBackendModel
                            |> otherwiseError


                    "ToBackend" ->
                        decodeType version intList T$currentVersion_.evg_decode_ToBackend
                            |> upgradeIsCurrent CurrentBackendModel
                            |> otherwiseError


                    "BackendMsg" ->
                        decodeType version intList T$currentVersion_.evg_decode_BackendMsg
                            |> upgradeIsCurrent CurrentBackendModel
                            |> otherwiseError


                    "ToFrontend" ->
                        decodeType version intList T$currentVersion_.evg_decode_ToFrontend
                            |> upgradeIsCurrent CurrentBackendModel
                            |> otherwiseError


                    _ ->
                        UnknownType tipe

            _ ->
                UnknownVersion ( version, tipe, intList )
  |]



testWriteLamderaGenerated :: Int -> Task.Task ()
testWriteLamderaGenerated nextVersion = do
  gen <- liftIO $ createLamderaGenerated nextVersion
  liftIO $ writeUtf8 gen "src/LamderaGenerated.elm"


getMigrationsSequence :: IO [[Int]]
getMigrationsSequence = do
  migrations <- Dir.listDirectory "src/Evergreen/Migrate"

  let
    getVersion :: FilePath -> Maybe Int
    getVersion filename =
      filename
        & drop 1 -- Drop the 'V'
        & takeWhile (\i -> i /= '.')
        & readMaybe

    versions =
      migrations
        -- There will never be a V1 migration but it is always
        -- the first version in our sequence
        & (++) ["V1.elm"]
        & List.sortBy Algorithms.NaturalSort.compare
        & fmap getVersion
        & justs -- This is bad? But those files probably aren't VX.elm files?

    sequences =
      versions
        & scanr (\v acc -> acc ++ [v]) []
        & filter ((/=) [])
        & fmap reverse

  pure sequences


justs :: [Maybe a] -> [a]
justs xs = [ x | Just x <- xs ]



earlyBail =
  Task.throw $ Exit.Lamdera
    $ Help.report "EARLY BAIL" (Just "I'm out!")
      ("You used earlyBail")
      []
