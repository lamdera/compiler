{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Dashboard
  ( ProjectArgs(..)
  , ProjectFlags(..)
  , TeamArgs(..)
  , SshArgs(..)
  , SshKeyFlags(..)
  , EnvFlags(..)
  , EnvArgs(..)
  , runProject
  , runTeams
  , runSsh
  , runEnv
  )
  where


import Control.Exception (SomeException, try)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as T
import qualified Reporting
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help
import System.FilePath (takeBaseName)
import System.IO (hFlush, stdout)
import qualified System.Process as Process

import Lamdera
import qualified Lamdera.Dashboard as Dashboard
import qualified Lamdera.Progress as Progress
import qualified Lamdera.Project


data ProjectArgs
  = ProjectAdd (Maybe String)


data ProjectFlags =
  ProjectFlags
    { _scope :: Maybe String
    , _team :: Maybe String
    , _plan :: Maybe String
    , _remote :: Bool
    }


data TeamArgs
  = TeamAdd (Maybe String) (Maybe String)


data SshArgs
  = SshAdd FilePath (Maybe String)


data SshKeyFlags =
  SshKeyFlags
    { _label :: Maybe String
    }


data EnvFlags =
  EnvFlags
    { _project :: Maybe String
    , _app :: Maybe String
    , _public :: Bool
    }


data EnvArgs
  = EnvList
  | EnvAdd String (Maybe String) (Maybe String)
  | EnvUpdate String (Maybe String) (Maybe String)
  | EnvSet String String
  | EnvRemove String (Maybe String)


runProject :: ProjectArgs -> ProjectFlags -> IO ()
runProject (ProjectAdd rawAppNameM) flags = do
  rawAppName <-
    case rawAppNameM of
      Just providedName ->
        pure providedName

      Nothing ->
        promptUntil "Project name: " parseNonEmpty "Enter a project name."

  appName <- normalizeProjectName rawAppName

  Dashboard.createProject
    appName
    (T.pack <$> preferredScope flags)
    (normalizePlan <$> _plan flags)

  Progress.report $
    D.fillSep
      [ "───>"
      , D.dullgreen "Project created:"
      , D.fromChars $ T.unpack appName
      ]

  maybeAddLamderaRemote appName (_remote flags)


runTeams :: TeamArgs -> () -> IO ()
runTeams (TeamAdd teamNameM billingEmailM) () = do
  teamName <-
    case teamNameM of
      Just providedName ->
        pure providedName

      Nothing ->
        promptUntil "Team name: " parseNonEmpty "Enter a team name."

  billingEmail <-
    case billingEmailM of
      Just providedEmail ->
        pure providedEmail

      Nothing ->
        promptUntil "Billing email: " parseEmail "Enter a valid billing email."

  Dashboard.createTeam (T.pack teamName) (T.pack billingEmail)

  Progress.report $
    D.fillSep
      [ "───>"
      , D.dullgreen "Team created:"
      , D.fromChars teamName
      ]


runSsh :: SshArgs -> SshKeyFlags -> IO ()
runSsh (SshAdd publicKeyPath argLabelM) flags = do
  publicKeyM <- readUtf8Text publicKeyPath

  publicKey <-
    case publicKeyM of
      Just key ->
        pure $ T.strip key

      Nothing ->
        Progress.throw $
          Help.report "MISSING SSH KEY" Nothing
            "I could not read that SSH public key file."
            [ D.reflow publicKeyPath ]

  onlyWhen (T.null publicKey) $
    Progress.throw $
      Help.report "EMPTY SSH KEY" Nothing
        "That SSH public key file is empty."
        [ D.reflow publicKeyPath ]

  let
    label =
      case argLabelM of
        Just customLabel ->
          T.pack customLabel

        Nothing ->
          case _label flags of
            Just customLabel ->
              T.pack customLabel

            Nothing ->
              T.pack $ takeBaseName publicKeyPath

  Dashboard.setSshKey publicKey label

  Progress.report $
    D.fillSep
      [ "───>"
      , D.dullgreen "SSH key uploaded:"
      , D.fromChars $ T.unpack label
      ]


runEnv :: EnvArgs -> EnvFlags -> IO ()
runEnv envArgs flags = do
  appName <- resolveAppName (preferredProject flags)

  case envArgs of
    EnvList -> do
      items <- Dashboard.fetchConfigItems appName
      reportEnvList appName items

    EnvAdd envName environmentM envValueM -> do
      validateEnvironmentHint environmentM
      envValue <- resolveEnvValue envName envValueM
      items <- Dashboard.fetchConfigItems appName

      let
        envNameText = T.pack envName

      onlyWhen
        (case findConfigItem envNameText items of
          Just _ ->
            True

          Nothing ->
            False
        ) $
        Progress.throw $
          Help.report "ENV VAR EXISTS" Nothing
            "That environment variable already exists."
            [ D.reflow "Use `lamdera env update` to change an existing value."
            ]

      saveEnvItem appName flags envName envValue items

    EnvUpdate envName environmentM envValueM -> do
      validateEnvironmentHint environmentM
      envValue <- resolveEnvValue envName envValueM
      items <- Dashboard.fetchConfigItems appName

      let
        envNameText = T.pack envName

      onlyWhen
        (case findConfigItem envNameText items of
          Just _ ->
            False

          Nothing ->
            True
        ) $
        Progress.throw $
          Help.report "UNKNOWN ENV VAR" Nothing
            "I could not find an environment variable with that name."
            [ D.reflow "Use `lamdera env add` to create it first."
            ]

      saveEnvItem appName flags envName envValue items

    EnvSet envName envValue -> do
      items <- Dashboard.fetchConfigItems appName
      saveEnvItem appName flags envName envValue items

    EnvRemove envName environmentM -> do
      validateEnvironmentHint environmentM
      items <- Dashboard.fetchConfigItems appName
      let
        envNameText = T.pack envName
        remainingItems =
          filter ((/= envNameText) . Dashboard.configItemName) items

      onlyWhen (length remainingItems == length items) $
        Progress.throw $
          Help.report "UNKNOWN ENV VAR" Nothing
            "I could not find an environment variable with that name."
            [ D.reflow envName ]

      Dashboard.saveConfigItems appName remainingItems

      Progress.report $
        D.fillSep
          [ "───>"
          , D.dullgreen "Environment variable removed:"
          , D.fromChars envName
          ]


saveEnvItem :: Text -> EnvFlags -> String -> String -> [Dashboard.ConfigItem] -> IO ()
saveEnvItem appName flags envName envValue items = do
  let
    envNameText = T.pack envName
    envValueText = T.pack envValue
    updatedItems =
      upsertConfigItem
        envNameText
        (Dashboard.ConfigItem
          { Dashboard.configItemName = envNameText
          , Dashboard.configItemValue = envValueText
          , Dashboard.configItemUsed = maybe False Dashboard.configItemUsed (findConfigItem envNameText items)
          , Dashboard.configItemSecret =
              case findConfigItem envNameText items of
                Just existingItem ->
                  if _public flags
                    then False
                    else Dashboard.configItemSecret existingItem

                Nothing ->
                  not (_public flags)
          }
        )
        items

  Dashboard.saveConfigItems appName updatedItems

  Progress.report $
    D.fillSep
      [ "───>"
      , D.dullgreen "Environment variable saved:"
      , D.fromChars envName
      ]


preferredScope :: ProjectFlags -> Maybe String
preferredScope flags =
  case _scope flags of
    Just scope ->
      Just scope

    Nothing ->
      _team flags


preferredProject :: EnvFlags -> Maybe String
preferredProject flags =
  case _project flags of
    Just projectName ->
      Just projectName

    Nothing ->
      _app flags


parseNonEmpty :: String -> Maybe String
parseNonEmpty value =
  if null (trim value)
    then Nothing
    else Just value


parseEmail :: String -> Maybe String
parseEmail value =
  if '@' `elem` value && '.' `elem` value
    then Just value
    else Nothing


promptUntil :: String -> (String -> Maybe a) -> String -> IO a
promptUntil promptText parser invalidMessage = do
  putStr promptText
  hFlush stdout
  value <- getLine

  case parser value of
    Just parsed ->
      pure parsed

    Nothing -> do
      atomicPutStrLn invalidMessage
      promptUntil promptText parser invalidMessage


resolveEnvValue :: String -> Maybe String -> IO String
resolveEnvValue envName envValueM =
  case envValueM of
    Just envValue ->
      pure envValue

    Nothing ->
      promptUntil
        ("Value for " <> envName <> ": ")
        parseNonEmpty
        "Enter a non-empty environment variable value."


validateEnvironmentHint :: Maybe String -> IO ()
validateEnvironmentHint environmentM =
  case environmentM of
    Nothing ->
      pure ()

    Just rawEnvironment ->
      case normalizeEnvironment rawEnvironment of
        Just _ ->
          pure ()

        Nothing ->
          Progress.throw $
            Help.report "INVALID ENVIRONMENT" Nothing
              "I did not recognize that environment name."
              [ D.reflow "Use `production`, `preview`, or `development`."
              ]


normalizeEnvironment :: String -> Maybe String
normalizeEnvironment rawEnvironment =
  case map Char.toLower (trim rawEnvironment) of
    "prod" ->
      Just "production"

    "production" ->
      Just "production"

    "preview" ->
      Just "preview"

    "dev" ->
      Just "development"

    "development" ->
      Just "development"

    _ ->
      Nothing


normalizeProjectName :: String -> IO Text
normalizeProjectName rawAppName = do
  let
    cleanedAppName =
      Lamdera.Project.makeNameClean (T.pack rawAppName)

  onlyWhen (T.null cleanedAppName) $
    Progress.throw $
      Help.report "INVALID APP NAME" Nothing
        "I could not turn that into a valid Lamdera app name."
        [ D.reflow "Use lowercase letters, numbers, and hyphens." ]

  onlyWhen (cleanedAppName /= T.pack rawAppName) $
    Progress.report $
      D.fillSep
        [ "───>"
        , D.dullyellow "Using sanitized app name:"
        , D.fromChars $ T.unpack cleanedAppName
        ]

  pure cleanedAppName


normalizePlan :: String -> Text
normalizePlan =
  T.toTitle . T.toLower . T.pack


resolveAppName :: Maybe String -> IO Text
resolveAppName appFlag =
  case appFlag of
    Just explicitAppName ->
      pure (T.pack explicitAppName)

    Nothing -> do
      appNameM <- Lamdera.Project.maybeAppName

      case appNameM of
        Just appName ->
          pure appName

        Nothing ->
          Progress.throw $
            Help.report "UNKNOWN APP" (Just "lamdera env ls --project <project-name>")
              "I cannot figure out which Lamdera project you want to manage."
              [ D.reflow "Pass `--project=<project-name>` explicitly, or run this inside a repo with a `lamdera` remote."
              ]


reportEnvList :: Text -> [Dashboard.ConfigItem] -> IO ()
reportEnvList appName items =
  if null items
    then
      Progress.report $
        D.fillSep
          [ "───>"
          , D.dullyellow "No environment variables configured for"
          , D.fromChars $ T.unpack appName
          ]
    else do
      atomicPutStrLn $ "Project: " <> T.unpack appName
      items
        & List.sortOn Dashboard.configItemName
        & mapM_ (atomicPutStrLn . formatEnvItem)


formatEnvItem :: Dashboard.ConfigItem -> String
formatEnvItem item =
  let
    visibility =
      if Dashboard.configItemSecret item
        then "secret"
        else "public"

    usage =
      if Dashboard.configItemUsed item
        then "used"
        else "unused"

    renderedValue =
      if Dashboard.configItemSecret item
        then "<hidden>"
        else T.unpack $ Dashboard.configItemValue item
  in
  T.unpack (Dashboard.configItemName item)
    <> " ["
    <> visibility
    <> ", "
    <> usage
    <> "] = "
    <> renderedValue


findConfigItem :: Text -> [Dashboard.ConfigItem] -> Maybe Dashboard.ConfigItem
findConfigItem itemName =
  List.find ((== itemName) . Dashboard.configItemName)


upsertConfigItem :: Text -> Dashboard.ConfigItem -> [Dashboard.ConfigItem] -> [Dashboard.ConfigItem]
upsertConfigItem itemName newItem items =
  case items of
    [] ->
      [ newItem ]

    item : rest ->
      if Dashboard.configItemName item == itemName
        then
          newItem : rest
        else
          item : upsertConfigItem itemName newItem rest


maybeAddLamderaRemote :: Text -> Bool -> IO ()
maybeAddLamderaRemote appName forceRemote = do
  gitRepo <- isInsideGitRepo
  let
    remoteUrl =
      "git@apps.lamdera.com:" <> T.unpack appName <> ".git"

  if not gitRepo
    then
      atomicPutStrLn $
        "Add this remote when your git repo is ready:\n  git remote add lamdera "
          <> remoteUrl
    else do
      existingRemoteM <- getLamderaRemoteUrl

      case existingRemoteM of
        Just existingRemote
          | trim existingRemote == remoteUrl ->
              Progress.report $
                D.fillSep
                  [ "───>"
                  , D.dullgreen "Git remote already configured:"
                  , D.fromChars existingRemote
                  ]

          | otherwise ->
              Progress.report $
                D.fillSep
                  [ "───>"
                  , D.dullyellow "Leaving existing `lamdera` git remote unchanged:"
                  , D.fromChars existingRemote
                  ]

        Nothing -> do
          shouldAddRemote <-
            if forceRemote
              then
                pure True
              else
                Reporting.ask $
                  D.fillSep
                    [ "Add"
                    , D.blue "lamdera"
                    , "git remote:"
                    , D.fromChars remoteUrl
                    , "?"
                    , "[Y/n]:"
                    ]

          onlyWhen shouldAddRemote $ do
            Process.callProcess "git" ["remote", "add", "lamdera", remoteUrl]
            Progress.report $
              D.fillSep
                [ "───>"
                , D.dullgreen "Added git remote:"
                , D.fromChars remoteUrl
                ]


isInsideGitRepo :: IO Bool
isInsideGitRepo = do
  result <- try $ Process.readProcess "git" ["rev-parse", "--is-inside-work-tree"] "" :: IO (Either SomeException String)

  pure $
    case result of
      Right output ->
        trim output == "true"

      Left _ ->
        False


getLamderaRemoteUrl :: IO (Maybe String)
getLamderaRemoteUrl = do
  result <- try $ Process.readProcess "git" ["remote", "get-url", "lamdera"] "" :: IO (Either SomeException String)

  pure $
    case result of
      Right output ->
        Just (trim output)

      Left _ ->
        Nothing


trim :: String -> String
trim =
  reverse . dropWhile Char.isSpace . reverse . dropWhile Char.isSpace
