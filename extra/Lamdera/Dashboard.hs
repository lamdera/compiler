{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Dashboard
  ( ConfigItem(..)
  , createProject
  , createTeam
  , setSshKey
  , fetchConfigItems
  , saveConfigItems
  )
  where


import System.Exit (exitFailure)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Json.Decode as JD
import qualified Json.Encode as JE
import qualified Json.String
import qualified Reporting.Doc as Doc
import qualified Reporting.Exit.Help as Help
import qualified Http as RawHttp
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as Status

import Lamdera
import qualified Lamdera.CLI.Login
import qualified Lamdera.Http
import qualified Lamdera.Progress as Progress


data ConfigItem =
  ConfigItem
    { configItemName :: Text
    , configItemValue :: Text
    , configItemUsed :: Bool
    , configItemSecret :: Bool
    }


createProject :: Text -> Maybe Text -> Maybe Text -> IO ()
createProject appName ownerM planM = do
  body <- authenticatedBody $
    [ field "appId" (JE.text appName)
    , field "appName" (JE.text appName)
    , field "name" (JE.text appName)
    ]
    ++ maybe [] (\owner ->
          [ field "owner" (JE.text owner)
          , field "team" (JE.text owner)
          , field "teamId" (JE.text owner)
          ]
       ) ownerM
    ++ maybe [] (\plan ->
          [ field "plan" (JE.text plan)
          , field "planChange" (JE.text plan)
          ]
       ) planM

  _ <-
    postFirstWorking
      Nothing
      [ "createAppJson"
      , "createProjectJson"
      , "appCreateJson"
      , "projectCreateJson"
      ]
      body
      JD.value
      "I needed to create a Lamdera project"

  pure ()


createTeam :: Text -> Text -> IO ()
createTeam teamName billingEmail = do
  body <- authenticatedBody
    [ field "name" (JE.text teamName)
    , field "teamName" (JE.text teamName)
    , field "billingEmail" (JE.text billingEmail)
    , field "teamBillingEmail" (JE.text billingEmail)
    ]

  _ <-
    postFirstWorking
      Nothing
      [ "createTeamJson"
      , "teamCreateJson"
      ]
      body
      JD.value
      "I needed to create a Lamdera team"

  pure ()


setSshKey :: Text -> Text -> IO ()
setSshKey sshKey sshLabel = do
  body <- authenticatedBody
    [ field "key" (JE.text sshKey)
    , field "label" (JE.text sshLabel)
    , field "sshKey" (JE.text sshKey)
    , field "sshLabel" (JE.text sshLabel)
    ]

  _ <-
    postFirstWorking
      Nothing
      [ "setSshKeyJson"
      , "addSshKeyJson"
      , "sshKeySetJson"
      , "sshKeyAddJson"
      ]
      body
      JD.value
      "I needed to upload an SSH public key"

  pure ()


fetchConfigItems :: Text -> IO [ConfigItem]
fetchConfigItems appName = do
  body <- authenticatedBody
    [ field "appId" (JE.text appName)
    , field "appName" (JE.text appName)
    ]

  postFirstWorking
    (Just appName)
    [ "configItemsJson"
    , "appConfigItemsJson"
    ]
    body
    configItemsDecoder
    "I needed to fetch the environment variables for this Lamdera app"


saveConfigItems :: Text -> [ConfigItem] -> IO ()
saveConfigItems appName items = do
  let encodedItems =
        JE.list encodeConfigItem items

  body <- authenticatedBody
    [ field "appId" (JE.text appName)
    , field "appName" (JE.text appName)
    , field "configItems" encodedItems
    , field "applicationConfig" encodedItems
    ]

  _ <-
    postFirstWorking
      (Just appName)
      [ "saveConfigItemsJson"
      , "configItemsSaveJson"
      , "setConfigItemsJson"
      , "configItemsSetJson"
      ]
      body
      JD.value
      "I needed to save the environment variables for this Lamdera app"

  pure ()


configItemsDecoder :: JD.Decoder x [ConfigItem]
configItemsDecoder =
  JD.list $
    JD.succeed ConfigItem
      & JD.required "name" JD.text
      & JD.required "value" JD.text
      & JD.required "used" JD.bool
      & JD.required "secret" JD.bool


encodeConfigItem :: ConfigItem -> JE.Value
encodeConfigItem item =
  JE.object
    [ field "name" (JE.text $ configItemName item)
    , field "value" (JE.text $ configItemValue item)
    , field "used" (JE.bool $ configItemUsed item)
    , field "secret" (JE.bool $ configItemSecret item)
    ]


authenticatedBody :: [(Json.String.String, JE.Value)] -> IO JE.Value
authenticatedBody bodyFields = do
  token <- Lamdera.CLI.Login.validateCliToken
  pure $
    JE.object $
      field "key" (JE.text token)
      : bodyFields


field :: String -> JE.Value -> (Json.String.String, JE.Value)
field name value =
  (Json.String.fromChars name, value)


dashboardEndpointUrl :: Maybe Text -> String -> IO String
dashboardEndpointUrl appNameHint endpointName = do
  baseUrl <- dashboardBaseUrl appNameHint
  pure $ baseUrl <> "/_r/" <> endpointName


dashboardBaseUrl :: Maybe Text -> IO String
dashboardBaseUrl appNameHint = do
  dashboardUrlM <- lookupEnv "LAMDERA_DASHBOARD_URL"

  pure $
    case dashboardUrlM of
      Just dashboardUrl ->
        stripTrailingSlash dashboardUrl

      Nothing
        | maybe False (textContains "-local") appNameHint ->
            "http://localhost:8082"

        | otherwise ->
            "https://dashboard.lamdera.app"


stripTrailingSlash :: String -> String
stripTrailingSlash =
  reverse . dropWhile (== '/') . reverse


postFirstWorking
  :: Maybe Text
  -> [String]
  -> JE.Value
  -> JD.Decoder () a
  -> String
  -> IO a
postFirstWorking appNameHint endpoints body successDecoder reason =
  tryEndpoints [] endpoints
  where
    decoder =
      withErrorDecoder successDecoder

    tryEndpoints attempted remaining =
      case remaining of
        [] ->
          Progress.throw $
            Help.report "MISSING DASHBOARD ENDPOINT" Nothing
              "I could not find a compatible Dashboard endpoint for this command."
              [ Doc.reflow $ "Tried: " <> List.intercalate ", " (reverse attempted)
              , Doc.reflow "If you are targeting a local dashboard, set `LAMDERA_DASHBOARD_URL` first."
              ]

        endpointName : rest -> do
          endpointUrl <- dashboardEndpointUrl appNameHint endpointName
          result <- Lamdera.Http.normalRpcJson endpointName body endpointUrl decoder

          case result of
            Right (Lamdera.Http.SuccessField value) ->
              pure value

            Right (Lamdera.Http.ErrorField errText) ->
              requestFailed errText

            Left err
              | isMissingEndpointError err ->
                  tryEndpoints (endpointName : attempted) rest

              | otherwise -> do
                  Lamdera.Http.printHttpError err reason
                  exitFailure

    requestFailed errText =
      Progress.throw $
        Help.report "DASHBOARD ERROR" Nothing
          "The Lamdera Dashboard rejected this request."
          [ Doc.red $ Doc.fromChars $ T.unpack errText ]


withErrorDecoder :: JD.Decoder x a -> JD.Decoder x (Lamdera.Http.WithErrorField a)
withErrorDecoder successDecoder =
  JD.oneOf
    [ Lamdera.Http.ErrorField <$> JD.field "error" JD.text
    , Lamdera.Http.SuccessField <$> successDecoder
    ]


isMissingEndpointError :: Lamdera.Http.Error -> Bool
isMissingEndpointError err =
  case err of
    Lamdera.Http.HttpError rawErr ->
      case rawErr of
        RawHttp.BadHttp _ httpException ->
          case httpException of
            HTTP.StatusCodeException response _ ->
              Status.statusCode (HTTP.responseStatus response) == 404

            _ ->
              False

        _ ->
          False

    _ ->
      False
