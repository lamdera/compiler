{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.App where

{- `lamdera app [info | list | create <name> | link <name> | move <team> | delete | plan <plan>]`,
and `lamdera status` (the same as `app info`). `create` and `link` also set up the project's
`lamdera` git remote and check this machine can push to it (Lamdera.CLI.Git). -}

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T

import qualified Reporting.Doc as D

import Lamdera.CLI.Dashboard hiding (Common(..))
import qualified Lamdera.CLI.Dashboard as Dash
import qualified Lamdera.CLI.Git as Git
import qualified Lamdera.Progress as Progress


data Args
  = Info
  | List
  | Create String
  | Link String
  | Move String
  | Delete
  | Plan String
  deriving (Show, Eq)


data Flags =
  Flags
    { _app :: Maybe String
    , _json :: Bool
    , _yes :: Bool
    , _team :: Maybe String
    , _plan :: Maybe String
    }
    deriving (Show, Eq)


plans :: [String]
plans =
  [ "free", "hobby", "professional" ]


common :: Flags -> Dash.Common
common flags =
  Dash.Common (_app flags) (_json flags) (_yes flags)


{-| The request for a command; the app name is the one the command is about (unused by
`list` and `create`). -}
toRequest :: T.Text -> Args -> Flags -> Request
toRequest appName args flags =
  case args of
    Info -> appRequest appName "appInfo" []
    List -> request "appList" []
    Create name ->
      request "appCreate" $
        [ ("name", FString (T.pack name)) ]
          ++ optional "team" (FString . T.pack) (_team flags)
          ++ optional "plan" (FString . T.pack) (_plan flags)
    Link name -> appRequest (T.pack name) "appInfo" []
    Move team -> appRequest appName "appUpdate" [ ("team", FString (T.pack team)) ]
    Delete -> appRequest appName "appDelete" [ ("confirm", FString appName) ]
    Plan plan -> appRequest appName "appUpdate" [ ("plan", FString (T.pack plan)) ]


run :: Args -> Flags -> IO ()
run args flags = do
  let c = common flags
  ctx <-
    case args of
      List -> setup c AppOptional
      Create name -> setup c { Dash._app = Just name } AppOptional
      Link name -> setup c { Dash._app = Just name } AppOptional
      _ -> setup c NeedsApp
  let appName = appOf ctx
  case args of
    Delete -> confirm c ("permanently delete the app " <> appName <> " and all its data") (Just appName)
    _ -> pure ()
  payload <- call ctx (toRequest appName args flags)
  case args of
    Create name -> withGit ctx (T.pack name) args payload
    Link name -> withGit ctx (T.pack name) args payload
    _ -> output ctx payload (render (_web ctx) args payload)


{-| After `create` and `link`: the `lamdera` git remote, then whether this machine can push
to it. Neither stops the command: the app is there either way, and the output says what is
left to do. -}
withGit :: Ctx -> T.Text -> Args -> Aeson.Value -> IO ()
withGit ctx appName args payload = do
  let
    url = Git.gitRemoteFor appName (textMaybe "gitRemote" payload)
    jsonOut = _jsonOut ctx
  if jsonOut then pure () else Progress.report (render (_web ctx) args payload)
  remote <- Git.ensureRemote url
  if jsonOut then pure () else do
    Progress.report ""
    Progress.report (saidText id (Git.remoteMessage url remote))
    Progress.report (saidText D.dullyellow ("Checking this machine can push to " <> Git.remoteHost url <> "..."))
  (outcome, details) <- Git.check url
  let message = readyMessage remote (Git.outcomeMessage url appName outcome details) appName outcome
  if jsonOut
    then output ctx (withGitFields url remote outcome message payload) ""
    else Progress.report (saidText (outcomeDoc outcome) message)


{-| Ready, but without the `lamdera` remote `git push lamdera` needs. -}
readyMessage :: Git.Remote -> T.Text -> T.Text -> Git.Outcome -> T.Text
readyMessage remote message appName outcome =
  if outcome == Git.Reachable && remote /= Git.RemoteAdded && remote /= Git.RemoteExists
    then "Ready: this machine can push to " <> appName <> ". Set up the `lamdera` git remote as above, then deploy with `lamdera deploy`."
    else message


{-| A `───>` line of text that wraps at words. -}
saidText :: (D.Doc -> D.Doc) -> T.Text -> D.Doc
saidText colour text =
  D.fillSep ("───>" : fmap (colour . D.fromChars . T.unpack) (T.words text))


outcomeDoc :: Git.Outcome -> D.Doc -> D.Doc
outcomeDoc outcome =
  case outcome of
    Git.Reachable -> D.dullgreen
    Git.AuthRefused -> D.dullyellow
    _ -> D.red


{-| The --json output of `create` and `link`: the app's info with what happened to the git
remote and the result of the connectivity check. -}
withGitFields :: T.Text -> Git.Remote -> Git.Outcome -> T.Text -> Aeson.Value -> Aeson.Value
withGitFields url remote outcome message payload =
  let
    fields =
      KeyMap.fromList $
        [ ("gitRemote", Aeson.String url)
        , ("remote", Aeson.String (Git.remoteStatus remote))
        , ("sshKey", Aeson.String (Git.sshKeyStatus outcome))
        , ("connectivity", Aeson.String (Git.outcomeStatus outcome))
        , ("connectivityMessage", Aeson.String message)
        ]
        ++ (if remote == Git.RemoteAdded || remote == Git.RemoteExists then [] else [ ("remoteCommand", Aeson.String (Git.addCommand url)) ])
  in
  case payload of
    Aeson.Object o -> Aeson.Object (KeyMap.union fields o)
    _ -> Aeson.Object (KeyMap.insert "app" payload fields)


textMaybe :: T.Text -> Aeson.Value -> Maybe T.Text
textMaybe key v =
  case at key v of
    Just (Aeson.String t) -> Just t
    _ -> Nothing


render :: String -> Args -> Aeson.Value -> D.Doc
render web args payload =
  case args of
    List -> renderList payload
    Info -> renderInfo web payload
    Create name ->
      D.vcat [ said (D.dullgreen (D.fromChars ("Created " <> name))), "", renderInfo web payload ]
    Link name ->
      D.vcat [ said (D.dullgreen (D.fromChars ("Linking to " <> name))), "", renderInfo web payload ]
    Move _ ->
      D.vcat [ said (D.dullgreen (D.fromChars ("Moved " <> T.unpack (textAt "appId" payload) <> ", its owner is now " <> T.unpack (textAt "owner" payload)))), "", renderInfo web payload ]
    Delete ->
      said (D.dullgreen (D.fromChars ("Deleted " <> T.unpack (textAt "deleted" payload))))
    Plan plan ->
      D.vcat [ said (D.dullgreen (D.fromChars ("The plan of " <> T.unpack (textAt "appId" payload) <> " is now " <> plan))), "", renderInfo web payload ]


renderList :: Aeson.Value -> D.Doc
renderList payload =
  case list payload of
    [] -> said "You have no apps yet. Create one with `lamdera app create <name>`."
    apps ->
      lines_ $
        table [ "APP", "PLAN", "OWNER", "VERSION", "CREATED", "URL" ]
          (fmap
            (\a ->
                [ textAt "appId" a <> (if boolAt "preview" a then " (preview)" else "")
                , textAt "plan" a
                , textAt "owner" a
                , version a
                , timeAt "created" a
                , textAt "url" a
                ]
            )
            apps
          )


version :: Aeson.Value -> T.Text
version a =
  case intAt "version" a of
    Just 0 -> "-"
    Just n -> "v" <> T.pack (show n)
    Nothing -> textAt "version" a


renderInfo :: String -> Aeson.Value -> D.Doc
renderInfo web a =
  let
    appName = textAt "appId" a
    deploys = listAt "deploys" a
    domains = listAt "domains" a
  in
  D.vcat $
    [ lines_ $
        table [] $
          [ [ "App", appName ]
          , [ "URL", textAt "url" a ]
          , [ "Plan", textAt "plan" a ]
          , [ "Owner", textAt "owner" a ]
          , [ "Version", version a ]
          , [ "Created", timeAt "created" a ]
          , [ "Logging", if boolAt "logging" a then "on" else "off" ]
          , [ "Domains", if null domains then "-" else T.intercalate ", " (fmap domainName domains) ]
          ]
          ++ (case at "gitRemote" a of { Just (Aeson.String r) -> [ [ "Git remote", r ] ]; _ -> [] })
          ++ (case intAt "sshKeys" a of { Just n -> [ [ "SSH keys", T.pack (show n) <> (if n == 0 then " (add this machine's with `lamdera keys add`)" else "") ] ]; Nothing -> [] })
    ]
    ++
    (if null deploys then [] else
      [ ""
      , lines_ $ table [ "DEPLOY", "SHA", "AT" ] (fmap (\d -> [ version d, T.take 8 (textAt "sha" d), timeAt "at" d ]) deploys)
      ])
    ++
    [ ""
    , "Dashboard:"
    , link web appName ""
    ]


domainName :: Aeson.Value -> T.Text
domainName d =
  case d of
    Aeson.String t -> t
    _ -> textAt "domain" d
