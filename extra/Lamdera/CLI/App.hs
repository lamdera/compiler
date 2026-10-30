{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.App where

{- `lamdera app [info | list | create <name> | delete | plan <plan>]` -}

import qualified Data.Aeson as Aeson
import qualified Data.Text as T

import qualified Reporting.Doc as D

import Lamdera.CLI.Dashboard hiding (Common(..))
import qualified Lamdera.CLI.Dashboard as Dash


data Args
  = Info
  | List
  | Create String
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
    Delete -> appRequest appName "appDelete" [ ("confirm", FString appName) ]
    Plan plan -> appRequest appName "appUpdate" [ ("plan", FString (T.pack plan)) ]


run :: Args -> Flags -> IO ()
run args flags = do
  let c = common flags
  ctx <-
    case args of
      List -> setup c AppOptional
      Create name -> setup c { Dash._app = Just name } AppOptional
      _ -> setup c NeedsApp
  let appName = appOf ctx
  case args of
    Delete -> confirm c ("permanently delete the app " <> appName <> " and all its data") (Just appName)
    _ -> pure ()
  payload <- call ctx (toRequest appName args flags)
  output ctx payload (render (_web ctx) args payload)


render :: String -> Args -> Aeson.Value -> D.Doc
render web args payload =
  case args of
    List -> renderList payload
    Info -> renderInfo web payload
    Create name ->
      D.vcat [ said (D.dullgreen (D.fromChars ("Created " <> name))), "", renderInfo web payload ]
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
        table []
          [ [ "App", appName ]
          , [ "URL", textAt "url" a ]
          , [ "Plan", textAt "plan" a ]
          , [ "Owner", textAt "owner" a ]
          , [ "Version", version a ]
          , [ "Created", timeAt "created" a ]
          , [ "Logging", if boolAt "logging" a then "on" else "off" ]
          , [ "Domains", if null domains then "-" else T.intercalate ", " (fmap domainName domains) ]
          ]
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
