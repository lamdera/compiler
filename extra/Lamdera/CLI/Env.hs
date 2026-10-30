{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Env where

{- `lamdera env [list | set <NAME> <value> | remove <NAME>]`: the app's environment
variables (the dashboard's config items, read by the app through `Env`). -}

import qualified Data.Aeson as Aeson
import qualified Data.Text as T

import qualified Reporting.Doc as D
import Reporting.Doc ((<+>))

import Lamdera.CLI.Dashboard hiding (Common(..))
import qualified Lamdera.CLI.Dashboard as Dash


data Args
  = List
  | Set String String
  | Remove String
  deriving (Show, Eq)


data Flags =
  Flags
    { _app :: Maybe String
    , _json :: Bool
    , _yes :: Bool
    , _secret :: Bool
    , _redeploy :: Bool
    }
    deriving (Show, Eq)


common :: Flags -> Dash.Common
common flags =
  Dash.Common (_app flags) (_json flags) (_yes flags)


toRequest :: T.Text -> Args -> Flags -> Request
toRequest appName args flags =
  case args of
    List -> appRequest appName "envList" []
    Set name value ->
      (appRequest appName "envSet"
        [ ("name", FString (T.pack name))
        , ("value", FString (T.pack value))
        , ("secret", FBool (_secret flags))
        , ("redeploy", FBool (_redeploy flags))
        ])
        -- a redeploy builds and deploys the app
        { _timeoutMs = if _redeploy flags then 600000 else 120000 }
    Remove name -> appRequest appName "envRemove" [ ("name", FString (T.pack name)) ]


run :: Args -> Flags -> IO ()
run args flags = do
  let c = common flags
  ctx <- setup c NeedsApp
  let appName = appOf ctx
  case args of
    Remove name -> confirm c ("remove the environment variable " <> T.pack name <> " from " <> appName) Nothing
    _ -> pure ()
  payload <- call ctx (toRequest appName args flags)
  output ctx payload (render (_web ctx) appName args payload)


render :: String -> T.Text -> Args -> Aeson.Value -> D.Doc
render web appName args payload =
  case args of
    List ->
      case list payload of
        [] ->
          D.vcat
            [ said ("No environment variables set for" <+> D.fromChars (T.unpack appName) <> ".")
            , "Set one with `lamdera env set <NAME> <value>`, or on the dashboard:"
            , link web appName "/config"
            ]
        items ->
          D.vcat
            [ lines_ $
                table [ "NAME", "VALUE", "USED", "SECRET" ]
                  (fmap (\i -> [ textAt "name" i, textAt "value" i, textAt "used" i, textAt "secret" i ]) items)
            , ""
            , "USED: the app's code reads it, so a new value only applies from the next deploy."
            , link web appName "/config"
            ]

    Set name _ ->
      let
        nameDoc = D.fromChars name
        followUp
          | boolAt "redeployed" payload = said ("Redeployed" <+> D.fromChars (T.unpack appName) <+> "with the new value.")
          | boolAt "used" payload =
              said (D.reflow ("The app uses " <> name <> ", so the new value applies from the next deploy. Deploy now with `lamdera env set " <> name <> " <value> --redeploy`, or with \"Deploy changes\" on the dashboard."))
          | otherwise =
              said (D.reflow ("The app's code does not read " <> name <> " yet."))
      in
      D.vcat [ said (D.dullgreen ("Set" <+> nameDoc)), followUp ]

    Remove name ->
      said (D.dullgreen ("Removed" <+> D.fromChars name))
