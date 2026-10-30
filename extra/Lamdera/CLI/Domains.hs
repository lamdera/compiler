{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Domains where

{- `lamdera domains [list | add <domain> | remove <domain>]`: the app's custom domains. -}

import qualified Data.Aeson as Aeson
import qualified Data.Text as T

import qualified Reporting.Doc as D
import Reporting.Doc ((<+>))

import Lamdera.CLI.Dashboard hiding (Common(..))
import qualified Lamdera.CLI.Dashboard as Dash


data Args
  = List
  | Add String
  | Remove String
  deriving (Show, Eq)


data Flags =
  Flags
    { _app :: Maybe String
    , _json :: Bool
    , _yes :: Bool
    }
    deriving (Show, Eq)


common :: Flags -> Dash.Common
common flags =
  Dash.Common (_app flags) (_json flags) (_yes flags)


toRequest :: T.Text -> Args -> Request
toRequest appName args =
  case args of
    List -> appRequest appName "domainList" []
    -- issuing a certificate can take a few minutes
    Add domain -> (appRequest appName "domainAdd" [ ("domain", FString (T.pack domain)) ]) { _timeoutMs = 300000 }
    Remove domain -> appRequest appName "domainRemove" [ ("domain", FString (T.pack domain)) ]


run :: Args -> Flags -> IO ()
run args flags = do
  let c = common flags
  ctx <- setup c NeedsApp
  let appName = appOf ctx
  case args of
    Remove domain -> confirm c ("remove the domain " <> T.pack domain <> " from " <> appName <> " (it stops serving the app)") Nothing
    _ -> pure ()
  payload <- call ctx (toRequest appName args)
  output ctx payload (render (_web ctx) appName args payload)


render :: String -> T.Text -> Args -> Aeson.Value -> D.Doc
render web appName args payload =
  case args of
    List ->
      case list payload of
        [] ->
          D.vcat
            [ said ("No custom domains on" <+> D.fromChars (T.unpack appName) <> ".")
            , "Add one with `lamdera domains add <domain>`."
            ]
        domains ->
          D.vcat
            [ lines_ $
                table [ "DOMAIN", "STATUS", "DNS", "CHECKED", "DETAIL" ]
                  (fmap (\d -> [ textAt "domain" d, textAt "status" d, textAt "dns" d, timeAt "checkedAt" d, textAt "detail" d ]) domains)
            , ""
            , link web appName "/domains"
            ]

    Add domain ->
      let
        status = textAt "status" payload
        headline =
          if status == "active"
            then said (D.dullgreen (D.fromChars (domain <> " is active")))
            else said (D.dullyellow (D.fromChars (domain <> ": " <> T.unpack status)))
        instructions =
          case textAt "instructions" payload of
            "-" -> []
            text -> [ "", D.reflow (T.unpack text) ]
      in
      D.vcat ([ headline ] ++ instructions ++ [ "", "Status:", link web appName "/domains" ])

    Remove domain ->
      said (D.dullgreen (D.fromChars ("Removed " <> domain)))
