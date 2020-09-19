{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI (live, login, check, deploy) where

import Text.Read (readMaybe)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Data.List as List

import Terminal
import qualified Develop -- has Develop.run for live
import qualified Lamdera.Login
import qualified Lamdera.Check
import qualified Lamdera.Deploy


live :: Terminal.Command
live =
  let
    summary =
      "Local development for full-stack Lamdera with live reload."

    details =
      "The `live` command starts a local server on your computer:"

    example =
      reflow
        "After running that command, you would have a server at <http://localhost:8000>\
        \ that helps with development. It emulates the Lamdera frontend/backend stack, \
        \ automatically reloads with state-restoration on changes to the project directory\
        \ and provides some UI utilities to help with development."

    liveFlags =
      flags Develop.Flags
        |-- flag "port" port_ "The port of the server (default: 8000)"
  in
  Terminal.Command "live" (Common summary) details example noArgs liveFlags Develop.run


port_ :: Parser Int
port_ =
  Parser
    { _singular = "port"
    , _plural = "ports"
    , _parser = readMaybe
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["3000","8000"]
    }


login :: Terminal.Command
login =
  let
    summary =
      "Log in to the Lamdera CLI."

    details =
      "The `lamdera` command authenticates the Lamdera CLI."

    example =
      reflow
        "It will open the Dashboard to authenticate you and use the\
        \ established session for other CLI operations requiring Dashboard info."
  in
  Terminal.Command "login" (Common summary) details example noArgs noFlags Lamdera.Login.run


check :: Terminal.Command
check =
  let
    summary =
      "Compile Lamdera project and type-check Evergreen migrations \
      \against the deployed production app."

    details =
      "The `check` command helps prepare Lamdera Elm projects for deployment:"

    example =
      reflow
        "It will query the production environment and supply\
        \ information about the next version and required migrations."
  in
  Terminal.Command "check" (Common summary) details example noArgs noFlags Lamdera.Check.run


deploy :: Terminal.Command
deploy =
  let
    summary =
      "Deploy Lamdera app after a successful `lamdera check`"

    details =
      "The `deploy` command is equivalent to `lamdera check && git push lamdera master`"

    example =
      reflow
        "It will query the production enviornment and supply\
        \ information about the next version and required migrations, \
        \ and then attempt to deploy."
  in
  Terminal.Command "deploy" (Common summary) details example noArgs noFlags Lamdera.Deploy.run


-- HELPERS


stack :: [P.Doc] -> P.Doc
stack docs =
  P.vcat $ List.intersperse "" docs


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string
