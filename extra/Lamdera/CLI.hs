{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI (live, login, check, deploy, reset, checkElmPages, annotate, eval) where

import Text.Read (readMaybe)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Data.List as List

import Terminal
import Terminal.Helpers
import qualified Develop -- has Develop.run for live
import qualified Lamdera.CLI.Login
import qualified Lamdera.CLI.Check
import qualified Lamdera.CLI.Deploy
import qualified Lamdera.CLI.Reset
import qualified Lamdera.CLI.CheckElmPages
import qualified Lamdera.CLI.Annotate
import qualified Lamdera.CLI.Interpreter


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
  Terminal.Command "login" (Common summary) details example noArgs noFlags Lamdera.CLI.Login.run


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
  Terminal.Command "check" (Common summary) details example noArgs noFlags Lamdera.CLI.Check.run


deploy :: Terminal.Command
deploy =
  let
    summary =
      "Deploy Lamdera app after a successful `lamdera check`"

    details =
      "The `deploy` command is equivalent to `lamdera check && git push lamdera main`"

    example =
      reflow
        "It will query the production enviornment and supply\
        \ information about the next version and required migrations, \
        \ and then attempt to deploy."
  in
  Terminal.Command "deploy" (Common summary) details example noArgs noFlags Lamdera.CLI.Deploy.run


reset :: Terminal.Command
reset =
  let
    summary =
      "Delete all compiler caches, useful for cache issues or new releases requiring it"

    details =
      "The `reset` command is equivalent to `rm -rf $ELM_HOME elm-stuff`"

    example =
      reflow
        "It will find the location of your configured ELM_HOME directory,\
        \ as well as your current project's elm-stuff cache, \
        \ and then attempt to remove them."
  in
  Terminal.Command "reset" (Common summary) details example noArgs noFlags Lamdera.CLI.Reset.run


checkElmPages :: Terminal.Command
checkElmPages =
  let
    summary =
      "Compile an elm-pages project and type-check all Data types for wire compatibility."

    details =
      "This is an elm-pages specific command."

    example =
      reflow
        "It will type-check all Data types for wire compatibility"
  in
  Terminal.Command "check-elm-pages" (Common summary) details example noArgs noFlags Lamdera.CLI.CheckElmPages.run


annotate :: Terminal.Command
annotate =
  let
    summary =
      "Lookup and print out the type annotation for the given file:expression."

    details =
      "The project should compile successfully before this command works consistently."

    example =
      reflow
        "It will attempt to load the artifacts cache for the given filename, and then \
        \ attempt to load the inferred annotation and display it as text."

    args =
      oneOf
        [ require2 Lamdera.CLI.Annotate.Args elmFile Lamdera.CLI.Annotate.expressionName
        ]
  in
  Terminal.Command "annotate" (Common summary) details example args noFlags Lamdera.CLI.Annotate.run


eval :: Terminal.Command
eval =
  let
    summary =
      "Lookup and evaluate the given file:expression."

    details =
      "This uses a native interpreter for Elm, implemented in Haskell."

    example =
      reflow
        "It will attempt to find the given filename:expression, and then evaluate it \
        \ until it is fully reduced to a value."

    args =
      oneOf
        [ require2 Lamdera.CLI.Interpreter.Args elmFile Lamdera.CLI.Interpreter.expressionName
        ]
  in
  Terminal.Command "eval" (Common summary) details example args noFlags Lamdera.CLI.Interpreter.run


-- HELPERS


stack :: [P.Doc] -> P.Doc
stack docs =
  P.vcat $ List.intersperse "" docs


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string
