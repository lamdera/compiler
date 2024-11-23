{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI (live, login, check, deploy, reset, update, annotate, eval) where

import Text.Read (readMaybe)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Data.List as List

import Terminal hiding (args)
import Terminal.Helpers
import qualified Develop -- has Develop.run for live
import qualified Lamdera.CLI.Login
import qualified Lamdera.CLI.Check
import qualified Lamdera.CLI.Deploy
import qualified Lamdera.CLI.Reset
import qualified Lamdera.CLI.Update
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
      "Compile and type-check against the deployed production app. \
      \Generates type snapshots and migrations if necessary."

    details =
      "The `check` command helps prepare Lamdera Elm projects for deployment:"

    example =
      reflow
        "It will compile the project and type-check migrations against the deployed production app. \
        \ It will generate type snapshots and migrations if necessary."

    checkFlags =
      flags Lamdera.CLI.Check.Flags
        |-- onOff "destructive-migration" "Generate a migration that will drop all production data when deployed, instead of the usual automatic migration generation."
        |-- onOff "force" "Force a production check for Evergreen, even if we're on a non main/master branch (i.e. a preview). You shouldn't be using this unless you know what you're doing."
  in
  Terminal.Command "check" (Common summary) details example noArgs checkFlags Lamdera.CLI.Check.run


deploy :: Terminal.Command
deploy =
  let
    summary =
      "Deploy Lamdera app after a successful `lamdera check`"

    details =
      "The `deploy` command is equivalent to `lamdera check && git push lamdera main`"

    example =
      reflow
        "It will query the production environment and supply\
        \ information about the next version and required migrations, \
        \ and then attempt to deploy. Deploying from non-main/master \
        \ branches will create/replace a preview app named after the branch."
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


update :: Terminal.Command
update =
  let
    summary =
      "Update the Lamdera compiler to the latest version if out of date."

    details =
      "The latest versions of the Lamdera compiler are available at https://dashboard.lamdera.app/docs/download"

    example =
      reflow
        "It will find the latest lamdera binary version, download it, and replace itself."

    updateFlags =
      flags Lamdera.CLI.Update.Flags
        |-- onOff "force" "Force update to the latest published version, regardless of what version is installed currently."
  in
  Terminal.Command "update" (Common summary) details example noArgs updateFlags Lamdera.CLI.Update.run


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
