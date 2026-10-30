{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI (live, login, check, deploy, reset, update, annotate, eval, backend, format, app, env, domains, logs, events, appArgs, appFlags, envArgs, envFlags, domainsArgs, domainsFlags, logsFlags, eventsArgs, eventsFlags) where

import Text.Read (readMaybe)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Data.List as List

import Terminal hiding (args, app)
import Terminal.Helpers
import qualified Develop -- has Develop.run for live
import qualified Lamdera.CLI.Login
import qualified Lamdera.CLI.Check
import qualified Lamdera.CLI.Deploy
import qualified Lamdera.CLI.Reset
import qualified Lamdera.CLI.Update
import qualified Lamdera.CLI.Annotate
import qualified Lamdera.CLI.Interpreter
import qualified Lamdera.CLI.Backend
import qualified Lamdera.CLI.App
import qualified Lamdera.CLI.Env
import qualified Lamdera.CLI.Domains
import qualified Lamdera.CLI.Logs
import qualified Lamdera.CLI.Events
import Lamdera.CLI.Dashboard (appName_, int_, text_, verb, choice)


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
        |-- flag "bind" bind_ "The address to listen on (default: 127.0.0.1, only this computer). Use --bind=0.0.0.0 to open the app from other devices on your network, like your phone. The REPL and file tools stay available only on this computer."
        |-- onOff "open" "Open a web browser to view the local server"
        |-- onOff "memory" "Keep the build artifacts and the sources only in memory between rebuilds, instead of writing elm-stuff on every rebuild and reading the sources from disk. elm-stuff is written on exit. (Either way `lamdera live` keeps what it built between rebuilds, reuses what did not change and sends hot patches; this only changes where it is kept.)"
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


bind_ :: Parser String
bind_ =
  Parser
    { _singular = "address"
    , _plural = "addresses"
    , _parser = Just
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["127.0.0.1","0.0.0.0"]
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
        |-- onOff "destructive-migration-except-backend" "Generate a migration that migrates the BackendModel as usual, but resets the FrontendModel and ignores in-flight messages from the old version if their types changed, instead of generating migrations for them."
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


backend :: Terminal.Command
backend =
  let
    summary =
      "Access the backend model."

    details =
      "The `backend` command gives you access to the backend model of the `lamdera live` session."

    example =
      stack
        [ "It evaluates the given expression (see below) and returns its value."
        , "In the expression, you can access the backend model under the name `model`."
        , reflow
            "The variable `model` contains the backend model of the current `lamdera live` session. \
            \ If the `lamdera live` server is not running, \
            \ it contains the saved backend model of the last `lamdera live` session."
        ]

    backendFlags =
      flags Lamdera.CLI.Backend.Flags
        |-- flag "eval" expression "The expression to evaluate (default: `model`)."
        |-- flag "import" Lamdera.CLI.Backend.importParser
              "Additional module imports used when evaluating the expression.\
              \ For the allowed syntax, see the Lamdera documentation.\
              \ Examples are 'Dict' or 'Dict, Set as S exposing (size)'."
        |-- onOff "repl" "Start a REPL session instead of evaluating a single expression."
        |-- flag "port" port_ "The port of the `lamdera live` server (default: 8000)."
        |-- onOff "no-colors" "Disable colors in the output. This can help if your terminal uses a non-standard color scheme that makes values hard to read."
        |-- flag "interpreter" interpreter "Path to an alternative JavaScript interpreter, such as `node` or `nodejs`."
  in
  Terminal.Command "backend" (Common summary) details example noArgs backendFlags Lamdera.CLI.Backend.run


interpreter :: Parser String
interpreter =
  Parser
    { _singular = "interpreter"
    , _plural = "interpreters"
    , _parser = Just
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["node","nodejs"]
    }


expression :: Parser String
expression =
  Parser
    { _singular = "expression"
    , _plural = "expressions"
    , _parser = Just
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["model","Debug.toString model"]
    }



-- FORMAT
-- @LAMDERA Stub - intercepted in Terminal.hs


format :: Terminal.Command
format =
  let
    summary =
      "Format Elm source files."

    details =
      "The `format` command is proxied to the embedded copy of elm-format."

    example =
      reflow "See elm-format documentation at <https://github.com/avh4/elm-format>"
  in
  Terminal.Command "format" (Common summary) details example noArgs noFlags (\_ _ -> return ())



-- APP MANAGEMENT
-- @LAMDERA the dashboard's app management from the CLI (Lamdera.CLI.Dashboard)


appFlagHelp, jsonFlagHelp, yesFlagHelp :: String
appFlagHelp = "The app to act on (default: the app of the `lamdera` git remote of the project in the current directory)."
jsonFlagHelp = "Print only the result, as JSON on stdout (errors as {\"error\": \"...\"} with exit code 1). For scripts and agents."
yesFlagHelp = "Do it without asking. Needed for destructive operations when there is no terminal to ask on, or with --json."


commonNote :: P.Doc
commonNote =
  reflow
    "Every app management command (app, env, domains, logs, events) takes --app NAME \
    \ (default: the app of the project you are in) and --json (machine readable output). \
    \ They need a CLI session: run `lamdera login` first."


examples :: [String] -> P.Doc
examples lines_ =
  P.indent 4 $ P.green $ P.vcat $ fmap P.text lines_


app :: Terminal.Command
app =
  let
    summary =
      "Manage your Lamdera apps: show, list, create, delete, change plan."

    details =
      "The `app` command manages your Lamdera apps, like the dashboard does:"

    example =
      stack
        [ examples
            [ "lamdera app                       # this app: plan, version, deploys, domains"
            , "lamdera app list                  # all your apps"
            , "lamdera app create my-app --plan hobby"
            , "lamdera app plan professional     # change this app's plan"
            , "lamdera app delete --app my-app --yes"
            ]
        , reflow "`app delete` deletes the app and all its data for good: it asks you to type the app's name, or needs --yes."
        , commonNote
        ]
  in
  Terminal.Command "app" (Common summary) details example appArgs appFlags Lamdera.CLI.App.run


appArgs :: Args Lamdera.CLI.App.Args
appArgs =
  oneOf
    [ require0 Lamdera.CLI.App.Info
    , require1 (const Lamdera.CLI.App.Info) (verb "info")
    , require1 (const Lamdera.CLI.App.List) (verb "list")
    , require2 (\_ name -> Lamdera.CLI.App.Create name) (verb "create") appName_
    , require1 (const Lamdera.CLI.App.Delete) (verb "delete")
    , require2 (\_ plan -> Lamdera.CLI.App.Plan plan) (verb "plan") (choice "plan" Lamdera.CLI.App.plans)
    ]


appFlags :: Flags Lamdera.CLI.App.Flags
appFlags =
  flags Lamdera.CLI.App.Flags
    |-- flag "app" appName_ appFlagHelp
    |-- onOff "json" jsonFlagHelp
    |-- onOff "yes" yesFlagHelp
    |-- flag "team" (text_ "team id" ["my-team"]) "`app create`: create the app in this team instead of your own account."
    |-- flag "plan" (choice "plan" Lamdera.CLI.App.plans) "`app create`: the plan to start on (default: free)."


env :: Terminal.Command
env =
  let
    summary =
      "Manage the app's environment variables (read in your code through `Env`)."

    details =
      "The `env` command manages the environment variables of a Lamdera app:"

    example =
      stack
        [ examples
            [ "lamdera env                                   # list them"
            , "lamdera env set stripeKey sk_live_123 --secret"
            , "lamdera env set greeting Hello --redeploy     # and deploy, if the app uses it"
            , "lamdera env remove greeting --yes"
            ]
        , reflow "Values are compiled into the app, so a new value of a variable the app uses takes effect with the next deploy: --redeploy deploys right away. A variable the app uses cannot be removed."
        , commonNote
        ]
  in
  Terminal.Command "env" (Common summary) details example envArgs envFlags Lamdera.CLI.Env.run


envArgs :: Args Lamdera.CLI.Env.Args
envArgs =
  oneOf
    [ require0 Lamdera.CLI.Env.List
    , require1 (const Lamdera.CLI.Env.List) (verb "list")
    , require3 (\_ name value -> Lamdera.CLI.Env.Set name value) (verb "set") (text_ "name" ["stripeKey"]) (text_ "value" ["sk_live_123"])
    , require2 (\_ name -> Lamdera.CLI.Env.Remove name) (verb "remove") (text_ "name" ["stripeKey"])
    ]


envFlags :: Flags Lamdera.CLI.Env.Flags
envFlags =
  flags Lamdera.CLI.Env.Flags
    |-- flag "app" appName_ appFlagHelp
    |-- onOff "json" jsonFlagHelp
    |-- onOff "yes" yesFlagHelp
    |-- onOff "secret" "`env set`: hide the value in the dashboard and in listings."
    |-- onOff "redeploy" "`env set`: deploy the app with the new value now, if the app uses it."


domains :: Terminal.Command
domains =
  let
    summary =
      "Manage the app's custom domains."

    details =
      "The `domains` command manages the custom domains of a Lamdera app:"

    example =
      stack
        [ examples
            [ "lamdera domains                    # list them, with their status"
            , "lamdera domains add example.com    # then point example.com at the app as shown"
            , "lamdera domains remove example.com --yes"
            ]
        , reflow "A domain is active once its DNS points at the app and its certificate is issued: `add` tells you which DNS record to create, and `domains` shows how far along it is. Custom domains are part of the Professional plan."
        , commonNote
        ]
  in
  Terminal.Command "domains" (Common summary) details example domainsArgs domainsFlags Lamdera.CLI.Domains.run


domainsArgs :: Args Lamdera.CLI.Domains.Args
domainsArgs =
  oneOf
    [ require0 Lamdera.CLI.Domains.List
    , require1 (const Lamdera.CLI.Domains.List) (verb "list")
    , require2 (\_ domain -> Lamdera.CLI.Domains.Add domain) (verb "add") (text_ "domain" ["example.com"])
    , require2 (\_ domain -> Lamdera.CLI.Domains.Remove domain) (verb "remove") (text_ "domain" ["example.com"])
    ]


domainsFlags :: Flags Lamdera.CLI.Domains.Flags
domainsFlags =
  flags Lamdera.CLI.Domains.Flags
    |-- flag "app" appName_ appFlagHelp
    |-- onOff "json" jsonFlagHelp
    |-- onOff "yes" yesFlagHelp


logs :: Terminal.Command
logs =
  let
    summary =
      "Show the app's log."

    details =
      "The `logs` command shows the log of a deployed Lamdera app:"

    example =
      stack
        [ examples
            [ "lamdera logs                     # the last 100 lines"
            , "lamdera logs --lines 500 --grep error"
            , "lamdera logs --msg 48213         # what msg #48213 logged (see `lamdera events`)"
            , "lamdera logs --follow            # keep showing new lines"
            ]
        , reflow "Lines a msg logged are tagged with the msg's event id, `#N`: `lamdera events show N` shows the msg itself."
        , commonNote
        ]
  in
  Terminal.Command "logs" (Common summary) details example noArgs logsFlags Lamdera.CLI.Logs.run


logsFlags :: Flags Lamdera.CLI.Logs.Flags
logsFlags =
  flags Lamdera.CLI.Logs.Flags
    |-- flag "app" appName_ appFlagHelp
    |-- onOff "json" jsonFlagHelp
    |-- flag "lines" (int_ "number" ["100", "1000"]) "How many log records to show (default: 100)."
    |-- onOff "head" "Show the oldest records instead of the newest."
    |-- flag "grep" (text_ "text" ["error"]) "Only records containing this text (case insensitive)."
    |-- flag "msg" (int_ "event id" ["48213"]) "Only what the msg with this event id logged."
    |-- onOff "follow" "Keep showing new records as they are logged (every 2 seconds) until Ctrl-C."


events :: Terminal.Command
events =
  let
    summary =
      "Browse the app's msg events: what came in, how long it took, what it logged."

    details =
      "The `events` command browses the msg event log of a deployed Lamdera app:"

    example =
      stack
        [ examples
            [ "lamdera events                          # the latest 50"
            , "lamdera events --kind ToBackend --name AddTodo"
            , "lamdera events --min-duration 100       # slower than 100ms"
            , "lamdera events show 48213               # its decoded value and log lines"
            , "lamdera events metrics --window 24h     # counts, timings and failures by msg"
            ]
        , reflow "Kinds: ToBackend, BackendMsg, ToFrontend, Connect, Disconnect, RPC, Upgrade, Restore, Snapshot, CodecFailure, Failure. Msg names and values are shown for versions deployed with a compiler that writes a wire schema (1.5.1 and later)."
        , commonNote
        ]
  in
  Terminal.Command "events" (Common summary) details example eventsArgs eventsFlags Lamdera.CLI.Events.run


eventsArgs :: Args Lamdera.CLI.Events.Args
eventsArgs =
  oneOf
    [ require0 Lamdera.CLI.Events.List
    , require1 (const Lamdera.CLI.Events.List) (verb "list")
    , require2 (\_ n -> Lamdera.CLI.Events.Show n) (verb "show") (int_ "event id" ["48213"])
    , require1 (const Lamdera.CLI.Events.Metrics) (verb "metrics")
    ]


eventsFlags :: Flags Lamdera.CLI.Events.Flags
eventsFlags =
  flags Lamdera.CLI.Events.Flags
    |-- flag "app" appName_ appFlagHelp
    |-- onOff "json" jsonFlagHelp
    |-- flag "limit" (int_ "number" ["50", "200"]) "How many events to list (default: 50, at most 200)."
    |-- flag "before" (int_ "event id" ["48213"]) "List the events older than this one (for paging back)."
    |-- flag "kind" (text_ "kind" ["ToBackend", "BackendMsg"]) "Only events of this kind."
    |-- flag "name" (text_ "msg name" ["AddTodo"]) "Only msgs with this constructor name."
    |-- flag "min-duration" (int_ "milliseconds" ["100"]) "Only events that took at least this long."
    |-- flag "window" (choice "window" Lamdera.CLI.Events.windows) "`events metrics`: the time window, 1h, 24h or 7d (default: 1h)."


-- HELPERS


stack :: [P.Doc] -> P.Doc
stack docs =
  P.vcat $ List.intersperse "" docs


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string