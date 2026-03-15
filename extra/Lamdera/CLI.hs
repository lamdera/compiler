{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI (live, login, project, teams, ssh, env, check, deploy, reset, update, annotate, eval, backend, format) where

import Text.Read (readMaybe)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Data.List as List
import qualified Data.Char as Char

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
import qualified Lamdera.CLI.Backend
import qualified Lamdera.CLI.Dashboard


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
        |-- onOff "open" "Open a web browser to view the local server"
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


project :: Terminal.Command
project =
  let
    summary =
      "Create and manage Lamdera projects on the Dashboard."

    details =
      "The `project` command currently supports the `add` workflow:"

    example =
      stack
        [ reflow "For example:"
        , P.indent 4 $ P.green "lamdera project add my-client-app --scope=client-team --remote"
        , P.indent 4 $ P.green "lamdera project add"
        , reflow "It creates the remote project first, and can add the local `lamdera` git remote afterwards."
        ]

    projectArgs =
      oneOf
        [ require1 (\_ -> Lamdera.CLI.Dashboard.ProjectAdd Nothing) projectAddActionParser
        , require2 (\_ name -> Lamdera.CLI.Dashboard.ProjectAdd (Just name)) projectAddActionParser projectNameParser
        ]

    projectFlags =
      flags Lamdera.CLI.Dashboard.ProjectFlags
        |-- flag "scope" scopeParser "Team scope to create the project under. Omit this to use your personal account."
        |-- flag "team" teamParser "Alias for `--scope`."
        |-- flag "plan" planParser "Requested plan for the new app. Supported values are `play`, `hobby`, `pro`, and `enterprise`."
        |-- onOff "remote" "Automatically add the local `lamdera` git remote after the app is created."
  in
  Terminal.Command "project" (Common summary) details example projectArgs projectFlags Lamdera.CLI.Dashboard.runProject


teams :: Terminal.Command
teams =
  let
    summary =
      "Create and manage Lamdera teams on the Dashboard."

    details =
      "The `teams` command currently supports the `add` workflow:"

    example =
      stack
        [ reflow "For example:"
        , P.indent 4 $ P.green "lamdera teams add client-team billing@example.com"
        , P.indent 4 $ P.green "lamdera teams add"
        ]
  in
  Terminal.Command "teams" (Common summary) details example teamsArgs noFlags Lamdera.CLI.Dashboard.runTeams
  where
    teamsArgs =
      oneOf
        [ require1 (\_ -> Lamdera.CLI.Dashboard.TeamAdd Nothing Nothing) teamsAddActionParser
        , require2 (\_ name -> Lamdera.CLI.Dashboard.TeamAdd (Just name) Nothing) teamsAddActionParser teamNameParser
        , require3 (\_ name email -> Lamdera.CLI.Dashboard.TeamAdd (Just name) (Just email)) teamsAddActionParser teamNameParser emailParser
        ]


ssh :: Terminal.Command
ssh =
  let
    summary =
      "Manage SSH keys for Lamdera deploy access."

    details =
      "The `ssh` command currently supports the `add` workflow:"

    example =
      stack
        [ reflow "For example:"
        , P.indent 4 $ P.green "lamdera ssh add ~/.ssh/id_ed25519.pub"
        , P.indent 4 $ P.green "lamdera ssh add ~/.ssh/id_ed25519.pub laptop"
        ]

    sshArgs =
      oneOf
        [ require2 (\_ path -> Lamdera.CLI.Dashboard.SshAdd path Nothing) sshAddActionParser publicKeyFileParser
        , require3 (\_ path label -> Lamdera.CLI.Dashboard.SshAdd path (Just label)) sshAddActionParser publicKeyFileParser sshKeyLabelParser
        ]

    sshKeyFlags =
      flags Lamdera.CLI.Dashboard.SshKeyFlags
        |-- flag "label" sshKeyLabelParser "Optional label shown in the Dashboard for this key."
  in
  Terminal.Command "ssh" (Common summary) details example sshArgs sshKeyFlags Lamdera.CLI.Dashboard.runSsh


env :: Terminal.Command
env =
  let
    summary =
      "Manage Lamdera environment variables."

    details =
      "The `env` command supports `ls`, `add`, `update`, and `rm`. The environment argument is accepted for compatibility, but Lamdera applies env vars to the target app:"

    example =
      stack
        [ reflow "For example:"
        , P.indent 4 $ P.green "lamdera env ls --project=my-client-app"
        , P.indent 4 $ P.green "lamdera env add API_KEY production"
        , P.indent 4 $ P.green "lamdera env update API_KEY production"
        , P.indent 4 $ P.green "lamdera env rm API_KEY production"
        , P.indent 4 $ P.green "lamdera env set API_KEY secret-value"
        ]

    envArgs =
      oneOf
        [ require1 (\_ -> Lamdera.CLI.Dashboard.EnvList) envListActionParser
        , require2 (\_ name -> Lamdera.CLI.Dashboard.EnvAdd name Nothing Nothing) envAddActionParser envNameParser
        , require3 (\_ name environment -> Lamdera.CLI.Dashboard.EnvAdd name (Just environment) Nothing) envAddActionParser envNameParser envEnvironmentParser
        , require4 (\_ name environment value -> Lamdera.CLI.Dashboard.EnvAdd name (Just environment) (Just value)) envAddActionParser envNameParser envEnvironmentParser envValueParser
        , require2 (\_ name -> Lamdera.CLI.Dashboard.EnvUpdate name Nothing Nothing) envUpdateActionParser envNameParser
        , require3 (\_ name environment -> Lamdera.CLI.Dashboard.EnvUpdate name (Just environment) Nothing) envUpdateActionParser envNameParser envEnvironmentParser
        , require4 (\_ name environment value -> Lamdera.CLI.Dashboard.EnvUpdate name (Just environment) (Just value)) envUpdateActionParser envNameParser envEnvironmentParser envValueParser
        , require3 (\_ name value -> Lamdera.CLI.Dashboard.EnvSet name value) envSetActionParser envNameParser envValueParser
        , require2 (\_ name -> Lamdera.CLI.Dashboard.EnvRemove name Nothing) envRemoveActionParser envNameParser
        , require3 (\_ name environment -> Lamdera.CLI.Dashboard.EnvRemove name (Just environment)) envRemoveActionParser envNameParser envEnvironmentParser
        ]

    envFlags =
      flags Lamdera.CLI.Dashboard.EnvFlags
        |-- flag "project" appNameParser "Target project name. If omitted, Lamdera will try to infer it from the current repo."
        |-- flag "app" appNameParser "Alias for `--project`."
        |-- onOff "public" "Store the variable as public instead of secret. Secret values remain backend-only."
  in
  Terminal.Command "env" (Common summary) details example envArgs envFlags Lamdera.CLI.Dashboard.runEnv


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


-- HELPERS


stack :: [P.Doc] -> P.Doc
stack docs =
  P.vcat $ List.intersperse "" docs


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string


nonEmptyParser :: String -> String -> [String] -> Parser String
nonEmptyParser singular plural examples =
  Parser
    { _singular = singular
    , _plural = plural
    , _parser = parseNonEmpty
    , _suggest = \_ -> return []
    , _examples = \_ -> return examples
    }


literalParser :: String -> [String] -> [String] -> Parser String
literalParser singular accepted examples =
  let
    acceptedLower =
      map (map Char.toLower) accepted
  in
  Parser
    { _singular = singular
    , _plural = singular <> "s"
    , _parser = \value ->
        let normalized = map Char.toLower value in
        if elem normalized acceptedLower
          then Just normalized
          else Nothing
    , _suggest = \_ -> return accepted
    , _examples = \_ -> return examples
    }


parseNonEmpty :: String -> Maybe String
parseNonEmpty value =
  if null value
    then Nothing
    else Just value


projectNameParser :: Parser String
projectNameParser =
  nonEmptyParser "project name" "project names" ["my-client-app"]


teamNameParser :: Parser String
teamNameParser =
  nonEmptyParser "team name" "team names" ["client-team"]


teamParser :: Parser String
teamParser =
  nonEmptyParser "team" "teams" ["client-team"]


scopeParser :: Parser String
scopeParser =
  nonEmptyParser "scope" "scopes" ["client-team"]


appNameParser :: Parser String
appNameParser =
  nonEmptyParser "project name" "project names" ["my-client-app"]


publicKeyFileParser :: Parser FilePath
publicKeyFileParser =
  Parser
    { _singular = "public key file"
    , _plural = "public key files"
    , _parser = parseNonEmpty
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["~/.ssh/id_ed25519.pub"]
    }


sshKeyLabelParser :: Parser String
sshKeyLabelParser =
  nonEmptyParser "label" "labels" ["laptop"]


emailParser :: Parser String
emailParser =
  Parser
    { _singular = "email"
    , _plural = "emails"
    , _parser = \value ->
        if '@' `elem` value && '.' `elem` value
          then Just value
          else Nothing
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["billing@example.com"]
    }


planParser :: Parser String
planParser =
  literalParser "plan" ["play", "hobby", "pro", "enterprise"] ["hobby", "pro"]


projectAddActionParser :: Parser String
projectAddActionParser =
  literalParser "action" ["add"] ["add"]


teamsAddActionParser :: Parser String
teamsAddActionParser =
  literalParser "action" ["add"] ["add"]


sshAddActionParser :: Parser String
sshAddActionParser =
  literalParser "action" ["add"] ["add"]


envListActionParser :: Parser String
envListActionParser =
  literalParser "action" ["list", "ls"] ["ls"]


envAddActionParser :: Parser String
envAddActionParser =
  literalParser "action" ["add"] ["add"]


envUpdateActionParser :: Parser String
envUpdateActionParser =
  literalParser "action" ["update"] ["update"]


envSetActionParser :: Parser String
envSetActionParser =
  literalParser "action" ["set"] ["set"]


envRemoveActionParser :: Parser String
envRemoveActionParser =
  literalParser "action" ["remove", "rm"] ["remove"]


envEnvironmentParser :: Parser String
envEnvironmentParser =
  literalParser "environment" ["production", "prod", "preview", "development", "dev"] ["production", "preview", "development"]


envNameParser :: Parser String
envNameParser =
  Parser
    { _singular = "environment variable name"
    , _plural = "environment variable names"
    , _parser = parseEnvName
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["apiKey", "stripeSecret"]
    }


parseEnvName :: String -> Maybe String
parseEnvName chars =
  case chars of
    first : rest ->
      if (Char.isAlpha first || first == '_') && all (\c -> Char.isAlphaNum c || c == '_') rest
        then Just chars
        else Nothing

    _ ->
      Nothing


envValueParser :: Parser String
envValueParser =
  nonEmptyParser "environment variable value" "environment variable values" ["secret-value"]
