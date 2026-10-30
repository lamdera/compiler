{-# LANGUAGE OverloadedStrings #-}

module Test.CLI where

{-| The app management commands (Lamdera.CLI.Dashboard and its commands): argument
parsing, request bodies, response envelopes and rendering, all pure; then the commands
end to end against a stand-in dashboard (test/cli-stub/stub.js) reached through
LAMDERA_DASHBOARD_URL.
-}

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.Directory as Dir
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hGetLine)
import qualified System.Process as Process
import qualified Test.Main

import EasyTest
import Test.Helpers
import Lamdera

import qualified Reporting.Doc as D
import qualified Terminal.Chomp as Chomp
import Terminal (Args, Flags)
import qualified Terminal

import qualified Lamdera.CLI
import qualified Lamdera.CLI.App as App
import qualified Lamdera.CLI.Dashboard as Dash
import qualified Lamdera.CLI.Domains as Domains
import qualified Lamdera.CLI.Env as Env
import qualified Lamdera.CLI.Events as Events
import qualified Lamdera.CLI.Logs as Logs
import qualified Lamdera.Relative


suite :: Test ()
suite =
  tests
    [ scope "parsing" parsing
    , scope "requests" requests
    , scope "envelopes" envelopes
    , scope "rendering" rendering
    , scope "logs --follow" following
    , scope "confirmation" confirmation
    , scope "against a stub dashboard" $ requireBinaryOnPath "node" endToEnd
    ]



-- PARSING


parse :: Args a -> Flags f -> [String] -> Maybe (a, f)
parse args flags strings =
  case snd (Chomp.chomp Nothing strings args flags) of
    Right v -> Just v
    Left _ -> Nothing


appF :: App.Flags
appF = App.Flags Nothing False False Nothing Nothing


envF :: Env.Flags
envF = Env.Flags Nothing False False False False


eventsF :: Events.Flags
eventsF = Events.Flags Nothing False Nothing Nothing Nothing Nothing Nothing Nothing


logsF :: Logs.Flags
logsF = Logs.Flags Nothing False Nothing False Nothing Nothing False


parsing :: Test ()
parsing =
  tests
    [ scope "app" $ do
        expectEqual (Just (App.Info, appF)) (parse Lamdera.CLI.appArgs Lamdera.CLI.appFlags [])
        expectEqual (Just (App.Info, appF)) (parse Lamdera.CLI.appArgs Lamdera.CLI.appFlags ["info"])
        expectEqual (Just (App.List, appF { App._json = True })) (parse Lamdera.CLI.appArgs Lamdera.CLI.appFlags ["list", "--json"])
        expectEqual (Just (App.Create "my-app", appF { App._team = Just "t1", App._plan = Just "hobby" }))
          (parse Lamdera.CLI.appArgs Lamdera.CLI.appFlags ["create", "my-app", "--team", "t1", "--plan=hobby"])
        expectEqual (Just (App.Delete, appF { App._app = Just "demo", App._yes = True }))
          (parse Lamdera.CLI.appArgs Lamdera.CLI.appFlags ["delete", "--app", "demo", "--yes"])
        expectEqual (Just (App.Plan "professional", appF)) (parse Lamdera.CLI.appArgs Lamdera.CLI.appFlags ["plan", "professional"])
        expectEqual Nothing (parse Lamdera.CLI.appArgs Lamdera.CLI.appFlags ["plan", "gold"])
        expectEqual Nothing (parse Lamdera.CLI.appArgs Lamdera.CLI.appFlags ["create"])
        expectEqual Nothing (parse Lamdera.CLI.appArgs Lamdera.CLI.appFlags ["bogus"])
        expectEqual Nothing (parse Lamdera.CLI.appArgs Lamdera.CLI.appFlags ["list", "--colour"])

    , scope "env" $ do
        expectEqual (Just (Env.List, envF)) (parse Lamdera.CLI.envArgs Lamdera.CLI.envFlags [])
        expectEqual (Just (Env.Set "stripeKey" "sk 1", envF { Env._secret = True, Env._redeploy = True }))
          (parse Lamdera.CLI.envArgs Lamdera.CLI.envFlags ["set", "stripeKey", "sk 1", "--secret", "--redeploy"])
        expectEqual (Just (Env.Remove "x", envF { Env._yes = True })) (parse Lamdera.CLI.envArgs Lamdera.CLI.envFlags ["remove", "x", "--yes"])
        expectEqual Nothing (parse Lamdera.CLI.envArgs Lamdera.CLI.envFlags ["set", "x"])

    , scope "domains" $ do
        expectEqual (Just (Domains.Add "example.com", Domains.Flags Nothing False False))
          (parse Lamdera.CLI.domainsArgs Lamdera.CLI.domainsFlags ["add", "example.com"])
        expectEqual (Just (Domains.List, Domains.Flags (Just "a") True False))
          (parse Lamdera.CLI.domainsArgs Lamdera.CLI.domainsFlags ["--app=a", "--json"])

    , scope "logs" $ do
        expectEqual (Just ((), logsF { Logs._lines = Just 500, Logs._grep = Just "error", Logs._msg = Just 7, Logs._follow = True }))
          (parse Terminal.noArgs Lamdera.CLI.logsFlags ["--lines", "500", "--grep", "error", "--msg=7", "--follow"])
        expectEqual Nothing (parse Terminal.noArgs Lamdera.CLI.logsFlags ["--lines", "-5"])

    , scope "events" $ do
        expectEqual (Just (Events.List, eventsF { Events._kind = Just "ToBackend", Events._minDuration = Just 100, Events._limit = Just 10 }))
          (parse Lamdera.CLI.eventsArgs Lamdera.CLI.eventsFlags ["--kind", "ToBackend", "--min-duration", "100", "--limit", "10"])
        expectEqual (Just (Events.Show 42, eventsF)) (parse Lamdera.CLI.eventsArgs Lamdera.CLI.eventsFlags ["show", "42"])
        expectEqual (Just (Events.Metrics, eventsF { Events._window = Just "24h" }))
          (parse Lamdera.CLI.eventsArgs Lamdera.CLI.eventsFlags ["metrics", "--window", "24h"])
        expectEqual Nothing (parse Lamdera.CLI.eventsArgs Lamdera.CLI.eventsFlags ["metrics", "--window", "2d"])
        expectEqual Nothing (parse Lamdera.CLI.eventsArgs Lamdera.CLI.eventsFlags ["show", "x"])
    ]



-- REQUESTS


body :: Dash.Request -> T.Text
body req =
  Dash.bodyText "tok" (Dash._body req)


requests :: Test ()
requests =
  tests
    [ scope "app" $ do
        let r = App.toRequest "demo" App.Info appF
        expectEqual "appInfo" (Dash._op r)
        expectEqual "{\"key\":\"tok\",\"appId\":\"demo\"}" (body r)
        expectEqual "{\"key\":\"tok\"}" (body (App.toRequest "" App.List appF))
        expectEqual "{\"key\":\"tok\",\"name\":\"new\",\"plan\":\"hobby\"}" (body (App.toRequest "" (App.Create "new") appF { App._plan = Just "hobby" }))
        expectEqual "{\"key\":\"tok\",\"appId\":\"demo\",\"confirm\":\"demo\"}" (body (App.toRequest "demo" App.Delete appF))
        expectEqual "{\"key\":\"tok\",\"appId\":\"demo\",\"plan\":\"professional\"}" (body (App.toRequest "demo" (App.Plan "professional") appF))
        expectEqual "appUpdate" (Dash._op (App.toRequest "demo" (App.Plan "professional") appF))
        expectEqual "/_r/cli_appList" (Dash.endpointPath "appList")

    , scope "env" $ do
        let r = Env.toRequest "demo" (Env.Set "greeting" "Hi \"you\"" ) envF { Env._redeploy = True }
        expectEqual "envSet" (Dash._op r)
        expectEqual "{\"key\":\"tok\",\"appId\":\"demo\",\"name\":\"greeting\",\"value\":\"Hi \\\"you\\\"\",\"secret\":false,\"redeploy\":true}" (body r)
        expectEqual 600000 (Dash._timeoutMs r)
        expectEqual "{\"key\":\"tok\",\"appId\":\"demo\",\"name\":\"x\"}" (body (Env.toRequest "demo" (Env.Remove "x") envF))

    , scope "domains" $ do
        let r = Domains.toRequest "demo" (Domains.Add "example.com")
        expectEqual ("domainAdd", "{\"key\":\"tok\",\"appId\":\"demo\",\"domain\":\"example.com\"}", 300000) (Dash._op r, body r, Dash._timeoutMs r)

    , scope "logs" $ do
        expectEqual "{\"key\":\"tok\",\"appId\":\"demo\",\"direction\":\"tail\"}" (body (Logs.toRequest "demo" logsF Nothing))
        expectEqual "{\"key\":\"tok\",\"appId\":\"demo\",\"lines\":5,\"direction\":\"head\",\"grep\":\"err\",\"msg\":7}"
          (body (Logs.toRequest "demo" logsF { Logs._lines = Just 5, Logs._head = True, Logs._grep = Just "err", Logs._msg = Just 7 } Nothing))
        expectEqual "{\"key\":\"tok\",\"appId\":\"demo\",\"lines\":1000,\"direction\":\"head\",\"from\":1700000000000}"
          (body (Logs.toRequest "demo" logsF (Just 1700000000000)))

    , scope "events" $ do
        expectEqual "{\"key\":\"tok\",\"appId\":\"demo\",\"before\":90,\"limit\":10,\"tipe\":\"ToBackend\",\"name\":\"AddTodo\",\"minDuration\":250000}"
          (body (Events.toRequest "demo" Events.List eventsF { Events._before = Just 90, Events._limit = Just 10, Events._kind = Just "ToBackend", Events._name = Just "AddTodo", Events._minDuration = Just 250 }))
        expectEqual ("event", "{\"key\":\"tok\",\"appId\":\"demo\",\"id\":42}") (Dash._op (Events.toRequest "demo" (Events.Show 42) eventsF), body (Events.toRequest "demo" (Events.Show 42) eventsF))
        expectEqual "{\"key\":\"tok\",\"appId\":\"demo\",\"window\":\"7d\"}" (body (Events.toRequest "demo" Events.Metrics eventsF { Events._window = Just "7d" }))
    ]



-- ENVELOPES


envelopes :: Test ()
envelopes = do
  expectEqual (Right (Dash.EOk (Aeson.toJSON [1 :: Int, 2]))) (Dash.parseEnvelope "{\"ok\": [1, 2]}")
  expectEqual (Right (Dash.EOk Aeson.Null)) (Dash.parseEnvelope "{\"ok\": null}")
  expectEqual (Right (Dash.EError "nope")) (Dash.parseEnvelope "{\"error\": \"nope\"}")
  expectEqual (Right (Dash.EPending "t1")) (Dash.parseEnvelope "{\"pending\": \"t1\"}")
  expectLeft (Dash.parseEnvelope "[1]") >> ok
  expectLeft (Dash.parseEnvelope "{\"other\": 1}") >> ok
  expectLeft (Dash.parseEnvelope "<html>") >> ok



-- RENDERING


json :: BS8.ByteString -> Aeson.Value
json s =
  maybe Aeson.Null id (Aeson.decodeStrict s)


shown :: D.Doc -> T.Text
shown =
  T.pack . D.toString


rendering :: Test ()
rendering =
  tests
    [ scope "table" $ do
        expectEqual [ "A    LONGER", "xyz  1", "x" ] (Dash.table [ "A", "LONGER" ] [ [ "xyz", "1" ], [ "x", "" ] ])
        expectEqual [ "k  v" ] (Dash.table [] [ [ "k", "v" ] ])

    , scope "app list" $ do
        let out = shown (App.render "https://dashboard.lamdera.app" App.List (json "[{\"appId\":\"demo\",\"plan\":\"hobby\",\"owner\":\"mario\",\"version\":3,\"created\":1700000000000,\"url\":\"https://demo.lamdera.app\",\"preview\":true}]"))
        expectEqual
          "APP             PLAN   OWNER  VERSION  CREATED              URL\n\
          \demo (preview)  hobby  mario  v3       2023-11-14 22:13:20  https://demo.lamdera.app"
          out
        expectTextContains (shown (App.render "w" App.List (json "[]"))) "no apps yet"

    , scope "app info" $ do
        let out = shown (App.render "https://dashboard.lamdera.app" App.Info (json "{\"appId\":\"demo\",\"plan\":\"professional\",\"version\":0,\"deploys\":[],\"domains\":[{\"domain\":\"example.com\"}],\"logging\":false}"))
        expectTextContainsAll out [ "App      demo", "Plan     professional", "Version  -", "Logging  off", "Domains  example.com", "https://dashboard.lamdera.app/app/demo" ]

    , scope "env set" $ do
        expectTextContains (shown (Env.render "w" "demo" (Env.Set "greeting" "x") (json "{\"name\":\"greeting\",\"used\":true,\"redeployed\":false}"))) "applies from the next deploy"
        expectTextContains (shown (Env.render "w" "demo" (Env.Set "greeting" "x") (json "{\"name\":\"greeting\",\"used\":true,\"redeployed\":true}"))) "Redeployed demo"
        expectTextContains (shown (Env.render "w" "demo" (Env.Set "greeting" "x") (json "{\"name\":\"greeting\",\"used\":false,\"redeployed\":false}"))) "does not read greeting yet"

    , scope "domains add" $ do
        let out = shown (Domains.render "w" "demo" (Domains.Add "example.com") (json "{\"domain\":\"example.com\",\"status\":\"pending-dns\",\"instructions\":\"CNAME example.com to demo.dns.lamdera.app\"}"))
        expectTextContainsAll out [ "example.com: pending-dns", "CNAME example.com to demo.dns.lamdera.app", "w/app/demo/domains" ]

    , scope "events list" $ do
        let out = shown (Events.render "w" "demo" Events.List (json "{\"events\":[{\"id\":42,\"version\":3,\"tipe\":1,\"kind\":\"ToBackend\",\"name\":null,\"start\":1700000000123,\"duration\":-1,\"bytes\":7}],\"next\":41}"))
        expectTextContainsAll out [ "ID  TIME                 KIND       NAME  DURATION  SIZE  VERSION", "42  2023-11-14 22:13:20  ToBackend  -     -         7     v3", "lamdera events --before 41" ]

    , scope "events show" $ do
        let out = shown (Events.render "w" "demo" (Events.Show 42) (json "{\"event\":{\"id\":42,\"kind\":\"ToBackend\",\"name\":\"AddTodo\",\"version\":3,\"duration\":2500000,\"bytes\":7,\"start\":0},\"payload\":{\"$\":\"AddTodo\",\"args\":[\"milk\"]},\"logs\":[\"#42 hi\"]}"))
        expectTextContainsAll out [ "Event     #42", "Name      AddTodo", "Duration  2.5s", "{\"$\":\"AddTodo\",\"args\":[\"milk\"]}", "  #42 hi", "w/app/demo/events/42" ]
        expectTextContains (shown (Events.render "w" "demo" (Events.Show 1) (json "{\"event\":{},\"payload\":null,\"logs\":[]}"))) "not decoded"

    , scope "events metrics" $ do
        let out = shown (Events.render "w" "demo" Events.Metrics (json "{\"window\":\"1h\",\"total\":10,\"byName\":[{\"kind\":\"ToBackend\",\"name\":\"AddTodo\",\"count\":9,\"avgUs\":120,\"p95Us\":1500,\"maxUs\":null,\"errors\":0}],\"perMinute\":[{\"t\":0,\"count\":1},{\"t\":1,\"count\":8},{\"t\":2,\"count\":0}],\"failures\":[],\"connections\":{\"connects\":2,\"disconnects\":1}}"))
        expectTextContainsAll out [ "Events       10", "Connects     2", "Activity  ▁█ ", "ToBackend  AddTodo  9      120us  1.5ms  -    0" ]

    , scope "durations" $ do
        expectEqual [ "-", "-", "999us", "1.0ms", "12.3ms", "1.5s" ] (fmap Events.durationText [ Nothing, Just (-1), Just 999, Just 1000, Just 12345, Just 1500000 ])

    , scope "pretty JSON" $ do
        expectEqual [ "{\"a\":1}" ] (Dash.prettyJson (json "{\"a\":1}"))
        let long = Dash.prettyJson (json "{\"title\":\"a fairly long title for a todo item\",\"tags\":[\"one\",\"two\",\"three\"],\"done\":false}")
        expectEqual [ "{", "  \"done\": false,", "  \"tags\": [\"one\",\"two\",\"three\"],", "  \"title\": \"a fairly long title for a todo item\"", "}" ] long
    ]



-- FOLLOWING


following :: Test ()
following = do
  let
    l1 = "[2026-10-30 10:00:00.000] #1 a"
    l2 = "[2026-10-30 10:00:01.000] #2 b"
    l2b = "[2026-10-30 10:00:01.000] #3 c"
    cont = "\tmore of c"
    l3 = "[2026-10-30 10:00:02.500] #4 d"
    t s = Logs.recordTime s
  expectEqual (Just 1793354400000) (t l1)
  expectEqual (Just 1793354402500) (t l3)
  expectEqual Nothing (t cont)
  expectEqual Nothing (t "[short]")

  let (out1, s1) = Logs.followStep (Nothing, []) [ l1, l2 ]
  expectEqual [ l1, l2 ] out1
  expectEqual (t l2, [[l2]]) s1

  -- the next poll starts at the last time, so l2 comes again with l2b (and its
  -- continuation line) after it
  let (out2, s2) = Logs.followStep s1 [ l2, l2b, cont ]
  expectEqual [ l2b, cont ] out2
  expectEqual (t l2, [[l2], [l2b, cont]]) s2

  let (out3, s3) = Logs.followStep s2 [ l2, l2b, cont, l3 ]
  expectEqual [ l3 ] out3
  expectEqual (t l3, [[l3]]) s3

  let (out4, s4) = Logs.followStep s3 []
  expectEqual [] out4
  expectEqual s3 s4



-- CONFIRMATION


confirmation :: Test ()
confirmation = do
  -- yes, json, tty
  expectEqual Dash.Proceed (Dash.decide True True False)
  expectEqual Dash.Proceed (Dash.decide True False False)
  expectEqual Dash.Refuse (Dash.decide False True True)
  expectEqual Dash.Refuse (Dash.decide False False False)
  expectEqual Dash.Ask (Dash.decide False False True)



-- END TO END


{-| Runs a command in-process against the stub, as `lamdera <args>` would with the same
environment: its stdout and exit code. -}
runIn :: (FilePath, String) -> IO () -> IO (T.Text, ExitCode)
runIn (elmHome, url) action = do
  result <- withEnvVars [ ("ELM_HOME", elmHome), ("LAMDERA_DASHBOARD_URL", url) ] (Test.Main.captureProcessResult action)
  pure (T.decodeUtf8 (Test.Main.prStdout result), Test.Main.prExitCode result)


endToEnd :: Test ()
endToEnd = do
  stub <- io $ Lamdera.Relative.requireFile "Test.CLI" "test/cli-stub/stub.js"
  tmp <- io $ Dir.getTemporaryDirectory
  let
    dir = tmp </> "lamdera-cli-test"
    elmHome = dir </> "elm-home"
    requestLog = dir </> "requests.jsonl"
  io $ do
    rmdir dir
    Dir.createDirectoryIfMissing True elmHome
    writeUtf8 (elmHome </> ".lamdera-cli") "test-token"
  (_, Just out, _, handle) <- io $ Process.createProcess (Process.proc "node" [stub, requestLog]) { Process.std_out = Process.CreatePipe }
  port <- io $ hGetLine out
  let
    env = (elmHome, "http://127.0.0.1:" ++ port)
    run args flags action = io $ runIn env (action args flags)

  (listOut, listExit) <- run App.List appF { App._json = True } App.run
  (infoOut, infoExit) <- run App.Info appF { App._app = Just "demo" } App.run
  (refusedOut, refusedExit) <- run App.Delete appF { App._app = Just "demo", App._json = True } App.run
  (deletedOut, deletedExit) <- run App.Delete appF { App._app = Just "demo", App._json = True, App._yes = True } App.run
  (errorOut, errorExit) <- run (Env.Set "greeting" "x") envF { Env._app = Just "demo", Env._json = True } Env.run
  (eventOut, eventExit) <- run (Events.Show 42) eventsF { Events._app = Just "demo", Events._json = True } Events.run
  (noAuthOut, noAuthExit) <- io $ runIn (dir </> "nobody", snd env) (App.run App.List appF { App._json = True })

  io $ Process.terminateProcess handle
  sent <- io $ readUtf8Text requestLog
  let sentLines = maybe [] T.lines sent

  tests
    [ scope "app list --json: the payload only" $ do
        expectEqual ExitSuccess listExit
        expectEqual "[{\"appId\":\"demo\",\"created\":1700000000000,\"owner\":\"mario\",\"plan\":\"hobby\",\"preview\":false,\"url\":\"https://demo.lamdera.app\",\"version\":3}]\n" listOut

    , scope "app info: polls a pending answer until it is ok" $ do
        expectEqual ExitSuccess infoExit
        expectTextContainsAll infoOut [ "App      demo", "Version  v3", "DEPLOY  SHA       AT", "/app/demo" ]
        expectEqual 2 (length (filter (T.isInfixOf "\"ticket\":\"info1\"") sentLines))

    , scope "app delete without --yes is refused, and sends nothing" $ do
        expectEqual (ExitFailure 1) refusedExit
        expectTextContains refusedOut "{\"error\":\"Refusing to permanently delete the app demo"
        expectEqual 1 (length (filter (T.isInfixOf "cli_appDelete") sentLines))

    , scope "app delete --yes confirms with the app name" $ do
        expectEqual ExitSuccess deletedExit
        expectEqual "{\"deleted\":\"demo\"}\n" deletedOut
        expectEqual 1 (length (filter (T.isInfixOf "{\"path\":\"/_r/cli_appDelete\",\"body\":{\"key\":\"test-token\",\"appId\":\"demo\",\"confirm\":\"demo\"}}") sentLines))

    , scope "an error answer: {\"error\"} and exit code 1" $ do
        expectEqual (ExitFailure 1) errorExit
        expectEqual "{\"error\":\"That name is not a valid Elm name\"}\n" errorOut

    , scope "events show --json" $ do
        expectEqual ExitSuccess eventExit
        expectTextContains eventOut "\"payload\":{\"$\":\"AddTodo\",\"args\":[\"milk\"]}"

    , scope "no CLI token: an error, before any request" $ do
        expectEqual (ExitFailure 1) noAuthExit
        expectEqual "{\"error\":\"No CLI auth, please run `lamdera login`\"}\n" noAuthOut

    , scope "the token is validated, then sent as `key`" $ do
        expect (any (T.isInfixOf "{\"path\":\"/_r/apiSessionJson\",\"body\":{\"token\":\"test-token\"}}") sentLines)
        expect (all (\l -> not ("/_r/cli_" `T.isInfixOf` l) || "\"key\":\"test-token\"" `T.isInfixOf` l) sentLines)
        expect (not (List.null sentLines))
    ]
