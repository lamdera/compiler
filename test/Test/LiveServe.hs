{-# LANGUAGE OverloadedStrings #-}

module Test.LiveServe where

{-| `lamdera live --serve` (Lamdera.CLI.LiveServe): the tunnel's frames, relay URLs and
messages; and what `lamdera live` serves to a request that is not local, as every
request through the tunnel is (Lamdera.CLI.LocalOnly, Lamdera.CLI.Live.allowedFromClient).
The tunnel itself runs end to end against the real relay in the platform repo's VM
smoke test (Test/NixosLiveServe.elm there).
-}

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T
import Snap.Core (Snap, writeBS)
import qualified Snap.Core
import qualified Snap.Test

import EasyTest

import Lamdera.CLI.LiveServe
import qualified Lamdera.CLI.LocalOnly as LocalOnly
import qualified Lamdera.CLI.Live as Live


suite :: Test ()
suite =
  tests
    [ scope "frames" frames
    , scope "relay url" relayUrls
    , scope "messages" controlMessages
    , scope "local or not" localOrNot
    , scope "serving a remote request" serving
    , scope "socket messages from remote clients" socketMessages
    ]


frames :: Test ()
frames = do
  let roundTrip f = expectEqual (Right f) (decodeFrame (encodeFrame f))
  roundTrip (Frame Open 1 "")
  roundTrip (Frame Data 0xfffffffe "hello")
  roundTrip (Frame End 7 "")
  roundTrip (Frame Reset 9 "no such stream")
  roundTrip (Frame Window 3 (windowPayload 65536))
  -- the same bytes as the relay (nixos/live-relay/relay.js encodeFrame)
  expectEqual (BSL.pack [1, 0, 0, 0, 1]) (encodeFrame (Frame Open 1 ""))
  expectEqual (BSL.pack [2, 0, 0, 1, 0, 104, 105]) (encodeFrame (Frame Data 256 "hi"))
  expectEqual (BS.pack [0, 1, 0, 0]) (windowPayload 65536)
  expectEqual (Just 65536) (windowIncrement (BS.pack [0, 1, 0, 0]))
  expectEqual Nothing (windowIncrement (BS.pack [0, 1, 0]))
  _ <- expectLeft (decodeFrame (BSL.pack [9, 0, 0, 0, 1]))
  _ <- expectLeft (decodeFrame (BSL.pack [2, 0, 0]))
  expect (maxChunk <= initialWindow)


relayUrls :: Test ()
relayUrls = do
  expectEqual (RelayUrl True "connect.live.lamdera.app" 443 "/tunnel") defaultRelay
  expectEqual (Right (RelayUrl True "connect.live.lamdera.app" 443 "/tunnel")) (parseRelayUrl "wss://connect.live.lamdera.app/tunnel")
  expectEqual (Right (RelayUrl False "localhost" 8099 "/tunnel")) (parseRelayUrl "ws://localhost:8099/tunnel")
  expectEqual (Right (RelayUrl False "relay.test" 80 "/")) (parseRelayUrl "ws://relay.test")
  expectEqual (Right (RelayUrl False "::1" 8099 "/t")) (parseRelayUrl "ws://[::1]:8099/t")
  _ <- expectLeft (parseRelayUrl "https://connect.live.lamdera.app/tunnel")
  _ <- expectLeft (parseRelayUrl "wss://host:port/x")
  _ <- expectLeft (parseRelayUrl "wss:///x")
  expectEqual [1, 2, 4, 8, 16, 30, 30, 30] (fmap backoffSeconds [0 .. 7])
  expectEqual (Target "127.0.0.1" 8000) (localTarget "0.0.0.0" 8000)
  expectEqual (Target "127.0.0.1" 8000) (localTarget "127.0.0.1" 8000)
  expectEqual (Target "192.168.1.5" 8000) (localTarget "192.168.1.5" 8000)


controlMessages :: Test ()
controlMessages = do
  let hello = Aeson.decode (helloJson "tok" "my-app" Nothing) :: Maybe Aeson.Value
  helloV <- expectJust hello
  expectEqual (Just "hello") (field "t" helloV)
  expectEqual (Just "tok") (field "token" helloV)
  expectEqual (Just "my-app") (field "app" helloV)
  expectEqual Nothing (field "resume" helloV)
  resumed <- expectJust (Aeson.decode (helloJson "tok" "my-app" (Just ("my-app-abc123.live.lamdera.app", "key"))) :: Maybe Aeson.Value)
  expectEqual (Just "my-app-abc123.live.lamdera.app") (field "resume" resumed)
  expectEqual (Just "key") (field "resumeKey" resumed)
  expectEqual
    (Ready "https://a-b.live.lamdera.app" "a-b.live.lamdera.app" "k")
    (parseServerMessage "{\"t\":\"ready\",\"url\":\"https://a-b.live.lamdera.app\",\"host\":\"a-b.live.lamdera.app\",\"resumeKey\":\"k\"}")
  expectEqual (Refused "invalid token") (parseServerMessage "{\"t\":\"error\",\"message\":\"invalid token\"}")
  expectEqual (Notice "hi") (parseServerMessage "{\"t\":\"notice\",\"message\":\"hi\"}")
  expectEqual (Unknown "nope") (parseServerMessage "nope")
  where
    field :: T.Text -> Aeson.Value -> Maybe T.Text
    field k v =
      case v of
        Aeson.Object o ->
          case KeyMap.lookup (Key.fromText k) o of
            Just (Aeson.String s) -> Just s
            _ -> Nothing
        _ -> Nothing

localOrNot :: Test ()
localOrNot = do
  expect (LocalOnly.isLocal "127.0.0.1" (Just "localhost:8000"))
  expect (LocalOnly.isLocal "127.0.0.1" (Just "app.localhost:8000"))
  expect (LocalOnly.isLocal "::1" (Just "[::1]:8000"))
  expect (LocalOnly.isLocal "127.0.0.1" Nothing)
  -- through the tunnel: from this computer, but to the preview's name
  expect (not (LocalOnly.isLocal "127.0.0.1" (Just "my-app-abc123.live.lamdera.app")))
  -- another device through --bind
  expect (not (LocalOnly.isLocal "192.168.1.20" (Just "192.168.1.5:8000")))
  expect (LocalOnly.hasDotSegment "/.env")
  expect (LocalOnly.hasDotSegment ".git/config")
  expect (LocalOnly.hasDotSegment "public/.secret")
  expect (LocalOnly.hasDotSegment "src/../elm.json")
  expect (LocalOnly.hasDotSegment "a\\.env")
  expect (not (LocalOnly.hasDotSegment "style.css"))
  expect (not (LocalOnly.hasDotSegment "images/logo.v2.png"))


{-| The order Develop.runWithRoot serves in, with stand-ins for the handlers. -}
liveLike :: Snap ()
liveLike =
  LocalOnly.forbidRemoteDevTools
    <|> LocalOnly.forbidRemoteDotfiles
    <|> LocalOnly.localOnly (writeBS "project file")
    <|> writeBS "the app"


serving :: Test ()
serving = do
  let
    request host path = do
      response <- io $ Snap.Test.runHandler (Snap.Test.get path mempty >> Snap.Test.setHeader "Host" host) liveLike
      body <- io $ Snap.Test.getResponseBody response
      pure (Snap.Core.rspStatus response, body)
    preview = "my-app-abc123.live.lamdera.app"
  -- this computer: everything as before
  local1 <- request "localhost:8000" "/src/Backend.elm"
  expectEqual (200, "project file") local1
  local2 <- request "localhost:8000" "/_x/list"
  expectEqual (200, "project file") local2
  -- a visitor: no dev tools, no dotfiles, no project files, but the app
  r1 <- request preview "/_x/list"
  expectEqual 403 (fst r1)
  r2 <- request preview "/_c"
  expectEqual 403 (fst r2)
  r3 <- request preview "/.env"
  expectEqual 404 (fst r3)
  r4 <- request preview "/%2Egit/config"
  expectEqual 404 (fst r4)
  r5 <- request preview "/src/Backend.elm"
  expectEqual (200, "the app") r5
  r6 <- request preview "/"
  expectEqual (200, "the app") r6


socketMessages :: Test ()
socketMessages = do
  let
    env = "{\"t\":\"env\",\"m\":\"Production\"}"
    persist = "{\"b\":\"AAA\",\"f\":\"\",\"t\":\"p\"}"
    toBackend = "{\"t\":\"ToBackend\",\"s\":\"s\",\"c\":\"c\",\"b\":\"AAA\"}"
  -- local: anything
  expect (Live.allowedFromClient False False env)
  expect (Live.allowedFromClient False False persist)
  -- remote: never the env mode, the backend model only from the leader
  expect (not (Live.allowedFromClient True True env))
  expect (not (Live.allowedFromClient True False persist))
  expect (Live.allowedFromClient True True persist)
  expect (Live.allowedFromClient True False toBackend)
