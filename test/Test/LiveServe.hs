{-# LANGUAGE OverloadedStrings #-}

module Test.LiveServe where

{-| `lamdera live --serve` (Lamdera.CLI.LiveServe): the tunnel's frames, relay URLs and
messages; and what `lamdera live` serves to a request that is not local, as every
request through the tunnel is (Lamdera.CLI.LocalOnly, Lamdera.CLI.Live.allowedFromClient),
or that a page on another site sends (Lamdera.CLI.LocalOnly.forbidCrossSite).
The tunnel itself runs end to end against the real relay in the platform repo's VM
smoke test (Test/NixosLiveServe.elm there).
-}

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T
import qualified Data.CaseInsensitive as CI
import Snap.Core (Snap, writeBS)
import qualified Snap.Core
import qualified Snap.Test

import qualified Data.Char as Char
import System.FilePath ((</>))
import qualified System.IO.Temp as Temp
import qualified System.PosixCompat.Files as Files

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
    , scope "machine secret" machineSecrets
    , scope "local or not" localOrNot
    , scope "serving a remote request" serving
    , scope "socket messages from remote clients" socketMessages
    , scope "requests from another site" crossSite
    , scope "dev tool methods" devToolMethods
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
  expectEqual (RelayUrl True "connect.lamdera.dev" 443 "/tunnel") defaultRelay
  expectEqual (Right (RelayUrl True "connect.lamdera.dev" 443 "/tunnel")) (parseRelayUrl "wss://connect.lamdera.dev/tunnel")
  expectEqual (Right (RelayUrl False "localhost" 8099 "/tunnel")) (parseRelayUrl "ws://localhost:8099/tunnel")
  expectEqual (Right (RelayUrl False "relay.test" 80 "/")) (parseRelayUrl "ws://relay.test")
  expectEqual (Right (RelayUrl False "::1" 8099 "/t")) (parseRelayUrl "ws://[::1]:8099/t")
  _ <- expectLeft (parseRelayUrl "https://connect.lamdera.dev/tunnel")
  _ <- expectLeft (parseRelayUrl "wss://host:port/x")
  _ <- expectLeft (parseRelayUrl "wss:///x")
  expectEqual [1, 2, 4, 8, 16, 30, 30, 30] (fmap backoffSeconds [0 .. 7])
  expectEqual (Target "127.0.0.1" 8000) (localTarget "0.0.0.0" 8000)
  expectEqual (Target "127.0.0.1" 8000) (localTarget "127.0.0.1" 8000)
  expectEqual (Target "192.168.1.5" 8000) (localTarget "192.168.1.5" 8000)


controlMessages :: Test ()
controlMessages = do
  let hello = Aeson.decode (helloJson "tok" "my-app" "machine-secret-1234" Nothing) :: Maybe Aeson.Value
  helloV <- expectJust hello
  expectEqual (Just "hello") (field "t" helloV)
  expectEqual (Just "tok") (field "token" helloV)
  expectEqual (Just "my-app") (field "app" helloV)
  expectEqual Nothing (field "resume" helloV)
  expectEqual (Just "machine-secret-1234") (field "machine" helloV)
  resumed <- expectJust (Aeson.decode (helloJson "tok" "my-app" "machine-secret-1234" (Just ("my-app-abc123.lamdera.dev", "key"))) :: Maybe Aeson.Value)
  expectEqual (Just "my-app-abc123.lamdera.dev") (field "resume" resumed)
  expectEqual (Just "key") (field "resumeKey" resumed)
  expectEqual
    (Ready "https://a-b.lamdera.dev" "a-b.lamdera.dev" "k")
    (parseServerMessage "{\"t\":\"ready\",\"url\":\"https://a-b.lamdera.dev\",\"host\":\"a-b.lamdera.dev\",\"resumeKey\":\"k\"}")
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

{-| The per-app secret --serve keeps on this computer, which the preview's name is
derived from: made once, kept, new with --serve-new. -}
machineSecrets :: Test ()
machineSecrets = do
  expectEqual ("/home/me/.elm" </> ".lamdera-live" </> "my-app") (machineSecretPath "/home/me/.elm" "my-app")
  (first, again, rotated, afterRotate, mode) <- io $ Temp.withSystemTempDirectory "live-serve" $ \dir -> do
    let path = machineSecretPath dir "my-app"
    first <- machineSecret path False
    again <- machineSecret path False
    rotated <- machineSecret path True
    afterRotate <- machineSecret path False
    mode <- Files.fileMode <$> Files.getFileStatus path
    pure (first, again, rotated, afterRotate, mode)
  expectEqual 48 (T.length first)
  expect (T.all (\c -> Char.isDigit c || (c >= 'a' && c <= 'f')) first)
  expectEqual first again
  expectNotEqual first rotated
  expectEqual rotated afterRotate
  expectEqual (Files.ownerReadMode `Files.unionFileModes` Files.ownerWriteMode) (mode `Files.intersectFileModes` Files.accessModes)


localOrNot :: Test ()
localOrNot = do
  expect (LocalOnly.isLocal "127.0.0.1" (Just "localhost:8000"))
  expect (LocalOnly.isLocal "127.0.0.1" (Just "app.localhost:8000"))
  expect (LocalOnly.isLocal "::1" (Just "[::1]:8000"))
  expect (LocalOnly.isLocal "127.0.0.1" Nothing)
  -- through the tunnel: from this computer, but to the preview's name
  expect (not (LocalOnly.isLocal "127.0.0.1" (Just "my-app-abc123.lamdera.dev")))
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
  LocalOnly.forbidCrossSite False
    <|> LocalOnly.forbidRemoteDevTools
    <|> Live.requireMethods
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
    preview = "my-app-abc123.lamdera.dev"
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



{-| A request with a method, a Host and other headers, served by `handler`: the status,
the Allow header and the body. -}
sendRequest :: Snap () -> Snap.Core.Method -> BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> Test (Int, Maybe BS.ByteString, BS.ByteString)
sendRequest handler method path headers = do
  response <- io $ Snap.Test.runHandler
    (do Snap.Test.get path mempty
        onlyIf (method /= Snap.Core.GET) $ Snap.Test.setRequestType (Snap.Test.RequestWithRawBody method "")
        mapM_ (\(name, value) -> Snap.Test.setHeader (CI.mk name) value) headers
    )
    handler
  body <- io $ Snap.Test.getResponseBody response
  pure (Snap.Core.rspStatus response, Snap.Core.getHeader "Allow" response, body)
  where
    onlyIf c a = if c then a else pure ()


{-| The guards of Develop.runWithRoot, then a stand-in for every handler that records the
paths it was run for. -}
guarded :: Bool -> IORef [BS.ByteString] -> Snap ()
guarded shared ran =
  LocalOnly.forbidCrossSite shared
    <|> LocalOnly.forbidRemoteDevTools
    <|> Live.requireMethods
    <|> LocalOnly.forbidRemoteDotfiles
    <|> (do
          path <- Snap.Core.rqPathInfo <$> Snap.Core.getRequest
          liftIO $ modifyIORef ran (path :)
          writeBS "ran"
        )


crossSite :: Test ()
crossSite = do
  ran <- io $ newIORef []
  let
    local = guarded False ran
    shared = guarded True ran
    host = ("Host", "localhost:8000")
    status (code, _, _) = code
    evil = ("Origin", "https://evil.example")
  -- from another site: refused
  r1 <- sendRequest local Snap.Core.GET "/_w" [host, evil, ("Sec-Fetch-Site", "cross-site")]
  expectEqual 403 (status r1)
  r2 <- sendRequest local Snap.Core.POST "/_x/write/.git/config" [host, evil]
  expectEqual 403 (status r2)
  r3 <- sendRequest local Snap.Core.POST "/_x/editor/src/Backend.elm" [host, ("Sec-Fetch-Site", "cross-site")]
  expectEqual 403 (status r3)
  -- an <img src>: no Origin, but Sec-Fetch-Site says where it's from
  r4 <- sendRequest local Snap.Core.GET "/_x/editor/src/Backend.elm" [host, ("Sec-Fetch-Site", "cross-site")]
  expectEqual 403 (status r4)
  -- another server on this computer is another origin (and the same site)
  r5 <- sendRequest local Snap.Core.GET "/_w" [host, ("Origin", "http://localhost:9000"), ("Sec-Fetch-Site", "same-site")]
  expectEqual 403 (status r5)
  r6 <- sendRequest local Snap.Core.GET "/_c/src/Main.elm" [host, ("Origin", "null")]
  expectEqual 403 (status r6)
  r7 <- sendRequest local Snap.Core.GET "/_x/bem" [host, ("Sec-Fetch-Site", "same-site")]
  expectEqual 403 (status r7)
  -- none of them ran
  ranCross <- io $ readIORef ran
  expectEqual [] ranCross
  -- RPCs stay open to other sites, as in production
  r8 <- sendRequest local Snap.Core.POST "/_r/hello" [host, evil, ("Sec-Fetch-Site", "cross-site")]
  expectEqual 200 (status r8)
  -- the app itself, too
  r9 <- sendRequest local Snap.Core.GET "/" [host, ("Sec-Fetch-Site", "cross-site")]
  expectEqual 200 (status r9)

  -- the app's own pages, and tools outside a browser
  a1 <- sendRequest local Snap.Core.GET "/_w" [host, ("Origin", "http://localhost:8000"), ("Sec-Fetch-Site", "same-origin")]
  expectEqual 200 (status a1)
  a2 <- sendRequest local Snap.Core.POST "/_x/write/src/Main.elm" [host]
  expectEqual 200 (status a2)
  a3 <- sendRequest local Snap.Core.GET "/_x/bem" [host, ("Sec-Fetch-Site", "none")]
  expectEqual 200 (status a3)
  a4 <- sendRequest local Snap.Core.GET "/_w" [("Host", "127.0.0.1:8000"), ("Origin", "http://127.0.0.1:8000")]
  expectEqual 200 (status a4)
  a5 <- sendRequest local Snap.Core.GET "/_w" [("Host", "[::1]:8000"), ("Origin", "http://[::1]:8000")]
  expectEqual 200 (status a5)
  a6 <- sendRequest local Snap.Core.POST "/_x/editor/src/Backend.elm" [("Host", "LocalHost:8000"), ("Origin", "http://localhost:8000"), ("Sec-Fetch-Site", "same-origin")]
  expectEqual 200 (status a6)
  a7 <- sendRequest local Snap.Core.GET "/_c/src/Main.elm" [host]
  expectEqual 200 (status a7)

  -- a --serve visitor: the tunnel keeps the preview's Host, the page is at the preview
  let preview = [("Host", "my-app-abc123.lamdera.dev"), ("Origin", "https://my-app-abc123.lamdera.dev"), ("Sec-Fetch-Site", "same-origin")]
  v1 <- sendRequest shared Snap.Core.GET "/_w" preview
  expectEqual 200 (status v1)
  -- (and still no dev tools for them)
  v2 <- sendRequest shared Snap.Core.GET "/_x/bem" preview
  expectEqual 403 (status v2)
  v3 <- sendRequest shared Snap.Core.GET "/_w" [("Host", "my-app-abc123.lamdera.dev"), ("Origin", "https://evil.example")]
  expectEqual 403 (status v3)
  -- a device through --bind
  b1 <- sendRequest shared Snap.Core.GET "/_w" [("Host", "192.168.1.5:8000"), ("Origin", "http://192.168.1.5:8000")]
  expectEqual 200 (status b1)
  b2 <- sendRequest shared Snap.Core.GET "/_w" [("Host", "mymac.local:8000"), ("Origin", "http://mymac.local:8000")]
  expectEqual 200 (status b2)

  -- DNS rebinding: a page's own name pointed at 127.0.0.1 is same-origin with itself, so
  -- /_w answers only local names, unless --serve or --bind is on
  let rebound = [("Host", "evil.example:8000"), ("Origin", "http://evil.example:8000"), ("Sec-Fetch-Site", "same-origin")]
  d1 <- sendRequest local Snap.Core.GET "/_w" rebound
  expectEqual 403 (status d1)
  d2 <- sendRequest shared Snap.Core.GET "/_w" rebound
  expectEqual 200 (status d2)
  d3 <- sendRequest local Snap.Core.GET "/_w" [("Host", "app.localhost:8000"), ("Origin", "http://app.localhost:8000")]
  expectEqual 200 (status d3)

  -- the checks themselves
  expect (LocalOnly.isSameOrigin (Just "localhost:8000") Nothing Nothing)
  expect (LocalOnly.isSameOrigin (Just "localhost:8000") (Just "http://localhost:8000") (Just "same-origin"))
  expect (LocalOnly.isSameOrigin (Just "example.com") (Just "https://example.com") Nothing)
  expect (LocalOnly.isSameOrigin (Just "example.com:443") (Just "https://example.com") Nothing)
  expect (LocalOnly.isSameOrigin (Just "localhost") (Just "http://localhost:80") Nothing)
  expect (not (LocalOnly.isSameOrigin (Just "localhost:8000") (Just "http://localhost") Nothing))
  expect (not (LocalOnly.isSameOrigin (Just "localhost:8000") (Just "http://localhost:8000.evil.example") Nothing))
  expect (not (LocalOnly.isSameOrigin (Just "localhost:8000") (Just "http://localhost:8000/") Nothing))
  expect (not (LocalOnly.isSameOrigin Nothing (Just "http://localhost:8000") Nothing))
  expect (not (LocalOnly.isSameOrigin (Just "localhost:8000") Nothing (Just "cross-site")))
  expect (LocalOnly.socketHostAllowed False (Just "localhost:8000"))
  expect (LocalOnly.socketHostAllowed False Nothing)
  expect (not (LocalOnly.socketHostAllowed False (Just "my-app-abc123.lamdera.dev")))
  expect (LocalOnly.socketHostAllowed True (Just "my-app-abc123.lamdera.dev"))


devToolMethods :: Test ()
devToolMethods = do
  ran <- io $ newIORef []
  let
    handler = guarded False ran
    host = ("Host", "localhost:8000")
  -- a CORS preflight no longer runs the handler: /_x/write used to write an empty file
  m1 <- sendRequest handler Snap.Core.OPTIONS "/_x/write/src/Main.elm" [host]
  expectEqual (405, Just "POST") (fst3 m1)
  m2 <- sendRequest handler Snap.Core.GET "/_x/editor/src/Backend.elm" [host]
  expectEqual (405, Just "POST") (fst3 m2)
  m3 <- sendRequest handler Snap.Core.GET "/_x/writeX" [host] -- serveExperimental takes any _x/write... path
  expectEqual (405, Just "POST") (fst3 m3)
  m4 <- sendRequest handler Snap.Core.POST "/_x/read/src/Main.elm" [host]
  expectEqual (405, Just "GET, HEAD") (fst3 m4)
  m5 <- sendRequest handler Snap.Core.PUT "/_x/list" [host]
  expectEqual (405, Just "GET, HEAD") (fst3 m5)
  m6 <- sendRequest handler Snap.Core.DELETE "/_x/bem" [host]
  expectEqual (405, Just "GET, HEAD") (fst3 m6)
  m7 <- sendRequest handler Snap.Core.POST "/_c/src/Main.elm" [host]
  expectEqual (405, Just "GET, HEAD") (fst3 m7)
  m8 <- sendRequest handler Snap.Core.POST "/_patch" [host]
  expectEqual (405, Just "GET, HEAD") (fst3 m8)
  m9 <- sendRequest handler Snap.Core.POST "/_w" [host]
  expectEqual (405, Just "GET") (fst3 m9)
  m10 <- sendRequest handler Snap.Core.OPTIONS "/_x\\write/src/Main.elm" [host]
  expectEqual (405, Just "POST") (fst3 m10)
  refused <- io $ readIORef ran
  expectEqual [] refused
  -- the methods they take
  ok1 <- sendRequest handler Snap.Core.POST "/_x/write/src/Main.elm" [host]
  expectEqual 200 (fst (fst3 ok1))
  ok2 <- sendRequest handler Snap.Core.POST "/_x/editor/src/Backend.elm" [host]
  expectEqual 200 (fst (fst3 ok2))
  ok3 <- sendRequest handler Snap.Core.GET "/_x/read/src/Main.elm" [host]
  expectEqual 200 (fst (fst3 ok3))
  ok4 <- sendRequest handler Snap.Core.HEAD "/_c/src/Main.elm" [host]
  expectEqual 200 (fst (fst3 ok4))
  ok5 <- sendRequest handler Snap.Core.GET "/_w" [host]
  expectEqual 200 (fst (fst3 ok5))
  -- the rest is not a dev tool
  ok6 <- sendRequest handler Snap.Core.OPTIONS "/_r/hello" [host]
  expectEqual 200 (fst (fst3 ok6))
  ok7 <- sendRequest handler Snap.Core.POST "/some/page" [host]
  expectEqual 200 (fst (fst3 ok7))
  ranOk <- io $ readIORef ran
  expectEqual 7 (length ranOk)
  where
    fst3 (a, b, _) = (a, b)
