{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Dashboard where

{- What the app management commands (`lamdera app`, `env`, `domains`, `logs`, `events`)
share: the common flags, talking to the dashboard, confirmations, and output.

Every command is one or more POSTs to the dashboard's `/_r/cli_<op>` with a JSON body
`{"key": <CLI token>, "appId": <app>, ...}` (docs/plans/cli-app-management.md in the
platform repo). The dashboard answers 200 with one of

  {"ok": <payload>}          done
  {"error": "<message>"}     refused or failed, written for the user
  {"pending": "<ticket>"}    the hosting platform is working on it: POST
                             /_r/cli_result {"key", "ticket"} every 500ms until it
                             answers ok or error (or pending again)

Output: by default for people (`───>` lines, aligned tables, dashboard links). With
--json only the `ok` payload goes to stdout as JSON, and any error as {"error": "..."}
with exit code 1, with no progress or other noise.

Request building, response decoding and rendering are pure, so they are unit tested
(test/Test/CLI.hs) without a dashboard.
-}

import Control.Concurrent (threadDelay)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Scientific as Sci
import qualified Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.Format as Time
import qualified Data.Vector as Vector
import System.Exit (ExitCode(..), exitFailure)
import qualified System.Process
import Control.Monad.Except (catchError)
import System.IO (hFlush, hIsTerminalDevice, stdin, stdout)

import qualified Http
import qualified Json.Encode as E
import qualified Reporting.Doc as D
import Terminal (Parser(..))

import Lamdera
import qualified Lamdera.CLI.Login
import qualified Lamdera.Http
import qualified Lamdera.Progress as Progress
import qualified Lamdera.Project



-- FLAGS


{-| The flags every app management command takes. -}
data Common =
  Common
    { _app :: Maybe String -- --app NAME
    , _json :: Bool        -- --json
    , _yes :: Bool         -- --yes
    }
    deriving (Show, Eq)


appName_ :: Parser String
appName_ =
  Parser
    { _singular = "app name"
    , _plural = "app names"
    , _parser = \s -> if null s || take 1 s == "-" then Nothing else Just s
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["my-app"]
    }


int_ :: String -> [String] -> Parser Int
int_ singular examples =
  Parser
    { _singular = singular
    , _plural = singular ++ "s"
    , _parser = \s -> case reads s of { [(n, "")] | n >= 0 -> Just n; _ -> Nothing }
    , _suggest = \_ -> return []
    , _examples = \_ -> return examples
    }


text_ :: String -> [String] -> Parser String
text_ singular examples =
  Parser
    { _singular = singular
    , _plural = singular ++ "s"
    , _parser = Just
    , _suggest = \_ -> return []
    , _examples = \_ -> return examples
    }


{-| A fixed word, the verb of a command (`lamdera app list`). Named "=word" so that help
and errors show it as is, and an error about another verb's alternative ranks last
(Terminal.Error.toToken, toArgErrorRank). -}
verb :: String -> Parser ()
verb word =
  Parser
    { _singular = "=" ++ word
    , _plural = "=" ++ word
    , _parser = \s -> if s == word then Just () else Nothing
    , _suggest = \_ -> return [word]
    , _examples = \_ -> return [word]
    }


{-| One of a fixed set of words. -}
choice :: String -> [String] -> Parser String
choice singular words_ =
  Parser
    { _singular = singular
    , _plural = singular ++ "s"
    , _parser = \s -> if s `elem` words_ then Just s else Nothing
    , _suggest = \_ -> return words_
    , _examples = \_ -> return words_
    }



-- REQUESTS


data Field
  = FString Text
  | FInt Int
  | FBool Bool
  deriving (Show, Eq)


data Request =
  Request
    { _op :: String
    , _body :: [(String, Field)] -- besides `key`
    , _timeoutMs :: Int          -- how long to wait for a pending answer
    }
    deriving (Show, Eq)


request :: String -> [(String, Field)] -> Request
request op body =
  Request op body 120000


{-| A request about one app: `appId` first, then the rest. -}
appRequest :: Text -> String -> [(String, Field)] -> Request
appRequest appName op body =
  request op (("appId", FString appName) : body)


optional :: String -> (a -> Field) -> Maybe a -> [(String, Field)]
optional name f m =
  maybe [] (\v -> [(name, f v)]) m


endpointPath :: String -> String
endpointPath op =
  "/_r/cli_" <> op


bodyJson :: Text -> [(String, Field)] -> E.Value
bodyJson token fields =
  E.object $
    ("key", E.chars (T.unpack token))
      : fmap (\(k, v) -> (fromChars_ k, fieldJson v)) fields
  where
    fieldJson f =
      case f of
        FString t -> E.chars (T.unpack t)
        FInt n -> E.int n
        FBool b -> E.bool b

    fromChars_ = Data.String.fromString


{-| The body as JSON text, as it is sent (for tests and debugging). -}
bodyText :: Text -> [(String, Field)] -> Text
bodyText token fields =
  T.decodeUtf8 $ BSL.toStrict $ B.toLazyByteString $ E.encodeUgly (bodyJson token fields)



-- RESPONSES


data Envelope
  = EOk Aeson.Value
  | EError Text
  | EPending Text
  deriving (Show, Eq)


parseEnvelope :: BS.ByteString -> Either Text Envelope
parseEnvelope bytes =
  case Aeson.eitherDecodeStrict bytes of
    Left err ->
      Left $ "The dashboard sent a response I could not read (" <> T.pack err <> "): " <> T.take 200 (T.decodeUtf8With (\_ _ -> Just '?') bytes)

    Right (Aeson.Object o) ->
      case (KeyMap.lookup "ok" o, KeyMap.lookup "error" o, KeyMap.lookup "pending" o) of
        (Just v, _, _) -> Right (EOk v)
        (_, Just (Aeson.String e), _) -> Right (EError e)
        (_, Just v, _) -> Right (EError (jsonText v))
        (_, _, Just (Aeson.String t)) -> Right (EPending t)
        _ -> Left $ "The dashboard sent an unexpected response: " <> jsonText (Aeson.Object o)

    Right v ->
      Left $ "The dashboard sent an unexpected response: " <> jsonText v



-- RUNNING


data Ctx =
  Ctx
    { _token :: Text
    , _dashboard :: Lamdera.Http.Dashboard
    , _web :: String       -- the dashboard for links
    , _appName :: Maybe Text
    , _jsonOut :: Bool
    }


data NeedsApp
  = NeedsApp
  | AppOptional


{-| The app, token and dashboard for a command, or the problem reported and exit. -}
setup :: Common -> NeedsApp -> IO Ctx
setup common needs = do
  appNameM <-
    case _app common of
      Just name -> pure (Just (T.pack name))
      Nothing -> do
        -- maybeAppName asks git, which complains on stderr outside a repository
        (exit, _, _) <- System.Process.readProcessWithExitCode "git" ["rev-parse", "--is-inside-work-tree"] ""
          `catchError` (\_ -> pure (ExitFailure 1, "", ""))
        if exit == ExitSuccess then Lamdera.Project.maybeAppName else pure Nothing
  case (needs, appNameM) of
    (NeedsApp, Nothing) ->
      failWith (_json common) "I don't know which app you mean: pass --app NAME, or run this inside a Lamdera project with a `lamdera` git remote."
    _ -> pure ()
  dashboard <- Lamdera.Http.dashboardFor appNameM
  web <- Lamdera.Http.dashboardWebUrl appNameM
  tokenR <- Lamdera.CLI.Login.checkCliToken appNameM
  case tokenR of
    Right token ->
      pure $ Ctx token dashboard web appNameM (_json common)

    Left (Lamdera.CLI.Login.CliAuthHttp err) | not (_json common) -> do
      Lamdera.Http.printHttpError err "I need to validate the CLI session"
      exitFailure

    Left problem ->
      failWith (_json common) (Lamdera.CLI.Login.cliAuthProblemText problem)


appOf :: Ctx -> Text
appOf ctx =
  maybe "" id (_appName ctx)


{-| Sends the request and waits out a pending answer: the `ok` payload, or the error
reported and exit. -}
call :: Ctx -> Request -> IO Aeson.Value
call ctx req = do
  started <- Time.getPOSIXTime
  first <- post ctx (_op req) (_body req)
  let
    loop envelope announced =
      case envelope of
        EOk v -> pure v
        EError e -> failWith (_jsonOut ctx) e
        EPending ticket -> do
          now <- Time.getPOSIXTime
          let elapsedMs = round ((now - started) * 1000) :: Int
          if elapsedMs > _timeoutMs req
            then
              failWith (_jsonOut ctx) $
                "The Lamdera platform did not finish within " <> T.pack (show (_timeoutMs req `div` 1000))
                  <> "s. It may still complete: check again in a minute."
            else do
              announced_ <-
                if not announced && elapsedMs >= 1000 && not (_jsonOut ctx)
                  then do
                    Progress.report $ D.fillSep [ "───>", D.dullyellow "Waiting for the Lamdera platform..." ]
                    pure True
                  else pure announced
              threadDelay 500000
              next <- post ctx "result" [("ticket", FString ticket)]
              loop next announced_
  loop first False


post :: Ctx -> String -> [(String, Field)] -> IO Envelope
post ctx op fields = do
  let
    dashboard = _dashboard ctx
    url = Lamdera.Http.dashboardEndpoint dashboard (endpointPath op)
    body = bodyJson (_token ctx) fields
  manager <- Lamdera.Http.managerFor (Lamdera.Http._dashboardSocket dashboard)
  debug $ "POSTING   " <> url <> " (cli_" <> op <> ")"
  res <- Http.postBody manager url Lamdera.Http.jsonHeaders body Lamdera.Http.HttpError (\bytes -> pure (Right bytes))
  case res of
    Right bytes ->
      case parseEnvelope bytes of
        Right envelope -> pure envelope
        Left problem -> failWith (_jsonOut ctx) problem

    Left err ->
      if _jsonOut ctx
        then failWith True ("I could not reach the Lamdera dashboard: " <> T.pack (show err))
        else do
          Lamdera.Http.printHttpError err "I needed to talk to the Lamdera dashboard"
          exitFailure



-- OUTPUT


failWith :: Bool -> Text -> IO a
failWith jsonOut message = do
  if jsonOut
    then BSL8.putStrLn (Aeson.encode (Aeson.object [ ("error", Aeson.String message) ]))
    else Progress.report $ D.fillSep ([ "───>", D.red "Error:" ] ++ fmap (D.fromChars . T.unpack) (T.words message))
  hFlush stdout
  exitFailure


{-| The payload as JSON (--json), or the rendering of it. -}
output :: Ctx -> Aeson.Value -> D.Doc -> IO ()
output ctx payload doc =
  if _jsonOut ctx
    then BSL8.putStrLn (Aeson.encode payload) >> hFlush stdout
    else Progress.report doc


said :: D.Doc -> D.Doc
said doc =
  D.fillSep [ "───>", doc ]


lines_ :: [Text] -> D.Doc
lines_ ls =
  D.vcat (fmap (D.fromChars . T.unpack) ls)


link :: String -> Text -> Text -> D.Doc
link web appName path =
  D.fromChars ("    " <> web <> "/app/" <> T.unpack appName <> T.unpack path)



-- CONFIRMATION


data Decision
  = Proceed
  | Ask
  | Refuse
  deriving (Show, Eq)


{-| Destructive operations go ahead with --yes, ask on a terminal, and are refused
otherwise: without a terminal there is nobody to ask (Reporting.ask would answer yes on
its own), and --json is for programs, which must say --yes. -}
decide :: Bool -> Bool -> Bool -> Decision
decide yes jsonOut tty
  | yes = Proceed
  | jsonOut || not tty = Refuse
  | otherwise = Ask


{-| Confirms a destructive `action` ("delete the app foo"). With `typed`, the person has
to type that text; otherwise y/N. -}
confirm :: Common -> Text -> Maybe Text -> IO ()
confirm common action typed = do
  tty <- hIsTerminalDevice stdin
  case decide (_yes common) (_json common) tty of
    Proceed -> pure ()
    Refuse ->
      failWith (_json common) ("Refusing to " <> action <> " without confirmation: pass --yes to do it.")
    Ask -> do
      case typed of
        Just expected -> do
          T.putStr ("This will " <> action <> ". Type `" <> expected <> "` to confirm: ")
          hFlush stdout
          answer <- T.strip <$> T.getLine
          if answer == expected then pure () else cancelled
        Nothing -> do
          T.putStr ("This will " <> action <> ". Continue? [y/N]: ")
          hFlush stdout
          answer <- T.toLower . T.strip <$> T.getLine
          if answer == "y" || answer == "yes" then pure () else cancelled
  where
    cancelled = do
      Progress.report $ said "Cancelled, nothing was changed."
      exitFailure



-- READING PAYLOADS


at :: Text -> Aeson.Value -> Maybe Aeson.Value
at key v =
  case v of
    Aeson.Object o -> KeyMap.lookup (Key.fromText key) o
    _ -> Nothing


list :: Aeson.Value -> [Aeson.Value]
list v =
  case v of
    Aeson.Array a -> Vector.toList a
    _ -> []


listAt :: Text -> Aeson.Value -> [Aeson.Value]
listAt key v =
  maybe [] list (at key v)


{-| A field as text: strings as they are, numbers and bools printed, null or missing "-". -}
textAt :: Text -> Aeson.Value -> Text
textAt key v =
  maybe "-" scalarText (at key v)


scalarText :: Aeson.Value -> Text
scalarText v =
  case v of
    Aeson.String t -> t
    Aeson.Number n -> numberText n
    Aeson.Bool True -> "yes"
    Aeson.Bool False -> "no"
    Aeson.Null -> "-"
    _ -> jsonText v


numberText :: Sci.Scientific -> Text
numberText n =
  case Sci.floatingOrInteger n :: Either Double Integer of
    Right i -> T.pack (show i)
    Left d -> T.pack (show d)


intAt :: Text -> Aeson.Value -> Maybe Int
intAt key v =
  case at key v of
    Just (Aeson.Number n) -> Sci.toBoundedInteger n
    _ -> Nothing


boolAt :: Text -> Aeson.Value -> Bool
boolAt key v =
  at key v == Just (Aeson.Bool True)


{-| A time field: ms since the epoch shown as UTC, strings as they are. -}
timeAt :: Text -> Aeson.Value -> Text
timeAt key v =
  case at key v of
    Just (Aeson.Number n) | Just ms <- Sci.toBoundedInteger n -> msText ms
    Just other -> scalarText other
    Nothing -> "-"


msText :: Int -> Text
msText ms =
  T.pack $ Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S" (Time.posixSecondsToUTCTime (fromIntegral ms / 1000))


jsonText :: Aeson.Value -> Text
jsonText v =
  T.decodeUtf8 (BSL.toStrict (Aeson.encode v))



-- TABLES AND JSON


{-| Aligned columns, two spaces apart, no trailing spaces; no header line without headers. -}
table :: [Text] -> [[Text]] -> [Text]
table headers rows =
  let
    all_ = (if null headers then [] else [headers]) ++ rows
    columns = maximum (0 : fmap length all_)
    widths = fmap (\i -> maximum (fmap (T.length . cell i) all_)) [0 .. columns - 1]
    cell i row = if i < length row then row !! i else ""
    render row = T.stripEnd $ T.intercalate "  " (zipWith (\w i -> T.justifyLeft w ' ' (cell i row)) widths [0 ..])
  in
  fmap render all_


{-| JSON for people: two space indents, short arrays and objects on one line. -}
prettyJson :: Aeson.Value -> [Text]
prettyJson value =
  go value
  where
    go v =
      let flat = jsonText v in
      case v of
        Aeson.Object o | T.length flat > 72 && not (KeyMap.null o) ->
          let
            entries = KeyMap.toList o
            entry (i, (k, x)) =
              let
                comma = if i == length entries - 1 then "" else ","
                inner = go x
              in
              case inner of
                [one] -> [ "  " <> jsonText (Aeson.String (Key.toText k)) <> ": " <> one <> comma ]
                first : rest -> ("  " <> jsonText (Aeson.String (Key.toText k)) <> ": " <> first) : indentLast comma rest
                [] -> []
          in
          ["{"] ++ concatMap entry (zip [0 :: Int ..] entries) ++ ["}"]

        Aeson.Array a | T.length flat > 72 && not (Vector.null a) ->
          let
            items = Vector.toList a
            item (i, x) =
              let comma = if i == length items - 1 then "" else "," in
              case go x of
                [one] -> [ "  " <> one <> comma ]
                first : rest -> ("  " <> first) : indentLast comma rest
                [] -> []
          in
          ["["] ++ concatMap item (zip [0 :: Int ..] items) ++ ["]"]

        _ ->
          [flat]

    indentLast comma rest =
      case reverse rest of
        lastLine : others -> fmap ("  " <>) (reverse others) ++ ["  " <> lastLine <> comma]
        [] -> []

