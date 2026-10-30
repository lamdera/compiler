{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Events where

{- `lamdera events [list | show <N> | metrics]`: the app's msg event log. Every backend
update (a ToBackend, a BackendMsg, a connect...) is an event with an id; the log lines it
wrote carry the same id as `#N` (see `lamdera logs --msg N`). Names and payloads are
decoded on the platform with the wire schema of the version that recorded the event. -}

import qualified Data.Aeson as Aeson
import qualified Data.Text as T

import qualified Reporting.Doc as D

import Lamdera.CLI.Dashboard hiding (Common(..))
import qualified Lamdera.CLI.Dashboard as Dash


data Args
  = List
  | Show Int
  | Metrics
  deriving (Show, Eq)


data Flags =
  Flags
    { _app :: Maybe String
    , _json :: Bool
    , _limit :: Maybe Int
    , _before :: Maybe Int
    , _kind :: Maybe String
    , _name :: Maybe String
    , _minDuration :: Maybe Int -- ms
    , _window :: Maybe String
    }
    deriving (Show, Eq)


windows :: [String]
windows =
  [ "1h", "24h", "7d" ]


common :: Flags -> Dash.Common
common flags =
  Dash.Common (_app flags) (_json flags) False


toRequest :: T.Text -> Args -> Flags -> Request
toRequest appName args flags =
  case args of
    List ->
      appRequest appName "events" $
        optional "before" FInt (_before flags)
          ++ optional "limit" FInt (_limit flags)
          ++ optional "tipe" (FString . T.pack) (_kind flags) -- a kind name; the dashboard maps it
          ++ optional "name" (FString . T.pack) (_name flags)
          ++ optional "minDuration" (FInt . (* 1000)) (_minDuration flags) -- us, like Event.duration
    Show n -> appRequest appName "event" [ ("id", FInt n) ]
    Metrics -> appRequest appName "metrics" (optional "window" (FString . T.pack) (_window flags))


run :: Args -> Flags -> IO ()
run args flags = do
  let c = common flags
  ctx <- setup c NeedsApp
  let appName = appOf ctx
  payload <- call ctx (toRequest appName args flags)
  output ctx payload (render (_web ctx) appName args payload)


render :: String -> T.Text -> Args -> Aeson.Value -> D.Doc
render web appName args payload =
  case args of
    List ->
      case listAt "events" payload of
        [] -> said "No events."
        events ->
          D.vcat $
            [ lines_ $ eventTable events ]
            ++
            (case intAt "next" payload of
              Just next -> [ "", D.fromChars ("Older: lamdera events --before " <> show next) ]
              Nothing -> [])
            ++
            [ "", link web appName "/events" ]

    Show n ->
      let
        event = maybe Aeson.Null id (at "event" payload)
        logs = fmap scalarText (listAt "logs" payload)
        payloadDoc =
          case at "payload" payload of
            Just Aeson.Null -> [ "  (not decoded: no wire schema for this version, or not a msg)" ]
            Nothing -> [ "  (not decoded: no wire schema for this version, or not a msg)" ]
            Just v -> fmap ("  " <>) (prettyJson v)
      in
      D.vcat
        [ lines_ $
            table []
              [ [ "Event", "#" <> textAt "id" event ]
              , [ "Time", timeAt "start" event ]
              , [ "Kind", textAt "kind" event ]
              , [ "Name", textAt "name" event ]
              , [ "Version", "v" <> textAt "version" event ]
              , [ "Duration", durationText (intAt "duration" event) ]
              , [ "Size", textAt "bytes" event <> " bytes" ]
              ]
        , ""
        , "Payload:"
        , lines_ payloadDoc
        , ""
        , D.fromChars ("Log lines (lamdera logs --msg " <> show n <> "):")
        , lines_ (if null logs then [ "  (none)" ] else fmap ("  " <>) logs)
        , ""
        , link web appName ("/events/" <> T.pack (show n))
        ]

    Metrics ->
      let
        byName = listAt "byName" payload
        failures = listAt "failures" payload
        perMinute = listAt "perMinute" payload
        connections = maybe Aeson.Null id (at "connections" payload)
      in
      D.vcat $
        [ lines_ $
            table []
              [ [ "Window", textAt "window" payload ]
              , [ "Events", textAt "total" payload ]
              , [ "Connects", textAt "connects" connections ]
              , [ "Disconnects", textAt "disconnects" connections ]
              , [ "Failures", T.pack (show (length failures)) ]
              ]
        ]
        ++
        (if null perMinute then [] else
          [ "", D.fromChars ("Activity  " <> T.unpack (sparkline (fmap (maybe 0 id . intAt "count") perMinute))) ])
        ++
        (if null byName then [] else
          [ ""
          , lines_ $
              table [ "KIND", "NAME", "COUNT", "AVG", "P95", "MAX", "ERRORS" ]
                (fmap
                  (\m ->
                      [ textAt "kind" m, textAt "name" m, textAt "count" m
                      , durationText (intAt "avgUs" m), durationText (intAt "p95Us" m), durationText (intAt "maxUs" m)
                      , textAt "errors" m
                      ]
                  )
                  (take 25 byName)
                )
          ])
        ++
        (if null failures then [] else
          [ "", "Recent failures:", lines_ (eventTable (take 10 failures)) ])
        ++
        [ "", link web appName "/events" ]


eventTable :: [Aeson.Value] -> [T.Text]
eventTable events =
  table [ "ID", "TIME", "KIND", "NAME", "DURATION", "SIZE", "VERSION" ]
    (fmap
      (\e ->
          [ textAt "id" e, timeAt "start" e, textAt "kind" e, textAt "name" e
          , durationText (intAt "duration" e), textAt "bytes" e, "v" <> textAt "version" e
          ]
      )
      events
    )


{-| A duration in microseconds, -1 (or none) when it was not timed. -}
durationText :: Maybe Int -> T.Text
durationText usM =
  case usM of
    Nothing -> "-"
    Just us
      | us < 0 -> "-"
      | us < 1000 -> T.pack (show us) <> "us"
      | us < 1000000 -> oneDecimal (fromIntegral us / 1000) <> "ms"
      | otherwise -> oneDecimal (fromIntegral us / 1000000) <> "s"


oneDecimal :: Double -> T.Text
oneDecimal d =
  let tenths = round (d * 10) :: Int in
  T.pack (show (tenths `div` 10) <> "." <> show (tenths `mod` 10))


sparkline :: [Int] -> T.Text
sparkline counts =
  let
    top = maximum (1 : counts)
    bars = "▁▂▃▄▅▆▇█"
    bar c = if c <= 0 then ' ' else T.index bars (min 7 ((c * 8 - 1) `div` top))
  in
  T.pack (fmap bar counts)
