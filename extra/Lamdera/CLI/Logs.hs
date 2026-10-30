{-# LANGUAGE OverloadedStrings #-}

module Lamdera.CLI.Logs where

{- `lamdera logs [--lines N] [--head] [--grep TEXT] [--msg N] [--follow]`: the app's log.

The log is a sequence of records (docs/app-log-format.md in the platform repo): a line
`[YYYY-MM-DD HH:MM:SS.mmm] ...` (UTC) starts one, and the lines without a time stamp that
follow it continue it. --follow asks every 2s for the records from the time of the last
one it printed onwards (`from`, oldest first) and leaves out the ones at that very time it
already printed.
-}

import Control.Concurrent (threadDelay)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.Format as Time
import System.IO (hFlush, stdout)

import qualified Reporting.Doc as D

import qualified Lamdera.Progress as Progress

import Lamdera.CLI.Dashboard hiding (Common(..))
import qualified Lamdera.CLI.Dashboard as Dash


data Flags =
  Flags
    { _app :: Maybe String
    , _json :: Bool
    , _lines :: Maybe Int
    , _head :: Bool
    , _grep :: Maybe String
    , _msg :: Maybe Int
    , _follow :: Bool
    }
    deriving (Show, Eq)


common :: Flags -> Dash.Common
common flags =
  Dash.Common (_app flags) (_json flags) False


-- How many records a --follow poll asks for at most; the next poll carries on from there.
followBatch :: Int
followBatch = 1000


toRequest :: T.Text -> Flags -> Maybe Int -> Request
toRequest appName flags fromM =
  appRequest appName "logs" $
    (case fromM of
      Nothing ->
        optional "lines" FInt (_lines flags)
          ++ [ ("direction", FString (if _head flags then "head" else "tail")) ]
      Just from ->
        [ ("lines", FInt followBatch), ("direction", FString "head"), ("from", FInt from) ]
    )
    ++ optional "grep" (FString . T.pack) (_grep flags)
    ++ optional "msg" FInt (_msg flags)


run :: () -> Flags -> IO ()
run () flags = do
  let c = common flags
  if _follow flags && _head flags
    then failWith (_json flags) "--head and --follow don't go together: --follow shows the newest records as they come."
    else pure ()
  ctx <- setup c NeedsApp
  let appName = appOf ctx
  payload <- call ctx (toRequest appName flags Nothing)
  let ls = listAt "lines" payload
  if not (_follow flags)
    then
      if _json flags
        then output ctx payload ""
        else
          case ls of
            [] -> Progress.report (said "No log lines.")
            _ -> printLines (fmap scalarText ls)
    else do
      let batch = fmap scalarText ls
      emit ctx payload batch
      let
        loop state = do
          threadDelay 2000000
          next <- call ctx (toRequest appName flags (fst state))
          let (fresh, state_) = followStep state (fmap scalarText (listAt "lines" next))
          if null fresh then pure () else emit ctx (Aeson.object [ ("lines", Aeson.toJSON fresh) ]) fresh
          loop state_
      loop (snd (followStep (Nothing, []) batch))


emit :: Ctx -> Aeson.Value -> [T.Text] -> IO ()
emit ctx payload ls =
  if _jsonOut ctx
    then BSL8.putStrLn (Aeson.encode payload) >> hFlush stdout
    else printLines ls


printLines :: [T.Text] -> IO ()
printLines ls =
  mapM_ T.putStrLn ls >> hFlush stdout



-- FOLLOWING


{-| The time of the last record printed, and the records printed at exactly that time. -}
type FollowState = (Maybe Int, [[T.Text]])


{-| What to print of a new batch (the records past the last time, and those at it that
were not printed yet), and the state after it. A batch without any time stamp leaves
the state alone. -}
followStep :: FollowState -> [T.Text] -> ([T.Text], FollowState)
followStep (lastM, seen) batch =
  let
    records = toRecords batch
    fresh =
      filter
        (\(t, r) ->
            case (lastM, t) of
              (Just lastT, Just rt) -> rt > lastT || (rt == lastT && notElem r seen)
              (Just _, Nothing) -> True
              (Nothing, _) -> True
        )
        records
    times = [ t | (Just t, _) <- records ]
    newLast = if null times then lastM else Just (maximum times)
    newSeen =
      case newLast of
        Just t | newLast /= lastM -> [ r | (Just rt, r) <- records, rt == t ]
        Just t -> seen ++ [ r | (Just rt, r) <- fresh, rt == t ]
        Nothing -> seen
  in
  (concatMap snd fresh, (newLast, newSeen))


{-| Lines grouped into records, each with its time (lines before the first time stamp
make records of their own). -}
toRecords :: [T.Text] -> [(Maybe Int, [T.Text])]
toRecords ls =
  case ls of
    [] -> []
    l : rest ->
      case recordTime l of
        Just t ->
          let (continued, others) = break (\x -> recordTime x /= Nothing) rest in
          (Just t, l : continued) : toRecords others
        Nothing ->
          (Nothing, [l]) : toRecords rest


{-| The time of a record's first line, in ms since the epoch (the stamp is UTC). -}
recordTime :: T.Text -> Maybe Int
recordTime line =
  if T.length line < 25 || T.take 1 line /= "[" || T.index line 24 /= ']' then Nothing else
  let
    stamp = T.unpack (T.take 23 (T.drop 1 line))
  in
  case Time.parseTimeM False Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" stamp of
    Just utc -> Just (round (Time.utcTimeToPOSIXSeconds utc * 1000))
    Nothing -> Nothing
