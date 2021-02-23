{-# LANGUAGE OverloadedStrings #-}

-- Ported from https://github.com/supermario/hilt/blob/master/src/Hilt/SocketServer.hs
-- Modified to remove managed and have clientId injection rather than auto-gen

module SocketServer where

import Data.Monoid           ((<>))
import Data.List             (find)
import Control.Exception     (finally)
import Control.Monad         (forM_, forever)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Control.Concurrent.STM

import Lamdera


clientsInit :: IO (TVar [Client])
clientsInit = newTVarIO []

leaderInit :: IO (TVar (Maybe ClientId))
leaderInit = newTVarIO Nothing


socketHandler :: TVar [Client] -> TVar (Maybe ClientId) -> TVar Text -> OnJoined -> OnReceive -> T.Text -> T.Text -> WS.ServerApp
socketHandler mClients mLeader beState onJoined onReceive clientId sessionId pending = do

  -- Lamdera.debugT $ "[websocket] ❇️  " <> clientId
  conn <- WS.acceptRequest pending

  let client     = (clientId, conn)
      disconnect = do
        leaderChanged <- atomically $ do
          clients <- readTVar mClients
          let remainingClients = removeClient client clients

          leader <- readTVar mLeader
          changed <- do
            case leader of
              Just leaderId ->
                if leaderId == clientId
                  then do
                    writeTVar mLeader (getNextLeader remainingClients)
                    pure True

                  else
                    -- No need to change leader ID
                    pure False

              Nothing ->
                -- No leader elected already somehow,
                -- should be impossible
                pure False

          writeTVar mClients remainingClients

          pure changed

        -- Lamdera.debugT ("[websocket] 🚫 " <> clientId)
        SocketServer.broadcastImpl mClients $ "{\"t\":\"d\",\"s\":\"" <> sessionId <> "\",\"c\":\""<> clientId <> "\"}"

        onlyWhen leaderChanged $ do
          sendToLeader mClients mLeader (\leader -> do
              -- Tell the new leader about the backend state they need
              atomically $ readTVar beState
            )
          -- Tell everyone about the new leader (also causes actual leader to go active as leader)
          broadcastLeader mClients mLeader

        pure ()

  flip finally disconnect $ do
    clientCount <- atomically $ do
      clients <- readTVar mClients
      writeTVar mClients $ addClient client clients
      pure $ length clients

    initText <- onJoined clientId clientCount
    case initText of
      Just text -> sendImpl mClients clientId text
      Nothing   -> do
        -- @TODO Should really be a NOTICE level log via a logger
        -- Lamdera.debugT "[websocket:notice] No init message for new client was provided"
        pure ()

    talk onReceive conn mClients client

electionFor :: ClientId -> Text
electionFor leaderId =
  "{\"t\":\"e\",\"l\":\"" <> leaderId <> "\"}"

getNextLeader :: [Client] -> Maybe ClientId
getNextLeader clients =
  case clients of
  [] ->
    Nothing

  (newLeaderId, _):_ ->
    (Just newLeaderId)


broadcastLeader :: TVar [Client] -> TVar (Maybe ClientId) -> IO ()
broadcastLeader mClients mLeader = do
  leader <- atomically $ readTVar mLeader
  case leader of
    Just leaderId ->
      broadcastImpl mClients $ "{\"t\":\"e\",\"l\":\"" <> leaderId <> "\"}"

    Nothing ->
      pure ()


sendToLeader :: TVar [Client] -> TVar (Maybe ClientId) -> (ClientId -> IO Text) -> IO ()
sendToLeader mClients mLeader fn = do
  leader <- atomically $ readTVar mLeader
  case leader of
    Just leaderId -> do
      text <- fn leaderId
      sendImpl mClients leaderId text

    Nothing ->
      pure ()


data Handle = Handle
  { send      :: Int -> T.Text -> IO ()
  , broadcast :: T.Text -> IO ()
  , app       :: WS.ServerApp
  }


-- OnJoined = clientId -> totalClients -> IO (Maybe (response message))
type OnJoined = ClientId -> Int -> IO (Maybe T.Text)


-- OnReceive = clientId -> receivedMessage -> IO ()
type OnReceive = ClientId -> T.Text -> IO ()


type ClientId = T.Text
type Client = (ClientId, WS.Connection)


sendImpl :: TVar [Client] -> ClientId -> T.Text -> IO ()
sendImpl mClients clientId message = do
  clients <- atomically $ readTVar mClients
  send_ clients clientId message


broadcastImpl :: TVar [Client] -> T.Text -> IO ()
broadcastImpl mClients message = do
  clients <- atomically $ readTVar mClients
  broadcast_ clients message


talk :: OnReceive -> WS.Connection -> TVar [Client] -> Client -> IO ()
talk onReceive conn _ (clientId, _) = forever $ do
  msg <- WS.receiveData conn
  -- Lamdera.debugT ("[websocket] ▶️  " <> T.pack (show clientId) <> ":" <> T.take 130 msg)
  onReceive clientId msg


addClient :: Client -> [Client] -> [Client]
addClient client clients = client : clients


removeClient :: Client -> [Client] -> [Client]
removeClient client = filter ((/=fst client) . fst)


removeClientId :: ClientId -> [Client] -> [Client]
removeClientId clientId = filter ((/= clientId) . fst)


findClient :: [Client] -> ClientId -> Maybe Client
findClient clients clientId = find ((==clientId) . fst) clients


send_ :: [Client] -> ClientId -> T.Text -> IO ()
send_ clients clientId text =
  case findClient clients clientId of
    Just (_, conn) -> WS.sendTextData conn text
    Nothing        -> pure ()


broadcast_ :: [Client] -> T.Text -> IO ()
broadcast_ clients message = do
  -- Lamdera.debugT ("[websocket] ◀️  " <> T.pack (show $ length clients) <> ":" <> T.take 130 message)
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message
