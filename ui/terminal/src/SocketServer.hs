{-# LANGUAGE OverloadedStrings #-}

-- Ported from https://github.com/supermario/hilt/blob/master/src/Hilt/SocketServer.hs

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


socketHandler :: TVar [Client] -> OnJoined -> OnReceive -> T.Text -> WS.ServerApp
socketHandler mClients onJoined onReceive clientId pending = do

  Lamdera.debugT $ "[websocket] Handling new client: " <> clientId
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30

  let client     = (clientId, conn)
      disconnect = do
        atomically $ do
          clients <- readTVar mClients
          writeTVar mClients $ removeClient client clients

        Lamdera.debugT ("[websocket:disconnect] " <> clientId)
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
        Lamdera.debugT "[websocket:notice] No init message for new client was provided"
        pure ()

    talk onReceive conn mClients client


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


-- load :: OnJoined -> OnReceive -> Managed Handle
-- load onJoined onReceive = managed $ withHandle onJoined onReceive


-- loadDefault :: OnJoined -> OnReceive -> Managed Handle
-- loadDefault onJoined onReceive = managed $ withHandle onJoined onReceive


-- withHandle :: OnJoined -> OnReceive -> (Handle -> IO a) -> IO a
-- withHandle onJoined onReceive f = loadRaw onJoined onReceive >>= f


-- loadRaw :: OnJoined -> OnReceive -> IO Handle
-- loadRaw onJoined onReceive = do
--   mClients   <- newTVarIO []
--   mClientIds <- newTVarIO [1 ..]
--
--   pure Handle
--     { send      = sendImpl mClients
--     , broadcast = broadcastImpl mClients
--     , app       = appImpl mClients mClientIds onJoined onReceive
--     }


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
  Lamdera.debugT ("[websocket:received] " <> T.pack (show clientId) <> ":" <> msg)
  onReceive clientId msg


addClient :: Client -> [Client] -> [Client]
addClient client clients = client : clients


removeClient :: Client -> [Client] -> [Client]
removeClient client = filter ((/=fst client) . fst)


findClient :: [Client] -> ClientId -> Maybe Client
findClient clients clientId = find ((==clientId) . fst) clients


send_ :: [Client] -> ClientId -> T.Text -> IO ()
send_ clients clientId text =
  case findClient clients clientId of
    Just (_, conn) -> WS.sendTextData conn text
    Nothing        -> pure ()


broadcast_ :: [Client] -> T.Text -> IO ()
broadcast_ clients message = do
  Lamdera.debugT ("[websocket:broadcast] " <> T.pack (show $ length clients) <> ":" <> message)
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message
