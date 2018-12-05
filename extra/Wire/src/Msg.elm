module Msg exposing (..)

import Json.Encode as E
import Evergreen as EG
import Json.Decode as D


type Herp
    = Derp


evg_e_Herp evg_p0 =
    Debug.todo "evg_e_Herp"


evg_d_Herp =
    Debug.todo "evg_d_Herp"


-- --------------
-- -- import Lamdera.Types exposing (ClientId, WsError)
--
--
-- type FrontendMsg
--     = MessageFieldChanged String
--     | MessageResponse (Result WsError ())
--     | MessageSubmitted
--     | Skip
--
--
-- type ToBackend
--     = MsgSubmitted String
--     | ClientJoined
--
--
-- type BackendMsg
--     = Noop
--     | MsgResponse ClientId (Result WsError ())
--
--
-- type ToFrontend
--     = RoomMsgReceived Message
--
--
-- type alias Message =
--     ( String, String )
--
--
--
-- --- Manual until we have Lamdera.Types working
--
--
-- type WsError
--     = TimeoutWithoutEverBeingOnline -- we didn't even try to send it, because we were offline
--     | SendAttempted -- we tried to send, but don't know if it arrived
--     | ReceivedAndProbablyProcessed -- we know we reached Lamdera, but not if it reached the backend app
--     | ReceivedButCertainlyNotProcessed -- we got a nack; msg was rejected by lamdera before it reached the backend app
--
--
-- type ClientId
--     = ClientId String
