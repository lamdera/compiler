module Msg exposing (Herp(..))

import Lamdera.Types exposing (ClientId, WsError)


type Herp
    = Derp String String


type FrontendMsg
    = MessageFieldChanged String
    | MessageResponse (Result WsError ())
    | MessageSubmitted
    | Skip


type ToBackend
    = MsgSubmitted String
    | ClientJoined


type BackendMsg
    = Noop
    | MsgResponse ClientId (Result WsError ())


type ToFrontend
    = RoomMsgReceived Message


type alias Message =
    ( String, String )
