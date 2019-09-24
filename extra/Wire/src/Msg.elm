module Msg exposing (..)

import Lamdera.Types exposing (ClientId, WsError)


type FrontendMsg
    = Increment
    | Decrement
    | FNoop


type ToBackend
    = ClientJoin
    | CounterIncremented
    | CounterDecremented


type BackendMsg
    = Noop


type ToFrontend
    = CounterNewValue Int
