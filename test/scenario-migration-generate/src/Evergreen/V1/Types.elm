module Evergreen.V1.Types exposing (..)

import Evergreen.V1.External


type alias BackendModel =
    { unchangedCore : Float
    , unchangedUser : UserType
    , externalUnion : Evergreen.V1.External.ExternalUnion
    , removed : String
    }


type UserType
    = UserFirst
    | UserSecond
    | UserRemoved


type alias FrontendModel =
    { basic : Int
    }


type FrontendMsg
    = Noop


type BackendMsg
    = NoOpBackendMsg


type ToBackend
    = Nooptobackend


type ToFrontend
    = Nooptofrontend
