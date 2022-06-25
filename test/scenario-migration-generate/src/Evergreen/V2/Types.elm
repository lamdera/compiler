module Evergreen.V2.Types exposing (..)

import Evergreen.V2.External


type alias BackendModel =
    { unchangedCore : Float
    , unchangedUser : UserType
    , unchangedAllTypes : Evergreen.V2.External.AllTypes
    , externalUnion : Evergreen.V2.External.ExternalUnion
    , added : Int
    }


type UserType
    = UserFirst
    | UserSecond
    | UserAdded


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
