module Evergreen.V1.Types exposing (..)

import Evergreen.V1.External


type alias BackendModel =
    { unchangedCore : Float
    , unchangedUser : UserType
    , unchangedAllTypes : Evergreen.V1.External.AllTypes
    , externalUnion : Evergreen.V1.External.ExternalUnion
    , removed : String
    , removedRecord : Evergreen.V1.External.AllTypes
    }


type UserType
    = UserFirst
    | UserSecond
    | UserRemoved


type alias AllTypes =
    { maybe : Maybe Int }


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
