module Evergreen.V1.Types exposing (..)

import Dict exposing (Dict)
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
    | UserWithParam Int
    | UserWithParams Float String (Dict Int String)
    | UserWithParamCustom CustomType


type CustomType
    = CustomOne
    | CustomTwo


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
