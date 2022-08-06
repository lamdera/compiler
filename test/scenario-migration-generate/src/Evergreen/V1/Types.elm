module Evergreen.V1.Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Evergreen.V1.External
import Set exposing (Set)


type alias BackendModel =
    { unchangedCore : Float
    , unchangedUser : UserType
    , unchangedAllTypes : Evergreen.V1.External.AllTypes
    , unchangedResult : Result Int String
    , unchangedDict : Dict Int String
    , changedMaybe : Maybe UserType
    , changedList : List UserType
    , changedSet : Set Int
    , changedArray : Array UserType
    , changedDict : Dict Int UserType
    , changedResult : Result UserType UserType
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
    | UserResultP1 (Result CustomType String)
    | UserResultP2 (Result Int CustomType)
    | UserResultPBoth (Result CustomType Evergreen.V1.External.ExternalUnion)


type CustomType
    = CustomOne
    | CustomTwo


type alias UserRecordChanged =
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
