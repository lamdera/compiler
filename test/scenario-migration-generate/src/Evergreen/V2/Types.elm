module Evergreen.V2.Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Evergreen.V2.External
import Set exposing (Set)


type alias BackendModel =
    { unchangedCore : Float
    , unchangedUser : UserType
    , unchangedAllTypes : Evergreen.V2.External.AllTypes
    , unchangedResult : Result Int String
    , unchangedDict : Dict Int String
    , unchangedAnonymousRecord : { name : String, age : Int, userType : UserType }
    , unchangedAnonymousRecordNested :
        { name : String
        , subrecord :
            { age : Int, userType : UserType }
        }
    , changedMaybe : Maybe UserType
    , changedList : List UserType
    , changedSet : Set String
    , changedArray : Array UserType
    , changedDict : Dict Int UserType
    , changedResult : Result UserType UserType
    , externalUnion : Evergreen.V2.External.ExternalUnion
    , added : Int
    }


type UserType
    = UserFirst
    | UserSecond
    | UserAdded
    | UserAddedParam Int
    | UserWithParam Int
    | UserWithParams Float String (Dict Int String)
    | UserWithParamCustom CustomType
    | UserResultP1 (Result CustomType String)
    | UserResultP2 (Result Int CustomType)
    | UserResultPBoth (Result CustomType Evergreen.V2.External.ExternalUnion)
    | UserAnonymous { record : String, userType : UserType }
    | UserAnonymousNested { record : String, subrecord : { userType : UserType } }


type CustomType
    = CustomOne
    | CustomTwo


type alias UserRecordChanged =
    { maybe : Maybe String }


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
