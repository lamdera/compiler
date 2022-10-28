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

    -- Drastic changes
    , typeToAlias : TypeToAlias
    , aliasToType : AliasToType

    -- WIP
    , apps : Dict String App
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
    | UserAnonymous { name : String, userType : UserType }
    | UserAnonymousNested { name : String, subrecord : { userType : UserType } }
    | UserAnonymousNestedAdded { name : String, subrecord : { userType : UserType, added : Int } }
    | UserAnonymousNestedRemoved { name : String, subrecord : { userType : UserType } }
    | UserAnonymousNestedAddedRemoved { name : String, subrecord : { userType : UserType, added : Int } }


type CustomType
    = CustomOne
    | CustomTwo


type alias TypeToAlias =
    { maybe : Maybe Int }


type AliasToType
    = TypeToAlias_


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


type alias App =
    { configUses : ConfigUses }


type alias ConfigUses =
    { fe : Maybe (List Int)

    -- , be : Maybe (List ConfigUse)
    }



-- type alias ConfigUse =
--     ( String, String, List String )
