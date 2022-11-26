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
    , unchangedAnonymousRecord : { name : String, age : Int, userType : UserType }
    , unchangedAnonymousRecordNested :
        { name : String
        , subrecord :
            { age : Int, userType : UserType }
        }
    , unchangedStringAlias : StringAlias
    , changedMaybe : Maybe UserType
    , changedList : List UserType
    , changedSet : Set Int
    , changedArray : Array UserType
    , changedDict : Dict Int UserType
    , changedResult : Result UserType UserType
    , externalUnion : Evergreen.V1.External.ExternalUnion
    , removed : String
    , removedRecord : Evergreen.V1.External.AllTypes

    -- Drastic changes
    , typeThatGetsMoved : TypeThatGetsMoved
    , typeToAlias : TypeToAlias
    , aliasToType : AliasToType

    -- WIP
    , depthTests : Dict String Depth
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
    | UserAnonymous { name : String, userType : UserType }
    | UserAnonymousNested { name : String, subrecord : { userType : UserType } }
    | UserAnonymousNestedAdded { name : String, subrecord : { userType : UserType } }
    | UserAnonymousNestedRemoved { name : String, subrecord : { userType : UserType, removed : String } }
    | UserAnonymousNestedAddedRemoved { name : String, subrecord : { userType : UserType, removed : String } }


type CustomType
    = CustomOne
    | CustomTwo


type TypeToAlias
    = TypeToAlias_


type alias AliasToType =
    { maybe : Maybe Int }


type alias FrontendModel =
    { basic : Int
    }


type alias StringAlias =
    String


type FrontendMsg
    = Noop
    | AllTypes Evergreen.V1.External.AllTypes


type BackendMsg
    = NoOpBackendMsg


type ToBackend
    = Nooptobackend


type ToFrontend
    = Nooptofrontend


type alias Depth =
    { typeUses : TypeUses }


type alias TypeUses =
    { m : Maybe (List ConfigUse)
    , l : List ConfigUse
    , s : Set ConfigUse
    , a : Array ConfigUse
    , d : Dict String (List ConfigUse)
    , r : Result String (List ConfigUse)
    }


type alias ConfigUse =
    ( String, String, List String )


type TypeThatGetsMoved
    = TypeThatGetsMoved
