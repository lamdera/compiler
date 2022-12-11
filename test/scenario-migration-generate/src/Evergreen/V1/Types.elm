module Evergreen.V1.Types exposing (..)

import Array exposing (Array)
import AssocList
import Audio
import Dict exposing (Dict)
import Evergreen.V1.External
import Evergreen.V1.IncludedByParam
import Evergreen.V1.IncludedBySpecialCasedParam
import Set exposing (Set)
import Time


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
    , unionThatGetsMoved : UnionThatGetsMoved
    , aliasThatGetsMoved : AliasThatGetsMoved
    , typeToAlias : TypeToAlias
    , aliasToType : AliasToType

    -- Package types
    , time : Time.Posix

    -- Special cased types
    , userCache : AssocList.Dict String Evergreen.V1.IncludedBySpecialCasedParam.Custom

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
    | UserTvarAlias (Evergreen.V1.External.Paramed CustomType)
    | UserTvarAlias2 (Evergreen.V1.External.Paramed2 CustomType Evergreen.V1.External.AllTypes)
    | UserTvarAliasSub (Evergreen.V1.External.ParamedSub Evergreen.V1.IncludedByParam.Record)


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


type alias FrontendMsg =
    Audio.Msg FrontendMsg_


type FrontendMsg_
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


type UnionThatGetsMoved
    = UnionThatGetsMoved


type alias AliasThatGetsMoved =
    { someThing : String }
