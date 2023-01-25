module Migrate_All.Old exposing (..)

import Array exposing (Array)
import AssocList
import Audio
import Dict exposing (Dict)
import Evergreen.V1.External
import Evergreen.V1.IncludedByParam
import Evergreen.V1.IncludedBySpecialCasedParam
import Lamdera
import Set exposing (Set)
import Time
import Url


type alias Target =
    BackendModel


type alias BackendModel =
    { unchangedCore : Float
    , unchangedUser : UserType
    , unchangedAllCoreTypes : Evergreen.V1.External.AllCoreTypes
    , unchangedResult : Result Int String
    , unchangedDict : Dict Int String
    , unchangedAnonymousRecord : { name : String, age : Int, userType : UserType }
    , unchangedAnonymousRecordNested :
        { name : String
        , subrecord :
            { age : Int, userType : UserType }
        }
    , unchangedStringAlias : StringAlias
    , withCustomMaybe : Maybe UserType
    , withCustomList : List UserType
    , withCustomSet : Set Int
    , withCustomArray : Array UserType
    , withCustomDict : Dict Int UserType
    , withCustomResult : Result UserType UserType
    , externalUnion : Evergreen.V1.External.ExternalUnion
    , removed : String
    , removedRecord : Evergreen.V1.External.AllCoreTypes

    -- Drastic changes
    , unionThatGetsMoved : UnionThatGetsMoved
    , aliasThatGetsMoved : AliasThatGetsMoved
    , typeToAlias : TypeToAlias
    , aliasToType : AliasToType

    -- Package types
    , time : Time.Posix
    , url : Url.Url

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
    | UserTuple ( Int, UserType )
    | UserTriple ( Int, Float, UserType )
    | UserTvarAlias (Evergreen.V1.External.Paramed CustomType)
    | UserTvarAlias2 (Evergreen.V1.External.Paramed2 CustomType Evergreen.V1.External.AllCoreTypes)
    | UserTvarAliasSub (Evergreen.V1.External.ParamedSub Evergreen.V1.IncludedByParam.Custom)
    | UserExtTime Time.Posix
    | UserExtResultTime (Result String Time.Posix)


type CustomType
    = CustomOne
    | CustomTwo


type TypeToAlias
    = TypeToAlias_


type alias AliasToType =
    { maybe : Maybe Int }


type alias FrontendModel =
    { basic : Int
    , url : Url.Url
    , key : Lamdera.Key
    }


type alias StringAlias =
    String


type alias FrontendMsg =
    Audio.Msg FrontendMsg_


type FrontendMsg_
    = Noop
    | AllCoreTypes Evergreen.V1.External.AllCoreTypes


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
