module Evergreen.V2.Types exposing (..)

import Array exposing (Array)
import AssocList
import Audio
import Dict exposing (Dict)
import Evergreen.V2.External
import Evergreen.V2.IncludedByParam
import Evergreen.V2.IncludedBySpecialCasedParam
import Lamdera
import Set exposing (Set)
import Time
import Url


type alias BackendModel =
    { unchangedCore : Float
    , unchangedUser : UserType
    , unchangedAllCoreTypes : Evergreen.V2.External.AllCoreTypes
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
    , withCustomSet : Set String
    , withCustomArray : Array UserType
    , withCustomDict : Dict Int UserType
    , withCustomResult : Result UserType UserType
    , externalUnion : Evergreen.V2.External.ExternalUnion
    , added : Int

    -- Drastic changes
    , unionThatGetsMoved : Evergreen.V2.External.UnionThatGetsMoved
    , aliasThatGetsMoved : Evergreen.V2.External.AliasThatGetsMoved
    , typeToAlias : TypeToAlias
    , aliasToType : AliasToType

    -- Package types
    , time : Time.Posix

    -- Special cased types
    , userCache : AssocList.Dict String Evergreen.V2.IncludedBySpecialCasedParam.Custom

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
    | UserTvarAlias (Evergreen.V2.External.Paramed CustomType)
    | UserTvarAlias2 (Evergreen.V2.External.Paramed2 CustomType Evergreen.V2.External.AllCoreTypes)
    | UserTvarAliasSub (Evergreen.V2.External.ParamedSub Evergreen.V2.IncludedByParam.Custom)
    | UserExtTime Time.Posix
    | UserExtResultTime (Result String Time.Posix)


type CustomType
    = CustomOne
    | CustomTwo


type alias TypeToAlias =
    { maybe : Maybe Int }


type AliasToType
    = TypeToAlias_


type alias FrontendModel =
    { basic : Int
    , added : Int
    , url : Url.Url
    , key : Lamdera.Key
    }


type alias StringAlias =
    String


type alias FrontendMsg =
    Audio.Msg FrontendMsg_


type FrontendMsg_
    = Noop
    | AllCoreTypes Evergreen.V2.External.AllCoreTypes


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


type alias ConfigUse =
    ( String, String, List String )
