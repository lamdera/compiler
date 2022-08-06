module Evergreen.Migrate.V2 exposing (..)

import Array
import Dict
import Evergreen.V1.External
import Evergreen.V1.Types
import Evergreen.V2.External
import Evergreen.V2.Types
import Lamdera.Migrations exposing (..)
import List
import Maybe
import Result
import Set


backendModel : Evergreen.V1.Types.BackendModel -> ModelMigration Evergreen.V2.Types.BackendModel Evergreen.V2.Types.BackendMsg
backendModel old =
    { unchangedCore = old.unchangedCore
    , unchangedUser = migrate_Types_UserType old.unchangedUser
    , unchangedAllTypes = migrate_External_AllTypes old.unchangedAllTypes
    , unchangedResult = old.unchangedResult
    , unchangedDict = old.unchangedDict
    , changedMaybe = Maybe.map migrate_Types_UserType old.changedMaybe
    , changedList = List.map migrate_Types_UserType old.changedList
    , changedSet =
        -- Type changed from `Set Int` to `Set String`
        Set.map Unimplemented old.changedSet
    , changedArray = Array.map migrate_Types_UserType old.changedArray
    , changedDict = Dict.fromList <| List.map (Tuple.mapFirst migrate_Types_UserType) <| Dict.toList old.changedDict
    , changedResult = Result.mapError migrate_Types_UserType <| Result.map migrate_Types_UserType <| old.changedResult
    , externalUnion = migrate_External_ExternalUnion old.externalUnion
    , added = Unimplemented -- Type `Int` added in V2
    , removed = Warning -- Type `String` removed in V2. This line is just a reminder and can be removed once you've handled it.
    , removedRecord = Warning -- Type `Evergreen.V1.External.AllTypes` removed in V2. This line is just a reminder and can be removed once you've handled it.
    }


frontendModel : Evergreen.V1.Types.FrontendModel -> ModelMigration Evergreen.V2.Types.FrontendModel Evergreen.V2.Types.FrontendMsg
frontendModel old =
    ModelUnchanged


migrate_External_AllTypes : Evergreen.V1.External.AllTypes -> Evergreen.V2.External.AllTypes
migrate_External_AllTypes old =
    { int = old.int
    , float = old.float
    , bool = old.bool
    , char = old.char
    , string = old.string
    , maybeBool = old.maybeBool
    , listInt = old.listInt
    , setFloat = old.setFloat
    , arrayString = old.arrayString
    , dict = old.dict
    , result = old.result
    , time = old.time
    , order = old.order
    , unit = old.unit
    }


migrate_External_ExternalUnion : Evergreen.V1.External.ExternalUnion -> Evergreen.V2.External.ExternalUnion
migrate_External_ExternalUnion old =
    case old of
        Evergreen.V1.External.External1 ->
            Evergreen.V2.External.External1

        Evergreen.V1.External.External2 ->
            Evergreen.V2.External.External2


migrate_Types_BackendMsg : Evergreen.V1.Types.BackendMsg -> Evergreen.V2.Types.BackendMsg
migrate_Types_BackendMsg old =
    case old of
        Evergreen.V1.Types.NoOpBackendMsg ->
            Evergreen.V2.Types.NoOpBackendMsg


migrate_Types_CustomType : Evergreen.V1.Types.CustomType -> Evergreen.V2.Types.CustomType
migrate_Types_CustomType old =
    case old of
        Evergreen.V1.Types.CustomOne ->
            Evergreen.V2.Types.CustomOne

        Evergreen.V1.Types.CustomTwo ->
            Evergreen.V2.Types.CustomTwo


migrate_Types_FrontendMsg : Evergreen.V1.Types.FrontendMsg -> Evergreen.V2.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V1.Types.Noop ->
            Evergreen.V2.Types.Noop


migrate_Types_ToBackend : Evergreen.V1.Types.ToBackend -> Evergreen.V2.Types.ToBackend
migrate_Types_ToBackend old =
    case old of
        Evergreen.V1.Types.Nooptobackend ->
            Evergreen.V2.Types.Nooptobackend


migrate_Types_ToFrontend : Evergreen.V1.Types.ToFrontend -> Evergreen.V2.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V1.Types.Nooptofrontend ->
            Evergreen.V2.Types.Nooptofrontend


migrate_Types_UserType : Evergreen.V1.Types.UserType -> Evergreen.V2.Types.UserType
migrate_Types_UserType old =
    case old of
        Evergreen.V1.Types.UserFirst ->
            Evergreen.V2.Types.UserFirst

        Evergreen.V1.Types.UserSecond ->
            Evergreen.V2.Types.UserSecond

        Evergreen.V1.Types.UserRemoved ->
            {- `UserRemoved` doesn't exist in Evergreen.V2.Types.UserType so I couldn't figure out how to migrate it.
               You'll need to decide what happens to this Evergreen.V1.Types.UserRemoved value in a migration.
               See https://lamdera.com/tips/modified-custom-type for more info.
            -}
            Unimplemented

        Evergreen.V1.Types.UserWithParam p0 ->
            Evergreen.V2.Types.UserWithParam p0

        Evergreen.V1.Types.UserWithParams p0 p1 p2 ->
            Evergreen.V2.Types.UserWithParams p0 p1 p2

        Evergreen.V1.Types.UserWithParamCustom p0 ->
            Evergreen.V2.Types.UserWithParamCustom (migrate_Types_CustomType p0)

        Evergreen.V1.Types.UserResultP1 p0 ->
            Evergreen.V2.Types.UserResultP1 (Result.map migrate_Types_CustomType <| p0)

        Evergreen.V1.Types.UserResultP2 p0 ->
            Evergreen.V2.Types.UserResultP2 (Result.mapError migrate_Types_CustomType <| p0)

        Evergreen.V1.Types.UserResultPBoth p0 ->
            Evergreen.V2.Types.UserResultPBoth (Result.mapError migrate_Types_CustomType <| Result.map migrate_External_ExternalUnion <| p0)

        notices ->
            {- `UserAdded` doesn't exist in the old Evergreen.V1.Types.UserType.
               This is just a reminder in case migrating some subset of the old data to this new value was important.
               See https://lamdera.com/tips/modified-custom-type for more info.
            -}
            {- `UserAddedParam Int` doesn't exist in the old Evergreen.V1.Types.UserType.
               This is just a reminder in case migrating some subset of the old data to this new value was important.
               See https://lamdera.com/tips/modified-custom-type for more info.
            -}
            Notice
