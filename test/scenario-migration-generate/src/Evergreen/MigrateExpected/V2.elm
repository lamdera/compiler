module Evergreen.MigrateExpected.V2 exposing (..)

import Evergreen.V1.External
import Evergreen.V1.Types
import Evergreen.V2.External
import Evergreen.V2.Types
import Lamdera.Migrations exposing (..)


backendModel : Evergreen.V1.Types.BackendModel -> ModelMigration Evergreen.V2.Types.BackendModel Evergreen.V2.Types.BackendMsg
backendModel old =
    { unchangedCore = old.unchangedCore
    , unchangedUser = old.unchangedUser |> migrate_Types_UserType
    , unchangedAllTypes = old.unchangedAllTypes |> migrate_External_AllTypes
    , externalUnion = old.externalUnion |> migrate_External_ExternalUnion
    , added = Unimplemented -- new in V2 (Basics.Int)
    , removed = Warning -- removed in V2 (String.String). This line is just a reminder and can be removed once you've handled it.
    , removedRecord = Warning -- removed in V2 (Evergreen.V1.External.AllTypes). This line is just a reminder and can be removed once you've handled it.
    }


frontendModel : Evergreen.V1.Types.FrontendModel -> ModelMigration Evergreen.V2.Types.FrontendModel Evergreen.V2.Types.FrontendMsg
frontendModel old =
    ModelUnchanged


migrate_External_AllTypes old =
    { int = old.int
    , float = old.float
    , bool = old.bool
    , char = old.char
    , string = old.string
    , listInt = old.listInt
    , setFloat = old.setFloat
    , arrayString = old.arrayString
    , dict = old.dict
    , time = old.time
    , order = old.order
    , unit = old.unit
    }


migrate_External_ExternalUnion old =
    case old of
        Evergreen.V1.External.External1 ->
            Evergreen.V2.External.External1

        Evergreen.V1.External.External2 ->
            Evergreen.V2.External.External2


migrate_Types_BackendMsg old =
    case old of
        Evergreen.V1.Types.NoOpBackendMsg ->
            Evergreen.V2.Types.NoOpBackendMsg


migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V1.Types.Noop ->
            Evergreen.V2.Types.Noop


migrate_Types_ToBackend old =
    case old of
        Evergreen.V1.Types.Nooptobackend ->
            Evergreen.V2.Types.Nooptobackend


migrate_Types_ToFrontend old =
    case old of
        Evergreen.V1.Types.Nooptofrontend ->
            Evergreen.V2.Types.Nooptofrontend


migrate_Types_UserType old =
    case old of
        Evergreen.V1.Types.UserFirst ->
            Evergreen.V2.Types.UserFirst

        Evergreen.V1.Types.UserSecond ->
            Evergreen.V2.Types.UserSecond

        Evergreen.V1.Types.UserRemoved ->
            {- `UserRemoved` doesn't exist in Evergreen.V2.Types.UserType so I couldn't figure out how to migrate it!
               You'll need to decide what happens to Evergreen.V1.Types.UserRemoved values in a migration.
               See https://lamdera.com/tips/modified-custom-type for more info.
            -}
            Unimplemented

        notices ->
            {- `UserAdded` doesn't exist in Evergreen.V2.Types.UserType.
               This is just a reminder in case migrating some subset of the old data to this new value was important.
               See https://lamdera.com/tips/modified-custom-type for more info.
            -}
            Notice
