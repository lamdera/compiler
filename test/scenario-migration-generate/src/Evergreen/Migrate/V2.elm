module Evergreen.Migrate.V2 exposing (..)

import Evergreen.V1.External
import Evergreen.V1.Types
import Evergreen.V2.External
import Evergreen.V2.Types
import Lamdera.Migrations exposing (..)


backendModel : Evergreen.V1.Types.BackendModel -> ModelMigration Evergreen.V2.Types.BackendModel Evergreen.V2.Types.BackendMsg
backendModel old =
    { unchangedCore = old.unchangedCore
    , unchangedUser = old.unchangedUser |> migrate_Types_UserType
    , externalUnion = old.externalUnion |> migrate_External_ExternalUnion
    , added = Unimplemented -- new field of type: Basics.Int
    , removed = Unimplemented -- String.String field removed. either remove this line (data dropped) or migrate into another field.
    }


frontendModel : Evergreen.V1.Types.FrontendModel -> ModelMigration Evergreen.V2.Types.FrontendModel Evergreen.V2.Types.FrontendMsg
frontendModel old =
    ModelUnchanged


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
            {- `UserRemoved` doesn't exist in Evergreen.V2.Types.UserType so I couldn't figure out how to migrate it.
               You'll need to decide what happens to Evergreen.V1.Types.UserRemoved values in a migration.
               See https://lamdera.com/tips/modified-custom-type for more info.
            -}
            Unimplemented

        notices ->
            {- `UserAdded` doesn't exist in Evergreen.V2.Types.UserType.
               This is just a reminder in case migrating some subset of the old data to this new value was important.
               See https://lamdera.com/tips/modified-custom-type for more info.
            -}
            Unimplemented
