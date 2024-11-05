module Evergreen.Migrate.V2 exposing (backendModel, backendMsg, frontendModel, frontendMsg, toBackend, toFrontend)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.TODOTYPEMsg
frontendModel old =
    ModelUnchanged


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.TODOTYPEMsg
backendModel old =
    let
        missingFields =
            -- removed field: String.String
            old.removed
    in
    { unchangedCore = old.unchangedCore
    , added = Unimplemented -- new field: Basics.Int
    , -- warning:
      removed = Warning -- removed field: String.String  either remove this line (data dropped) or migrate into another field.
    }


migrateFrontendMsg old =
    case old of
        Evergreen.V1.Types.Noop ->
            Evergreen.V2.Types.Noop


migrateToBackend old =
    case old of
        Evergreen.V1.Types.Nooptobackend ->
            Evergreen.V2.Types.Nooptobackend


migrateBackendMsg old =
    case old of
        Evergreen.V1.Types.NoOpBackendMsg ->
            Evergreen.V2.Types.NoOpBackendMsg


migrateToFrontend old =
    case old of
        Evergreen.V1.Types.Nooptofrontend ->
            Evergreen.V2.Types.Nooptofrontend
