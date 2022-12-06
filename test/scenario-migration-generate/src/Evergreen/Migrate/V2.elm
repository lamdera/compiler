module Evergreen.Migrate.V2 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.com/docs/evergreen> for more info.

-}

import Array
import Audio
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


frontendModel : Evergreen.V1.Types.FrontendModel -> ModelMigration Evergreen.V2.Types.FrontendModel Evergreen.V2.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V1.Types.BackendModel -> ModelMigration Evergreen.V2.Types.BackendModel Evergreen.V2.Types.BackendMsg
backendModel old =
    ModelMigrated ( migrate_Types_BackendModel old, Cmd.none )


frontendMsg : Evergreen.V1.Types.FrontendMsg -> MsgMigration Evergreen.V2.Types.FrontendMsg Evergreen.V2.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V1.Types.ToBackend -> MsgMigration Evergreen.V2.Types.ToBackend Evergreen.V2.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V1.Types.BackendMsg -> MsgMigration Evergreen.V2.Types.BackendMsg Evergreen.V2.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V1.Types.ToFrontend -> MsgMigration Evergreen.V2.Types.ToFrontend Evergreen.V2.Types.FrontendMsg
toFrontend old =
    MsgUnchanged


migrate_Types_BackendModel : Evergreen.V1.Types.BackendModel -> Evergreen.V2.Types.BackendModel
migrate_Types_BackendModel old =
    { unchangedCore = old.unchangedCore
    , unchangedUser = old.unchangedUser |> migrate_Types_UserType
    , unchangedAllTypes = old.unchangedAllTypes
    , unchangedResult = old.unchangedResult
    , unchangedDict = old.unchangedDict
    , unchangedAnonymousRecord =
        old.unchangedAnonymousRecord
            |> (\rec ->
                    { name = rec.name
                    , age = rec.age
                    , userType = rec.userType |> migrate_Types_UserType
                    }
               )
    , unchangedAnonymousRecordNested =
        old.unchangedAnonymousRecordNested
            |> (\rec ->
                    { name = rec.name
                    , subrecord =
                        rec.subrecord
                            |> (\rec1 ->
                                    { age = rec1.age
                                    , userType = rec1.userType |> migrate_Types_UserType
                                    }
                               )
                    }
               )
    , unchangedStringAlias = old.unchangedStringAlias
    , changedMaybe = old.changedMaybe |> Maybe.map migrate_Types_UserType
    , changedList = old.changedList |> List.map migrate_Types_UserType
    , changedSet = old.changedSet |> Set.map Unimplemented -- Type changed from `Set Int` to `Set String`
    , changedArray = old.changedArray |> Array.map migrate_Types_UserType
    , changedDict = old.changedDict |> Dict.map (\k v -> v |> migrate_Types_UserType)
    , changedResult = old.changedResult |> Result.mapError migrate_Types_UserType |> Result.map migrate_Types_UserType
    , externalUnion = old.externalUnion |> migrate_External_ExternalUnion
    , added = Unimplemented -- Type `Int` was added in V2. I need you to set a default value.
    , unionThatGetsMoved = old.unionThatGetsMoved |> Unimplemented -- I couldn't find an old type named `UnionThatGetsMoved`. I need you to write this migration.
    , aliasThatGetsMoved = old.aliasThatGetsMoved
    , typeToAlias = old.typeToAlias |> migrate_Types_TypeToAlias
    , aliasToType = old.aliasToType |> Unimplemented -- `AliasToType` was a type alias, but now it's a custom type. I need you to write this migration.
    , apps = Unimplemented -- Type `Dict (String) (Evergreen.V2.Types.App)` was added in V2. I need you to set a default value.
    , depthTests = Unimplemented -- Field of type `Dict (String) (Evergreen.V1.Types.Depth)` was removed in V2. I need you to do something with the `old.depthTests` value if you wish to keep the data, then remove this line.
    , removed = Unimplemented -- Field of type `String` was removed in V2. I need you to do something with the `old.removed` value if you wish to keep the data, then remove this line.
    , removedRecord = Unimplemented -- Field of type `Evergreen.V1.External.AllTypes` was removed in V2. I need you to do something with the `old.removedRecord` value if you wish to keep the data, then remove this line.
    }


migrate_Types_FrontendModel : Evergreen.V1.Types.FrontendModel -> Evergreen.V2.Types.FrontendModel
migrate_Types_FrontendModel old =
    { basic = old.basic
    , added = Unimplemented -- Type `Int` was added in V2. I need you to set a default value.
    }


migrate_Types_FrontendMsg : Evergreen.V1.Types.FrontendMsg -> Evergreen.V2.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    migrate_Audio_Msg migrate_Types_FrontendMsg_


migrate_Audio_FromJSMsg : Audio.FromJSMsg -> Audio.FromJSMsg
migrate_Audio_FromJSMsg old =
    case old of
        Audio.AudioLoadSuccess p0 ->
            Audio.AudioLoadSuccess p0

        Audio.AudioLoadFailed p0 ->
            Audio.AudioLoadFailed p0

        Audio.InitAudioContext p0 ->
            Audio.InitAudioContext p0

        Audio.JsonParseError p0 ->
            Audio.JsonParseError p0


migrate_Audio_Msg : (userMsgOld -> userMsgNew) -> Audio.Msg userMsgOld -> Audio.Msg userMsgNew
migrate_Audio_Msg userMsgMigrate old =
    case old of
        Audio.FromJSMsg p0 ->
            Audio.FromJSMsg p0

        Audio.UserMsg p0 ->
            Audio.UserMsg (userMsgMigrate p0)


migrate_External_AllTypes : Evergreen.V1.External.AllTypes -> Evergreen.V2.External.AllTypes
migrate_External_AllTypes p0 =
    { int = p0.int
    , float = p0.float
    , bool = p0.bool
    , char = p0.char
    , string = p0.string
    , maybeBool = p0.maybeBool
    , listInt = p0.listInt
    , setFloat = p0.setFloat
    , arrayString = p0.arrayString
    , dict = p0.dict
    , result = p0.result
    , time = p0.time
    , order = p0.order
    , unit = p0.unit
    }


migrate_External_ExternalUnion : Evergreen.V1.External.ExternalUnion -> Evergreen.V2.External.ExternalUnion
migrate_External_ExternalUnion old =
    case old of
        Evergreen.V1.External.External1 ->
            Evergreen.V2.External.External1

        Evergreen.V1.External.External2 ->
            Evergreen.V2.External.External2


migrate_Types_CustomType : Evergreen.V1.Types.CustomType -> Evergreen.V2.Types.CustomType
migrate_Types_CustomType old =
    case old of
        Evergreen.V1.Types.CustomOne ->
            Evergreen.V2.Types.CustomOne

        Evergreen.V1.Types.CustomTwo ->
            Evergreen.V2.Types.CustomTwo


migrate_Types_FrontendMsg_ : Evergreen.V1.Types.FrontendMsg_ -> Evergreen.V2.Types.FrontendMsg_
migrate_Types_FrontendMsg_ old =
    case old of
        Evergreen.V1.Types.Noop ->
            Evergreen.V2.Types.Noop

        Evergreen.V1.Types.AllTypes p0 ->
            Evergreen.V2.Types.AllTypes (p0 |> migrate_External_AllTypes)


migrate_Types_TypeToAlias : Evergreen.V1.Types.TypeToAlias -> Evergreen.V2.Types.TypeToAlias
migrate_Types_TypeToAlias old =
    -- `TypeToAlias` was a concrete type, but now it's a type alias. I need you to write this migration.
    Unimplemented


migrate_Types_UserType : Evergreen.V1.Types.UserType -> Evergreen.V2.Types.UserType
migrate_Types_UserType old =
    case old of
        Evergreen.V1.Types.UserFirst ->
            Evergreen.V2.Types.UserFirst

        Evergreen.V1.Types.UserSecond ->
            Evergreen.V2.Types.UserSecond

        Evergreen.V1.Types.UserRemoved ->
            {- `UserRemoved` was removed or renamed in V2 so I couldn't figure out how to migrate it.
               I need you to decide what happens to this Evergreen.V1.Types.UserRemoved value in a migration.
               See https://lamdera.com/tips/modified-custom-type for more info.
            -}
            Unimplemented

        Evergreen.V1.Types.UserWithParam p0 ->
            Evergreen.V2.Types.UserWithParam p0

        Evergreen.V1.Types.UserWithParams p0 p1 p2 ->
            Evergreen.V2.Types.UserWithParams p0 p1 p2

        Evergreen.V1.Types.UserWithParamCustom p0 ->
            Evergreen.V2.Types.UserWithParamCustom (p0 |> migrate_Types_CustomType)

        Evergreen.V1.Types.UserResultP1 p0 ->
            Evergreen.V2.Types.UserResultP1 (p0 |> Result.mapError migrate_Types_CustomType)

        Evergreen.V1.Types.UserResultP2 p0 ->
            Evergreen.V2.Types.UserResultP2 (p0 |> Result.map migrate_Types_CustomType)

        Evergreen.V1.Types.UserResultPBoth p0 ->
            Evergreen.V2.Types.UserResultPBoth (p0 |> Result.mapError migrate_Types_CustomType |> Result.map migrate_External_ExternalUnion)

        Evergreen.V1.Types.UserAnonymous p0 ->
            Evergreen.V2.Types.UserAnonymous
                { name = p0.name
                , userType = p0.userType |> migrate_Types_UserType
                }

        Evergreen.V1.Types.UserAnonymousNested p0 ->
            Evergreen.V2.Types.UserAnonymousNested
                { name = p0.name
                , subrecord =
                    p0.subrecord
                        |> (\rec1 ->
                                { userType = rec1.userType |> migrate_Types_UserType
                                }
                           )
                }

        Evergreen.V1.Types.UserAnonymousNestedAdded p0 ->
            Evergreen.V2.Types.UserAnonymousNestedAdded
                { name = p0.name
                , subrecord =
                    p0.subrecord
                        |> (\rec1 ->
                                { userType = rec1.userType |> migrate_Types_UserType
                                , added = Unimplemented -- Type `Int` was added in V2. I need you to set a default value.
                                }
                           )
                }

        Evergreen.V1.Types.UserAnonymousNestedRemoved p0 ->
            Evergreen.V2.Types.UserAnonymousNestedRemoved
                { name = p0.name
                , subrecord =
                    p0.subrecord
                        |> (\rec1 ->
                                { userType = rec1.userType |> migrate_Types_UserType
                                , removed = Unimplemented -- Field of type `String` was removed in V2. I need you to do something with the `rec1.removed` value if you wish to keep the data, then remove this line.
                                }
                           )
                }

        Evergreen.V1.Types.UserAnonymousNestedAddedRemoved p0 ->
            Evergreen.V2.Types.UserAnonymousNestedAddedRemoved
                { name = p0.name
                , subrecord =
                    p0.subrecord
                        |> (\rec1 ->
                                { userType = rec1.userType |> migrate_Types_UserType
                                , added = Unimplemented -- Type `Int` was added in V2. I need you to set a default value.
                                , removed = Unimplemented -- Field of type `String` was removed in V2. I need you to do something with the `rec1.removed` value if you wish to keep the data, then remove this line.
                                }
                           )
                }

        notices ->
            {- @NOTICE `UserAdded` was added in V2.
               This is just a reminder in case migrating some subset of the old data to this new value was important.
               See https://lamdera.com/tips/modified-custom-type for more info.
            -}
            {- @NOTICE `UserAddedParam Int` was added in V2.
               This is just a reminder in case migrating some subset of the old data to this new value was important.
               See https://lamdera.com/tips/modified-custom-type for more info.
            -}
            Unimplemented
