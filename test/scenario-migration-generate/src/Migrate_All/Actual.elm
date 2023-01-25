module Migrate_All.Actual exposing (..)

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
import AssocList
import Dict
import Evergreen.V1.External
import Evergreen.V1.IncludedByParam
import Evergreen.V1.IncludedBySpecialCasedParam
import Evergreen.V2.External
import Evergreen.V2.IncludedByParam
import Evergreen.V2.IncludedBySpecialCasedParam
import Lamdera.Migrations exposing (..)
import List
import Maybe
import Migrate_All.New
import Migrate_All.Old
import Result
import Set


target =
    migrate_Migrate_All_New_Target


migrate_Migrate_All_New_Target : Migrate_All.Old.Target -> Migrate_All.New.Target
migrate_Migrate_All_New_Target old =
    old |> migrate_Migrate_All_New_BackendModel


migrate_AssocList_Dict : (a_old -> a_new) -> (b_old -> b_new) -> AssocList.Dict a_old b_old -> AssocList.Dict a_new b_new
migrate_AssocList_Dict migrate_a migrate_b old =
    old
        |> AssocList.toList
        |> List.map (Tuple.mapBoth migrate_a migrate_b)
        |> AssocList.fromList


migrate_External_AliasThatGetsMoved : Migrate_All.Old.AliasThatGetsMoved -> Evergreen.V2.External.AliasThatGetsMoved
migrate_External_AliasThatGetsMoved old =
    old


migrate_External_AllCoreTypes : Evergreen.V1.External.AllCoreTypes -> Evergreen.V2.External.AllCoreTypes
migrate_External_AllCoreTypes old =
    old


migrate_External_ExternalUnion : Evergreen.V1.External.ExternalUnion -> Evergreen.V2.External.ExternalUnion
migrate_External_ExternalUnion old =
    case old of
        Evergreen.V1.External.External1 ->
            Evergreen.V2.External.External1

        Evergreen.V1.External.External2 ->
            Evergreen.V2.External.External2


migrate_External_Paramed : (a_old -> a_new) -> Evergreen.V1.External.Paramed a_old -> Evergreen.V2.External.Paramed a_new
migrate_External_Paramed migrate_a old =
    { subtype = old.subtype |> migrate_a
    , string = old.string
    }


migrate_External_Paramed2 : (a_old -> a_new) -> (b_old -> b_new) -> Evergreen.V1.External.Paramed2 a_old b_old -> Evergreen.V2.External.Paramed2 a_new b_new
migrate_External_Paramed2 migrate_a migrate_b old =
    { subtype = old.subtype |> migrate_a
    , subtype2 = old.subtype2 |> migrate_b
    , string = old.string
    }


migrate_External_ParamedSub : (x_old -> x_new) -> Evergreen.V1.External.ParamedSub x_old -> Evergreen.V2.External.ParamedSub x_new
migrate_External_ParamedSub migrate_x old =
    { subtypeParamed = old.subtypeParamed |> migrate_External_Paramed migrate_x
    , string = old.string
    }


migrate_External_UnionThatGetsMoved : Migrate_All.Old.UnionThatGetsMoved -> Evergreen.V2.External.UnionThatGetsMoved
migrate_External_UnionThatGetsMoved old =
    case old of
        Migrate_All.Old.UnionThatGetsMoved ->
            Evergreen.V2.External.UnionThatGetsMoved


migrate_IncludedByParam_Custom : Evergreen.V1.IncludedByParam.Custom -> Evergreen.V2.IncludedByParam.Custom
migrate_IncludedByParam_Custom old =
    case old of
        Evergreen.V1.IncludedByParam.Custom ->
            Evergreen.V2.IncludedByParam.Custom


migrate_IncludedBySpecialCasedParam_Custom : Evergreen.V1.IncludedBySpecialCasedParam.Custom -> Evergreen.V2.IncludedBySpecialCasedParam.Custom
migrate_IncludedBySpecialCasedParam_Custom old =
    case old of
        Evergreen.V1.IncludedBySpecialCasedParam.Custom ->
            Evergreen.V2.IncludedBySpecialCasedParam.Custom


migrate_Migrate_All_New_BackendModel : Migrate_All.Old.BackendModel -> Migrate_All.New.BackendModel
migrate_Migrate_All_New_BackendModel old =
    { unchangedCore = old.unchangedCore
    , unchangedUser = old.unchangedUser |> migrate_Migrate_All_New_UserType
    , unchangedAllCoreTypes = old.unchangedAllCoreTypes |> migrate_External_AllCoreTypes
    , unchangedResult = old.unchangedResult
    , unchangedDict = old.unchangedDict
    , unchangedAnonymousRecord =
        old.unchangedAnonymousRecord
            |> (\rec ->
                    { name = rec.name
                    , age = rec.age
                    , userType = rec.userType |> migrate_Migrate_All_New_UserType
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
                                    , userType = rec1.userType |> migrate_Migrate_All_New_UserType
                                    }
                               )
                    }
               )
    , unchangedStringAlias = old.unchangedStringAlias
    , withCustomMaybe = old.withCustomMaybe |> Maybe.map migrate_Migrate_All_New_UserType
    , withCustomList = old.withCustomList |> List.map migrate_Migrate_All_New_UserType
    , withCustomSet = old.withCustomSet |> Set.map (Unimplemented {- Type changed from `Int` to `String`. I need you to write this migration. -})
    , withCustomArray = old.withCustomArray |> Array.map migrate_Migrate_All_New_UserType
    , withCustomDict = old.withCustomDict |> Dict.map (\k v -> v |> migrate_Migrate_All_New_UserType)
    , withCustomResult = old.withCustomResult |> Result.mapError migrate_Migrate_All_New_UserType |> Result.map migrate_Migrate_All_New_UserType
    , externalUnion = old.externalUnion |> migrate_External_ExternalUnion
    , added = (Unimplemented {- Type `Int` was added in V2. I need you to set a default value. -})
    , unionThatGetsMoved = old.unionThatGetsMoved |> migrate_External_UnionThatGetsMoved
    , aliasThatGetsMoved = old.aliasThatGetsMoved |> migrate_External_AliasThatGetsMoved
    , typeToAlias = old.typeToAlias |> (Unimplemented {- Type changed from `Migrate_All.Old.TypeToAlias` to `Migrate_All.New.TypeToAlias`. I need you to write this migration. -})
    , aliasToType = old.aliasToType |> (Unimplemented {- Type changed from `Migrate_All.Old.AliasToType` to `Migrate_All.New.AliasToType`. I need you to write this migration. -})
    , time = old.time
    , url = old.url
    , userCache = old.userCache |> migrate_AssocList_Dict identity migrate_IncludedBySpecialCasedParam_Custom
    , apps = (Unimplemented {- Type `Dict (String) (Migrate_All.New.App)` was added in V2. I need you to set a default value. -})
    , depthTests = (Unimplemented {- Field of type `Dict (String) (Migrate_All.Old.Depth)` was removed in V2. I need you to do something with the `old.depthTests` value if you wish to keep the data, then remove this line. -})
    , removed = (Unimplemented {- Field of type `String` was removed in V2. I need you to do something with the `old.removed` value if you wish to keep the data, then remove this line. -})
    , removedRecord = (Unimplemented {- Field of type `Evergreen.V1.External.AllCoreTypes` was removed in V2. I need you to do something with the `old.removedRecord` value if you wish to keep the data, then remove this line. -})
    }


migrate_Migrate_All_New_CustomType : Migrate_All.Old.CustomType -> Migrate_All.New.CustomType
migrate_Migrate_All_New_CustomType old =
    case old of
        Migrate_All.Old.CustomOne ->
            Migrate_All.New.CustomOne

        Migrate_All.Old.CustomTwo ->
            Migrate_All.New.CustomTwo


migrate_Migrate_All_New_UserType : Migrate_All.Old.UserType -> Migrate_All.New.UserType
migrate_Migrate_All_New_UserType old =
    case old of
        Migrate_All.Old.UserFirst ->
            Migrate_All.New.UserFirst

        Migrate_All.Old.UserSecond ->
            Migrate_All.New.UserSecond

        Migrate_All.Old.UserRemoved ->
            (Unimplemented
             {- `UserRemoved` was removed or renamed in V2 so I couldn't figure out how to migrate it.
                I need you to decide what happens to this Migrate_All.Old.UserRemoved value in a migration.
                See https://lamdera.com/tips/modified-custom-type for more info.
             -}
            )

        Migrate_All.Old.UserWithParam p0 ->
            Migrate_All.New.UserWithParam p0

        Migrate_All.Old.UserWithParams p0 p1 p2 ->
            Migrate_All.New.UserWithParams p0 p1 p2

        Migrate_All.Old.UserWithParamCustom p0 ->
            Migrate_All.New.UserWithParamCustom (p0 |> migrate_Migrate_All_New_CustomType)

        Migrate_All.Old.UserResultP1 p0 ->
            Migrate_All.New.UserResultP1 (p0 |> Result.mapError migrate_Migrate_All_New_CustomType)

        Migrate_All.Old.UserResultP2 p0 ->
            Migrate_All.New.UserResultP2 (p0 |> Result.map migrate_Migrate_All_New_CustomType)

        Migrate_All.Old.UserResultPBoth p0 ->
            Migrate_All.New.UserResultPBoth (p0 |> Result.mapError migrate_Migrate_All_New_CustomType |> Result.map migrate_External_ExternalUnion)

        Migrate_All.Old.UserAnonymous p0 ->
            Migrate_All.New.UserAnonymous
                { name = p0.name
                , userType = p0.userType |> migrate_Migrate_All_New_UserType
                }

        Migrate_All.Old.UserAnonymousNested p0 ->
            Migrate_All.New.UserAnonymousNested
                { name = p0.name
                , subrecord =
                    p0.subrecord
                        |> (\rec1 ->
                                { userType = rec1.userType |> migrate_Migrate_All_New_UserType
                                }
                           )
                }

        Migrate_All.Old.UserAnonymousNestedAdded p0 ->
            Migrate_All.New.UserAnonymousNestedAdded
                { name = p0.name
                , subrecord =
                    p0.subrecord
                        |> (\rec1 ->
                                { userType = rec1.userType |> migrate_Migrate_All_New_UserType
                                , added = (Unimplemented {- Type `Int` was added in V2. I need you to set a default value. -})
                                }
                           )
                }

        Migrate_All.Old.UserAnonymousNestedRemoved p0 ->
            Migrate_All.New.UserAnonymousNestedRemoved
                { name = p0.name
                , subrecord =
                    p0.subrecord
                        |> (\rec1 ->
                                { userType = rec1.userType |> migrate_Migrate_All_New_UserType
                                , removed = (Unimplemented {- Field of type `String` was removed in V2. I need you to do something with the `rec1.removed` value if you wish to keep the data, then remove this line. -})
                                }
                           )
                }

        Migrate_All.Old.UserAnonymousNestedAddedRemoved p0 ->
            Migrate_All.New.UserAnonymousNestedAddedRemoved
                { name = p0.name
                , subrecord =
                    p0.subrecord
                        |> (\rec1 ->
                                { userType = rec1.userType |> migrate_Migrate_All_New_UserType
                                , added = (Unimplemented {- Type `Int` was added in V2. I need you to set a default value. -})
                                , removed = (Unimplemented {- Field of type `String` was removed in V2. I need you to do something with the `rec1.removed` value if you wish to keep the data, then remove this line. -})
                                }
                           )
                }

        Migrate_All.Old.UserTuple p0 ->
            Migrate_All.New.UserTuple (p0 |> Tuple.mapSecond migrate_Migrate_All_New_UserType)

        Migrate_All.Old.UserTriple p0 ->
            Migrate_All.New.UserTriple (p0 |> (\( t1, t2, t3 ) -> ( t1, t2, t3 |> migrate_Migrate_All_New_UserType )))

        Migrate_All.Old.UserTvarAlias p0 ->
            Migrate_All.New.UserTvarAlias (p0 |> migrate_External_Paramed migrate_Migrate_All_New_CustomType)

        Migrate_All.Old.UserTvarAlias2 p0 ->
            Migrate_All.New.UserTvarAlias2 (p0 |> migrate_External_Paramed2 migrate_Migrate_All_New_CustomType migrate_External_AllCoreTypes)

        Migrate_All.Old.UserTvarAliasSub p0 ->
            Migrate_All.New.UserTvarAliasSub (p0 |> migrate_External_ParamedSub migrate_IncludedByParam_Custom)

        Migrate_All.Old.UserExtTime p0 ->
            Migrate_All.New.UserExtTime p0

        Migrate_All.Old.UserExtResultTime p0 ->
            Migrate_All.New.UserExtResultTime p0

        notices ->
            {- @NOTICE `UserAdded` was added in V2.
               This is just a reminder in case migrating some subset of the old data to this new value was important.
               See https://lamdera.com/tips/modified-custom-type for more info.
            -}
            {- @NOTICE `UserAddedParam Int` was added in V2.
               This is just a reminder in case migrating some subset of the old data to this new value was important.
               See https://lamdera.com/tips/modified-custom-type for more info.
            -}
            (Unimplemented {- New constructors were added. I need you to resolve the above notices and then remove this case. -})
