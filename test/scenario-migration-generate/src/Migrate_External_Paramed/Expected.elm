module Migrate_External_Paramed.Expected exposing (..)

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

import Evergreen.V1.External
import Evergreen.V2.External
import Lamdera.Migrations exposing (..)
import Migrate_External_Paramed.New
import Migrate_External_Paramed.Old


target =
    migrate_Migrate_External_Paramed_New_Target


migrate_External_Paramed : (a_old -> a_new) -> Evergreen.V1.External.Paramed a_old -> Evergreen.V2.External.Paramed a_new
migrate_External_Paramed migrate_a old =
    { subtype = old.subtype |> migrate_a
    , string = old.string
    }


migrate_Migrate_External_Paramed_New_AnalyticsModel : Migrate_External_Paramed.Old.AnalyticsModel -> Migrate_External_Paramed.New.AnalyticsModel
migrate_Migrate_External_Paramed_New_AnalyticsModel old =
    old


migrate_Migrate_External_Paramed_New_CustomType : Migrate_External_Paramed.Old.CustomType -> Migrate_External_Paramed.New.CustomType
migrate_Migrate_External_Paramed_New_CustomType old =
    case old of
        Migrate_External_Paramed.Old.CustomOne ->
            Migrate_External_Paramed.New.CustomOne

        Migrate_External_Paramed.Old.CustomTwo ->
            Migrate_External_Paramed.New.CustomTwo


migrate_Migrate_External_Paramed_New_Target : Migrate_External_Paramed.Old.Target -> Migrate_External_Paramed.New.Target
migrate_Migrate_External_Paramed_New_Target old =
    case old of
        Migrate_External_Paramed.Old.UserTvarAlias p0 ->
            Migrate_External_Paramed.New.UserTvarAlias (p0 |> migrate_External_Paramed migrate_Migrate_External_Paramed_New_CustomType)

        Migrate_External_Paramed.Old.UserMixPackage p0 ->
            Migrate_External_Paramed.New.UserMixPackage (p0 |> migrate_Migrate_External_Paramed_New_AnalyticsModel)
