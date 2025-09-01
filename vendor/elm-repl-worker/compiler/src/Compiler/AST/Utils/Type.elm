module Compiler.AST.Utils.Type exposing
    ( dealias
    , deepDealias
    , delambda
    , iteratedDealias
    )

import Compiler.AST.Canonical as Can
import Compiler.Data.Name as Name
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- DELAMBDA


delambda : Can.Type -> TList Can.Type
delambda tipe =
    case tipe of
        Can.TLambda arg result ->
            arg :: delambda result

        _ ->
            [ tipe ]



-- DEALIAS


dealias : TList ( Name.Name, Can.Type ) -> Can.AliasType -> Can.Type
dealias args aliasType =
    case aliasType of
        Can.Holey tipe ->
            dealiasHelp (Map.fromList args) tipe

        Can.Filled tipe ->
            tipe


dealiasHelp : Map.Map Name.Name Can.Type -> Can.Type -> Can.Type
dealiasHelp typeTable tipe =
    case tipe of
        Can.TLambda a b ->
            Can.TLambda
                (dealiasHelp typeTable a)
                (dealiasHelp typeTable b)

        Can.TVar x ->
            Map.findWithDefault tipe x typeTable

        Can.TRecord fields ext ->
            Can.TRecord (Map.map (dealiasField typeTable) fields) ext

        Can.TAlias home name args t ->
            Can.TAlias home name (MList.map (Tuple.mapSecond (dealiasHelp typeTable)) args) t

        Can.TType home name args ->
            Can.TType home name (MList.map (dealiasHelp typeTable) args)

        Can.TUnit ->
            Can.TUnit

        Can.TTuple a b maybeC ->
            Can.TTuple
                (dealiasHelp typeTable a)
                (dealiasHelp typeTable b)
                (Maybe.map (dealiasHelp typeTable) maybeC)


dealiasField : Map.Map Name.Name Can.Type -> Can.FieldType -> Can.FieldType
dealiasField typeTable (Can.FieldType index tipe) =
    Can.FieldType index (dealiasHelp typeTable tipe)



-- DEEP DEALIAS


deepDealias : Can.Type -> Can.Type
deepDealias tipe =
    case tipe of
        Can.TLambda a b ->
            Can.TLambda (deepDealias a) (deepDealias b)

        Can.TVar _ ->
            tipe

        Can.TRecord fields ext ->
            Can.TRecord (Map.map deepDealiasField fields) ext

        Can.TAlias _ _ args tipe_ ->
            deepDealias (dealias args tipe_)

        Can.TType home name args ->
            Can.TType home name (MList.map deepDealias args)

        Can.TUnit ->
            Can.TUnit

        Can.TTuple a b c ->
            Can.TTuple (deepDealias a) (deepDealias b) (Maybe.map deepDealias c)


deepDealiasField : Can.FieldType -> Can.FieldType
deepDealiasField (Can.FieldType index tipe) =
    Can.FieldType index (deepDealias tipe)



-- ITERATED DEALIAS


iteratedDealias : Can.Type -> Can.Type
iteratedDealias tipe =
    case tipe of
        Can.TAlias _ _ args realType ->
            iteratedDealias (dealias args realType)

        _ ->
            tipe
