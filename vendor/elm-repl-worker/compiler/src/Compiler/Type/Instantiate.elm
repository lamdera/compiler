{- MANUALLY FORMATTED -}
module Compiler.Type.Instantiate exposing
  ( {- FreeVars
  ,-} fromSrcType
  )


import Compiler.AST.Canonical as Can
import Compiler.Data.Name as Name
import Compiler.Type.Type as Type
import Extra.System.IO.Pure as IO
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe
import Extra.Type.Tuple as MTuple



-- IO


type alias IO t a = Type.IO t a



-- FROM SOURCE TYPE


fromSrcType : Map.Map Name.Name Type.Type -> Can.Type -> IO t Type.Type
fromSrcType freeVars sourceType =
  case sourceType of
    Can.TLambda arg result ->
      IO.pure Type.FunN
        |> IO.andMap (fromSrcType freeVars arg)
        |> IO.andMap (fromSrcType freeVars result)

    Can.TVar name ->
      IO.return (Map.ex freeVars name)

    Can.TType home name args ->
      IO.fmap (Type.AppN home name) <| IO.traverseList (fromSrcType freeVars) args

    Can.TAlias home name args aliasedType ->
      IO.bind (IO.traverseList (MTuple.traverseSecond IO.fmap (fromSrcType freeVars)) args) <| \targs ->
      IO.fmap (Type.AliasN home name targs) <|
        case aliasedType of
          Can.Filled realType ->
            fromSrcType freeVars realType

          Can.Holey realType ->
            fromSrcType (Map.fromList targs) realType

    Can.TTuple a b maybeC ->
      IO.pure Type.TupleN
        |> IO.andMap (fromSrcType freeVars a)
        |> IO.andMap (fromSrcType freeVars b)
        |> IO.andMap (MMaybe.traverse IO.pure IO.fmap (fromSrcType freeVars) maybeC)

    Can.TUnit ->
      IO.return Type.UnitN

    Can.TRecord fields maybeExt ->
      IO.pure Type.RecordN
        |> IO.andMap (IO.traverseMap (fromSrcFieldType freeVars) fields)
        |> IO.andMap
          (case maybeExt of
            Nothing ->
              IO.return Type.EmptyRecordN

            Just ext ->
              IO.return (Map.ex freeVars ext))


fromSrcFieldType : Map.Map Name.Name Type.Type -> Can.FieldType -> IO t Type.Type
fromSrcFieldType freeVars (Can.FieldType _ tipe) =
  fromSrcType freeVars tipe
