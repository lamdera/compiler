{- MANUALLY FORMATTED -}
module Compiler.Canonicalize.Type exposing
  ( toAnnotation
  , canonicalize
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Data.Name as Name
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as MResult
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- RESULT


type alias TResult i w a =
  MResult.TResult i w Error.Error a



-- TO ANNOTATION


toAnnotation : Env.Env -> Src.Type -> TResult i w Can.Annotation
toAnnotation env srcType =
  MResult.bind (canonicalize env srcType) <| \tipe ->
  MResult.ok <| Can.Forall (addFreeVars Map.empty tipe) tipe



-- CANONICALIZE TYPES


canonicalize : Env.Env -> Src.Type -> TResult i w Can.Type
canonicalize env (A.At typeRegion tipe) =
  case tipe of
    Src.TVar x ->
        MResult.ok (Can.TVar x)

    Src.TType region name args ->
        MResult.andThen (canonicalizeType env typeRegion name args)
          (Env.findType region env name)

    Src.TTypeQual region home name args ->
        MResult.andThen (canonicalizeType env typeRegion name args)
          (Env.findTypeQual region env home name)

    Src.TLambda a b ->
        MResult.pure Can.TLambda
          |> MResult.andMap (canonicalize env a)
          |> MResult.andMap (canonicalize env b)

    Src.TRecord fields ext ->
        MResult.bind (MResult.andThen MResult.sequenceAMap (Dups.checkFields (canonicalizeFields env fields))) <| \cfields ->
        MResult.return <| Can.TRecord cfields (Maybe.map A.toValue ext)

    Src.TUnit ->
        MResult.ok Can.TUnit

    Src.TTuple a b cs ->
        MResult.pure Can.TTuple
          |> MResult.andMap (canonicalize env a)
          |> MResult.andMap (canonicalize env b)
          |> MResult.andMap
            (case cs of
              [] ->
                MResult.ok Nothing

              [c] ->
                MResult.fmap Just (canonicalize env c)

              _ ->
                MResult.throw <| Error.TupleLargerThanThree typeRegion)


canonicalizeFields : Env.Env -> TList (A.Located Name.Name, Src.Type) -> TList (A.Located Name.Name, TResult i w Can.FieldType)
canonicalizeFields env fields =
  let
    len = MList.length fields
    canonicalizeField index (name, srcType) =
      (name, MResult.fmap (Can.FieldType index) (canonicalize env srcType))
  in
  MList.zipWith canonicalizeField (MList.range 0 len) fields



-- CANONICALIZE TYPE


canonicalizeType : Env.Env -> A.Region -> Name.Name -> TList Src.Type -> Env.Type -> TResult i w Can.Type
canonicalizeType env region name args info =
  MResult.bind (MResult.traverseList (canonicalize env) args) <| \cargs ->
  case info of
    Env.Alias arity home argNames aliasedType ->
      checkArity arity region name args <|
        Can.TAlias home name (MList.zip argNames cargs) (Can.Holey aliasedType)

    Env.Union arity home ->
      checkArity arity region name args <|
        Can.TType home name cargs


checkArity : Int -> A.Region -> Name.Name -> TList (A.Located arg) -> answer -> TResult i w answer
checkArity expected region name args answer =
  let actual = MList.length args in
  if expected == actual then
    MResult.ok answer
  else
    MResult.throw (Error.BadArity region Error.TypeArity name expected actual)



-- ADD FREE VARS


addFreeVars : Map.Map Name.Name () -> Can.Type -> Map.Map Name.Name ()
addFreeVars freeVars tipe =
  case tipe of
    Can.TLambda arg result ->
      addFreeVars (addFreeVars freeVars result) arg

    Can.TVar var ->
      Map.insert var () freeVars

    Can.TType _ _ args ->
      MList.foldl addFreeVars freeVars args

    Can.TRecord fields Nothing ->
      Map.foldl addFieldFreeVars freeVars fields

    Can.TRecord fields (Just ext) ->
      Map.foldl addFieldFreeVars (Map.insert ext () freeVars) fields

    Can.TUnit ->
      freeVars

    Can.TTuple a b maybeC ->
      case maybeC of
        Nothing ->
          addFreeVars (addFreeVars freeVars a) b

        Just c ->
          addFreeVars (addFreeVars (addFreeVars freeVars a) b) c

    Can.TAlias _ _ args _ ->
      MList.foldl (\fvs (_,arg) -> addFreeVars fvs arg) freeVars args


addFieldFreeVars : Map.Map Name.Name () -> Can.FieldType -> Map.Map Name.Name ()
addFieldFreeVars freeVars (Can.FieldType _ tipe) =
  addFreeVars freeVars tipe
