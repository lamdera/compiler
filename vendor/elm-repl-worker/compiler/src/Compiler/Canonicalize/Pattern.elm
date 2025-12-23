{- MANUALLY FORMATTED -}
module Compiler.Canonicalize.Pattern exposing
  ( verify
  , Bindings
  , DupsDict
  , canonicalize
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as MResult
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- RESULTS


type alias TResult i w a =
  MResult.TResult i w Error.Error a


type alias Bindings =
  Map.Map Name.Name A.Region



-- VERIFY


verify : Error.DuplicatePatternContext -> TResult DupsDict w a -> TResult i w (a, Bindings)
verify context (MResult.CResult k) =
  MResult.CResult <| \info warnings ->
    case k Dups.none warnings of
      MResult.Rbad _ warnings1 errors ->
          MResult.Rbad info warnings1 errors
      MResult.Rgood bindings warnings1 value ->
          case Dups.detect (Error.DuplicatePattern context) bindings of
            MResult.CResult k1 ->
              case k1 () () of
                MResult.Rbad () () errs -> MResult.Rbad info warnings1 errs
                MResult.Rgood () () dict -> MResult.Rgood info warnings1 (value, dict)



-- CANONICALIZE


type alias DupsDict =
  Dups.TDict A.Region


canonicalize : Env.Env -> Src.Pattern -> TResult DupsDict w Can.Pattern
canonicalize env (A.At region pattern) =
  MResult.fmap (A.At region) <|
  case pattern of
    Src.PAnything ->
      MResult.ok Can.PAnything

    Src.PVar name ->
      logVar name region (Can.PVar name)

    Src.PRecord fields ->
      logFields fields (Can.PRecord (MList.map A.toValue fields))

    Src.PUnit ->
      MResult.ok Can.PUnit

    Src.PTuple a b cs ->
      MResult.pure Can.PTuple
        |> MResult.andMap (canonicalize env a)
        |> MResult.andMap (canonicalize env b)
        |> MResult.andMap (canonicalizeTuple region env cs)

    Src.PCtor nameRegion name patterns ->
      MResult.andThen (canonicalizeCtor env region name patterns) (Env.findCtor nameRegion env name)

    Src.PCtorQual nameRegion home name patterns ->
      MResult.andThen (canonicalizeCtor env region name patterns) (Env.findCtorQual nameRegion env home name)

    Src.PList patterns ->
      MResult.fmap Can.PList (canonicalizeList env patterns)

    Src.PCons first rest ->
      MResult.pure Can.PCons
        |> MResult.andMap (canonicalize env first)
        |> MResult.andMap (canonicalize env rest)

    Src.PAlias ptrn (A.At reg name) ->
      MResult.bind (canonicalize env ptrn) <| \cpattern ->
      logVar name reg (Can.PAlias cpattern name)

    Src.PChr chr ->
      MResult.ok (Can.PChr chr)

    Src.PStr str ->
      MResult.ok (Can.PStr str)

    Src.PInt int ->
      MResult.ok (Can.PInt int)


canonicalizeCtor : Env.Env -> A.Region -> Name.Name -> TList Src.Pattern -> Env.Ctor -> TResult DupsDict w Can.Pattern_
canonicalizeCtor env region name patterns ctor =
  case ctor of
    Env.Ctor home tipe union index args ->
      let
        toCanonicalArg argIndex argPattern argTipe =
          MResult.fmap (Can.PatternCtorArg argIndex argTipe) (canonicalize env argPattern)
      in
      MResult.bind (Index.indexedZipWithA MResult.pure MResult.pure MResult.fmap MResult.liftA2 toCanonicalArg patterns args) <| \verifiedList ->
      case verifiedList of
        Index.LengthMatch cargs ->
          if tipe == Name.bool && home == ModuleName.basics then
            MResult.ok (Can.PBool union (name == Name.true))
          else
            MResult.ok (Can.PCtor home tipe union name index cargs)

        Index.LengthMismatch actualLength expectedLength ->
          MResult.throw (Error.BadArity region Error.PatternArity name expectedLength actualLength)

    Env.RecordCtor _ _ _ ->
      MResult.throw (Error.PatternHasRecordCtor region name)


canonicalizeTuple : A.Region -> Env.Env -> TList Src.Pattern -> TResult DupsDict w (Maybe Can.Pattern)
canonicalizeTuple tupleRegion env extras =
  case extras of
    [] ->
      MResult.ok Nothing

    [three] ->
      MResult.fmap Just (canonicalize env three)

    _ ->
      MResult.throw <| Error.TupleLargerThanThree tupleRegion


canonicalizeList : Env.Env -> TList Src.Pattern -> TResult DupsDict w (TList Can.Pattern)
canonicalizeList env list =
  case list of
    [] ->
      MResult.ok []

    pattern :: otherPatterns ->
      MResult.pure (::)
        |> MResult.andMap (canonicalize env pattern)
        |> MResult.andMap (canonicalizeList env otherPatterns)



-- LOG BINDINGS


logVar : Name.Name -> A.Region -> a -> TResult DupsDict w a
logVar name region value =
  MResult.CResult <| \bindings warnings ->
    MResult.Rgood (Dups.insert name region region bindings) warnings value


logFields : TList (A.Located Name.Name) -> a -> TResult DupsDict w a
logFields fields value =
  let
    addField dict (A.At region name) =
      Dups.insert name region region dict
  in
  MResult.CResult <| \bindings warnings ->
    MResult.Rgood (MList.foldl addField bindings fields) warnings value
