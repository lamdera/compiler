{- MANUALLY FORMATTED -}
module Compiler.Canonicalize.Environment exposing
  ( Env(..)
  , Exposed
  , Qualified
  , Info(..)
  , mergeInfo
  , Var(..)
  , Type(..)
  , Ctor(..)
  , addLocals
  , findType
  , findTypeQual
  , findCtor
  , findCtorQual
  , findBinop
  , Binop(..)
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Binop as Binop
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as MResult
import Extra.Type.List exposing (TList)
import Extra.Type.Map as Map



-- RESULT


type alias TResult i w a =
  MResult.TResult i w Error.Error a



-- ENVIRONMENT


type Env =
  Env
    {- home -} ModuleName.Canonical
    {- vars -} (Map.Map Name.Name Var)
    {- types -} (Exposed Type)
    {- ctors -} (Exposed Ctor)
    {- binops -} (Exposed Binop)
    {- q_vars -} (Qualified Can.Annotation)
    {- q_types -} (Qualified Type)
    {- q_ctors -} (Qualified Ctor)


type alias Exposed a =
  Map.Map Name.Name (Info a)


type alias Qualified a =
  Map.Map Name.Name (Map.Map Name.Name (Info a))



-- INFO


type Info a
  = Specific ModuleName.Canonical a
  | Ambiguous ModuleName.Canonical (OneOrMore.OneOrMore ModuleName.Canonical)


mergeInfo : Info a -> Info a -> Info a
mergeInfo info1 info2 =
  case info1 of
    Specific h1 _ ->
      case info2 of
        Specific h2 _    -> if h1 == h2 then info1 else Ambiguous h1 (OneOrMore.one h2)
        Ambiguous h2 hs2 -> Ambiguous h1 (OneOrMore.more (OneOrMore.one h2) hs2)

    Ambiguous h1 hs1 ->
      case info2 of
        Specific h2 _    -> Ambiguous h1 (OneOrMore.more hs1 (OneOrMore.one h2))
        Ambiguous h2 hs2 -> Ambiguous h1 (OneOrMore.more hs1 (OneOrMore.more (OneOrMore.one h2) hs2))



-- VARIABLES


type Var
  = Local A.Region
  | TopLevel A.Region
  | Foreign ModuleName.Canonical Can.Annotation
  | Foreigns ModuleName.Canonical (OneOrMore.OneOrMore ModuleName.Canonical)



-- TYPES


type Type
  = Alias Int ModuleName.Canonical (TList Name.Name) Can.Type
  | Union Int ModuleName.Canonical



-- CTORS


type Ctor
  = RecordCtor ModuleName.Canonical (TList Name.Name) Can.Type
  | Ctor
      {- c_home -} ModuleName.Canonical
      {- c_type -} Name.Name
      {- c_union -} Can.Union
      {- c_index -} Index.ZeroBased
      {- c_args -} (TList Can.Type)



-- BINOPS


type Binop =
  Binop
    {- op -} Name.Name
    {- op_home -} ModuleName.Canonical
    {- op_name -} Name.Name
    {- op_annotation -} Can.Annotation
    {- op_associativity -} Binop.Associativity
    {- op_precedence -} Binop.Precedence



-- VARIABLE -- ADD LOCALS


addLocals : Map.Map Name.Name A.Region -> Env -> TResult i w Env
addLocals names (Env home vars ts cs bs qvs qts qcs) =
  MResult.bind
    (MResult.mergeA
      (MResult.mapMissing addLocalLeft)
      (MResult.mapMissing (\_ homes -> homes))
      (MResult.zipWithAMatched addLocalBoth)
      names
      vars) <| \newVars ->

  MResult.ok (Env home newVars ts cs bs qvs qts qcs)


addLocalLeft : Name.Name -> A.Region -> Var
addLocalLeft _ region =
  Local region


addLocalBoth : Name.Name -> A.Region -> Var -> TResult i w Var
addLocalBoth name region var =
  case var of
    Foreign _ _ ->
      MResult.ok (Local region)

    Foreigns _ _ ->
      MResult.ok (Local region)

    Local parentRegion ->
      MResult.throw (Error.Shadowing name parentRegion region)

    TopLevel parentRegion ->
      MResult.throw (Error.Shadowing name parentRegion region)




-- FIND TYPE


findType : A.Region -> Env -> Name.Name -> TResult i w Type
findType region (Env _ _ ts _ _ _ qts _) name =
  case Map.lookup name ts of
    Just (Specific _ tipe) ->
      MResult.ok tipe

    Just (Ambiguous h hs) ->
      MResult.throw (Error.AmbiguousType region Nothing name h hs)

    Nothing ->
      MResult.throw (Error.NotFoundType region Nothing name (toPossibleNames ts qts))


findTypeQual : A.Region -> Env -> Name.Name -> Name.Name -> TResult i w Type
findTypeQual region (Env _ _ ts _ _ _ qts _) prefix name =
  case Map.lookup prefix qts of
    Just qualified ->
      case Map.lookup name qualified of
        Just (Specific _ tipe) ->
          MResult.ok tipe

        Just (Ambiguous h hs) ->
          MResult.throw (Error.AmbiguousType region (Just prefix) name h hs)

        Nothing ->
          MResult.throw (Error.NotFoundType region (Just prefix) name (toPossibleNames ts qts))

    Nothing ->
      MResult.throw (Error.NotFoundType region (Just prefix) name (toPossibleNames ts qts))



-- FIND CTOR


findCtor : A.Region -> Env -> Name.Name -> TResult i w Ctor
findCtor region (Env _ _ _ cs _ _ _ qcs) name =
  case Map.lookup name cs of
    Just (Specific _ ctor) ->
      MResult.ok ctor

    Just (Ambiguous h hs) ->
      MResult.throw (Error.AmbiguousVariant region Nothing name h hs)

    Nothing ->
      MResult.throw (Error.NotFoundVariant region Nothing name (toPossibleNames cs qcs))


findCtorQual : A.Region -> Env -> Name.Name -> Name.Name -> TResult i w Ctor
findCtorQual region (Env _ _ _ cs _ _ _ qcs) prefix name =
  case Map.lookup prefix qcs of
    Just qualified ->
      case Map.lookup name qualified of
        Just (Specific _ pattern) ->
          MResult.ok pattern

        Just (Ambiguous h hs) ->
          MResult.throw (Error.AmbiguousVariant region (Just prefix) name h hs)

        Nothing ->
          MResult.throw (Error.NotFoundVariant region (Just prefix) name (toPossibleNames cs qcs))

    Nothing ->
      MResult.throw (Error.NotFoundVariant region (Just prefix) name (toPossibleNames cs qcs))



-- FIND BINOP


findBinop : A.Region -> Env -> Name.Name -> TResult i w Binop
findBinop region (Env _ _ _ _ binops _ _ _) name =
  case Map.lookup name binops of
    Just (Specific _ binop) ->
      MResult.ok binop

    Just (Ambiguous h hs) ->
      MResult.throw (Error.AmbiguousBinop region name h hs)

    Nothing ->
      MResult.throw (Error.NotFoundBinop region name (Map.keysSet binops))



-- TO POSSIBLE NAMES


toPossibleNames : Exposed a -> Qualified a -> Error.PossibleNames
toPossibleNames exposed qualified =
  Error.PossibleNames (Map.keysSet exposed) (Map.map Map.keysSet qualified)
