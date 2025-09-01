{- MANUALLY FORMATTED -}
module Compiler.Elm.Compiler.Type.Extract exposing
  ( Types(..)
  , mergeMany
  , merge
  , fromInterface
  , fromDependencyInterface
  , fromMsg
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Utils.Type as Type
import Compiler.Data.Name as Name
import Compiler.Elm.Compiler.Type as T
import Compiler.Elm.Interface as I
import Compiler.Elm.ModuleName as ModuleName
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe
import Extra.Type.Set as Set
import Extra.Type.Tuple as MTuple
import Extra.Class.Functor as Functor
import Extra.Class.Applicative as Applicative
import Extra.Class.Monad as Monad



-- EXTRACTION


extract : Can.Type -> Extractor z T.Type
extract astType =
  case astType of
    Can.TLambda arg result ->
      return T.Lambda
        |> andMap (extract arg)
        |> andMap (extract result)

    Can.TVar x ->
      pure (T.Var x)

    Can.TType home name args ->
      addUnion (Opt.toGlobalComparable <| Opt.Global home name) (T.Type (toPublicName home name))
        |> andMap (MList.traverse pure liftA2 extract args)

    Can.TRecord fields ext ->
      bind (MList.traverse pure liftA2 (MTuple.traverseSecond fmap extract) (Can.fieldsToList fields)) <| \efields ->
      pure (T.Record efields ext)

    Can.TUnit ->
      pure T.Unit

    Can.TTuple a b maybeC ->
      return T.Tuple
        |> andMap (extract a)
        |> andMap (extract b)
        |> andMap (MList.traverse pure liftA2 extract (MMaybe.maybeToList maybeC))

    Can.TAlias home name args aliasType ->
      bind (addAlias (Opt.toGlobalComparable <| Opt.Global home name) ()) <| \_ ->
      bind (extract (Type.dealias args aliasType)) <| \_ ->
      fmap (T.Type (toPublicName home name)) <|
        MList.traverse pure liftA2 (extract << Tuple.second) args


toPublicName : ModuleName.Canonical -> Name.Name -> Name.Name
toPublicName (ModuleName.Canonical _ home) name =
  Name.sepBy 0x2E {- . -} home name



-- TRANSITIVELY AVAILABLE TYPES


type Types =
  Types (Map.Map ModuleName.Comparable Types_)
  -- PERF profile Opt.Global representation
  -- current representation needs less allocation
  -- but maybe the lookup is much worse


type Types_ =
  Types_
    {- union_info -} (Map.Map Name.Name Can.Union)
    {- alias_info -} (Map.Map Name.Name Can.Alias)

getUnionInfo (Types_ union_info _) = union_info
getAliasInfo (Types_ _ alias_info) = alias_info


mergeMany : TList Types -> Types
mergeMany listOfTypes =
  case listOfTypes of
    [] -> Types Map.empty
    t::ts -> MList.foldr merge t ts


merge : Types -> Types -> Types
merge (Types types1) (Types types2) =
  Types (Map.union types1 types2)


fromInterface : ModuleName.Raw -> I.Interface -> Types
fromInterface name (I.Interface pkg _ unions aliases _) =
  Types <| Map.singleton (ModuleName.toComparable <| ModuleName.Canonical pkg name) <|
    Types_ (Map.map I.extractUnion unions) (Map.map I.extractAlias aliases)


fromDependencyInterface : ModuleName.Comparable -> I.DependencyInterface -> Types
fromDependencyInterface home di =
  Types <| Map.singleton home <|
    case di of
      I.Public (I.Interface _ _ unions aliases _) ->
        Types_ (Map.map I.extractUnion unions) (Map.map I.extractAlias aliases)

      I.Private _ unions aliases ->
        Types_ unions aliases



-- EXTRACT MODEL, MSG, AND ANY TRANSITIVE DEPENDENCIES


fromMsg : Types -> Can.Type -> T.DebugMetadata
fromMsg types message =
  let
    (msgDeps, msgType) =
      run (extract message)

    (aliases, unions) =
      extractTransitive types noDeps msgDeps
  in
  T.DebugMetadata msgType aliases unions


extractTransitive : Types -> Deps -> Deps -> ( TList T.Alias, TList T.Union )
extractTransitive types (Deps seenAliases seenUnions) (Deps nextAliases nextUnions) =
  let
    aliases = Set.difference nextAliases seenAliases
    unions = Set.difference nextUnions seenUnions
  in
    if Set.null aliases && Set.null unions then
      ( [], [] )

    else
      let
        (newDeps, result) =
          run
            (return Tuple.pair
              |> andMap (MList.traverse pure liftA2 (extractAlias types) (Set.toList aliases))
              |> andMap (MList.traverse pure liftA2 (extractUnion types) (Set.toList unions)))

        oldDeps =
          Deps (Set.union seenAliases nextAliases) (Set.union seenUnions nextUnions)

        remainingResult =
          extractTransitive types oldDeps newDeps
      in
      MTuple.mappend MList.mappend MList.mappend result remainingResult


extractAlias : Types -> Opt.GlobalComparable -> Extractor z T.Alias
extractAlias (Types dict) comparable =
  let
    (Opt.Global home name) = Opt.fromGlobalComparable comparable
    (Can.Alias args aliasType) = Map.ex (getAliasInfo (Map.ex dict (ModuleName.toComparable home))) name
  in
  fmap (T.Alias (toPublicName home name) args) <| extract aliasType


extractUnion : Types -> Opt.GlobalComparable -> Extractor z T.Union
extractUnion (Types dict) comparable =
  let (Opt.Global home name) = Opt.fromGlobalComparable comparable in
  if name == Name.list && home == ModuleName.list
    then return <| T.Union (toPublicName home name) ["a"] []
    else
      let
        pname = toPublicName home name
        (Can.Union vars ctors _ _) = Map.ex (getUnionInfo (Map.ex dict (ModuleName.toComparable home))) name
      in
      fmap (T.Union pname vars) <| MList.traverse pure liftA2 extractCtor ctors


extractCtor : Can.Ctor -> Extractor z (Name.Name, TList T.Type)
extractCtor (Can.Ctor ctor _ _ args) =
  fmap (Tuple.pair ctor) <| MList.traverse pure liftA2 extract args



-- DEPS


type Deps =
  Deps
    {- aliases -} (Set.Set Opt.GlobalComparable)
    {- unions -} (Set.Set Opt.GlobalComparable)


noDeps : Deps
noDeps =
  Deps Set.empty Set.empty



-- EXTRACTOR


type Extractor z a =
  Extractor (
    Set.Set Opt.GlobalComparable
    -> Set.Set Opt.GlobalComparable
    -> (Set.Set Opt.GlobalComparable -> Set.Set Opt.GlobalComparable -> a -> z)
    -> z
  )


run : Extractor (Deps, a) a -> (Deps, a)
run (Extractor k) =
  k Set.empty Set.empty <| \aliases unions value ->
    ( Deps aliases unions, value )


addAlias : Opt.GlobalComparable -> a -> Extractor z a
addAlias alias value =
  Extractor <| \aliases unions ok ->
    ok (Set.insert alias aliases) unions value


addUnion : Opt.GlobalComparable -> a -> Extractor z a
addUnion union value =
  Extractor <| \aliases unions ok ->
    ok aliases (Set.insert union unions) value


fmap : Functor.Fmap a (Extractor z a) b (Extractor z b)
fmap func (Extractor k) =
  Extractor <| \aliases unions ok ->
    let
      ok1 a1 u1 value =
        ok a1 u1 (func value)
    in
    k aliases unions ok1


pure : Applicative.Pure a (Extractor z a)
pure value =
  Extractor <| \aliases unions ok ->
    ok aliases unions value

andMap : Applicative.AndMap (Extractor z a) (Extractor z (a -> b)) (Extractor z b)
andMap (Extractor kv) (Extractor kf) =
  Extractor <| \aliases unions ok ->
    let
      ok1 a1 u1 func =
        let
          ok2 a2 u2 value =
            ok a2 u2 (func value)
        in
        kv a1 u1 ok2
    in
    kf aliases unions ok1

liftA2 : Applicative.LiftA2 a (Extractor z a) b (Extractor z b) c (Extractor z c)
liftA2 = Applicative.liftA2 fmap andMap


return : Monad.Return a (Extractor z a)
return = pure

bind : Monad.Bind a (Extractor z a) (Extractor z b)
bind (Extractor ka) callback =
  Extractor <| \aliases unions ok ->
    let
      ok1 a1 u1 value =
        case callback value of
          Extractor kb -> kb a1 u1 ok
    in
    ka aliases unions ok1
