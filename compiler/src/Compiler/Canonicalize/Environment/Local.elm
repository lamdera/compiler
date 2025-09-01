{- MANUALLY FORMATTED -}
module Compiler.Canonicalize.Environment.Local exposing
  ( add
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Canonicalize.Type as Type
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as MResult
import Extra.Data.Graph as Graph
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- RESULT


type alias TResult i w a =
  MResult.TResult i w Error.Error a


type alias Unions = Map.Map Name.Name Can.Union
type alias Aliases = Map.Map Name.Name Can.Alias


add : Src.Module -> Env.Env -> TResult i w (Env.Env, Unions, Aliases)
add module_ env =
  MResult.andThen (addCtors module_) (MResult.andThen (addVars module_) (addTypes module_ env))



-- ADD VARS


addVars : Src.Module -> Env.Env -> TResult i w Env.Env
addVars module_ (Env.Env home vs ts cs bs qvs qts qcs) =
  MResult.bind (collectVars module_) <| \topLevelVars ->
  let vs2 = Map.union topLevelVars vs in
  -- Use union to overwrite foreign stuff.
  MResult.ok <| Env.Env home vs2 ts cs bs qvs qts qcs


collectVars : Src.Module -> TResult i w (Map.Map Name.Name Env.Var)
collectVars (Src.Module _ _ _ values _ _ _ effects) =
  let
    addDecl dict (A.At _ (Src.Value (A.At region name) _ _ _)) =
      Dups.insert name region (Env.TopLevel region) dict
  in
  Dups.detect Error.DuplicateDecl <|
    MList.foldl addDecl (toEffectDups effects) values


toEffectDups : Src.Effects -> Dups.TDict Env.Var
toEffectDups effects =
  case effects of
    Src.NoEffects ->
      Dups.none

    Src.Ports ports ->
      let
        addPort dict (Src.Port (A.At region name) _) =
          Dups.insert name region (Env.TopLevel region) dict
      in
      MList.foldl addPort Dups.none ports

    Src.Manager _ manager ->
      case manager of
        Src.CCmd (A.At region _) ->
          Dups.one "command" region (Env.TopLevel region)

        Src.CSub (A.At region _) ->
          Dups.one "subscription" region (Env.TopLevel region)

        Src.Fx (A.At regionCmd _) (A.At regionSub _) ->
          Dups.union
            (Dups.one "command" regionCmd (Env.TopLevel regionCmd))
            (Dups.one "subscription" regionSub (Env.TopLevel regionSub))



-- ADD TYPES


addTypes : Src.Module -> Env.Env -> TResult i w Env.Env
addTypes (Src.Module _ _ _ _ unions aliases _ _) (Env.Env home vs ts cs bs qvs qts qcs) =
  let
    addAliasDups dups (A.At _ (Src.Alias (A.At region name) _ _)) = Dups.insert name region () dups
    addUnionDups dups (A.At _ (Src.Union (A.At region name) _ _)) = Dups.insert name region () dups
    typeNameDups =
      MList.foldl addUnionDups (MList.foldl addAliasDups Dups.none aliases) unions
  in
  MResult.bind (Dups.detect Error.DuplicateType typeNameDups) <| \_ ->
  MResult.bind (MResult.foldM (addUnion home) ts unions) <| \ts1 ->
  addAliases aliases (Env.Env home vs ts1 cs bs qvs qts qcs)


addUnion : ModuleName.Canonical -> Env.Exposed Env.Type -> A.Located Src.Union -> TResult i w (Env.Exposed Env.Type)
addUnion home types ((A.At _ (Src.Union (A.At _ name) _ _)) as union) =
  MResult.bind (checkUnionFreeVars union) <| \arity ->
  let one = Env.Specific home (Env.Union arity home) in
  MResult.ok <| Map.insert name one types



-- ADD TYPE ALIASES


addAliases : TList (A.Located Src.Alias) -> Env.Env -> TResult i w Env.Env
addAliases aliases env =
  let
    nodes = MList.map toNode aliases
    sccs = Graph.stronglyConnComp nodes
  in
  MResult.foldM addAlias env sccs


addAlias : Env.Env -> Graph.SCC (A.Located Src.Alias) -> TResult i w Env.Env
addAlias ((Env.Env home vs ts cs bs qvs qts qcs) as env) scc =
  case scc of
    Graph.AcyclicSCC ((A.At _ (Src.Alias (A.At _ name) _ tipe)) as alias) ->
      MResult.bind (checkAliasFreeVars alias) <| \args ->
      MResult.bind (Type.canonicalize env tipe) <| \ctype ->
      let one = Env.Specific home (Env.Alias (MList.length args) home args ctype) in
      let ts1 = Map.insert name one ts in
      MResult.ok <| Env.Env home vs ts1 cs bs qvs qts qcs

    Graph.CyclicSCC [] ->
      MResult.ok env

    Graph.CyclicSCC (((A.At _ (Src.Alias (A.At region name1) _ tipe)) as alias) :: others) ->
      MResult.bind (checkAliasFreeVars alias) <| \args ->
      let toName (A.At _ (Src.Alias (A.At _ name) _ _)) = name in
      MResult.throw (Error.RecursiveAlias region name1 args tipe (MList.map toName others))



-- DETECT TYPE ALIAS CYCLES


toNode : A.Located Src.Alias -> (A.Located Src.Alias, Name.Name, TList Name.Name)
toNode ((A.At _ (Src.Alias (A.At _ name) _ tipe)) as alias) =
  ( alias, name, getEdges [] tipe )


getEdges : TList Name.Name -> Src.Type -> TList Name.Name
getEdges edges (A.At _ tipe) =
  case tipe of
    Src.TLambda arg result ->
      getEdges (getEdges edges arg) result

    Src.TVar _ ->
      edges

    Src.TType _ name args ->
      MList.foldl getEdges (name::edges) args

    Src.TTypeQual _ _ _ args ->
      MList.foldl getEdges edges args

    Src.TRecord fields _ ->
      MList.foldl (\es (_,t) -> getEdges es t) edges fields

    Src.TUnit ->
      edges

    Src.TTuple a b cs ->
      MList.foldl getEdges (getEdges (getEdges edges a) b) cs



-- CHECK FREE VARIABLES


checkUnionFreeVars : A.Located Src.Union -> TResult i w Int
checkUnionFreeVars (A.At unionRegion (Src.Union (A.At _ name) args ctors)) =
  let
    addArg (A.At region arg) dict =
      Dups.insert arg region region dict

    addCtorFreeVars (_, tipes) freeVars =
      MList.foldl addFreeVars freeVars tipes
  in
  MResult.bind (Dups.detect (Error.DuplicateUnionArg name) (MList.foldr addArg Dups.none args)) <| \boundVars ->
  let freeVars = MList.foldr addCtorFreeVars Map.empty ctors in
  case Map.toList (Map.difference freeVars boundVars) of
    [] ->
      MResult.ok (MList.length args)

    unbound::unbounds ->
      MResult.throw <|
        Error.TypeVarsUnboundInUnion unionRegion name (MList.map A.toValue args) unbound unbounds


checkAliasFreeVars : A.Located Src.Alias -> TResult i w (TList Name.Name)
checkAliasFreeVars (A.At aliasRegion (Src.Alias (A.At _ name) args tipe)) =
  let
    addArg (A.At region arg) dict =
      Dups.insert arg region region dict
  in
  MResult.bind (Dups.detect (Error.DuplicateAliasArg name) (MList.foldr addArg Dups.none args)) <| \boundVars ->
  let freeVars = addFreeVars Map.empty tipe in
  let overlap = Map.size (Map.intersection boundVars freeVars) in
  if Map.size boundVars == overlap && Map.size freeVars == overlap
    then MResult.ok (MList.map A.toValue args)
    else
      MResult.throw <|
        Error.TypeVarsMessedUpInAlias aliasRegion name
          (MList.map A.toValue args)
          (Map.toList (Map.difference boundVars freeVars))
          (Map.toList (Map.difference freeVars boundVars))


addFreeVars : Map.Map Name.Name A.Region -> Src.Type -> Map.Map Name.Name A.Region
addFreeVars freeVars (A.At region tipe) =
  case tipe of
    Src.TLambda arg result ->
      addFreeVars (addFreeVars freeVars arg) result

    Src.TVar name ->
      Map.insert name region freeVars

    Src.TType _ _ args ->
      MList.foldl addFreeVars freeVars args

    Src.TTypeQual _ _ _ args ->
      MList.foldl addFreeVars freeVars args

    Src.TRecord fields maybeExt ->
      let
        extFreeVars =
          case maybeExt of
            Nothing ->
              freeVars

            Just (A.At extRegion ext) ->
              Map.insert ext extRegion freeVars
      in
      MList.foldl (\fvs (_,t) -> addFreeVars fvs t) extFreeVars fields

    Src.TUnit ->
      freeVars

    Src.TTuple a b cs ->
      MList.foldl addFreeVars (addFreeVars (addFreeVars freeVars a) b) cs



-- ADD CTORS


addCtors : Src.Module -> Env.Env -> TResult i w (Env.Env, Unions, Aliases)
addCtors (Src.Module _ _ _ _ unions aliases _ _) ((Env.Env home vs ts cs bs qvs qts qcs) as env) =
  MResult.bind (MResult.traverseList (canonicalizeUnion env) unions) <| \unionInfo ->
  MResult.bind (MResult.traverseList (canonicalizeAlias env) aliases) <| \aliasInfo ->

  MResult.bind
    (Dups.detect Error.DuplicateCtor <|
      Dups.union
        (Dups.unions (MList.map Tuple.second unionInfo))
        (Dups.unions (MList.map Tuple.second aliasInfo))) <| \ctors ->

  let cs2 = Map.union ctors cs in

  MResult.ok
    ( Env.Env home vs ts cs2 bs qvs qts qcs
    , Map.fromList (MList.map Tuple.first unionInfo)
    , Map.fromList (MList.map Tuple.first aliasInfo)
    )


type alias CtorDups = Dups.TDict (Env.Info Env.Ctor)



-- CANONICALIZE ALIAS


canonicalizeAlias : Env.Env -> A.Located Src.Alias -> TResult i w ( (Name.Name, Can.Alias), CtorDups )
canonicalizeAlias ((Env.Env home _ _ _ _ _ _ _) as env) (A.At _ (Src.Alias (A.At region name) args tipe)) =
  let vars = MList.map A.toValue args in
  MResult.bind (Type.canonicalize env tipe) <| \ctipe ->
  MResult.ok
    ( (name, Can.Alias vars ctipe)
    ,
      case ctipe of
        Can.TRecord fields Nothing ->
          Dups.one name region (Env.Specific home (toRecordCtor home name vars fields))

        _ ->
          Dups.none
    )


toRecordCtor : ModuleName.Canonical -> Name.Name -> TList Name.Name -> Map.Map Name.Name Can.FieldType -> Env.Ctor
toRecordCtor home name vars fields =
  let
    avars = MList.map (\var -> (var, Can.TVar var)) vars
    alias_ =
      MList.foldr
        (\(_,t1) t2 -> Can.TLambda t1 t2)
        (Can.TAlias home name avars (Can.Filled (Can.TRecord fields Nothing)))
        (Can.fieldsToList fields)
  in
  Env.RecordCtor home vars alias_



-- CANONICALIZE UNION


canonicalizeUnion : Env.Env -> A.Located Src.Union -> TResult i w ( (Name.Name, Can.Union), CtorDups )
canonicalizeUnion ((Env.Env home _ _ _ _ _ _ _) as env) (A.At _ (Src.Union (A.At _ name) avars ctors)) =
  MResult.bind (Index.indexedTraverse MResult.pure MResult.liftA2 (canonicalizeCtor env) ctors) <| \cctors ->
  let vars = MList.map A.toValue avars in
  let alts = MList.map A.toValue cctors in
  let union = Can.Union vars alts (MList.length alts) (toOpts ctors) in
  MResult.ok
    ( (name, union)
    , Dups.unions <| MList.map (toCtor home name union) cctors
    )


canonicalizeCtor : Env.Env -> Index.ZeroBased -> ( A.Located Name.Name, TList Src.Type ) -> TResult i w (A.Located Can.Ctor)
canonicalizeCtor env index ( A.At region ctor, tipes ) =
    MResult.bind (MResult.traverseList (Type.canonicalize env) tipes) <|
        \ctipes ->
            MResult.ok <|
                A.At region <|
                    Can.Ctor ctor index (MList.length ctipes) ctipes


toOpts : TList (A.Located Name.Name, TList Src.Type) -> Can.CtorOpts
toOpts ctors =
  case ctors of
    [ (_,[_]) ] ->
      Can.Unbox

    _ ->
      if MList.all (MList.null << Tuple.second) ctors then Can.Enum else Can.Normal


toCtor : ModuleName.Canonical -> Name.Name -> Can.Union -> A.Located Can.Ctor -> CtorDups
toCtor home typeName union (A.At region (Can.Ctor name index _ args)) =
  Dups.one name region <| Env.Specific home <|
    Env.Ctor home typeName union index args
