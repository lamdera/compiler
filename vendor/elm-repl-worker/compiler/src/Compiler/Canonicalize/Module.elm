{- MANUALLY FORMATTED -}
module Compiler.Canonicalize.Module exposing
  ( canonicalize
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Effects as Effects
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Canonicalize.Environment.Foreign as Foreign
import Compiler.Canonicalize.Environment.Local as Local
import Compiler.Canonicalize.Expression as Expr
import Compiler.Canonicalize.Pattern as Pattern
import Compiler.Canonicalize.Type as Type
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.Interface as I
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as MResult
import Compiler.Reporting.Warning as W
import Extra.Data.Graph as Graph
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- RESULT


type alias TResult i w a =
  MResult.TResult i w Error.Error a



-- MODULES


canonicalize : Pkg.Name -> Map.Map ModuleName.Raw I.Interface -> Src.Module -> TResult i (TList W.Warning) Can.Module
canonicalize pkg ifaces ((Src.Module _ exports imports values _ _ binops effects) as modul) =
  let home = ModuleName.Canonical pkg (Src.getName modul) in
  let cbinops = Map.fromList (MList.map canonicalizeBinop binops) in

  MResult.bind
    (MResult.andThen (Local.add modul)
      (Foreign.createInitialEnv home ifaces imports)) <| \(env, cunions, caliases) ->

  MResult.bind (canonicalizeValues env values) <| \cvalues ->
  MResult.bind (Effects.canonicalize env values cunions effects) <| \ceffects ->
  MResult.bind (canonicalizeExports values cunions caliases cbinops ceffects exports) <| \cexports ->

  MResult.return <| Can.Module home cexports cvalues cunions caliases cbinops ceffects



-- CANONICALIZE BINOP


canonicalizeBinop : A.Located Src.Infix -> ( Name.Name, Can.Binop )
canonicalizeBinop (A.At _ (Src.Infix op associativity precedence func)) =
  ( op, Can.Binop_ associativity precedence func )



-- DECLARATIONS / CYCLE DETECTION
--
-- There are two phases of cycle detection:
--
-- 1. Detect cycles using ALL dependencies => needed for type inference
-- 2. Detect cycles using DIRECT dependencies => nonterminating recursion
--


canonicalizeValues : Env.Env -> TList (A.Located Src.Value) -> TResult i (TList W.Warning) Can.Decls
canonicalizeValues env values =
  MResult.bind (MResult.traverseList (toNodeOne env) values) <| \nodes ->
  detectCycles (Graph.stronglyConnComp nodes)


detectCycles : TList (Graph.SCC NodeTwo) -> TResult i w Can.Decls
detectCycles sccs =
  case sccs of
    [] ->
      MResult.ok Can.SaveTheEnvironment

    scc :: otherSccs ->
      case scc of
        Graph.AcyclicSCC (def, _, _) ->
          MResult.fmap (Can.Declare def) (detectCycles otherSccs)

        Graph.CyclicSCC subNodes ->
          MResult.bind (MResult.traverseList detectBadCycles (Graph.stronglyConnComp subNodes)) <| \defs ->
          case defs of
            []    -> detectCycles otherSccs
            d::ds -> MResult.fmap (Can.DeclareRec d ds) (detectCycles otherSccs)


detectBadCycles : Graph.SCC Can.Def -> TResult i w Can.Def
detectBadCycles scc =
  case scc of
    Graph.AcyclicSCC def ->
      MResult.ok def

    Graph.CyclicSCC [] ->
      Debug.todo "The definition of Data.Graph.SCC should not allow empty CyclicSCC!"

    Graph.CyclicSCC (def::defs) ->
      let
        (A.At region name) = extractDefName def
        names = MList.map (A.toValue << extractDefName) defs
      in
      MResult.throw (Error.RecursiveDecl region name names)


extractDefName : Can.Def -> A.Located Name.Name
extractDefName def =
  case def of
    Can.Def name _ _ -> name
    Can.TypedDef name _ _ _ _ -> name



-- DECLARATIONS / CYCLE DETECTION SETUP
--
-- toNodeOne and toNodeTwo set up nodes for the two cycle detection phases.
--

-- Phase one nodes track ALL dependencies.
-- This allows us to find cyclic values for type inference.
type alias NodeOne =
  (NodeTwo, Name.Name, TList Name.Name)


-- Phase two nodes track DIRECT dependencies.
-- This allows us to detect cycles that definitely do not terminate.
type alias NodeTwo =
  (Can.Def, Name.Name, TList Name.Name)


toNodeOne : Env.Env -> A.Located Src.Value -> TResult i (TList W.Warning) NodeOne
toNodeOne env (A.At _ (Src.Value ((A.At _ name) as aname) srcArgs body maybeType)) =
  case maybeType of
    Nothing ->
      MResult.bind
        (Pattern.verify (Error.DPFuncArgs name) <|
          MResult.traverseList (Pattern.canonicalize env) srcArgs) <| \(args, argBindings) ->

      MResult.bind
        (Env.addLocals argBindings env) <| \newEnv ->

      MResult.bind
        (Expr.verifyBindings W.Pattern argBindings (Expr.canonicalize newEnv body)) <| \(cbody, freeLocals) ->

      let def = Can.Def aname args cbody in
      MResult.return
        ( toNodeTwo name srcArgs def freeLocals
        , name
        , Map.keys freeLocals
        )

    Just srcType ->
      MResult.bind (Type.toAnnotation env srcType) <| \(Can.Forall freeVars tipe) ->

      MResult.bind
        (Pattern.verify (Error.DPFuncArgs name) <|
          Expr.gatherTypedArgs env name srcArgs tipe Index.first []) <| \((args,resultType), argBindings) ->

      MResult.bind
        (Env.addLocals argBindings env) <| \newEnv ->

      MResult.bind
        (Expr.verifyBindings W.Pattern argBindings (Expr.canonicalize newEnv body)) <| \(cbody, freeLocals) ->

      let def = Can.TypedDef aname freeVars args cbody resultType in
      MResult.return
        ( toNodeTwo name srcArgs def freeLocals
        , name
        , Map.keys freeLocals
        )


toNodeTwo : Name.Name -> TList arg -> Can.Def -> Expr.FreeLocals -> NodeTwo
toNodeTwo name args def freeLocals =
  case args of
    [] ->
      (def, name, Map.foldrWithKey addDirects [] freeLocals)

    _ ->
      (def, name, [])


addDirects : Name.Name -> Expr.Uses -> TList Name.Name -> TList Name.Name
addDirects name (Expr.Uses directUses _) directDeps =
  if directUses > 0 then
    name::directDeps
  else
    directDeps



-- CANONICALIZE EXPORTS


canonicalizeExports
  : TList (A.Located Src.Value)
  -> Map.Map Name.Name union
  -> Map.Map Name.Name alias
  -> Map.Map Name.Name binop
  -> Can.Effects
  -> A.Located Src.Exposing
  -> TResult i w Can.Exports
canonicalizeExports values unions aliases binops effects (A.At _ exposing_) =
  case exposing_ of
    Src.Open ->
      MResult.ok Can.ExportEverything

    Src.Explicit exposeds ->
      let names = Map.fromList (MList.map valueToName values) in
      MResult.bind (MResult.traverseList (checkExposed names unions aliases binops effects) exposeds) <| \infos ->
      MResult.fmap Can.Export (Dups.detect Error.ExportDuplicate (Dups.unions infos))


valueToName : A.Located Src.Value -> ( Name.Name, () )
valueToName (A.At _ (Src.Value (A.At _ name) _ _ _)) =
  ( name, () )


checkExposed
  : Map.Map Name.Name value
  -> Map.Map Name.Name union
  -> Map.Map Name.Name alias
  -> Map.Map Name.Name binop
  -> Can.Effects
  -> Src.Exposed
  -> TResult i w (Dups.TDict (A.Located Can.Export))
checkExposed values unions aliases binops effects exposed =
  case exposed of
    Src.Lower (A.At region name) ->
      if Map.member name values then
        ok name region Can.ExportValue
      else
        case checkPorts effects name of
          Nothing ->
            ok name region Can.ExportPort

          Just ports ->
            MResult.throw <| Error.ExportNotFound region Error.BadVar name <|
              ports ++ Map.keys values

    Src.Operator region name ->
      if Map.member name binops then
        ok name region Can.ExportBinop
      else
        MResult.throw <| Error.ExportNotFound region Error.BadOp name <|
          Map.keys binops

    Src.Upper (A.At region name) (Src.Public dotDotRegion) ->
      if Map.member name unions then
        ok name region Can.ExportUnionOpen
      else if Map.member name aliases then
        MResult.throw <| Error.ExportOpenAlias dotDotRegion name
      else
        MResult.throw <| Error.ExportNotFound region Error.BadType name <|
          Map.keys unions ++ Map.keys aliases

    Src.Upper (A.At region name) Src.Private ->
      if Map.member name unions then
        ok name region Can.ExportUnionClosed
      else if Map.member name aliases then
        ok name region Can.ExportAlias
      else
        MResult.throw <| Error.ExportNotFound region Error.BadType name <|
          Map.keys unions ++ Map.keys aliases


checkPorts : Can.Effects -> Name.Name -> Maybe (TList Name.Name)
checkPorts effects name =
  case effects of
    Can.NoEffects ->
      Just []

    Can.Ports ports ->
      if Map.member name ports then Nothing else Just (Map.keys ports)

    Can.Manager _ _ _ _ ->
      Just []


ok : Name.Name -> A.Region -> Can.Export -> TResult i w (Dups.TDict (A.Located Can.Export))
ok name region export =
  MResult.ok <| Dups.one name region (A.At region export)
