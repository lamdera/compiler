{- MANUALLY FORMATTED -}
module Compiler.Optimize.Module exposing
  ( optimize
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Utils.Type as Type
import Compiler.Canonicalize.Effects as Effects
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Optimize.Names as Names
import Compiler.Optimize.Expression as Expr
import Compiler.Optimize.Port as Port
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Main as E
import Compiler.Reporting.Result as MResult
import Compiler.Reporting.Warning as W
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Set as Set



-- OPTIMIZE


type alias TResult i w a =
  MResult.TResult i w E.Error a


type alias Annotations =
  Map.Map Name.Name Can.Annotation


optimize : Annotations -> Can.Module -> TResult i (TList W.Warning) Opt.LocalGraph
optimize annotations (Can.Module home _ decls unions aliases _ effects) =
  addDecls home annotations decls <|
    addEffects home effects <|
      addUnions home unions <|
        addAliases home aliases <|
          Opt.LocalGraph Nothing Map.empty Map.empty



-- UNION


type alias Nodes =
  Map.Map Opt.GlobalComparable Opt.Node


addUnions : ModuleName.Canonical -> Map.Map Name.Name Can.Union -> Opt.LocalGraph -> Opt.LocalGraph
addUnions home unions (Opt.LocalGraph main nodes fields) =
  Opt.LocalGraph main (Map.foldr (addUnion home) nodes unions) fields


addUnion : ModuleName.Canonical -> Can.Union -> Nodes -> Nodes
addUnion home (Can.Union _ ctors _ opts) nodes =
  MList.foldl (addCtorNode home opts) nodes ctors


addCtorNode : ModuleName.Canonical -> Can.CtorOpts -> Nodes -> Can.Ctor -> Nodes
addCtorNode home opts nodes (Can.Ctor name index numArgs _) =
  let
    node =
      case opts of
        Can.Normal -> Opt.Ctor index numArgs
        Can.Unbox -> Opt.Box
        Can.Enum -> Opt.Enum index
  in
  Map.insert (Opt.toGlobalComparable <| Opt.Global home name) node nodes



-- ALIAS


addAliases : ModuleName.Canonical -> Map.Map Name.Name Can.Alias -> Opt.LocalGraph -> Opt.LocalGraph
addAliases home aliases graph =
  Map.foldrWithKey (addAlias home) graph aliases


addAlias : ModuleName.Canonical -> Name.Name -> Can.Alias -> Opt.LocalGraph -> Opt.LocalGraph
addAlias home name (Can.Alias _ tipe) ((Opt.LocalGraph main nodes fieldCounts) as graph) =
  case tipe of
    Can.TRecord fields Nothing ->
      let
        function =
          Opt.Function (MList.map Tuple.first (Can.fieldsToList fields)) <| Opt.Record <|
            Map.mapWithKey (\field _ -> Opt.VarLocal field) fields

        node =
          Opt.Define function Set.empty
      in
      Opt.LocalGraph
        main
        (Map.insert (Opt.toGlobalComparable <| Opt.Global home name) node nodes)
        (Map.foldrWithKey addRecordCtorField fieldCounts fields)

    _ ->
      graph


addRecordCtorField : Name.Name -> Can.FieldType -> Map.Map Name.Name Int -> Map.Map Name.Name Int
addRecordCtorField name _ fields =
  Map.insertWith (+) name 1 fields



-- ADD EFFECTS


addEffects : ModuleName.Canonical -> Can.Effects -> Opt.LocalGraph -> Opt.LocalGraph
addEffects home effects ((Opt.LocalGraph main nodes fields) as graph) =
  case effects of
    Can.NoEffects ->
      graph

    Can.Ports ports ->
      Map.foldrWithKey (addPort home) graph ports

    Can.Manager _ _ _ manager ->
      let
        fx = Opt.Global home "$fx$"
        cmd = Opt.Global home "command"
        sub = Opt.Global home "subscription"
        link = Opt.Link fx
        newNodes =
          case manager of
            Can.CCmd _ ->
              Map.insert (Opt.toGlobalComparable cmd) link <|
              Map.insert (Opt.toGlobalComparable fx) (Opt.Manager Opt.CCmd) nodes

            Can.CSub _ ->
              Map.insert (Opt.toGlobalComparable sub) link <|
              Map.insert (Opt.toGlobalComparable fx) (Opt.Manager Opt.CSub) nodes

            Can.Fx _ _ ->
              Map.insert (Opt.toGlobalComparable cmd) link <|
              Map.insert (Opt.toGlobalComparable sub) link <|
              Map.insert (Opt.toGlobalComparable fx) (Opt.Manager Opt.Fx) nodes
      in
      Opt.LocalGraph main newNodes fields


addPort : ModuleName.Canonical -> Name.Name -> Can.Port -> Opt.LocalGraph -> Opt.LocalGraph
addPort home name port_ graph =
  case port_ of
    Can.Incoming _ payloadType _ ->
      let
        (deps, fields, decoder) = Names.run (Port.toDecoder payloadType)
        node = Opt.PortIncoming decoder deps
      in
      addToGraph (Opt.Global home name) node fields graph

    Can.Outgoing _ payloadType _ ->
      let
        (deps, fields, encoder) = Names.run (Port.toEncoder payloadType)
        node = Opt.PortOutgoing encoder deps
      in
      addToGraph (Opt.Global home name) node fields graph



-- HELPER


addToGraph : Opt.Global -> Opt.Node -> Map.Map Name.Name Int -> Opt.LocalGraph -> Opt.LocalGraph
addToGraph name node fields (Opt.LocalGraph main nodes fieldCounts) =
  Opt.LocalGraph
    main
    (Map.insert (Opt.toGlobalComparable name) node nodes)
    (Map.unionWith (+) fields fieldCounts)



-- ADD DECLS


addDecls : ModuleName.Canonical -> Annotations -> Can.Decls -> Opt.LocalGraph -> TResult i (TList W.Warning) Opt.LocalGraph
addDecls home annotations decls graph =
  MResult.loop (addDeclsHelp home annotations) (decls, graph)


addDeclsHelp : ModuleName.Canonical -> Annotations -> (Can.Decls, Opt.LocalGraph) -> TResult i (TList W.Warning) (MResult.Step (Can.Decls, Opt.LocalGraph) Opt.LocalGraph)
addDeclsHelp home annotations (decls, graph) =
  case decls of
    Can.Declare def subDecls ->
      MResult.fmap (\g -> MResult.Loop (subDecls, g)) (addDef home annotations def graph)

    Can.DeclareRec d ds subDecls ->
      let defs = d::ds in
      case findMain defs of
        Nothing ->
          MResult.return (MResult.Loop (subDecls, addRecDefs home defs graph))

        Just region ->
          MResult.throw <| E.BadCycle region (defToName d) (MList.map defToName ds)

    Can.SaveTheEnvironment ->
      MResult.return (MResult.Done graph)


findMain : TList Can.Def -> Maybe A.Region
findMain defs =
  case defs of
    [] ->
      Nothing

    def::rest ->
      case def of
        Can.Def (A.At region name) _ _ ->
          if name == Name.l_main then Just region else findMain rest

        Can.TypedDef (A.At region name) _ _ _ _ ->
          if name == Name.l_main then Just region else findMain rest


defToName : Can.Def -> Name.Name
defToName def =
  case def of
    Can.Def (A.At _ name) _ _          -> name
    Can.TypedDef (A.At _ name) _ _ _ _ -> name



-- ADD DEFS


addDef : ModuleName.Canonical -> Annotations -> Can.Def -> Opt.LocalGraph -> TResult i (TList W.Warning) Opt.LocalGraph
addDef home annotations def graph =
  case def of
    Can.Def (A.At region name) args body ->
      MResult.bind (MResult.warn <| W.MissingTypeAnnotation) <| \_ ->
      addDefHelp region annotations home name args body graph

    Can.TypedDef (A.At region name) _ typedArgs body _ ->
      addDefHelp region annotations home name (MList.map Tuple.first typedArgs) body graph


addDefHelp : A.Region -> Annotations -> ModuleName.Canonical -> Name.Name -> TList Can.Pattern -> Can.Expr -> Opt.LocalGraph -> TResult i w Opt.LocalGraph
addDefHelp region annotations home name args body ((Opt.LocalGraph _ nodes fieldCounts) as graph) =
  if name /= Name.l_main then
    MResult.ok (addDefNode home name args body Set.empty graph)
  else
    let
      (Can.Forall _ tipe) = Map.ex annotations name

      addMain (deps, fields, main) =
        addDefNode home name args body deps <|
          Opt.LocalGraph (Just main) nodes (Map.unionWith (+) fields fieldCounts)
    in
    let
      otherwise () =
        MResult.throw (E.BadType region tipe)
    in
    case Type.deepDealias tipe of
      Can.TType hm nm [_] -> if hm == ModuleName.virtualDom && nm == Name.node then
        MResult.ok <| addMain <| Names.run <|
          Names.registerKernel Name.virtualDom Opt.Static else otherwise ()

      Can.TType hm nm [flags, _, message] -> if hm == ModuleName.platform && nm == Name.program then
        case Effects.checkPayload flags of
          Right () ->
            MResult.ok <| addMain <| Names.run <|
              Names.fmap (Opt.Dynamic message) <| Port.toFlagsDecoder flags

          Left (_, invalidPayload) ->
            MResult.throw (E.BadFlags region invalidPayload)
        else otherwise ()

      _ ->
        otherwise ()


addDefNode : ModuleName.Canonical -> Name.Name -> TList Can.Pattern -> Can.Expr -> Set.Set Opt.GlobalComparable -> Opt.LocalGraph -> Opt.LocalGraph
addDefNode home name args body mainDeps graph =
  let
    (deps, fields, def) =
      Names.run <|
        case args of
          [] ->
            Expr.optimize Set.empty body

          _ ->
            Names.bind (Expr.destructArgs args) <| \(argNames, destructors) ->
            Names.bind (Expr.optimize Set.empty body) <| \obody ->
            Names.pure <| Opt.Function argNames <|
              MList.foldr Opt.Destruct obody destructors
  in
  addToGraph (Opt.Global home name) (Opt.Define def (Set.union deps mainDeps)) fields graph



-- ADD RECURSIVE DEFS


type State =
  State
    {- values -} (TList (Name.Name, Opt.Expr))
    {- functions -} (TList Opt.Def)


addRecDefs : ModuleName.Canonical -> TList Can.Def -> Opt.LocalGraph -> Opt.LocalGraph
addRecDefs home defs (Opt.LocalGraph main nodes fieldCounts) =
  let
    names = MList.reverse (MList.map toName defs)
    cycleName = Opt.Global home (Name.fromManyNames names)
    cycle = MList.foldr addValueName Set.empty defs
    links = MList.foldr (addLink home (Opt.Link cycleName)) Map.empty defs

    (deps, fields, State values funcs) =
      Names.run <|
        MList.foldlM Names.return Names.bind (addRecDef cycle) (State [] []) defs
  in
  Opt.LocalGraph
    main
    (Map.insert (Opt.toGlobalComparable cycleName) (Opt.Cycle names values funcs deps) (Map.union links nodes))
    (Map.unionWith (+) fields fieldCounts)


toName : Can.Def -> Name.Name
toName def =
  case def of
    Can.Def      (A.At _ name) _ _     -> name
    Can.TypedDef (A.At _ name) _ _ _ _ -> name


addValueName : Can.Def -> Set.Set Name.Name -> Set.Set Name.Name
addValueName def names =
  case def of
    Can.Def      (A.At _ name)   args _   -> if MList.null args then Set.insert name names else names
    Can.TypedDef (A.At _ name) _ args _ _ -> if MList.null args then Set.insert name names else names


addLink : ModuleName.Canonical -> Opt.Node -> Can.Def -> Map.Map Opt.GlobalComparable Opt.Node -> Map.Map Opt.GlobalComparable Opt.Node
addLink home link def links =
  case def of
    Can.Def (A.At _ name) _ _ ->
      Map.insert (Opt.toGlobalComparable <| Opt.Global home name) link links

    Can.TypedDef (A.At _ name) _ _ _ _ ->
      Map.insert (Opt.toGlobalComparable <| Opt.Global home name) link links



-- ADD RECURSIVE DEFS


addRecDef : Set.Set Name.Name -> State -> Can.Def -> Names.Tracker State
addRecDef cycle state def =
  case def of
    Can.Def (A.At _ name) args body ->
      addRecDefHelp cycle state name args body

    Can.TypedDef (A.At _ name) _ args body _ ->
      addRecDefHelp cycle state name (MList.map Tuple.first args) body


addRecDefHelp : Set.Set Name.Name -> State -> Name.Name -> TList Can.Pattern -> Can.Expr -> Names.Tracker State
addRecDefHelp cycle (State values funcs) name args body =
  case args of
    [] ->
      Names.bind (Expr.optimize cycle body) <| \obody ->
      Names.pure <| State ((name, obody) :: values) funcs

    _::_ ->
      Names.bind (Expr.optimizePotentialTailCall cycle name args body) <| \odef ->
      Names.pure <| State values (odef :: funcs)
