{- MANUALLY FORMATTED -}
module Compiler.Canonicalize.Expression exposing
  ( canonicalize
  , FreeLocals
  , Uses(..)
  , verifyBindings
  , gatherTypedArgs
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.AST.Utils.Binop as Binop
import Compiler.AST.Utils.Type as UType
import Compiler.Canonicalize.Environment as Env
import Compiler.Canonicalize.Environment.Dups as Dups
import Compiler.Canonicalize.Pattern as Pattern
import Compiler.Canonicalize.Type as Type
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as MResult
import Compiler.Reporting.Warning as W
import Extra.Data.Graph as Graph
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- RESULTS


type alias TResult i w a =
  MResult.TResult i w Error.Error a


type alias FreeLocals =
  Map.Map Name.Name Uses


type Uses =
  Uses
    {- direct -} Int
    {- delayed -} Int



-- CANONICALIZE


canonicalize : Env.Env -> Src.Expr -> TResult FreeLocals (TList W.Warning) Can.Expr
canonicalize env (A.At region expression) =
  MResult.fmap (A.At region) <|
  case expression of
    Src.Str string ->
      MResult.ok (Can.Str string)

    Src.Chr char ->
      MResult.ok (Can.Chr char)

    Src.CInt int ->
      MResult.ok (Can.CInt int)

    Src.CFloat float ->
      MResult.ok (Can.CFloat float)

    Src.Var varType name ->
      case varType of
        Src.LowVar -> findVar region env name
        Src.CapVar -> MResult.fmap (toVarCtor name) (Env.findCtor region env name)

    Src.VarQual varType prefix name ->
      case varType of
        Src.LowVar -> findVarQual region env prefix name
        Src.CapVar -> MResult.fmap (toVarCtor name) (Env.findCtorQual region env prefix name)

    Src.CList exprs ->
      MResult.fmap Can.CList (MResult.traverseList (canonicalize env) exprs)

    Src.Op op ->
      MResult.bind (Env.findBinop region env op) <| \(Env.Binop _ home name annotation _ _) ->
      MResult.return (Can.VarOperator op home name annotation)

    Src.Negate expr ->
      MResult.fmap Can.Negate (canonicalize env expr)

    Src.Binops ops final ->
      MResult.fmap A.toValue (canonicalizeBinops region env ops final)

    Src.Lambda srcArgs body ->
      delayedUsage <|
      MResult.bind
        (Pattern.verify Error.DPLambdaArgs <|
          MResult.traverseList (Pattern.canonicalize env) srcArgs) <| \(args, bindings) ->

      MResult.bind
        (Env.addLocals bindings env) <| \newEnv ->

      MResult.bind
        (verifyBindings W.Pattern bindings (canonicalize newEnv body)) <| \(cbody, freeLocals) ->

      MResult.return (Can.Lambda args cbody, freeLocals)

    Src.Call func args ->
      MResult.pure Can.Call
        |> MResult.andMap (canonicalize env func)
        |> MResult.andMap (MResult.traverseList (canonicalize env) args)

    Src.If branches finally ->
      MResult.pure Can.If
        |> MResult.andMap (MResult.traverseList (canonicalizeIfBranch env) branches)
        |> MResult.andMap (canonicalize env finally)

    Src.Let defs expr ->
      MResult.fmap A.toValue (canonicalizeLet region env defs expr)

    Src.Case expr branches ->
      MResult.pure Can.Case
        |> MResult.andMap (canonicalize env expr)
        |> MResult.andMap (MResult.traverseList (canonicalizeCaseBranch env) branches)

    Src.Accessor field ->
      MResult.ok <| Can.Accessor field

    Src.Access record field ->
      MResult.pure Can.Access
        |> MResult.andMap (canonicalize env record)
        |> MResult.andMap (MResult.ok field)

    Src.Update (A.At reg name) fields ->
      let
        makeCanFields =
          Dups.checkFields_ (\r t -> MResult.fmap (Can.FieldUpdate r) (canonicalize env t)) fields
      in
      MResult.pure (Can.Update name)
        |> MResult.andMap (MResult.fmap (A.At reg) (findVar reg env name))
        |> MResult.andMap (MResult.andThen MResult.sequenceAMap makeCanFields)

    Src.Record fields ->
      MResult.bind (Dups.checkFields fields) <| \fieldDict ->
      MResult.fmap Can.Record (MResult.traverseMap (canonicalize env) fieldDict)

    Src.Unit ->
      MResult.ok Can.Unit

    Src.Tuple a b cs ->
      MResult.pure Can.Tuple
        |> MResult.andMap (canonicalize env a)
        |> MResult.andMap (canonicalize env b)
        |> MResult.andMap (canonicalizeTupleExtras region env cs)

    Src.Shader src tipe ->
        MResult.ok (Can.Shader src tipe)



-- CANONICALIZE TUPLE EXTRAS


canonicalizeTupleExtras : A.Region -> Env.Env -> TList Src.Expr -> TResult FreeLocals (TList W.Warning) (Maybe Can.Expr)
canonicalizeTupleExtras region env extras =
  case extras of
    [] ->
      MResult.ok Nothing

    [three] ->
      MResult.fmap Just (canonicalize env three)

    _ ->
      MResult.throw (Error.TupleLargerThanThree region)



-- CANONICALIZE IF BRANCH


canonicalizeIfBranch : Env.Env -> (Src.Expr, Src.Expr) -> TResult FreeLocals (TList W.Warning) (Can.Expr, Can.Expr)
canonicalizeIfBranch env (condition, branch) =
  MResult.pure Tuple.pair
    |> MResult.andMap (canonicalize env condition)
    |> MResult.andMap (canonicalize env branch)



-- CANONICALIZE CASE BRANCH

canonicalizeCaseBranch : Env.Env -> (Src.Pattern, Src.Expr) -> TResult FreeLocals (TList W.Warning) Can.CaseBranch
canonicalizeCaseBranch env (pattern, expr) =
  directUsage <|
  MResult.bind
    (Pattern.verify Error.DPCaseBranch <|
      Pattern.canonicalize env pattern) <| \(cpattern, bindings) ->
  MResult.bind (Env.addLocals bindings env) <| \newEnv ->

  MResult.bind
    (verifyBindings W.Pattern bindings (canonicalize newEnv expr)) <| \(cexpr, freeLocals) ->

  MResult.return (Can.CaseBranch cpattern cexpr, freeLocals)



-- CANONICALIZE BINOPS


canonicalizeBinops : A.Region -> Env.Env -> TList (Src.Expr, A.Located Name.Name) -> Src.Expr -> TResult FreeLocals (TList W.Warning) Can.Expr
canonicalizeBinops overallRegion env ops final =
  let
    canonicalizeHelp (expr, A.At region op) =
      MResult.pure Tuple.pair
        |> MResult.andMap (canonicalize env expr)
        |> MResult.andMap (Env.findBinop region env op)
  in
  MResult.andThen (runBinopStepper overallRegion) (
    MResult.pure More
      |> MResult.andMap (MResult.traverseList canonicalizeHelp ops)
      |> MResult.andMap (canonicalize env final)
  )


type Step
  = Done Can.Expr
  | More (TList (Can.Expr, Env.Binop)) Can.Expr
  | Error Env.Binop Env.Binop


runBinopStepper : A.Region -> Step -> TResult FreeLocals w Can.Expr
runBinopStepper overallRegion step =
  case step of
    Done expr ->
      MResult.ok expr

    More [] expr ->
      MResult.ok expr

    More ( (expr, op) :: rest ) final ->
      runBinopStepper overallRegion <|
        toBinopStep (toBinop op expr) op rest final

    Error (Env.Binop op1 _ _ _ _ _) (Env.Binop op2 _ _ _ _ _) ->
      MResult.throw (Error.Binop overallRegion op1 op2)


toBinopStep : (Can.Expr -> Can.Expr) -> Env.Binop -> TList (Can.Expr, Env.Binop) -> Can.Expr -> Step
toBinopStep makeBinop ((Env.Binop _ _ _ _ rootAssociativity rootPrecedence) as rootOp) middle final =
  case middle of
    [] ->
      Done (makeBinop final)

    ( expr, ((Env.Binop _ _ _ _ associativity precedence) as op) ) :: rest ->
      if Binop.toInt precedence < Binop.toInt rootPrecedence then

        More ((makeBinop expr, op) :: rest) final

      else if Binop.toInt precedence > Binop.toInt rootPrecedence then

        case toBinopStep (toBinop op expr) op rest final of
          Done newLast ->
            Done (makeBinop newLast)

          More newMiddle newLast ->
            toBinopStep makeBinop rootOp newMiddle newLast

          Error a b ->
            Error a b

      else

        case (rootAssociativity, associativity) of
          (Binop.Left, Binop.Left) ->
            -- without this construct, we get an endless recursion...
            (\() -> toBinopStep (\right -> toBinop op (makeBinop expr) right) op rest final) ()

          (Binop.Right, Binop.Right) ->
            -- without this construct, we get an endless recursion...
            (\() -> toBinopStep (\right -> makeBinop (toBinop op expr right)) op rest final) ()

          _ ->
            Error rootOp op


toBinop : Env.Binop -> Can.Expr -> Can.Expr -> Can.Expr
toBinop (Env.Binop op home name annotation _ _) left right =
  A.merge left right (Can.Binop op home name annotation left right)



-- CANONICALIZE LET


canonicalizeLet : A.Region -> Env.Env -> TList (A.Located Src.Def) -> Src.Expr -> TResult FreeLocals (TList W.Warning) Can.Expr
canonicalizeLet letRegion env defs body =
  directUsage <|
    MResult.bind
      (Dups.detect (Error.DuplicatePattern Error.DPLetBinding) <|
        MList.foldl addBindings Dups.none defs) <| \bindings ->

    MResult.bind (Env.addLocals bindings env) <| \newEnv ->

    verifyBindings W.Def bindings <|
      MResult.bind (MResult.foldM (addDefNodes newEnv) [] defs) <| \nodes ->
      MResult.bind (canonicalize newEnv body) <| \cbody ->
      detectCycles letRegion (Graph.stronglyConnComp nodes) cbody



-- ADD BINDINGS


addBindings : Dups.TDict A.Region -> A.Located Src.Def -> Dups.TDict A.Region
addBindings bindings (A.At _ def) =
  case def of
    Src.Define (A.At region name) _ _ _ ->
      Dups.insert name region region bindings

    Src.Destruct pattern _ ->
      addBindingsHelp bindings pattern


addBindingsHelp : Dups.TDict A.Region -> Src.Pattern -> Dups.TDict A.Region
addBindingsHelp bindings (A.At region pattern) =
  case pattern of
    Src.PAnything ->
      bindings

    Src.PVar name ->
      Dups.insert name region region bindings

    Src.PRecord fields ->
      let
        addField dict (A.At fieldRegion name) =
          Dups.insert name fieldRegion fieldRegion dict
      in
      MList.foldl addField bindings fields

    Src.PUnit ->
      bindings

    Src.PTuple a b cs ->
      MList.foldl addBindingsHelp bindings (a::b::cs)

    Src.PCtor _ _ patterns ->
      MList.foldl addBindingsHelp bindings patterns

    Src.PCtorQual _ _ _ patterns ->
      MList.foldl addBindingsHelp bindings patterns

    Src.PList patterns ->
      MList.foldl addBindingsHelp bindings patterns

    Src.PCons hd tl ->
      addBindingsHelp (addBindingsHelp bindings hd) tl

    Src.PAlias aliasPattern (A.At nameRegion name) ->
      Dups.insert name nameRegion nameRegion <|
        addBindingsHelp bindings aliasPattern

    Src.PChr _ ->
      bindings

    Src.PStr _ ->
      bindings

    Src.PInt _ ->
      bindings



-- BUILD BINDINGS GRAPH


type alias Node =
  (Binding, Name.Name, TList Name.Name)


type Binding
  = Define Can.Def
  | Edge (A.Located Name.Name)
  | Destruct Can.Pattern Can.Expr


addDefNodes : Env.Env -> TList Node -> A.Located Src.Def -> TResult FreeLocals (TList W.Warning) (TList Node)
addDefNodes env nodes (A.At _ def) =
  case def of
    Src.Define ((A.At _ name) as aname) srcArgs body maybeType ->
      case maybeType of
        Nothing ->
          MResult.bind
            (Pattern.verify (Error.DPFuncArgs name) <|
              MResult.traverseList (Pattern.canonicalize env) srcArgs) <| \(args, argBindings) ->

          MResult.bind
            (Env.addLocals argBindings env) <| \newEnv ->

          MResult.bind
            (verifyBindings W.Pattern argBindings (canonicalize newEnv body)) <| \(cbody, freeLocals) ->

          let cdef = Can.Def aname args cbody in
          let node = ( Define cdef, name, Map.keys freeLocals ) in
          logLetLocals args freeLocals (node::nodes)

        Just tipe ->
          MResult.bind (Type.toAnnotation env tipe) <| \(Can.Forall freeVars ctipe) ->
          MResult.bind
            (Pattern.verify (Error.DPFuncArgs name) <|
              gatherTypedArgs env name srcArgs ctipe Index.first []) <| \((args, resultType), argBindings) ->

          MResult.bind
            (Env.addLocals argBindings env) <| \newEnv ->

          MResult.bind
            (verifyBindings W.Pattern argBindings (canonicalize newEnv body)) <| \(cbody, freeLocals) ->

          let cdef = Can.TypedDef aname freeVars args cbody resultType in
          let node = ( Define cdef, name, Map.keys freeLocals ) in
          logLetLocals args freeLocals (node::nodes)

    Src.Destruct pattern body ->
      MResult.bind
        (Pattern.verify Error.DPDestruct <|
          Pattern.canonicalize env pattern) <| \(cpattern, _) ->

      MResult.CResult <| \fs ws ->
        case canonicalize env body of
          MResult.CResult k ->
            case k Map.empty ws of
              MResult.Rbad freeLocals warnings errors ->
                  MResult.Rbad (Map.unionWith combineUses freeLocals fs) warnings errors
              MResult.Rgood freeLocals warnings cbody ->
                  let
                    names = getPatternNames [] pattern
                    name = Name.fromManyNames (MList.map A.toValue names)
                    node = ( Destruct cpattern cbody, name, Map.keys freeLocals )
                  in
                  MResult.Rgood
                    (Map.unionWith combineUses fs freeLocals)
                    warnings
                    (MList.foldl (addEdge [name]) (node::nodes) names)


logLetLocals : TList arg -> FreeLocals -> value -> TResult FreeLocals w value
logLetLocals args letLocals value =
  MResult.CResult <| \freeLocals warnings ->
    MResult.Rgood
      ( Map.unionWith combineUses freeLocals <|
          case args of
            [] -> letLocals
            _ -> Map.map delayUse letLocals
      )
      warnings
      value


addEdge : TList Name.Name -> TList Node -> A.Located Name.Name -> TList Node
addEdge edges nodes ((A.At _ name) as aname) =
  (Edge aname, name, edges) :: nodes


getPatternNames : TList (A.Located Name.Name) -> Src.Pattern -> TList (A.Located Name.Name)
getPatternNames names (A.At region pattern) =
  case pattern of
    Src.PAnything            -> names
    Src.PVar name            -> A.At region name :: names
    Src.PRecord fields       -> fields ++ names
    Src.PAlias ptrn name     -> getPatternNames (name :: names) ptrn
    Src.PUnit                -> names
    Src.PTuple a b cs        -> MList.foldl getPatternNames (getPatternNames (getPatternNames names a) b) cs
    Src.PCtor _ _ args       -> MList.foldl getPatternNames names args
    Src.PCtorQual _ _ _ args -> MList.foldl getPatternNames names args
    Src.PList patterns       -> MList.foldl getPatternNames names patterns
    Src.PCons hd tl          -> getPatternNames (getPatternNames names hd) tl
    Src.PChr _               -> names
    Src.PStr _               -> names
    Src.PInt _               -> names



-- GATHER TYPED ARGS


gatherTypedArgs
  : Env.Env
  -> Name.Name
  -> TList Src.Pattern
  -> Can.Type
  -> Index.ZeroBased
  -> TList (Can.Pattern, Can.Type)
  -> TResult Pattern.DupsDict w (TList (Can.Pattern, Can.Type), Can.Type)
gatherTypedArgs env name srcArgs tipe index revTypedArgs =
  case srcArgs of
    [] ->
      MResult.return (MList.reverse revTypedArgs, tipe)

    srcArg :: otherSrcArgs ->
      case UType.iteratedDealias tipe of
        Can.TLambda argType resultType ->
          MResult.bind (Pattern.canonicalize env srcArg) <| \arg ->
          gatherTypedArgs env name otherSrcArgs resultType (Index.next index) <|
            (arg, argType) :: revTypedArgs

        _ ->
          let (A.At start _, A.At end _) = (MList.head srcArgs, MList.last srcArgs) in
          MResult.throw <|
            Error.AnnotationTooShort (A.mergeRegions start end) name index (MList.length srcArgs)



-- DETECT CYCLES


detectCycles : A.Region -> TList (Graph.SCC Binding) -> Can.Expr -> TResult i w Can.Expr
detectCycles letRegion sccs body =
  case sccs of
    [] ->
      MResult.ok body

    scc :: subSccs ->
      case scc of
        Graph.AcyclicSCC binding ->
          case binding of
            Define def ->
              MResult.fmap (A.At letRegion << Can.Let def) (detectCycles letRegion subSccs body)

            Edge _ ->
              detectCycles letRegion subSccs body

            Destruct pattern expr ->
              MResult.fmap (A.At letRegion << Can.LetDestruct pattern expr) (detectCycles letRegion subSccs body)

        Graph.CyclicSCC bindings ->
          MResult.fmap (A.At letRegion)
            (MResult.pure Can.LetRec
              |> MResult.andMap (checkCycle bindings [])
              |> MResult.andMap (detectCycles letRegion subSccs body)
            )


checkCycle : TList Binding -> TList Can.Def -> TResult i w (TList Can.Def)
checkCycle bindings defs =
  case bindings of
    [] ->
      MResult.ok defs

    binding :: otherBindings ->
      case binding of
        Define ((Can.Def name args _) as def) ->
          if MList.null args then
            MResult.throw (Error.RecursiveLet name (toNames otherBindings defs))
          else
            checkCycle otherBindings (def::defs)

        Define ((Can.TypedDef name _ args _ _) as def) ->
          if MList.null args then
            MResult.throw (Error.RecursiveLet name (toNames otherBindings defs))
          else
            checkCycle otherBindings (def::defs)

        Edge name ->
          MResult.throw (Error.RecursiveLet name (toNames otherBindings defs))

        Destruct _ _ ->
          -- a Destruct cannot appear in a cycle without any Edge values
          -- so we just keep going until we get to the edges
          checkCycle otherBindings defs


toNames : TList Binding -> TList Can.Def -> TList Name.Name
toNames bindings revDefs =
  case bindings of
    [] ->
      MList.reverse (MList.map getDefName revDefs)

    binding :: otherBindings ->
      case binding of
        Define def         -> getDefName def :: toNames otherBindings revDefs
        Edge (A.At _ name) -> name :: toNames otherBindings revDefs
        Destruct _ _       -> toNames otherBindings revDefs


getDefName : Can.Def -> Name.Name
getDefName def =
  case def of
    Can.Def (A.At _ name) _ _ ->
      name

    Can.TypedDef (A.At _ name) _ _ _ _ ->
      name



-- LOG VARIABLE USES


logVar : Name.Name -> a -> TResult FreeLocals w a
logVar name value =
  MResult.CResult <| \freeLocals warnings ->
    MResult.Rgood (Map.insertWith combineUses name oneDirectUse freeLocals) warnings value


oneDirectUse : Uses
oneDirectUse =
  Uses 1 0


combineUses : Uses -> Uses -> Uses
combineUses (Uses a b) (Uses x y) =
  Uses (a + x) (b + y)


delayUse : Uses -> Uses
delayUse (Uses direct delayed) =
  Uses 0 (direct + delayed)



-- MANAGING BINDINGS


verifyBindings
  : W.Context
  -> Pattern.Bindings
  -> TResult FreeLocals (TList W.Warning) value
  -> TResult info (TList W.Warning) (value, FreeLocals)
verifyBindings context bindings (MResult.CResult k) =
  MResult.CResult <| \info warnings ->
    case k Map.empty warnings of
      MResult.Rbad _ warnings1 err ->
        MResult.Rbad info warnings1 err
      MResult.Rgood freeLocals warnings1 value ->
        let
          outerFreeLocals =
            Map.difference freeLocals bindings

          warnings2 =
            -- NOTE: Uses Map.size for O(1) lookup. This means there is
            -- no dictionary allocation unless a problem is detected.
            if Map.size bindings + Map.size outerFreeLocals == Map.size freeLocals then
              warnings1
            else
              Map.foldlWithKey (addUnusedWarning context) warnings1 <|
                Map.difference bindings freeLocals
        in
        MResult.Rgood info warnings2 (value, outerFreeLocals)


addUnusedWarning : W.Context -> TList W.Warning -> Name.Name -> A.Region -> TList W.Warning
addUnusedWarning _ warnings _ _ =
  W.UnusedVariable :: warnings


directUsage : TResult () w (expr, FreeLocals) -> TResult FreeLocals w expr
directUsage (MResult.CResult k) =
  MResult.CResult <| \freeLocals warnings ->
    case k () warnings of
      MResult.Rbad () ws es -> MResult.Rbad freeLocals ws es
      MResult.Rgood () ws (value, newFreeLocals) ->
          MResult.Rgood (Map.unionWith combineUses freeLocals newFreeLocals) ws value


delayedUsage : TResult () w (expr, FreeLocals) -> TResult FreeLocals w expr
delayedUsage (MResult.CResult k) =
  MResult.CResult <| \freeLocals warnings ->
    case k () warnings of
      MResult.Rbad () ws es -> MResult.Rbad freeLocals ws es
      MResult.Rgood () ws (value, newFreeLocals) ->
          let delayedLocals = Map.map delayUse newFreeLocals in
          MResult.Rgood (Map.unionWith combineUses freeLocals delayedLocals) ws value



-- FIND VARIABLE


findVar : A.Region -> Env.Env -> Name.Name -> TResult FreeLocals w Can.Expr_
findVar region (Env.Env localHome vs _ _ _ qvs _ _) name =
  case Map.lookup name vs of
    Just var ->
      case var of
        Env.Local _ ->
          logVar name (Can.VarLocal name)

        Env.TopLevel _ ->
          logVar name (Can.VarTopLevel localHome name)

        Env.Foreign home annotation ->
          MResult.ok <|
            if home == ModuleName.debug then
              Can.VarDebug localHome name annotation
            else
              Can.VarForeign home name annotation

        Env.Foreigns h hs ->
          MResult.throw (Error.AmbiguousVar region Nothing name h hs)

    Nothing ->
      MResult.throw (Error.NotFoundVar region Nothing name (toPossibleNames vs qvs))


findVarQual : A.Region -> Env.Env -> Name.Name -> Name.Name -> TResult FreeLocals w Can.Expr_
findVarQual region (Env.Env localHome vs _ _ _ qvs _ _) prefix name =
  case Map.lookup prefix qvs of
    Just qualified ->
      case Map.lookup name qualified of
        Just (Env.Specific home annotation) ->
          MResult.ok <|
            if home == ModuleName.debug then
              Can.VarDebug localHome name annotation
            else
              Can.VarForeign home name annotation

        Just (Env.Ambiguous h hs) ->
          MResult.throw (Error.AmbiguousVar region (Just prefix) name h hs)

        Nothing ->
          MResult.throw (Error.NotFoundVar region (Just prefix) name (toPossibleNames vs qvs))

    Nothing ->
      if Name.isKernel prefix && Pkg.isKernel (ModuleName.getPackage localHome) then
        MResult.ok <| Can.VarKernel (Name.getKernel prefix) name
      else
        MResult.throw (Error.NotFoundVar region (Just prefix) name (toPossibleNames vs qvs))


toPossibleNames : Map.Map Name.Name Env.Var -> Env.Qualified Can.Annotation -> Error.PossibleNames
toPossibleNames exposed qualified =
  Error.PossibleNames (Map.keysSet exposed) (Map.map Map.keysSet qualified)



-- FIND CTOR


toVarCtor : Name.Name -> Env.Ctor -> Can.Expr_
toVarCtor name ctor =
  case ctor of
    Env.Ctor home typeName (Can.Union vars _ _ opts) index args ->
      let
        freeVars = Map.fromList (MList.map (\v -> (v, ())) vars)
        result = Can.TType home typeName (MList.map Can.TVar vars)
        tipe = MList.foldr Can.TLambda result args
      in
      Can.VarCtor opts home name index (Can.Forall freeVars tipe)

    Env.RecordCtor home vars tipe ->
      let
        freeVars = Map.fromList (MList.map (\v -> (v, ())) vars)
      in
      Can.VarCtor Can.Normal home name Index.first (Can.Forall freeVars tipe)
