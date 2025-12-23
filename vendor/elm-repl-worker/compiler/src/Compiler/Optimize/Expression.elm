{- MANUALLY FORMATTED -}
module Compiler.Optimize.Expression exposing
  ( optimize
  , destructArgs
  , optimizePotentialTailCall
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Optimize.Case as Case
import Compiler.Optimize.Names as Names
import Compiler.Reporting.Annotation as A
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe
import Extra.Type.Set as Set
import Compiler.Optimize.Names as Names
import Compiler.Optimize.Names as Names



-- OPTIMIZE


type alias Cycle =
  Set.Set Name.Name


optimize : Cycle -> Can.Expr -> Names.Tracker Opt.Expr
optimize cycle (A.At region expression) =
  case expression of
    Can.VarLocal name ->
      Names.pure (Opt.VarLocal name)

    Can.VarTopLevel home name ->
      if Set.member name cycle then
        Names.pure (Opt.VarCycle home name)
      else
        Names.registerGlobal home name

    Can.VarKernel home name ->
      Names.registerKernel home (Opt.VarKernel home name)

    Can.VarForeign home name _ ->
      Names.registerGlobal home name

    Can.VarCtor opts home name index _ ->
      Names.registerCtor home name index opts

    Can.VarDebug home name _ ->
      Names.registerDebug name home region

    Can.VarOperator _ home name _ ->
      Names.registerGlobal home name

    Can.Chr chr ->
      Names.registerKernel Name.utils (Opt.Chr chr)

    Can.Str str ->
      Names.pure (Opt.Str str)

    Can.CInt int ->
      Names.pure (Opt.CInt int)

    Can.CFloat float ->
      Names.pure (Opt.CFloat float)

    Can.CList entries ->
      Names.andMap
        (MList.traverse Names.pure Names.liftA2 (optimize cycle) entries)
        (Names.registerKernel Name.list Opt.CList)

    Can.Negate expr ->
      Names.bind (Names.registerGlobal ModuleName.basics Name.negate) <| \func ->
      Names.bind (optimize cycle expr) <| \arg ->
      Names.pure <| Opt.Call func [arg]

    Can.Binop _ home name _ left right ->
      Names.bind (Names.registerGlobal home name) <| \optFunc ->
      Names.bind (optimize cycle left) <| \optLeft ->
      Names.bind (optimize cycle right) <| \optRight ->
      Names.return (Opt.Call optFunc [optLeft, optRight])

    Can.Lambda args body ->
      Names.bind (destructArgs args) <| \(argNames, destructors) ->
      Names.bind (optimize cycle body) <| \obody ->
      Names.pure <| Opt.Function argNames (MList.foldr Opt.Destruct obody destructors)

    Can.Call func args ->
      Names.pure Opt.Call
        |> Names.andMap (optimize cycle func)
        |> Names.andMap (MList.traverse Names.pure Names.liftA2 (optimize cycle) args)

    Can.If branches finally ->
      let
        optimizeBranch (condition, branch) =
          Names.pure Tuple.pair
            |> Names.andMap (optimize cycle condition)
            |> Names.andMap (optimize cycle branch)
      in
      Names.pure Opt.If
        |> Names.andMap (MList.traverse Names.pure Names.liftA2 optimizeBranch branches)
        |> Names.andMap (optimize cycle finally)

    Can.Let def body ->
      Names.andThen (optimizeDef cycle def) <| optimize cycle body

    Can.LetRec defs body ->
      case defs of
        [def] ->
          Names.pure Opt.Let
            |> Names.andMap (optimizePotentialTailCallDef cycle def)
            |> Names.andMap (optimize cycle body)

        _ ->
          Names.bind (optimize cycle body) <| \obody ->
          MList.foldlM Names.return Names.bind (\bod def -> optimizeDef cycle def bod) obody defs

    Can.LetDestruct pattern expr body ->
      Names.bind (destruct pattern) <| \(name, destructs) ->
      Names.bind (optimize cycle expr) <| \oexpr ->
      Names.bind (optimize cycle body) <| \obody ->
      Names.pure <|
        Opt.Let (Opt.Def name oexpr) (MList.foldr Opt.Destruct obody destructs)

    Can.Case expr branches ->
      let
        optimizeBranch root (Can.CaseBranch pattern branch) =
          Names.bind (destructCase root pattern) <| \destructors ->
          Names.bind (optimize cycle branch) <| \obranch ->
          Names.pure (pattern, MList.foldr Opt.Destruct obranch destructors)
      in
        Names.bind (Names.generate) <| \temp ->
        Names.bind (optimize cycle expr) <| \oexpr ->
        case oexpr of
          Opt.VarLocal root ->
            Names.fmap (Case.optimize temp root) <| MList.traverse Names.pure Names.liftA2 (optimizeBranch root) branches

          _ ->
            Names.bind (MList.traverse Names.pure Names.liftA2 (optimizeBranch temp) branches) <| \obranches ->
            Names.return <| Opt.Let (Opt.Def temp oexpr) (Case.optimize temp temp obranches)

    Can.Accessor field ->
      Names.registerField field (Opt.Accessor field)

    Can.Access record (A.At _ field) ->
      Names.bind (optimize cycle record) <| \optRecord ->
      Names.registerField field (Opt.Access optRecord field)

    Can.Update _ record updates ->
      Names.registerFieldDict updates Opt.Update
        |> Names.andMap (optimize cycle record)
        |> Names.andMap (Map.traverse Names.pure Names.liftA2 (optimizeUpdate cycle) updates)

    Can.Record fields ->
      Names.registerFieldDict fields Opt.Record
        |> Names.andMap (Map.traverse Names.pure Names.liftA2 (optimize cycle) fields)

    Can.Unit ->
      Names.registerKernel Name.utils Opt.Unit

    Can.Tuple a b maybeC ->
      Names.registerKernel Name.utils Opt.Tuple
        |> Names.andMap (optimize cycle a)
        |> Names.andMap (optimize cycle b)
        |> Names.andMap (MMaybe.traverse Names.pure Names.fmap (optimize cycle) maybeC)

    Can.Shader src (Shader.Types attributes uniforms _) ->
      Names.pure (Opt.Shader src attributes uniforms)



-- UPDATE


optimizeUpdate : Cycle -> Can.FieldUpdate -> Names.Tracker Opt.Expr
optimizeUpdate cycle (Can.FieldUpdate _ expr) =
  optimize cycle expr



-- DEFINITION


optimizeDef : Cycle -> Can.Def -> Opt.Expr -> Names.Tracker Opt.Expr
optimizeDef cycle def body =
  case def of
    Can.Def (A.At _ name) args expr ->
      optimizeDefHelp cycle name args expr body

    Can.TypedDef (A.At _ name) _ typedArgs expr _ ->
      optimizeDefHelp cycle name (MList.map Tuple.first typedArgs) expr body


optimizeDefHelp : Cycle -> Name.Name -> TList Can.Pattern -> Can.Expr -> Opt.Expr -> Names.Tracker Opt.Expr
optimizeDefHelp cycle name args expr body =
  Names.bind (optimize cycle expr) <| \oexpr ->
  case args of
    [] ->
      Names.pure <| Opt.Let (Opt.Def name oexpr) body

    _ ->
      Names.bind (destructArgs args) <| \(argNames, destructors) ->
      let ofunc = Opt.Function argNames (MList.foldr Opt.Destruct oexpr destructors) in
      Names.pure <| Opt.Let (Opt.Def name ofunc) body



-- DESTRUCTURING


destructArgs : TList Can.Pattern -> Names.Tracker (TList Name.Name, TList Opt.Destructor)
destructArgs args =
  Names.bind (Names.fmap MList.unzip <| MList.traverse Names.pure Names.liftA2 destruct args) <| \(argNames, destructorLists) ->
  Names.return (argNames, MList.concat destructorLists)


destructCase : Name.Name -> Can.Pattern -> Names.Tracker (TList Opt.Destructor)
destructCase rootName pattern =
  Names.fmap MList.reverse <| destructHelp (Opt.Root rootName) pattern []


destruct : Can.Pattern -> Names.Tracker (Name.Name, TList Opt.Destructor)
destruct ((A.At _ ptrn) as pattern) =
  case ptrn of
    Can.PVar name ->
      Names.pure (name, [])

    Can.PAlias subPattern name ->
      Names.bind (destructHelp (Opt.Root name) subPattern []) <| \revDs ->
      Names.pure (name, MList.reverse revDs)

    _ ->
      Names.bind (Names.generate) <| \name ->
      Names.bind (destructHelp (Opt.Root name) pattern []) <| \revDs ->
      Names.pure (name, MList.reverse revDs)


destructHelp : Opt.Path -> Can.Pattern -> TList Opt.Destructor -> Names.Tracker (TList Opt.Destructor)
destructHelp path (A.At region pattern) revDs =
  case pattern of
    Can.PAnything ->
      Names.pure revDs

    Can.PVar name ->
      Names.pure (Opt.Destructor name path :: revDs)

    Can.PRecord fields ->
      let
        toDestruct name =
          Opt.Destructor name (Opt.Field name path)
      in
      Names.registerFieldList fields (MList.map toDestruct fields ++ revDs)

    Can.PAlias subPattern name ->
      destructHelp (Opt.Root name) subPattern <|
        Opt.Destructor name path :: revDs

    Can.PUnit ->
      Names.pure revDs

    Can.PTuple a b Nothing ->
      destructTwo path a b revDs

    Can.PTuple a b (Just c) ->
      case path of
        Opt.Root _ ->
          Names.andThen (destructHelp (Opt.Index Index.third path) c) <|
            Names.andThen (destructHelp (Opt.Index Index.second path) b) <|
              destructHelp (Opt.Index Index.first path) a revDs

        _ ->
          Names.bind (Names.generate) <| \name ->
          let newRoot = Opt.Root name in
          Names.andThen (destructHelp (Opt.Index Index.third newRoot) c) <|
            Names.andThen (destructHelp (Opt.Index Index.second newRoot) b) <|
              destructHelp (Opt.Index Index.first newRoot) a (Opt.Destructor name path :: revDs)

    Can.PList [] ->
      Names.pure revDs

    Can.PList (hd::tl) ->
      destructTwo path hd (A.At region (Can.PList tl)) revDs

    Can.PCons hd tl ->
      destructTwo path hd tl revDs

    Can.PChr _ ->
      Names.pure revDs

    Can.PStr _ ->
      Names.pure revDs

    Can.PInt _ ->
      Names.pure revDs

    Can.PBool _ _ ->
      Names.pure revDs

    Can.PCtor _ _ (Can.Union _ _ _ opts) _ _ args ->
      case args of
        [Can.PatternCtorArg _ _ arg] ->
          case opts of
            Can.Normal -> destructHelp (Opt.Index Index.first path) arg revDs
            Can.Unbox  -> destructHelp (Opt.Unbox path) arg revDs
            Can.Enum   -> destructHelp (Opt.Index Index.first path) arg revDs

        _ ->
          case path of
            Opt.Root _ ->
              MList.foldlM Names.return Names.bind (destructCtorArg path) revDs args

            _ ->
              Names.bind (Names.generate) <| \name ->
              MList.foldlM Names.return Names.bind (destructCtorArg (Opt.Root name)) (Opt.Destructor name path :: revDs) args


destructTwo : Opt.Path -> Can.Pattern -> Can.Pattern -> TList Opt.Destructor -> Names.Tracker (TList Opt.Destructor)
destructTwo path a b revDs =
  case path of
    Opt.Root _ ->
      Names.andThen (destructHelp (Opt.Index Index.second path) b) <|
        destructHelp (Opt.Index Index.first path) a revDs

    _ ->
      Names.bind (Names.generate) <| \name ->
      let newRoot = Opt.Root name in
      Names.andThen (destructHelp (Opt.Index Index.second newRoot) b) <|
        destructHelp (Opt.Index Index.first newRoot) a (Opt.Destructor name path :: revDs)


destructCtorArg : Opt.Path -> TList Opt.Destructor -> Can.PatternCtorArg -> Names.Tracker (TList Opt.Destructor)
destructCtorArg path revDs (Can.PatternCtorArg index _ arg) =
  destructHelp (Opt.Index index path) arg revDs



-- TAIL CALL


optimizePotentialTailCallDef : Cycle -> Can.Def -> Names.Tracker Opt.Def
optimizePotentialTailCallDef cycle def =
  case def of
    Can.Def (A.At _ name) args expr ->
      optimizePotentialTailCall cycle name args expr

    Can.TypedDef (A.At _ name) _ typedArgs expr _ ->
      optimizePotentialTailCall cycle name (MList.map Tuple.first typedArgs) expr


optimizePotentialTailCall : Cycle -> Name.Name -> TList Can.Pattern -> Can.Expr -> Names.Tracker Opt.Def
optimizePotentialTailCall cycle name args expr =
  Names.bind (destructArgs args) <| \(argNames, destructors) ->
  Names.fmap (toTailDef name argNames destructors) <|
    optimizeTail cycle name argNames expr


optimizeTail : Cycle -> Name.Name -> TList Name.Name -> Can.Expr -> Names.Tracker Opt.Expr
optimizeTail cycle rootName argNames ((A.At _ expression) as locExpr) =
  case expression of
    Can.Call func args ->
      Names.bind (MList.traverse Names.pure Names.liftA2 (optimize cycle) args) <| \oargs ->

      let isMatchingName =
            case A.toValue func of
              Can.VarLocal      name -> rootName == name
              Can.VarTopLevel _ name -> rootName == name
              _                      -> False in

      if isMatchingName
        then
          case Index.indexedZipWith (\_ a b -> (a,b)) argNames oargs of
            Index.LengthMatch pairs ->
              Names.pure <| Opt.TailCall rootName pairs

            Index.LengthMismatch _ _ ->
              Names.bind (optimize cycle func) <| \ofunc ->
              Names.pure <| Opt.Call ofunc oargs
        else
          Names.bind (optimize cycle func) <| \ofunc ->
          Names.pure <| Opt.Call ofunc oargs

    Can.If branches finally ->
      let
        optimizeBranch (condition, branch) =
          Names.pure Tuple.pair
            |> Names.andMap (optimize cycle condition)
            |> Names.andMap (optimizeTail cycle rootName argNames branch)
      in
      Names.pure Opt.If
        |> Names.andMap (MList.traverse Names.pure Names.liftA2 optimizeBranch branches)
        |> Names.andMap (optimizeTail cycle rootName argNames finally)

    Can.Let def body ->
      Names.andThen (optimizeDef cycle def) <| optimizeTail cycle rootName argNames body

    Can.LetRec defs body ->
      case defs of
        [def] ->
          Names.pure Opt.Let
            |> Names.andMap (optimizePotentialTailCallDef cycle def)
            |> Names.andMap (optimizeTail cycle rootName argNames body)

        _ ->
          Names.bind (optimizeTail cycle rootName argNames body) <| \obody ->
          MList.foldlM Names.return Names.bind (\bod def -> optimizeDef cycle def bod) obody defs

    Can.LetDestruct pattern expr body ->
      Names.bind (destruct pattern) <| \(dname, destructors) ->
      Names.bind (optimize cycle expr) <| \oexpr ->
      Names.bind (optimizeTail cycle rootName argNames body) <| \obody ->
      Names.pure <|
        Opt.Let (Opt.Def dname oexpr) (MList.foldr Opt.Destruct obody destructors)

    Can.Case expr branches ->
      let
        optimizeBranch root (Can.CaseBranch pattern branch) =
          Names.bind (destructCase root pattern) <| \destructors ->
          Names.bind (optimizeTail cycle rootName argNames branch) <| \obranch ->
          Names.pure (pattern, MList.foldr Opt.Destruct obranch destructors)
      in
        Names.bind (Names.generate) <| \temp ->
        Names.bind (optimize cycle expr) <| \oexpr ->
        case oexpr of
          Opt.VarLocal root ->
            Names.fmap (Case.optimize temp root) <| MList.traverse Names.pure Names.liftA2 (optimizeBranch root) branches

          _ ->
            Names.bind (MList.traverse Names.pure Names.liftA2 (optimizeBranch temp) branches) <| \obranches ->
            Names.return <| Opt.Let (Opt.Def temp oexpr) (Case.optimize temp temp obranches)

    _ ->
      optimize cycle locExpr



-- DETECT TAIL CALLS


toTailDef : Name.Name -> TList Name.Name -> TList Opt.Destructor -> Opt.Expr -> Opt.Def
toTailDef name argNames destructors body =
  if hasTailCall body then
    Opt.TailDef name argNames (MList.foldr Opt.Destruct body destructors)
  else
    Opt.Def name (Opt.Function argNames (MList.foldr Opt.Destruct body destructors))


hasTailCall : Opt.Expr -> Bool
hasTailCall expression =
  case expression of
    Opt.TailCall _ _ ->
      True

    Opt.If branches finally ->
      hasTailCall finally || MList.any (hasTailCall << Tuple.second) branches

    Opt.Let _ body ->
      hasTailCall body

    Opt.Destruct _ body ->
      hasTailCall body

    Opt.Case _ _ decider jumps ->
      decidecHasTailCall decider || MList.any (hasTailCall << Tuple.second) jumps

    _ ->
      False


decidecHasTailCall : Opt.Decider Opt.Choice -> Bool
decidecHasTailCall decider =
  case decider of
    Opt.Leaf choice ->
      case choice of
        Opt.Inline expr ->
          hasTailCall expr

        Opt.Jump _ ->
          False

    Opt.Chain _ success failure ->
      decidecHasTailCall success || decidecHasTailCall failure

    Opt.FanOut _ tests fallback ->
      decidecHasTailCall fallback || MList.any (decidecHasTailCall << Tuple.second) tests
