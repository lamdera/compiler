{- MANUALLY FORMATTED -}
module Compiler.Generate.JavaScript exposing
  ( generate
  , generateForRepl
  --, generateForReplEndpoint
  --
  , CodeKind(..)
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.Kernel as K
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Generate.JavaScript.Builder as JS
import Compiler.Generate.JavaScript.Expression as Expr
import Compiler.Generate.JavaScript.Functions as Functions
import Compiler.Generate.JavaScript.Name as JsName
import Compiler.Generate.Mode as Mode
import Compiler.Json.Encode as JE
import Compiler.Reporting.Doc as D
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Set as Set


-- GENERATE


type alias Graph = Map.Map Opt.GlobalComparable Opt.Node
type alias Mains = Map.Map ModuleName.Comparable Opt.Main


generate : Mode.Mode -> Opt.GlobalGraph -> Mains -> String
generate mode (Opt.GlobalGraph graph _) mains =
  let
    state = Map.foldrWithKey (addMain mode graph) emptyState mains
  in
  "(function(scope){\n'use strict';"
  ++ Functions.functions
  ++ perfNote mode
  ++ stateToBuilder state
  ++ toMainExports mode mains
  ++ "}(this));"


addMain : Mode.Mode -> Graph -> ModuleName.Comparable -> Opt.Main -> State -> State
addMain mode graph home _ state =
  addGlobal mode graph state (Opt.toGlobalComparable <| Opt.Global (ModuleName.fromComparable home) "main")


perfNote : Mode.Mode -> String
perfNote mode =
  case mode of
    Mode.Prod _ ->
      ""

    Mode.Dev Mode.DevNormal ->
      "console.warn('Compiled in DEV mode. Follow the advice at "
      ++ (D.makeNakedLink "optimize")
      ++ " for better performance and smaller assets.');"

    Mode.Dev (Mode.DevDebug _) ->
      "console.warn('Compiled in DEBUG mode. Follow the advice at "
      ++ (D.makeNakedLink "optimize")
      ++ " for better performance and smaller assets.');"

    Mode.Dev (Mode.DevAsync _ _) ->
      "console.warn('Compiled in ASYNC mode. Follow the advice at "
      ++ (D.makeNakedLink "optimize")
      ++ " for better performance and smaller assets.');"



-- GENERATE FOR REPL


{- NEW: CodeKind -}
type CodeKind = ValueKind | HtmlKind

generateForRepl : Bool -> Bool -> L.Localizer -> Opt.GlobalGraph -> ModuleName.Canonical -> Name.Name -> Can.Annotation -> (CodeKind, String)
generateForRepl ansi htmlEnabled localizer (Opt.GlobalGraph graph _ as globalGraph) home name (Can.Forall _ tipe) =
  if htmlEnabled && isStaticHtml tipe then
    ( HtmlKind
    , generateHtmlForRepl globalGraph home name
    )
  else
    ( ValueKind
    , let
        mode = Mode.Dev Mode.DevNormal

        pipeAddGlobal moduleName defName state =
          addGlobal mode graph state (Opt.toGlobalComparable <| Opt.Global moduleName defName)

        evalState =
          emptyState
            |> pipeAddGlobal ModuleName.debug "toString"
            {- NEW: force_quit_ -}
            |> pipeAddGlobal home "force_quit_"
            |> pipeAddGlobal home name
      in
      ""--"process.on('uncaughtException', function(err) { process.stderr.write(err.toString() + '\\n'); process.exit(1); });"
      ++ Functions.functions
      ++ stateToBuilder evalState
      ++ print ansi localizer home name tipe
    )


{- NEW: isStaticHtml -}
isStaticHtml : Can.Type -> Bool
isStaticHtml tipe =
  case tipe of
    Can.TAlias _ _ _ (Can.Filled (Can.TType valModule valType _)) ->
      valModule == ModuleName.virtualDom && valType == Name.node

    _ ->
      False


print : Bool -> L.Localizer -> ModuleName.Canonical -> Name.Name -> Can.Type -> String
print ansi localizer home name tipe =
  let
    value = JsName.toBuilder (JsName.fromGlobal home name)
    toString = JsName.toBuilder (JsName.fromKernel Name.debug "toAnsiString")
    tipeDoc = RT.canToDoc localizer RT.None tipe
    bool = if ansi then "true" else "false"
  in
  "var _value = " ++ toString ++ "(" ++ bool ++ ", " ++ value ++ ");\n"
  ++ "var _type = " ++ JE.encodeUgly (JE.chars (D.toString tipeDoc)) ++ ";\n"
  ++ "function _print(t) { return _value + (" ++ bool ++ " ? '\u{001b}[90m' + t + '\u{001b}[0m' : t); }\n"
  ++ "var _result = (_value.length + 3 + _type.length >= 80 || _type.indexOf('\\n') >= 0)\n"
  ++ "  ? _print('\\n    : ' + _type.split('\\n').join('\\n      '))\n"
  ++ "  : _print(' : ' + _type);\n"
  {- NEW: force_quit_ -}
  ++ "var force_quit_ = " ++ JsName.toBuilder (JsName.fromGlobal home "force_quit_") ++ "();\n"
  --++ "console.log(_result);\n"


{- NEW: generateHtmlForRepl -}
generateHtmlForRepl : Opt.GlobalGraph -> ModuleName.Canonical -> Name.Name -> String
generateHtmlForRepl (Opt.GlobalGraph graph _) home name =
  let
    mode = Mode.Dev Mode.DevNormal
    mains = Map.singleton (ModuleName.toComparable home) Opt.Static
    state = addGlobal mode graph emptyState (Opt.toGlobalComparable <| Opt.Global home name)
    evalState = addStmt state (JS.Var (JsName.fromGlobal home Name.l_main) (JS.Ref (JsName.fromGlobal home name)))
  in
  "(function(scope){\n'use strict';"
  ++ Functions.functions
  ++ stateToBuilder evalState
  ++ toMainExports mode mains
  ++ "}(this));"




-- GRAPH TRAVERSAL STATE


type State =
  State
    {- revKernels -} (TList String)
    {- revBuilders -} (TList String)
    {- seenGlobals -} (Set.Set Opt.GlobalComparable)


emptyState : State
emptyState =
  State [] [] Set.empty


stateToBuilder : State -> String
stateToBuilder (State revKernels revBuilders _) =
  prependBuilders revKernels (prependBuilders revBuilders "")


prependBuilders : TList String -> String -> String
prependBuilders revBuilders monolith =
  MList.foldl (\m b -> b ++ m) monolith revBuilders



-- ADD DEPENDENCIES


addGlobal : Mode.Mode -> Graph -> State -> Opt.GlobalComparable -> State
addGlobal mode graph ((State revKernels builders seen) as state) global =
  if Set.member global seen then
    state
  else
    addGlobalHelp mode graph global <|
      State revKernels builders (Set.insert global seen)


addGlobalHelp : Mode.Mode -> Graph -> Opt.GlobalComparable -> State -> State
addGlobalHelp mode graph comparable state =
  let
    global = Opt.fromGlobalComparable comparable

    addDeps deps someState =
      Set.foldl (addGlobal mode graph) someState deps
  in
  case Map.ex graph comparable of
    Opt.Define expr deps ->
      {- NEW: Expr.generateAsync -}
      let
        ( isLocalAppDef, moduleName, defName ) =
          case global of
            Opt.Global (ModuleName.Canonical pkg mName) dName -> ( pkg == Pkg.dummyName, mName, dName )

        ( isAsyncDef, maybeBpNames ) =
          case expr of
            Opt.Function _ _ ->
              ( Mode.isAsyncActive mode && isLocalAppDef, Nothing )
            Opt.Call func args ->
              ( Mode.isAsyncActive mode && isLocalAppDef && Expr.isBreakpointDef func args, Just ( moduleName, defName ) )
            _ ->
              ( False, Nothing )

        exprMode =
          if isAsyncDef then mode else Mode.deActivate mode
      in
      addStmt (addDeps deps state) (
        var global (Expr.generateAsync isAsyncDef maybeBpNames exprMode expr)
      )

    Opt.DefineTailFunc argNames body deps ->
      addStmt (addDeps deps state) (
        let (Opt.Global _ name) = global in
        var global (Expr.generateTailDef mode name argNames body)
      )

    Opt.Ctor index arity ->
      addStmt state (
        var global (Expr.generateCtor mode global index arity)
      )

    Opt.Link linkedGlobal ->
      addGlobal mode graph state (Opt.toGlobalComparable linkedGlobal)

    Opt.Cycle names values functions deps ->
      addStmt (addDeps deps state) (
        generateCycle mode global names values functions
      )

    Opt.Manager effectsType ->
      generateManager mode graph global effectsType state

    Opt.Kernel chunks deps ->
      if isDebugger global && not (Mode.isDebug mode) then
        state
      else
        addKernel (addDeps deps state) (generateKernel mode chunks)

    Opt.Enum index ->
      addStmt state (
        generateEnum mode global index
      )

    Opt.Box ->
      addStmt (addGlobal mode graph state (Opt.toGlobalComparable identity_)) (
        generateBox mode global
      )

    Opt.PortIncoming decoder deps ->
      addStmt (addDeps deps state) (
        generatePort mode global "incomingPort" decoder
      )

    Opt.PortOutgoing encoder deps ->
      addStmt (addDeps deps state) (
        generatePort mode global "outgoingPort" encoder
      )


addStmt : State -> JS.Stmt -> State
addStmt state stmt =
  addBuilder state (JS.stmtToBuilder stmt)


addBuilder : State -> String -> State
addBuilder (State revKernels revBuilders seen) builder =
  State revKernels (builder::revBuilders) seen


addKernel : State -> String -> State
addKernel (State revKernels revBuilders seen) kernel =
  State (kernel::revKernels) revBuilders seen


var : Opt.Global -> Expr.Code -> JS.Stmt
var (Opt.Global home name) code =
  JS.Var (JsName.fromGlobal home name) (Expr.codeToExpr code)


isDebugger : Opt.Global -> Bool
isDebugger (Opt.Global (ModuleName.Canonical _ home) _) =
  home == Name.debugger



-- GENERATE CYCLES


generateCycle : Mode.Mode -> Opt.Global -> TList Name.Name -> TList (Name.Name, Opt.Expr) -> TList Opt.Def -> JS.Stmt
generateCycle mode (Opt.Global home _) names values functions =
  JS.Block
    [ JS.Block <| MList.map (generateCycleFunc mode home) functions
    , JS.Block <| MList.map (generateSafeCycle mode home) values
    , case MList.map (generateRealCycle home) values of
        [] ->
          JS.EmptyStmt

        ((_::_) as realBlock) ->
          case mode of
            Mode.Prod _ ->
              JS.Block realBlock

            Mode.Dev _ ->
              JS.Try (JS.Block realBlock) JsName.dollar <| JS.Throw <| JS.CString <|
                "Some top-level definitions from `" ++ Name.toBuilder (ModuleName.getModule home) ++ "` are causing infinite recursion:\\n"
                ++ drawCycle names
                ++ "\\n\\nThese errors are very tricky, so read "
                ++ D.makeNakedLink "bad-recursion"
                ++ " to learn how to fix it!"
    ]


generateCycleFunc : Mode.Mode -> ModuleName.Canonical -> Opt.Def -> JS.Stmt
generateCycleFunc mode home def =
  case def of
    Opt.Def name expr ->
      JS.Var (JsName.fromGlobal home name) (Expr.codeToExpr (Expr.generate mode expr))

    Opt.TailDef name args expr ->
      JS.Var (JsName.fromGlobal home name) (Expr.codeToExpr (Expr.generateTailDef mode name args expr))


generateSafeCycle : Mode.Mode -> ModuleName.Canonical -> (Name.Name, Opt.Expr) -> JS.Stmt
generateSafeCycle mode home (name, expr) =
  JS.FunctionStmt (JsName.fromCycle home name) [] <|
    Expr.codeToStmtList (Expr.generate mode expr)


generateRealCycle : ModuleName.Canonical -> (Name.Name, expr) -> JS.Stmt
generateRealCycle home (name, _) =
  let
    safeName = JsName.fromCycle home name
    realName = JsName.fromGlobal home name
  in
  JS.Block
    [ JS.Var realName (JS.Call (JS.Ref safeName) [])
    , JS.ExprStmt <| JS.Assign (JS.LRef safeName) <|
        JS.Function Nothing [] [ JS.Return (JS.Ref realName) ]
    ]


drawCycle : TList Name.Name -> String
drawCycle names =
  let
    topLine       = "\\n  ┌─────┐"
    nameLine name = "\\n  │    " ++ Name.toBuilder name
    midLine       = "\\n  │     ↓"
    bottomLine    = "\\n  └─────┘"
  in
  String.concat (topLine :: MList.intersperse midLine (MList.map nameLine names) ++ [ bottomLine ])



-- GENERATE KERNEL


generateKernel : Mode.Mode -> TList K.Chunk -> String
generateKernel mode chunks =
  MList.foldr (addChunk mode) "" chunks


addChunk : Mode.Mode -> K.Chunk -> String -> String
addChunk mode chunk builder =
  case chunk of
    K.JS javascript ->
      javascript ++ builder

    K.ElmVar home name ->
      JsName.toBuilder (JsName.fromGlobal home name) ++ builder

    K.JsVar home name ->
      JsName.toBuilder (JsName.fromKernel home name) ++ builder

    K.ElmField name ->
      JsName.toBuilder (Expr.generateField mode name) ++ builder

    K.JsField int ->
      JsName.toBuilder (JsName.fromInt int) ++ builder

    K.JsEnum int ->
      String.fromInt int ++ builder

    K.Debug ->
      case mode of
        Mode.Dev _ ->
          builder

        Mode.Prod _ ->
          "_UNUSED" ++ builder

    K.Prod ->
      case mode of
        Mode.Dev _ ->
          "_UNUSED" ++ builder

        Mode.Prod _ ->
          builder

    K.Async ->
      case mode of
        Mode.Dev (Mode.DevAsync _ _) ->
          builder

        _ ->
          "_UNUSED" ++ builder



-- GENERATE ENUM


generateEnum : Mode.Mode -> Opt.Global -> Index.ZeroBased -> JS.Stmt
generateEnum mode ((Opt.Global home name) as global) index =
  JS.Var (JsName.fromGlobal home name) <|
    case mode of
      Mode.Dev _ ->
        Expr.codeToExpr (Expr.generateCtor mode global index 0)

      Mode.Prod _ ->
        JS.CInt (Index.toMachine index)



-- GENERATE BOX


generateBox : Mode.Mode -> Opt.Global -> JS.Stmt
generateBox mode ((Opt.Global home name) as global) =
  JS.Var (JsName.fromGlobal home name) <|
    case mode of
      Mode.Dev _ ->
        Expr.codeToExpr (Expr.generateCtor mode global Index.first 1)

      Mode.Prod _ ->
        JS.Ref (JsName.fromGlobal ModuleName.basics Name.identity_)


identity_ : Opt.Global
identity_ =
  Opt.Global ModuleName.basics Name.identity_



-- GENERATE PORTS


generatePort : Mode.Mode -> Opt.Global -> Name.Name -> Opt.Expr -> JS.Stmt
generatePort mode (Opt.Global home name) makePort converter =
  JS.Var (JsName.fromGlobal home name) <|
    JS.Call (JS.Ref (JsName.fromKernel Name.platform makePort))
      [ JS.CString (Name.toBuilder name)
      , Expr.codeToExpr (Expr.generate mode converter)
      ]



-- GENERATE MANAGER


generateManager : Mode.Mode -> Graph -> Opt.Global -> Opt.EffectsType -> State -> State
generateManager mode graph (Opt.Global ((ModuleName.Canonical _ moduleName) as home) _) effectsType state =
  let
    managerLVar =
      JS.LBracket
        (JS.Ref (JsName.fromKernel Name.platform "effectManagers"))
        (JS.CString (Name.toBuilder moduleName))

    (deps, args, stmts) =
      generateManagerHelp home effectsType

    createManager =
      JS.ExprStmt <| JS.Assign managerLVar <|
        JS.Call (JS.Ref (JsName.fromKernel Name.platform "createManager")) args
  in
  addStmt (MList.foldl (addGlobal mode graph) state deps) <|
    JS.Block (createManager :: stmts)


generateLeaf : ModuleName.Canonical -> Name.Name -> JS.Stmt
generateLeaf ((ModuleName.Canonical _ moduleName) as home) name =
  JS.Var (JsName.fromGlobal home name) <|
    JS.Call leaf [ JS.CString (Name.toBuilder moduleName) ]


leaf : JS.Expr
leaf =
  JS.Ref (JsName.fromKernel Name.platform "leaf")


generateManagerHelp : ModuleName.Canonical -> Opt.EffectsType -> (TList Opt.GlobalComparable, TList JS.Expr, TList JS.Stmt)
generateManagerHelp home effectsType =
  let
    dep name = Opt.toGlobalComparable <| Opt.Global home name
    ref name = JS.Ref (JsName.fromGlobal home name)
  in
  case effectsType of
    Opt.CCmd ->
      ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "cmdMap" ]
      , [ ref "init", ref "onEffects", ref "onSelfMsg", ref "cmdMap" ]
      , [ generateLeaf home "command" ]
      )

    Opt.CSub ->
      ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "subMap" ]
      , [ ref "init", ref "onEffects", ref "onSelfMsg", JS.CInt 0, ref "subMap" ]
      , [ generateLeaf home "subscription" ]
      )

    Opt.Fx ->
      ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "cmdMap", dep "subMap" ]
      , [ ref "init", ref "onEffects", ref "onSelfMsg", ref "cmdMap", ref "subMap" ]
      , [ generateLeaf home "command"
        , generateLeaf home "subscription"
        ]
      )



-- MAIN EXPORTS


toMainExports : Mode.Mode -> Mains -> String
toMainExports mode mains =
  let
    export = JsName.fromKernel Name.platform "export"
    exports = generateExports mode (Map.foldrWithKey addToTrie emptyTrie mains)
  in
  JsName.toBuilder export ++ "(" ++ exports ++ ");"


generateExports : Mode.Mode -> Trie -> String
generateExports mode (Trie maybeMain subs) =
  let
    starter end =
      case maybeMain of
        Nothing ->
          "{"

        Just (home, main) ->
          "{'init':"
          ++ JS.exprToBuilder (Expr.generateMain mode home main)
          ++ end

    {- NEW: lastLine -}
    lastLine =
      if Mode.isAsyncActive mode then ", 'get_state': _Breakpoint_get_state }" else "}"
  in
    case Map.toList subs of
      [] ->
        starter "" ++ lastLine

      (name, subTrie) :: otherSubTries ->
        starter "," ++
        "'" ++ name ++ "':"
        ++ generateExports (Mode.deActivate mode) subTrie
        ++ MList.foldl (addSubTrie mode) lastLine otherSubTries


addSubTrie : Mode.Mode -> String -> (Name.Name, Trie) -> String
addSubTrie mode end (name, trie) =
  ",'" ++ name ++ "':" ++ generateExports mode trie ++ end



-- BUILD TRIES


type Trie =
  Trie
    {- main -} (Maybe (ModuleName.Canonical, Opt.Main))
    {- subs -} (Map.Map Name.Name Trie)


emptyTrie : Trie
emptyTrie =
  Trie Nothing Map.empty


addToTrie : ModuleName.Comparable -> Opt.Main -> Trie -> Trie
addToTrie comparable main trie =
  let ((ModuleName.Canonical _ moduleName) as home) = ModuleName.fromComparable comparable in
  merge trie <| segmentsToTrie home (Name.splitDots moduleName) main


segmentsToTrie : ModuleName.Canonical -> TList Name.Name -> Opt.Main -> Trie
segmentsToTrie home segments main =
  case segments of
    [] ->
      Trie (Just (home, main)) Map.empty

    segment :: otherSegments ->
      Trie Nothing (Map.singleton segment (segmentsToTrie home otherSegments main))


merge : Trie -> Trie -> Trie
merge (Trie main1 subs1) (Trie main2 subs2) =
  Trie
    (checkedMerge main1 main2)
    (Map.unionWith merge subs1 subs2)


checkedMerge : Maybe a -> Maybe a -> Maybe a
checkedMerge a b =
  case (a, b) of
    (Nothing, main) ->
      main

    (main, Nothing) ->
      main

    (Just _, Just _) ->
      Debug.todo "cannot have two modules with the same name"
