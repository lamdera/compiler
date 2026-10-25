{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Generate.JavaScript
  ( generate
  , generateEsm
  , generateForRepl
  , generateForReplEndpoint
  , JsCache, emptyJsCache, generateCached -- @LAMDERA the lamdera live session
  , Listing(..), Emitted(..), EmitKind(..), jsCacheListing, patchHook -- @LAMDERA lamdera live hot patches, see Lamdera.HotPatch
  )
  where


import Prelude hiding (cycle, print)
import qualified Data.ByteString as BS -- @LAMDERA
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL -- @LAMDERA
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#) -- @LAMDERA
import Data.Monoid ((<>))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Set as Set

import qualified Crash
import qualified String as S

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified Data.Index as Index
import qualified Elm.Kernel as K
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Expression as Expr
import qualified Generate.JavaScript.Functions as Functions
import qualified Generate.JavaScript.Name as JsName
import qualified Generate.Mode as Mode
import qualified Reporting.Doc as D
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L


import qualified Lamdera.Injection
import qualified Lamdera.Wire3.Direct -- @LAMDERA JsCache

-- GENERATE


type Graph = Map.Map Opt.Global Opt.Node
type FnArgLookup = ModuleName.Canonical -> N.Name -> Maybe Int
type Mains = Map.Map ModuleName.Canonical Opt.Main


generate :: Mode.Mode -> Opt.GlobalGraph -> Mains -> B.Builder
generate mode globalGraph mains =
  fst (generateFrom emptyState mode globalGraph mains) -- @LAMDERA see generateCached


-- @LAMDERA the original `generate`, from a given starting state
generateFrom :: State -> Mode.Mode -> Opt.GlobalGraph -> Mains -> (B.Builder, State)
generateFrom startState mode (Opt.GlobalGraph graph_ _) mains =
  generateFromGraph startState mempty mode (Lamdera.Injection.graphModifications mode mains graph_) mains


-- @LAMDERA generateFrom, given the graph after Lamdera.Injection.graphModifications, and
-- with `inside` added at the end of the scope the definitions are in
generateFromGraph :: State -> B.Builder -> Mode.Mode -> Graph -> Mains -> (B.Builder, State)
generateFromGraph startState inside mode graph mains =
  let
    state = Map.foldrWithKey (addMain mode graph) startState mains
  in
  flip (,) state $
  Lamdera.Injection.versionsComment <>
  "(function(scope){\n'use strict';"
  <> Functions.functions
  -- <> perfNote mode -- @NOTE given user never manages JS generation in Lamdera, hide the perf note
  <> stateToBuilder state
  <> toMainExports mode mains
  <> Lamdera.Injection.source mode mains
  <> inside
  <> "}(this));"
  <> "\n" <> Lamdera.Injection.elmPkgJs mode mains <> "\n"

addMain :: Mode.Mode -> Graph -> ModuleName.Canonical -> Opt.Main -> State -> State
addMain mode graph home _ state =
  addGlobal mode graph state (Opt.Global home N.main)


perfNote :: Mode.Mode -> B.Builder
perfNote mode =
  case mode of
    Mode.Prod _ ->
      ""

    Mode.Dev Nothing ->
      "console.warn('Compiled in DEV mode. Follow the advice at "
      <> B.stringUtf8 (D.makeNakedLink "optimize")
      <> " for better performance and smaller assets.');"

    Mode.Dev (Just _) ->
      "console.warn('Compiled in DEBUG mode. Follow the advice at "
      <> B.stringUtf8 (D.makeNakedLink "optimize")
      <> " for better performance and smaller assets.');"



-- GENERATE FOR REPL


generateForRepl :: Bool -> L.Localizer -> Opt.GlobalGraph -> ModuleName.Canonical -> N.Name -> Can.Annotation -> B.Builder
generateForRepl ansi localizer (Opt.GlobalGraph graph _) home name (Can.Forall _ tipe) =
  let
    mode = Mode.Dev Nothing
    debugState = addGlobal mode graph emptyState (Opt.Global ModuleName.debug [N.ascii|toString|])
    evalState = addGlobal mode graph debugState (Opt.Global home name)
  in
  "process.on('uncaughtException', function(err) { process.stderr.write(err.toString() + '\\n'); process.exit(1); });"
  <> Functions.functions
  <> stateToBuilder evalState
  <> print ansi localizer home name tipe


print :: Bool -> L.Localizer -> ModuleName.Canonical -> N.Name -> Can.Type -> B.Builder
print ansi localizer home name tipe =
  let
    value = JsName.toBuilder (JsName.fromGlobal home name)
    toString = JsName.toBuilder (JsName.fromKernel Module.kernel_debug [N.ascii|toAnsiString|])
    tipeDoc = RT.canToDoc localizer RT.None tipe
    bool = if ansi then "true" else "false"
  in
  "var _value = " <> toString <> "(" <> bool <> ", " <> value <> ");\n\
  \var _type = " <> B.stringUtf8 (show (D.toString tipeDoc)) <> ";\n\
  \function _print(t) { console.log(_value + (" <> bool <> " ? '\x1b[90m' + t + '\x1b[0m' : t)); }\n\
  \if (_value.length + 3 + _type.length >= 80 || _type.indexOf('\\n') >= 0) {\n\
  \    _print('\\n    : ' + _type.split('\\n').join('\\n      '));\n\
  \} else {\n\
  \    _print(' : ' + _type);\n\
  \}\n"



-- GENERATE FOR REPL ENDPOINT


generateForReplEndpoint :: L.Localizer -> Opt.GlobalGraph -> ModuleName.Canonical -> Maybe N.Name -> Can.Annotation -> B.Builder
generateForReplEndpoint localizer (Opt.GlobalGraph graph _) home maybeName (Can.Forall _ tipe) =
  let
    name = maybe N.replValueToPrint id maybeName
    mode = Mode.Dev Nothing
    debugState = addGlobal mode graph emptyState (Opt.Global ModuleName.debug [N.ascii|toString|])
    evalState = addGlobal mode graph debugState (Opt.Global home name)
  in
  Functions.functions
  <> stateToBuilder evalState
  <> postMessage localizer home maybeName tipe


postMessage :: L.Localizer -> ModuleName.Canonical -> Maybe N.Name -> Can.Type -> B.Builder
postMessage localizer home maybeName tipe =
  let
    name = maybe N.replValueToPrint id maybeName
    value = JsName.toBuilder (JsName.fromGlobal home name)
    toString = JsName.toBuilder (JsName.fromKernel Module.kernel_debug [N.ascii|toAnsiString|])
    tipeDoc = RT.canToDoc localizer RT.None tipe
    toName n = "\"" <> N.toBuilder n <> "\""
  in
  "self.postMessage({\n\
  \  name: " <> maybe "null" toName maybeName <> ",\n\
  \  value: " <> toString <> "(true, " <> value <> "),\n\
  \  type: " <> B.stringUtf8 (show (D.toString tipeDoc)) <> "\n\
  \});\n"



-- GRAPH TRAVERSAL STATE


data State =
  State
    { _revKernels :: [B.Builder]
    , _revBuilders :: [B.Builder]
    , _seenGlobals :: Set.Set Opt.Global
    , _caching :: !Caching -- @LAMDERA see JS CACHE
    }


emptyState :: State
emptyState =
  State mempty [] Set.empty NoCaching


stateToBuilder :: State -> B.Builder
stateToBuilder (State revKernels revBuilders _ _) =
  prependBuilders revKernels (prependBuilders revBuilders mempty)


prependBuilders :: [B.Builder] -> B.Builder -> B.Builder
prependBuilders revBuilders monolith =
  List.foldl' (\m b -> b <> m) monolith revBuilders



-- ADD DEPENDENCIES


addGlobal :: Mode.Mode -> Graph -> State -> Opt.Global -> State
addGlobal mode graph state@(State revKernels builders seen caching) global =
  if Set.member global seen then
    state
  else
    addGlobalHelp mode graph global $
      State revKernels builders (Set.insert global seen) caching


addGlobalHelp :: Mode.Mode -> Graph -> Opt.Global -> State -> State
addGlobalHelp mode graph global state =
  let
    addDeps deps someState =
      Set.foldl' (addGlobal mode graph) someState deps

    globalToChars (Opt.Global (ModuleName.Canonical p h) n) =
      Module.toChars h ++ "." ++ N.toChars n ++ " in " ++ Pkg.toChars p

    argLookup = makeArgLookup graph
  in
  case $(Map.require 'addGlobalHelp) global graph globalToChars of
    -- @LAMDERA
    -- (addStmtFor is addStmt that can reuse the statement from the last build, see JS CACHE)
    node@(Opt.Define (Opt.Function args body) deps)
      | length args > 1 ->
          addStmtFor EFunction argLookup global node deps
            (addDeps deps state)
            (fn global args (Expr.generateFunctionImplementation mode argLookup args body))

    node@(Opt.Define expr deps) ->
      addStmtFor (case expr of { Opt.Function _ _ -> EFunction; _ -> EValue }) argLookup global node deps (addDeps deps state) (
        var global (Expr.generate mode argLookup expr)
      )

    node@(Opt.DefineTailFunc argNames body deps) ->
      addStmtFor EFunction argLookup global node deps (addDeps deps state) (
        let (Opt.Global _ name) = global in
        var global (Expr.generateTailDef mode argLookup name argNames body)
      )

    -- @LAMDERA
    node@(Opt.Ctor index arity)
      | arity > 1 ->
          addStmtFor EConstant argLookup global node Set.empty
            state
            (ctor global arity (Expr.generateCtorImplementation mode global index arity))

    node@(Opt.Ctor index arity) ->
      addStmtFor EConstant argLookup global node Set.empty state (
        var global (Expr.generateCtor mode global index arity)
      )

    Opt.Link linkedGlobal ->
      addLink global linkedGlobal $ -- @LAMDERA see JS CACHE
      addGlobal mode graph state linkedGlobal

    node@(Opt.Cycle names values functions deps) ->
      addStmtFor (if null values then EFunction else ECycleValues) argLookup global node deps (addDeps deps state) (
        generateCycle mode argLookup global names values functions
      )

    Opt.Manager effectsType ->
      generateManager mode graph global effectsType state

    node@(Opt.Kernel chunks deps) ->
      if isDebugger global && not (Mode.isDebug mode) then
        state
      else
        addKernelFor global node (addDeps deps state) (generateKernel mode chunks) -- @LAMDERA was addKernel, see JS CACHE

    node@(Opt.Enum index) ->
      addStmtFor EConstant argLookup global node Set.empty state (
        generateEnum mode global index
      )

    node@Opt.Box ->
      addStmtFor EConstant argLookup global node (Set.singleton identity) (addGlobal mode graph state identity) (
        generateBox mode global
      )

    node@(Opt.PortIncoming decoder deps) ->
      addStmtFor EPort argLookup global node deps (addDeps deps state) (
        generatePort mode global [N.ascii|incomingPort|] decoder
      )

    node@(Opt.PortOutgoing encoder deps) ->
      addStmtFor EPort argLookup global node deps (addDeps deps state) (
        generatePort mode global [N.ascii|outgoingPort|] encoder
      )


addStmt :: State -> JS.Stmt -> State
addStmt state stmt =
  addBuilder state (JS.stmtToBuilder stmt)


addBuilder :: State -> B.Builder -> State
addBuilder (State revKernels revBuilders seen caching) builder =
  State revKernels (builder:revBuilders) seen caching


addKernel :: State -> B.Builder -> State
addKernel (State revKernels revBuilders seen caching) kernel =
  State (kernel:revKernels) revBuilders seen caching


var :: Opt.Global -> Expr.Code -> JS.Stmt
var (Opt.Global home name) code =
  JS.Var (JsName.fromGlobal home name) (Expr.codeToExpr code)


isDebugger :: Opt.Global -> Bool
isDebugger (Opt.Global (ModuleName.Canonical _ home) _) =
  Module.toString home == Module.kernelToString Module.kernel_debugger



-- GENERATE CYCLES


generateCycle :: Mode.Mode -> FnArgLookup -> Opt.Global -> [N.Name] -> [(N.Name, Opt.Expr)] -> [Opt.Def] -> JS.Stmt
generateCycle mode argLookup (Opt.Global home _) names values functions =
  JS.Block
    [ JS.Block $ map (generateCycleFunc mode argLookup home) functions
    , JS.Block $ map (generateSafeCycle mode argLookup home) values
    , case map (generateRealCycle home) values of
        [] ->
          JS.EmptyStmt

        realBlock@(_:_) ->
            case mode of
              Mode.Prod _ ->
                JS.Block realBlock

              Mode.Dev _ ->
                JS.Try (JS.Block realBlock) JsName.dollar $ JS.Throw $ JS.String $
                  "Some top-level definitions from `" <> Module.toBuilder (ModuleName._module home) <> "` are causing infinite recursion:\\n"
                  <> drawCycle names
                  <> "\\n\\nThese errors are very tricky, so read "
                  <> B.stringUtf8 (D.makeNakedLink "bad-recursion")
                  <> " to learn how to fix it!"
    ]


generateCycleFunc :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Opt.Def -> JS.Stmt
generateCycleFunc mode argLookup home def =
  case def of
    -- @LAMDERA
    Opt.Def name (Opt.Function args body)
      | length args > 1 ->
          fn (Opt.Global home name) args (Expr.generateFunctionImplementation mode argLookup args body)
    
    Opt.Def name expr ->
      JS.Var (JsName.fromGlobal home name) (Expr.codeToExpr (Expr.generate mode argLookup expr))
    
    -- @LAMDERA
    Opt.TailDef name args expr
      | length args > 1 ->
          let
            directFnName = JsName.fromGlobalDirectFn home name
            argNames = map JsName.fromLocal args
          in
          JS.Block
            [ JS.Var directFnName (Expr.codeToExpr (Expr.generateTailDefImplementation mode argLookup name args expr))
            , JS.Var (JsName.fromGlobal home name) (Expr.codeToExpr (Expr.generateCurriedFunctionRef argNames directFnName))
            ]
    
    Opt.TailDef name args expr ->
      JS.Var (JsName.fromGlobal home name) (Expr.codeToExpr (Expr.generateTailDef mode argLookup name args expr))


generateSafeCycle :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> (N.Name, Opt.Expr) -> JS.Stmt
generateSafeCycle mode argLookup home (name, expr) =
  JS.FunctionStmt (JsName.fromCycle home name) [] $
    Expr.codeToStmtList (Expr.generate mode argLookup expr)


generateRealCycle :: ModuleName.Canonical -> (N.Name, expr) -> JS.Stmt
generateRealCycle home (name, _) =
  let
    safeName = JsName.fromCycle home name
    realName = JsName.fromGlobal home name
  in
  JS.Block
    [ JS.Var realName (JS.Call (JS.Ref safeName) [])
    , JS.ExprStmt $ JS.Assign (JS.LRef safeName) $
        JS.Function Nothing [] [ JS.Return (JS.Ref realName) ]
    ]


drawCycle :: [N.Name] -> B.Builder
drawCycle names =
  let
    topLine       = "\\n  ┌─────┐"
    nameLine name = "\\n  │    " <> N.toBuilder name
    midLine       = "\\n  │     ↓"
    bottomLine    = "\\n  └─────┘"
  in
  mconcat (topLine : List.intersperse midLine (map nameLine names) ++ [ bottomLine ])



-- GENERATE KERNEL


generateKernel :: Mode.Mode -> [K.Chunk] -> B.Builder
generateKernel mode chunks =
  List.foldr (addChunk mode) mempty chunks


addChunk :: Mode.Mode -> K.Chunk -> B.Builder -> B.Builder
addChunk mode chunk builder =
  case chunk of
    K.JS javascript ->
      B.byteString javascript <> builder

    K.ElmVar home name ->
      JsName.toBuilder (JsName.fromGlobal home name) <> builder

    K.JsVar home name ->
      JsName.toBuilder (JsName.fromKernel home name) <> builder

    K.ElmField name ->
      JsName.toBuilder (Expr.generateField mode name) <> builder

    K.JsField int ->
      JsName.toBuilder (JsName.fromInt int) <> builder

    K.JsEnum int ->
      B.intDec int <> builder

    K.Debug ->
      case mode of
        Mode.Dev _ ->
          builder

        Mode.Prod _ ->
          "_UNUSED" <> builder

    K.Prod ->
      case mode of
        Mode.Dev _ ->
          "_UNUSED" <> builder

        Mode.Prod _ ->
          builder



-- GENERATE ENUM


generateEnum :: Mode.Mode -> Opt.Global -> Index.ZeroBased -> JS.Stmt
generateEnum mode global@(Opt.Global home name) index =
  JS.Var (JsName.fromGlobal home name) $
    case mode of
      Mode.Dev _ ->
        Expr.codeToExpr (Expr.generateCtor mode global index 0)

      Mode.Prod _ ->
        JS.Int (Index.toMachine index)



-- GENERATE BOX


generateBox :: Mode.Mode -> Opt.Global -> JS.Stmt
generateBox mode global@(Opt.Global home name) =
  JS.Var (JsName.fromGlobal home name) $
    case mode of
      Mode.Dev _ ->
        Expr.codeToExpr (Expr.generateCtor mode global Index.first 1)

      Mode.Prod _ ->
        JS.Ref (JsName.fromGlobal ModuleName.basics N.identity)


{-# NOINLINE identity #-}
identity :: Opt.Global
identity =
  Opt.Global ModuleName.basics N.identity



-- GENERATE PORTS


generatePort :: Mode.Mode -> Opt.Global -> N.Name -> Opt.Expr -> JS.Stmt
generatePort mode (Opt.Global home name) makePort converter =
  JS.Var (JsName.fromGlobal home name) $
    JS.Call (JS.Ref (JsName.fromKernel Module.kernel_platform makePort))
      [ JS.String (N.toBuilder name)
      , Expr.codeToExpr (Expr.generate mode (\_ _ -> Nothing) converter)
      ]



-- GENERATE MANAGER


generateManager :: Mode.Mode -> Graph -> Opt.Global -> Opt.EffectsType -> State -> State
generateManager mode graph global@(Opt.Global home@(ModuleName.Canonical _ moduleName) _) effectsType state =
  let
    managerLVar =
      JS.LBracket
        (JS.Ref (JsName.fromKernel Module.kernel_platform [N.ascii|effectManagers|]))
        (JS.String (Module.toBuilder moduleName))

    (deps, args, stmts) =
      generateManagerHelp home effectsType

    createManager =
      JS.ExprStmt $ JS.Assign managerLVar $
        JS.Call (JS.Ref (JsName.fromKernel Module.kernel_platform [N.ascii|createManager|])) args
  in
  addRecorded EManager global (Set.fromList deps) (List.foldl' (addGlobal mode graph) state deps) $ -- @LAMDERA was addStmt, see JS CACHE
    JS.Block (createManager : stmts)


generateLeaf :: ModuleName.Canonical -> N.Name -> JS.Stmt
generateLeaf home@(ModuleName.Canonical _ moduleName) name =
  JS.Var (JsName.fromGlobal home name) $
    JS.Call leaf [ JS.String (Module.toBuilder moduleName) ]



{-# NOINLINE leaf #-}
leaf :: JS.Expr
leaf =
  JS.Ref (JsName.fromKernel Module.kernel_platform [N.ascii|leaf|])


generateManagerHelp :: ModuleName.Canonical -> Opt.EffectsType -> ([Opt.Global], [JS.Expr], [JS.Stmt])
generateManagerHelp home effectsType =
  let
    dep name = Opt.Global home name
    ref name = JS.Ref (JsName.fromGlobal home name)
  in
  case effectsType of
    Opt.Cmd ->
      ( [ dep [N.ascii|init|], dep [N.ascii|onEffects|], dep [N.ascii|onSelfMsg|], dep [N.ascii|cmdMap|] ]
      , [ ref [N.ascii|init|], ref [N.ascii|onEffects|], ref [N.ascii|onSelfMsg|], ref [N.ascii|cmdMap|] ]
      , [ generateLeaf home [N.ascii|command|] ]
      )

    Opt.Sub ->
      ( [ dep [N.ascii|init|], dep [N.ascii|onEffects|], dep [N.ascii|onSelfMsg|], dep [N.ascii|subMap|] ]
      , [ ref [N.ascii|init|], ref [N.ascii|onEffects|], ref [N.ascii|onSelfMsg|], JS.Int 0, ref [N.ascii|subMap|] ]
      , [ generateLeaf home [N.ascii|subscription|] ]
      )

    Opt.Fx ->
      ( [ dep [N.ascii|init|], dep [N.ascii|onEffects|], dep [N.ascii|onSelfMsg|], dep [N.ascii|cmdMap|], dep [N.ascii|subMap|] ]
      , [ ref [N.ascii|init|], ref [N.ascii|onEffects|], ref [N.ascii|onSelfMsg|], ref [N.ascii|cmdMap|], ref [N.ascii|subMap|] ]
      , [ generateLeaf home [N.ascii|command|]
        , generateLeaf home [N.ascii|subscription|]
        ]
      )



-- MAIN EXPORTS


toMainExports :: Mode.Mode -> Mains -> B.Builder
toMainExports mode mains =
  let
    export = JsName.fromKernel Module.kernel_platform [N.ascii|export|]
    exports = generateExports mode (Map.foldrWithKey addToTrie emptyTrie mains)
  in
  JsName.toBuilder export <> "(" <> exports <> ");"


generateExports :: Mode.Mode -> Trie -> B.Builder
generateExports mode (Trie maybeMain subs) =
  let
    starter end =
      case maybeMain of
        Nothing ->
          "{"

        Just (home, main) ->
          "{'init':"
          <> JS.exprToBuilder (Expr.generateMain mode (\_ _ -> Nothing) home main)
          <> end
    in
    case Map.toList subs of
      [] ->
        starter "" <> "}"

      (name, subTrie) : otherSubTries ->
        starter "," <>
        "'" <> S.toBuilder name <> "':"
        <> generateExports mode subTrie
        <> List.foldl' (addSubTrie mode) "}" otherSubTries


addSubTrie :: Mode.Mode -> B.Builder -> (S.String, Trie) -> B.Builder
addSubTrie mode end (name, trie) =
  ",'" <> S.toBuilder name <> "':" <> generateExports mode trie <> end



-- BUILD TRIES


data Trie =
  Trie
    { _main :: Maybe (ModuleName.Canonical, Opt.Main)
    , _subs :: Map.Map S.String Trie
    }


emptyTrie :: Trie
emptyTrie =
  Trie Nothing Map.empty


addToTrie :: ModuleName.Canonical -> Opt.Main -> Trie -> Trie
addToTrie home@(ModuleName.Canonical _ h) main trie =
  merge trie $ segmentsToTrie home (S.split 0x2E#Word8 {-.-} (Module.toString h)) main


segmentsToTrie :: ModuleName.Canonical -> [S.String] -> Opt.Main -> Trie
segmentsToTrie home segments main =
  case segments of
    []       -> Trie (Just (home, main)) Map.empty
    seg:segs -> Trie Nothing (Map.singleton seg (segmentsToTrie home segs main))


merge :: Trie -> Trie -> Trie
merge (Trie main1 subs1) (Trie main2 subs2) =
  Trie
    (checkedMerge main1 main2)
    (Map.unionWith merge subs1 subs2)


checkedMerge :: Maybe a -> Maybe a -> Maybe a
checkedMerge a b =
  case (a, b) of
    (Nothing, main)  -> main
    (main, Nothing)  -> main
    (Just _, Just _) -> $(Crash.crash 'checkedMerge) "cannot have two modules with the same name"



-- @LAMDERA
-- FUNCTION ARGUMENT LOOKUP


makeArgLookup :: Graph -> FnArgLookup
makeArgLookup graph home name =
  case Map.lookup (Opt.Global home name) graph of
    Just (Opt.Define (Opt.Function args _) _) ->
      Just (length args)

    Just (Opt.Ctor _ arity) ->
      Just arity

    Just (Opt.Link global) ->
      case Map.lookup global graph of
        Just (Opt.Cycle names _ defs _) ->
          case List.find (\d -> defName d == name) defs of
            Just (Opt.Def _ (Opt.Function args _)) ->
              Just (length args)

            Just (Opt.TailDef _ args _) ->
              Just (length args)

            _ ->
              -- This disables direct function calls eg. for mutually recursive
              -- functions (with or without partial application). These are
              -- technically possible but not implemented here.
              Nothing

        _ ->
          Nothing

    _ ->
      Nothing


defName :: Opt.Def -> N.Name
defName (Opt.Def name _) = name
defName (Opt.TailDef name _ _) = name


fn :: Opt.Global -> [N.Name] -> Expr.Code -> JS.Stmt
fn (Opt.Global home name) args code =
  let
    directFnName = JsName.fromGlobalDirectFn home name
    argNames = map JsName.fromLocal args
  in
  JS.Block
    [ JS.Var directFnName (Expr.codeToExpr code)
    , JS.Var (JsName.fromGlobal home name) $ Expr.codeToExpr (Expr.generateCurriedFunctionRef argNames directFnName)
    ]


ctor :: Opt.Global -> Int -> Expr.Code -> JS.Stmt
ctor (Opt.Global home name) arity code =
  let
    directFnName = JsName.fromGlobalDirectFn home name
    argNames = Index.indexedMap (\i _ -> JsName.fromIndex i) [1 .. arity]
  in
  JS.Block
    [ JS.Var directFnName (Expr.codeToExpr code)
    , JS.Var (JsName.fromGlobal home name) $ Expr.codeToExpr (Expr.generateCurriedFunctionRef argNames directFnName)
    ]


generateEsm :: Mode.Mode -> Opt.GlobalGraph -> Mains -> B.Builder
generateEsm mode (Opt.GlobalGraph graph_ _) mains =
  let
    graph = Lamdera.Injection.graphModifications mode mains graph_
    state = Map.foldrWithKey (addMain mode graph) emptyState mains
  in
  "var scope = {};\n"
  <> Functions.functions
  -- <> perfNote mode -- @NOTE given user never manages JS generation in Lamdera, hide the perf note
  <> stateToBuilder state
  <> toMainExports mode mains
  <> "\nexport const Elm = scope.Elm;\n"
  <> Lamdera.Injection.source mode mains
  <> "\n" <> Lamdera.Injection.elmPkgJs mode mains <> "\n"


-- JS CACHE
--
-- @LAMDERA `lamdera live` keeps every module's objects in memory (Ext.MemoryCache, either backing), so between two
-- builds a module that was not recompiled has the very same Opt.Node values. The JS of a
-- top-level definition depends on its node and, through direct calls (makeArgLookup), on
-- the arities of the globals it refers to, all of which are in its deps. So the rendered
-- statement of the last build is reused when the node is the same value (pointer equal,
-- so the check costs nothing and can only ever miss, never be wrong) and the arities of
-- its deps are unchanged. The traversal still runs in full, so the output order is the
-- same; only the rendering of unchanged definitions is skipped.
--
-- Every statement and kernel chunk goes through the cache (Define, DefineTailFunc and
-- Cycle statements are where nearly all the work is; the Wire3 codecs are kernel nodes
-- that Lamdera.Wire3.Direct.replaceCodecsCached keeps the same between builds). Use it
-- with one Mode only (Generate.dev uses it for Mode.Dev Nothing).
--
-- Each cached build also lists what it emitted, in output order (a Listing), so that
-- Lamdera.HotPatch can work out what a running page needs to catch up with the next
-- build, and the output gets `patchHook`, the page's way into the scope of the
-- definitions.


-- (strict fields, so evaluating a JsCache to WHNF builds the whole Map, the kept Wire3
-- codecs and the listing)
data JsCache =
  JsCache !(Map.Map Opt.Global CachedStmt) !Lamdera.Wire3.Direct.CodecCache !Listing


emptyJsCache :: JsCache
emptyJsCache =
  JsCache Map.empty Lamdera.Wire3.Direct.emptyCodecCache emptyListing


jsCacheListing :: JsCache -> Listing
jsCacheListing (JsCache _ _ listing) =
  listing


data CachedStmt =
  CachedStmt Opt.Node [Maybe Int] !BS.ByteString


data Caching
  = NoCaching
  | Caching !(Map.Map Opt.Global CachedStmt) !(Map.Map Opt.Global CachedStmt) [Emitted] [Emitted] -- last build, this build, and this build's statements and kernels (last first)


-- What one build emitted, for Lamdera.HotPatch.
data Listing =
  Listing
    { _lsStatements :: ![Emitted] -- in output order, links included (with no bytes)
    , _lsKernels :: ![Emitted] -- in output order (all of them come before the statements)
    , _lsExports :: !BS.ByteString -- toMainExports
    , _lsFixed :: !BS.ByteString -- the rest of what is not a definition (injections)
    }


emptyListing :: Listing
emptyListing =
  Listing [] [] BS.empty BS.empty


data Emitted =
  Emitted
    { _emGlobal :: !Opt.Global
    , _emKind :: !EmitKind
    , _emBytes :: !BS.ByteString
    , _emDeps :: !(Set.Set Opt.Global)
    }


data EmitKind
  = EFunction -- functions, tail functions and cycles of functions: refer to others when called
  | EValue -- worked out once, when the page loads
  | EConstant -- constructors, enums and boxes
  | ECycleValues
  | EPort
  | EManager
  | EKernel
  | ELink -- no output: a name for its target (a cycle)
  deriving (Eq, Show)


-- The page's way into the scope of the definitions: runs a patch's statements there.
patchHook :: B.Builder
patchHook =
  "\nscope['__lamdera_patch'] = function(__lamdera_code) { eval(__lamdera_code); };\n"


{-| `generate`, reusing the statements of the last build where they are still valid, and
returning the cache for the next one.
-}
generateCached :: JsCache -> Mode.Mode -> Opt.GlobalGraph -> Mains -> (B.Builder, JsCache)
generateCached (JsCache old codecs _) mode (Opt.GlobalGraph graph_ _) mains =
  let
    (graph, codecs_) = Lamdera.Injection.graphModificationsCached codecs mode mains graph_
    (builder, state) = generateFromGraph (emptyState { _caching = Caching old Map.empty [] [] }) patchHook mode graph mains
    exports = strictBytes (toMainExports mode mains)
    fixed = strictBytes (Lamdera.Injection.source mode mains <> Lamdera.Injection.elmPkgJs mode mains)
  in
  ( builder
  , case _caching state of
      Caching _ new stmts kernels -> JsCache new codecs_ (mkListing (reverse stmts) (reverse kernels) exports fixed)
      NoCaching                   -> JsCache Map.empty codecs_ emptyListing
  )


mkListing :: [Emitted] -> [Emitted] -> BS.ByteString -> BS.ByteString -> Listing
mkListing stmts kernels exports fixed =
  let
    forceAll = List.foldl' (\() e -> e `seq` ()) ()
  in
  forceAll stmts `seq` forceAll kernels `seq` Listing stmts kernels exports fixed


strictBytes :: B.Builder -> BS.ByteString
strictBytes =
  BSL.toStrict . B.toLazyByteString


addStmtFor :: EmitKind -> FnArgLookup -> Opt.Global -> Opt.Node -> Set.Set Opt.Global -> State -> JS.Stmt -> State
addStmtFor kind argLookup global node deps state stmt =
  case _caching state of
    NoCaching ->
      addStmt state stmt

    Caching old new stmts kernels ->
      let
        arities = map (\(Opt.Global home name) -> argLookup home name) (Set.toList deps)

        entry =
          case Map.lookup global old of
            Just cached@(CachedStmt oldNode oldArities _)
              | isTrue# (reallyUnsafePtrEquality# oldNode node) && oldArities == arities ->
                  cached

            _ ->
              CachedStmt node arities (strictBytes (JS.stmtToBuilder stmt))

        CachedStmt _ _ bytes = entry
      in
      addBuilder (state { _caching = Caching old (Map.insert global entry new) (Emitted global kind bytes deps : stmts) kernels }) (B.byteString bytes)


-- addKernel, through the cache
addKernelFor :: Opt.Global -> Opt.Node -> State -> B.Builder -> State
addKernelFor global node state kernel =
  case _caching state of
    NoCaching ->
      addKernel state kernel

    Caching old new stmts kernels ->
      let
        entry =
          case Map.lookup global old of
            Just cached@(CachedStmt oldNode _ _) | isTrue# (reallyUnsafePtrEquality# oldNode node) ->
              cached

            _ ->
              CachedStmt node [] (strictBytes kernel)

        CachedStmt _ _ bytes = entry
      in
      addKernel (state { _caching = Caching old (Map.insert global entry new) stmts (Emitted global EKernel bytes Set.empty : kernels) }) (B.byteString bytes)


-- addStmt, listed (not cached)
addRecorded :: EmitKind -> Opt.Global -> Set.Set Opt.Global -> State -> JS.Stmt -> State
addRecorded kind global deps state stmt =
  case _caching state of
    NoCaching ->
      addStmt state stmt

    Caching old new stmts kernels ->
      let bytes = strictBytes (JS.stmtToBuilder stmt) in
      addBuilder (state { _caching = Caching old new (Emitted global kind bytes deps : stmts) kernels }) (B.byteString bytes)


addLink :: Opt.Global -> Opt.Global -> State -> State
addLink global target state =
  case _caching state of
    NoCaching ->
      state

    Caching old new stmts kernels ->
      state { _caching = Caching old new (Emitted global ELink BS.empty (Set.singleton target) : stmts) kernels }
