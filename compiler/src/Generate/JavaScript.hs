{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript
  ( generate
  , generateForRepl
  , generateForReplEndpoint
  )
  where


import Prelude hiding (cycle, print)
import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.List as List
import Data.List (isPrefixOf)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
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
import qualified Lamdera

-- GENERATE

{-| Module: Generate.JavaScript

This module generates JavaScript code from optimized Elm AST.

## Experimental JS/TS Exports Feature

When --experimental-js-ts-exports is enabled via Lamdera.enableExportAllFunctions:
  
  * Exports all top-level functions from modules that have a 'main' function
  * Generates both curried and uncurried versions for multi-argument functions
  * Filters out internal wire protocol functions (w3_ prefix)
  * Produces clean ES6 module exports suitable for JavaScript/TypeScript consumption
  * Works in conjunction with Generate.TypeScript to produce .d.ts files

The feature is designed for creating reusable Elm modules that can be 
consumed from JavaScript/TypeScript projects while maintaining Elm's 
currying semantics.
-}


type Graph = Map.Map Opt.Global Opt.Node
type FnArgLookup = ModuleName.Canonical -> Name.Name -> Maybe Int
type Mains = Map.Map ModuleName.Canonical Opt.Main


generate :: Mode.Mode -> Opt.GlobalGraph -> Mains -> B.Builder
generate mode globalGraph@(Opt.GlobalGraph graph_ _) mains =
  let
    graph = Lamdera.Injection.graphModifications mode mains graph_
    exportAllFunctions = Lamdera.isExportAllFunctionsEnabled_
    state = if exportAllFunctions
            then 
              -- For experimental JS/TS exports, add all exposed functions from modules that have mains
              Map.foldrWithKey (addModuleExports mode graph) emptyState mains
            else 
              -- Normal mode: just add main functions
              Map.foldrWithKey (addMain mode graph) emptyState mains
  in
  "(function(scope){\n'use strict';"
  <> Functions.functions
  -- <> perfNote mode -- @NOTE given user never manages JS generation in Lamdera, hide the perf note
  <> stateToBuilder state
  <> if exportAllFunctions
     then toAllExports mode globalGraph mains graph
          <> "}(this));"  -- Close wrapper for export-all mode
     else toMainExports mode mains
          <> Lamdera.Injection.source mode mains
          <> "}(this));"
          <> "\n" <> Lamdera.Injection.elmPkgJs mode <> "\n"

addMain :: Mode.Mode -> Graph -> ModuleName.Canonical -> Opt.Main -> State -> State
addMain mode graph home _ state =
  addGlobal mode graph state (Opt.Global home "main")

-- For --experimental-js-ts-exports: add all globals from a module
addModuleExports :: Mode.Mode -> Graph -> ModuleName.Canonical -> Opt.Main -> State -> State
addModuleExports mode graph home _ state =
  -- Find all globals that belong to this module and add them
  Map.foldrWithKey (\global node acc ->
    case global of
      Opt.Global globalHome name ->
        if globalHome == home && shouldExportName name
        then addGlobal mode graph acc global
        else acc
  ) state graph



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


generateForRepl :: Bool -> L.Localizer -> Opt.GlobalGraph -> ModuleName.Canonical -> Name.Name -> Can.Annotation -> B.Builder
generateForRepl ansi localizer (Opt.GlobalGraph graph _) home name (Can.Forall _ tipe) =
  let
    mode = Mode.Dev Nothing
    debugState = addGlobal mode graph emptyState (Opt.Global ModuleName.debug "toString")
    evalState = addGlobal mode graph debugState (Opt.Global home name)
  in
  "process.on('uncaughtException', function(err) { process.stderr.write(err.toString() + '\\n'); process.exit(1); });"
  <> Functions.functions
  <> stateToBuilder evalState
  <> print ansi localizer home name tipe


print :: Bool -> L.Localizer -> ModuleName.Canonical -> Name.Name -> Can.Type -> B.Builder
print ansi localizer home name tipe =
  let
    value = JsName.toBuilder (JsName.fromGlobal home name)
    toString = JsName.toBuilder (JsName.fromKernel Name.debug "toAnsiString")
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


generateForReplEndpoint :: L.Localizer -> Opt.GlobalGraph -> ModuleName.Canonical -> Maybe Name.Name -> Can.Annotation -> B.Builder
generateForReplEndpoint localizer (Opt.GlobalGraph graph _) home maybeName (Can.Forall _ tipe) =
  let
    name = maybe Name.replValueToPrint id maybeName
    mode = Mode.Dev Nothing
    debugState = addGlobal mode graph emptyState (Opt.Global ModuleName.debug "toString")
    evalState = addGlobal mode graph debugState (Opt.Global home name)
  in
  Functions.functions
  <> stateToBuilder evalState
  <> postMessage localizer home maybeName tipe


postMessage :: L.Localizer -> ModuleName.Canonical -> Maybe Name.Name -> Can.Type -> B.Builder
postMessage localizer home maybeName tipe =
  let
    name = maybe Name.replValueToPrint id maybeName
    value = JsName.toBuilder (JsName.fromGlobal home name)
    toString = JsName.toBuilder (JsName.fromKernel Name.debug "toAnsiString")
    tipeDoc = RT.canToDoc localizer RT.None tipe
    toName n = "\"" <> Name.toBuilder n <> "\""
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
    }


emptyState :: State
emptyState =
  State mempty [] Set.empty


stateToBuilder :: State -> B.Builder
stateToBuilder (State revKernels revBuilders _) =
  prependBuilders revKernels (prependBuilders revBuilders mempty)


prependBuilders :: [B.Builder] -> B.Builder -> B.Builder
prependBuilders revBuilders monolith =
  List.foldl' (\m b -> b <> m) monolith revBuilders



-- ADD DEPENDENCIES


addGlobal :: Mode.Mode -> Graph -> State -> Opt.Global -> State
addGlobal mode graph state@(State revKernels builders seen) global =
  if Set.member global seen then
    state
  else
    addGlobalHelp mode graph global $
      State revKernels builders (Set.insert global seen)


addGlobalHelp :: Mode.Mode -> Graph -> Opt.Global -> State -> State
addGlobalHelp mode graph global state =
  let
    addDeps deps someState =
      Set.foldl' (addGlobal mode graph) someState deps
    
    argLookup = makeArgLookup graph
  in
  case Map.lookup global graph of
    Nothing -> state  -- Global not in graph, skip it
    Just node ->
      case node of
        -- @LAMDERA
        Opt.Define (Opt.Function args body) deps
          | length args > 1 ->
              addStmt
                (addDeps deps state)
                (fn global args (Expr.generateFunctionImplementation mode argLookup args body))

        Opt.Define expr deps ->
          addStmt (addDeps deps state) (
            var global (Expr.generate mode argLookup expr)
          )

        Opt.DefineTailFunc argNames body deps ->
          addStmt (addDeps deps state) (
            let (Opt.Global _ name) = global in
            var global (Expr.generateTailDef mode argLookup name argNames body)
          )

        -- @LAMDERA
        Opt.Ctor index arity
          | arity > 1 ->
              addStmt
                state
                (ctor global arity (Expr.generateCtorImplementation mode global index arity))

        Opt.Ctor index arity ->
          addStmt state (
            var global (Expr.generateCtor mode global index arity)
          )

        Opt.Link linkedGlobal ->
          addGlobal mode graph state linkedGlobal

        Opt.Cycle names values functions deps ->
          addStmt (addDeps deps state) (
            generateCycle mode argLookup global names values functions
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
          addStmt (addGlobal mode graph state identity) (
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


addStmt :: State -> JS.Stmt -> State
addStmt state stmt =
  addBuilder state (JS.stmtToBuilder stmt)


addBuilder :: State -> B.Builder -> State
addBuilder (State revKernels revBuilders seen) builder =
  State revKernels (builder:revBuilders) seen


addKernel :: State -> B.Builder -> State
addKernel (State revKernels revBuilders seen) kernel =
  State (kernel:revKernels) revBuilders seen


var :: Opt.Global -> Expr.Code -> JS.Stmt
var (Opt.Global home name) code =
  JS.Var (JsName.fromGlobal home name) (Expr.codeToExpr code)


isDebugger :: Opt.Global -> Bool
isDebugger (Opt.Global (ModuleName.Canonical _ home) _) =
  home == Name.debugger



-- GENERATE CYCLES


generateCycle :: Mode.Mode -> FnArgLookup -> Opt.Global -> [Name.Name] -> [(Name.Name, Opt.Expr)] -> [Opt.Def] -> JS.Stmt
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
                  "Some top-level definitions from `" <> Name.toBuilder (ModuleName._module home) <> "` are causing infinite recursion:\\n"
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


generateSafeCycle :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> (Name.Name, Opt.Expr) -> JS.Stmt
generateSafeCycle mode argLookup home (name, expr) =
  JS.FunctionStmt (JsName.fromCycle home name) [] $
    Expr.codeToStmtList (Expr.generate mode argLookup expr)


generateRealCycle :: ModuleName.Canonical -> (Name.Name, expr) -> JS.Stmt
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


drawCycle :: [Name.Name] -> B.Builder
drawCycle names =
  let
    topLine       = "\\n  ┌─────┐"
    nameLine name = "\\n  │    " <> Name.toBuilder name
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
        JS.Ref (JsName.fromGlobal ModuleName.basics Name.identity)


{-# NOINLINE identity #-}
identity :: Opt.Global
identity =
  Opt.Global ModuleName.basics Name.identity



-- GENERATE PORTS


generatePort :: Mode.Mode -> Opt.Global -> Name.Name -> Opt.Expr -> JS.Stmt
generatePort mode (Opt.Global home name) makePort converter =
  JS.Var (JsName.fromGlobal home name) $
    JS.Call (JS.Ref (JsName.fromKernel Name.platform makePort))
      [ JS.String (Name.toBuilder name)
      , Expr.codeToExpr (Expr.generate mode (\_ _ -> Nothing) converter)
      ]



-- GENERATE MANAGER


generateManager :: Mode.Mode -> Graph -> Opt.Global -> Opt.EffectsType -> State -> State
generateManager mode graph (Opt.Global home@(ModuleName.Canonical _ moduleName) _) effectsType state =
  let
    managerLVar =
      JS.LBracket
        (JS.Ref (JsName.fromKernel Name.platform "effectManagers"))
        (JS.String (Name.toBuilder moduleName))

    (deps, args, stmts) =
      generateManagerHelp home effectsType

    createManager =
      JS.ExprStmt $ JS.Assign managerLVar $
        JS.Call (JS.Ref (JsName.fromKernel Name.platform "createManager")) args
  in
  addStmt (List.foldl' (addGlobal mode graph) state deps) $
    JS.Block (createManager : stmts)


generateLeaf :: ModuleName.Canonical -> Name.Name -> JS.Stmt
generateLeaf home@(ModuleName.Canonical _ moduleName) name =
  JS.Var (JsName.fromGlobal home name) $
    JS.Call leaf [ JS.String (Name.toBuilder moduleName) ]



{-# NOINLINE leaf #-}
leaf :: JS.Expr
leaf =
  JS.Ref (JsName.fromKernel Name.platform "leaf")


generateManagerHelp :: ModuleName.Canonical -> Opt.EffectsType -> ([Opt.Global], [JS.Expr], [JS.Stmt])
generateManagerHelp home effectsType =
  let
    dep name = Opt.Global home name
    ref name = JS.Ref (JsName.fromGlobal home name)
  in
  case effectsType of
    Opt.Cmd ->
      ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "cmdMap" ]
      , [ ref "init", ref "onEffects", ref "onSelfMsg", ref "cmdMap" ]
      , [ generateLeaf home "command" ]
      )

    Opt.Sub ->
      ( [ dep "init", dep "onEffects", dep "onSelfMsg", dep "subMap" ]
      , [ ref "init", ref "onEffects", ref "onSelfMsg", JS.Int 0, ref "subMap" ]
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


toMainExports :: Mode.Mode -> Mains -> B.Builder
toMainExports mode mains =
  let
    export = JsName.fromKernel Name.platform "export"
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
        "'" <> Utf8.toBuilder name <> "':"
        <> generateExports mode subTrie
        <> List.foldl' (addSubTrie mode) "}" otherSubTries


addSubTrie :: Mode.Mode -> B.Builder -> (Name.Name, Trie) -> B.Builder
addSubTrie mode end (name, trie) =
  ",'" <> Utf8.toBuilder name <> "':" <> generateExports mode trie <> end



-- EXPORT ALL FUNCTIONS
-- @LAMDERA


toAllExports :: Mode.Mode -> Opt.GlobalGraph -> Mains -> Graph -> B.Builder
toAllExports mode (Opt.GlobalGraph allNodes _) mains graph =
  let
    -- Generate module exports for standalone use
    moduleExports = generateStandaloneModuleExports mode mains graph
  in
  "if (typeof module !== 'undefined' && module.exports) {\n"
  <> "  module.exports = " <> moduleExports <> ";\n"
  <> "} else if (typeof scope !== 'undefined') {\n"
  <> "  scope.Elm = " <> moduleExports <> ";\n"
  <> "}"

-- Generate exports for standalone modules with --export-all-functions
generateStandaloneModuleExports :: Mode.Mode -> Mains -> Graph -> B.Builder
generateStandaloneModuleExports mode mains graph =
  "{\n" <> Map.foldlWithKey' (addModuleObject mode graph) "" mains <> "}"

addModuleObject :: Mode.Mode -> Graph -> B.Builder -> ModuleName.Canonical -> Opt.Main -> B.Builder
addModuleObject mode graph acc home@(ModuleName.Canonical _ moduleName) _ =
  let
    moduleNameStr = Name.toBuilder moduleName
    exports = generateModuleFunctionExports mode graph home
  in
  if B.toLazyByteString acc == ""
  then "  " <> moduleNameStr <> ": " <> exports
  else acc <> ",\n  " <> moduleNameStr <> ": " <> exports

generateModuleFunctionExports :: Mode.Mode -> Graph -> ModuleName.Canonical -> B.Builder
generateModuleFunctionExports mode graph home =
  let (exports, currySetup) = Map.foldlWithKey' (addExportedFunction mode home) ("", []) graph
  in
  if null currySetup
  then "{\n" <> exports <> "\n  }"
  else "(function() {\n  var _module = {\n" <> exports <> "\n  };\n" 
       <> mconcat (reverse currySetup) <> "\n  return _module;\n})()"

addExportedFunction :: Mode.Mode -> ModuleName.Canonical -> (B.Builder, [B.Builder]) -> Opt.Global -> Opt.Node -> (B.Builder, [B.Builder])
addExportedFunction mode home (acc, currySetup) global@(Opt.Global globalHome name) node =
  if globalHome == home && shouldExportNode node && shouldExportName name
  then
    let
      nameStr = JsName.toBuilder (JsName.fromGlobal globalHome name)
      exportName = Name.toBuilder name
      -- For multi-argument functions, export direct version as default and add curry property
      (export, newCurrySetup) = case node of
                 Opt.Define (Opt.Function args _) _ ->
                   if length args > 1
                   then ("    " <> exportName <> ": " <> nameStr <> "$",
                         ("  _module." <> exportName <> ".curry = " <> nameStr <> ";\n") : currySetup)
                   else ("    " <> exportName <> ": " <> nameStr, currySetup)
                 Opt.DefineTailFunc args _ _ ->
                   if length args > 1
                   then ("    " <> exportName <> ": " <> nameStr <> "$",
                         ("  _module." <> exportName <> ".curry = " <> nameStr <> ";\n") : currySetup)
                   else ("    " <> exportName <> ": " <> nameStr, currySetup)
                 _ -> ("    " <> exportName <> ": " <> nameStr, currySetup)
    in
    if B.toLazyByteString acc == ""
    then (export, newCurrySetup)
    else (acc <> ",\n" <> export, newCurrySetup)
  else (acc, currySetup)

-- | Determines if a function should be exported based on its name.
-- Filters out internal implementation details like wire protocol functions.
shouldExportName :: Name.Name -> Bool
shouldExportName name =
  let nameStr = Name.toChars name
  in not (isPrefixOf "w3_" nameStr)  -- Filter out wire3 encode/decode functions

-- | Determines if an AST node represents an exportable definition.
-- Includes functions, constructors, enums, and ports.
shouldExportNode :: Opt.Node -> Bool
shouldExportNode node =
  case node of
    Opt.Define _ _ -> True
    Opt.DefineTailFunc _ _ _ -> True
    Opt.Ctor _ _ -> True
    Opt.Enum _ -> True
    Opt.PortIncoming _ _ -> True
    Opt.PortOutgoing _ _ -> True
    _ -> False


-- Extract user modules from local compilation graph
extractLocalModules :: Mode.Mode -> Graph -> State -> Map.Map ModuleName.Canonical (Map.Map Name.Name Opt.Global)
extractLocalModules mode graph (State _ _ seenGlobals) =
  Set.foldl (addLocalGlobal mode graph) Map.empty seenGlobals

addLocalGlobal :: Mode.Mode -> Graph -> Map.Map ModuleName.Canonical (Map.Map Name.Name Opt.Global) -> Opt.Global -> Map.Map ModuleName.Canonical (Map.Map Name.Name Opt.Global)
addLocalGlobal mode graph acc global@(Opt.Global home name) =
  case Map.lookup global graph of
    Just node ->
      case home of
        ModuleName.Canonical _ moduleName ->
          -- Only include user modules (not kernel/core modules)
          if Name.isKernel moduleName || isInternalNode node
          then acc
          else
            let
              moduleMap = Map.findWithDefault Map.empty home acc
              newModuleMap = Map.insert name global moduleMap
            in
            Map.insert home newModuleMap acc
    Nothing -> acc


addGlobalIfExposed :: Mode.Mode -> Graph -> State -> Opt.Global -> Opt.Node -> State
addGlobalIfExposed mode graph state global@(Opt.Global home name) node =
  -- Skip kernel modules and compiler-generated names
  case home of
    ModuleName.Canonical _ moduleName ->
      if Name.isKernel moduleName || isInternalNode node
      then state
      else addGlobal mode graph state global


groupByModule :: Map.Map ModuleName.Canonical (Map.Map Name.Name Opt.Global) -> Opt.Global -> Opt.Node -> Map.Map ModuleName.Canonical (Map.Map Name.Name Opt.Global)
groupByModule acc global@(Opt.Global home name) node =
  case home of
    ModuleName.Canonical _ moduleName ->
      if Name.isKernel moduleName || isInternalNode node
      then acc
      else
        let
          moduleMap = Map.findWithDefault Map.empty home acc
          newModuleMap = Map.insert name global moduleMap
        in
        Map.insert home newModuleMap acc

-- Check if a package is from elm core (elm/*, elm-explorations/*, lamdera/*)
isElmCorePackage :: Pkg.Name -> Bool
isElmCorePackage pkg = Pkg.isKernel pkg


-- Generate module exports for the Elm object structure
generateModuleExportsForElm :: Mode.Mode -> Map.Map ModuleName.Canonical (Map.Map Name.Name Opt.Global) -> B.Builder
generateModuleExportsForElm mode moduleGroups =
  let
    -- Filter to only user modules for export (not core modules)
    userModuleGroups = Map.filterWithKey isUserModule moduleGroups
    entries = Map.foldrWithKey (addModuleToElmExportBuilder mode) [] userModuleGroups
    entriesBuilder = mconcat $ List.intersperse ", " entries
  in
  "{" <> entriesBuilder <> "}"

-- Check if a module should be exported (user modules only, not core)
isUserModule :: ModuleName.Canonical -> Map.Map Name.Name Opt.Global -> Bool
isUserModule (ModuleName.Canonical pkg _) _ = not (isElmCorePackage pkg)

addModuleToElmExportBuilder :: Mode.Mode -> ModuleName.Canonical -> Map.Map Name.Name Opt.Global -> [B.Builder] -> [B.Builder]
addModuleToElmExportBuilder mode home@(ModuleName.Canonical _ moduleName) functions acc =
  let
    moduleExport = generateModuleObject mode home functions
    moduleNameBuilder = Name.toBuilder moduleName
    entry = "'" <> moduleNameBuilder <> "': " <> moduleExport
  in
  entry : acc


generateModuleObject :: Mode.Mode -> ModuleName.Canonical -> Map.Map Name.Name Opt.Global -> B.Builder
generateModuleObject mode home functions =
  let
    functionExports = Map.foldrWithKey (addFunctionExport mode) "" functions
  in
  "(function() {\n" <>
  "var module = {};\n" <>
  functionExports <>
  "return module;\n" <>
  "}())"


addFunctionExport :: Mode.Mode -> Name.Name -> Opt.Global -> B.Builder -> B.Builder
addFunctionExport mode name (Opt.Global home funcName) acc =
  let
    jsName = JsName.toBuilder (JsName.fromGlobal home funcName)
    nameStr = Utf8.toBuilder name
    
    -- Also export the direct version if it exists for multi-argument functions
    directJsName = JsName.toBuilder (JsName.fromGlobalDirectFn home funcName)
    directExport = 
      "if (typeof " <> directJsName <> " !== 'undefined') {\n" <>
      "  module['" <> nameStr <> "']['$direct'] = " <> directJsName <> ";\n" <>
      "}\n"
  in
  "module['" <> nameStr <> "'] = " <> jsName <> ";\n" <>
  directExport <>
  acc


-- Check if a node should be considered internal (not exposed)
isInternalNode :: Opt.Node -> Bool
isInternalNode node =
  case node of
    Opt.Cycle _ _ _ _ -> True  -- Cycles are internal
    Opt.Manager _ -> True      -- Effect managers are internal
    _ -> False


-- BUILD TRIES


data Trie =
  Trie
    { _main :: Maybe (ModuleName.Canonical, Opt.Main)
    , _subs :: Map.Map Name.Name Trie
    }


emptyTrie :: Trie
emptyTrie =
  Trie Nothing Map.empty


addToTrie :: ModuleName.Canonical -> Opt.Main -> Trie -> Trie
addToTrie home@(ModuleName.Canonical _ moduleName) main trie =
  merge trie $ segmentsToTrie home (Name.splitDots moduleName) main


segmentsToTrie :: ModuleName.Canonical -> [Name.Name] -> Opt.Main -> Trie
segmentsToTrie home segments main =
  case segments of
    [] ->
      Trie (Just (home, main)) Map.empty

    segment : otherSegments ->
      Trie Nothing (Map.singleton segment (segmentsToTrie home otherSegments main))


merge :: Trie -> Trie -> Trie
merge (Trie main1 subs1) (Trie main2 subs2) =
  Trie
    (checkedMerge main1 main2)
    (Map.unionWith merge subs1 subs2)


checkedMerge :: Maybe a -> Maybe a -> Maybe a
checkedMerge a b =
  case (a, b) of
    (Nothing, main) ->
      main

    (main, Nothing) ->
      main

    (Just _, Just _) ->
      error "cannot have two modules with the same name"



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


defName :: Opt.Def -> Name.Name
defName (Opt.Def name _) = name
defName (Opt.TailDef name _ _) = name


fn :: Opt.Global -> [Name.Name] -> Expr.Code -> JS.Stmt
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
