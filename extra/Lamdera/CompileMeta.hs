{-# LANGUAGE OverloadedStrings #-}

{-|

Compilation metadata, written to elm-stuff/lamdera/.lamdera-compile-meta
during production builds: facts about the compiled app for downstream tooling.
Expected to grow more fields over time.

Current fields:

  timers : Bool — whether the app's Backend.subscriptions can ever produce a
  self-driven subscription that must fire while there are no connected clients.
  In practice that means Time.every: it ticks on a clock independent of any
  client connection, so a slept (exited) app would silently drop those ticks.

  Connection-gated subs (Lamdera.onConnect / onDisconnect) do NOT set this — a
  sleeping app has no connections, and a new connection wakes it (~socket
  activation) before onConnect fires; a disconnect can't be missed because sleep
  requires zero connected clients in the first place. Everything else a backend
  could reference is either browser/DOM-driven (never fires headless) or not
  compilable in app code, so it can't matter here.

  We answer by reachability over the optimized dependency graph, starting from
  the `subscriptions` field of a direct `Lamdera.backend { ... }` call — so a
  Time.every hidden behind helpers, if/case branches, Sub.batch, or aliases is
  still found. If we can't locate a proper backend app record, we default to
  timers:false (sleep-eligible): the deliberate choice is to never keep an app
  awake for subscriptions that do nothing, and to treat any genuinely-new
  self-driven subscription as a future edge case rather than penalise every app
  that happens to reference an inert (e.g. browser) subscription.

-}
module Lamdera.CompileMeta where

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.FilePath ((</>))

import AST.Optimized
import Elm.ModuleName (Canonical(..))
import qualified Elm.Package as Pkg

import Lamdera
import qualified Lamdera.AppConfig


{-| Called during production builds (Lamdera.CLI.Check.buildProductionJsFiles),
right after Lamdera.AppConfig.writeUsage. -}
write :: IO ()
write = do
  cache <- lamderaCache_
  graph <- Lamdera.AppConfig.loadLamderaAppGraph
  writeUtf8 (cache </> ".lamdera-compile-meta") $
    if backendSubsUseTimeEvery graph
      then "{\"timers\":true}"
      else "{\"timers\":false}"


{-| Does the backend's `subscriptions` field transitively reach `Time.every`? -}
backendSubsUseTimeEvery :: GlobalGraph -> Bool
backendSubsUseTimeEvery graph =
  case resolveGlobal graph 0 backendApp of
    Just (Call fn [recordArg]) | isLamderaBackendFn fn ->
      case reduce graph 0 recordArg of
        Just (Record fields) ->
          case Map.lookup "subscriptions" fields of
            Just subs -> reachesTimeEvery graph (globalsInExpr subs) Set.empty
            Nothing -> False

        _ ->
          False

    _ ->
      False
  where
    backendApp =
      Global (Canonical (Pkg.Name "author" "project") "Backend") "app"

    isLamderaBackendFn fn =
      case fn of
        VarGlobal (Global (Canonical pkg "Lamdera") "backend") -> pkg == Pkg.lamderaCore
        _ -> False


{-| The one subscription that must keep an app awake. -}
timeEvery :: Global
timeEvery = Global (Canonical (Pkg.Name "elm" "time") "Time") "every"


{-| Breadth-first reachability over the optimized graph's per-node dependency
sets. Returns True as soon as Time.every is reached. The `seen` set makes this
terminate on the (cyclic) global graph. -}
reachesTimeEvery :: GlobalGraph -> [Global] -> Set.Set Global -> Bool
reachesTimeEvery graph worklist seen =
  case worklist of
    [] ->
      False

    g : rest
      | g == timeEvery ->
          True

      | Set.member g seen ->
          reachesTimeEvery graph rest seen

      | otherwise ->
          let deps = nodeDeps (Map.lookup g (_g_nodes graph))
          in reachesTimeEvery graph (Set.toList deps ++ rest) (Set.insert g seen)


{-| The globals a node directly depends on (for transitive closure). We only need
to reach Time.every, so nodes without a dependency set contribute nothing. -}
nodeDeps :: Maybe Node -> Set.Set Global
nodeDeps mn =
  case mn of
    Just (Define _ ds) -> ds
    Just (DefineTailFunc _ _ ds) -> ds
    Just (Cycle _ _ _ ds) -> ds
    Just (Kernel _ ds) -> ds
    Just (PortIncoming _ ds) -> ds
    Just (PortOutgoing _ ds) -> ds
    Just (Link g) -> Set.singleton g
    _ -> Set.empty


{-| Every global referenced anywhere in an expression tree. Used to seed the
reachability search from the (possibly inline) `subscriptions` field. -}
globalsInExpr :: Expr -> [Global]
globalsInExpr expr =
  case expr of
    VarGlobal g -> [g]
    VarEnum g _ -> [g]
    VarBox g -> [g]
    List es -> concatMap globalsInExpr es
    Function _ e -> globalsInExpr e
    Call e es -> globalsInExpr e ++ concatMap globalsInExpr es
    TailCall _ nes -> concatMap (globalsInExpr . snd) nes
    If pairs def -> concatMap (\(c, e) -> globalsInExpr c ++ globalsInExpr e) pairs ++ globalsInExpr def
    Let def e -> globalsInDef def ++ globalsInExpr e
    Destruct _ e -> globalsInExpr e
    Case _ _ decider branches -> globalsInDecider decider ++ concatMap (globalsInExpr . snd) branches
    Access e _ -> globalsInExpr e
    Update e m -> globalsInExpr e ++ concatMap globalsInExpr (Map.elems m)
    Record m -> concatMap globalsInExpr (Map.elems m)
    Tuple a b mc -> globalsInExpr a ++ globalsInExpr b ++ maybe [] globalsInExpr mc
    _ -> []


globalsInDef :: Def -> [Global]
globalsInDef d =
  case d of
    Def _ e -> globalsInExpr e
    TailDef _ _ e -> globalsInExpr e


globalsInDecider :: Decider Choice -> [Global]
globalsInDecider d =
  case d of
    Leaf (Inline e) -> globalsInExpr e
    Leaf (Jump _) -> []
    Chain _ s f -> globalsInDecider s ++ globalsInDecider f
    FanOut _ tests fb -> concatMap (globalsInDecider . snd) tests ++ globalsInDecider fb


{-| Look up a global's defining expression in the graph. -}
resolveGlobal :: GlobalGraph -> Int -> Global -> Maybe Expr
resolveGlobal graph depth g
  | depth > maxDepth = Nothing
  | otherwise =
      case Map.lookup g (_g_nodes graph) of
        Just (Define expr _) -> Just expr
        Just (DefineTailFunc args expr _) -> Just (Function args expr)
        Just (Link g2) -> resolveGlobal graph (depth + 1) g2
        _ -> Nothing


{-| Reduce an expression to a concrete form by following global references,
e.g. `app = Lamdera.backend appRecord` where appRecord is defined elsewhere. -}
reduce :: GlobalGraph -> Int -> Expr -> Maybe Expr
reduce graph depth expr
  | depth > maxDepth = Nothing
  | otherwise =
      case expr of
        VarGlobal g -> resolveGlobal graph (depth + 1) g >>= reduce graph (depth + 1)
        _ -> Just expr


maxDepth :: Int
maxDepth = 20
