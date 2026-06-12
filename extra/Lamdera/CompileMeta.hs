{-# LANGUAGE OverloadedStrings #-}

{-|

Compilation metadata, written to elm-stuff/lamdera/.lamdera-compile-meta
during production builds: facts about the compiled app for downstream tooling.
Expected to grow more fields over time.

Current fields:

  subscriptions : Bool — whether the app's Backend.subscriptions can ever
  produce an active subscription. False only when Backend.app is a direct
  `Lamdera.backend { ... }` call whose `subscriptions` field provably reduces
  to `Sub.none` (including `\_ -> Sub.none`, `always Sub.none`, `Sub.batch []`,
  and aliases thereof). The analysis is deliberately conservative: anything
  that cannot be proven reduces to subscriptions: true.

-}
module Lamdera.CompileMeta where

import qualified Data.Map as Map
import System.FilePath ((</>))

import AST.Optimized
import Elm.ModuleName (Canonical(..))
import qualified Elm.ModuleName as ModuleName
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
    if backendSubsProvablyNone graph
      then "{\"subscriptions\":false}"
      else "{\"subscriptions\":true}"


backendSubsProvablyNone :: GlobalGraph -> Bool
backendSubsProvablyNone graph =
  case resolveGlobal graph 0 backendApp of
    Just (Call fn [recordArg]) | isLamderaBackendFn fn ->
      case reduce graph 0 recordArg of
        Just (Record fields) ->
          case Map.lookup "subscriptions" fields of
            Just subs -> provablyNone graph 0 subs
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


provablyNone :: GlobalGraph -> Int -> Expr -> Bool
provablyNone graph depth expr
  | depth > maxDepth = False
  | otherwise =
      case expr of
        VarGlobal g
          | g == subNone ->
              True

          | otherwise ->
              -- An alias, e.g. `subscriptions = noSubs` — follow it
              case resolveGlobal graph (depth + 1) g of
                Just e -> provablyNone graph (depth + 1) e
                Nothing -> False

        -- `subscriptions model = ...` / `\_ -> ...` — args are irrelevant since
        -- the body must reduce to Sub.none regardless of the model
        Function _ body ->
          provablyNone graph (depth + 1) body

        Call (VarGlobal g) [arg]
          | g == subBatch ->
              case arg of
                List subs -> all (provablyNone graph (depth + 1)) subs
                _ -> False

          | g == basicsAlways ->
              provablyNone graph (depth + 1) arg

        _ ->
          False
  where
    subNone = Global ModuleName.sub "none"
    subBatch = Global ModuleName.sub "batch"
    basicsAlways = Global ModuleName.basics "always"


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
