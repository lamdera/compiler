{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.Evergreen.ModifyAST (update) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified AST.Prim.Name as Name
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import NeatInterpolation

import qualified Data.Utf8
-- @LAMDERA the annotation below is written as an Elm 0.19.3 type and converted
import Lamdera.Compat.Canonical (Annotation(..), Type(..))
import qualified Lamdera.Compat.Convert as Convert
import Elm.Package
import qualified AST.Canonical as Can
import qualified AST.Prim.Fields as F
import qualified Elm.Interface as I
import qualified Data.Maybe as Maybe
import qualified Graph
import qualified Elm.ModuleName as Module
import qualified Reporting.Annotation
import qualified Data.ByteString.Builder as B

import Lamdera
import StandaloneInstances


{-|

Finds any local vars `unsafeCoerce` within the `LamderaHelpers` module
and replaces them with a foreign var `Lamdera.Effect.unsafeCoerce`.

This allows us to use `unsafeCoerce` in the generated code without exposing
it in the lamdera/core module, so it cannot be used in regular user code.

-}
update :: Can.Module -> Can.Module
update canonical =
  let
    moduleName :: Module.Canonical = (Can._name canonical)
    decls :: Can.Decls = (Can._decls canonical) & removeUnsafeCoercePlaceholder
    newDecls :: Can.Decls = updateDecls moduleName decls
    newCanonical :: Can.Module = canonical { Can._decls = newDecls }
  in
  case moduleName of
    Module.Canonical (Name "author" "project") "LamderaHelpers" ->   newCanonical
    Module.Canonical (Name "author" "project") "LamderaCheckBoth" -> newCanonical
    Module.Canonical (Name "author" "project") "LamderaGenerated" -> newCanonical
    _ -> canonical


-- @LAMDERA Elm 0.19.4 Decls are a sorted list of Decl
removeUnsafeCoercePlaceholder :: Can.Decls -> Can.Decls
removeUnsafeCoercePlaceholder (Can.Decls (Graph.Sorted decls)) =
  let
    defName def =
      case def of
        Can.DValue_ (Reporting.Annotation.At _ n) _ -> n
        Can.DValue (Reporting.Annotation.At _ n) _ _ _ -> n
        Can.DFunc_ (Reporting.Annotation.At _ n) _ _ _ -> n
        Can.DFunc (Reporting.Annotation.At _ n) _ _ _ _ _ -> n

    keep def =
      defName def /= "unsafeCoerce"
  in
  decls
    & Maybe.mapMaybe (\decl ->
        case decl of
          Can.Decl def ->
            if keep def then Just decl else Nothing

          Can.DeclRec def defs ->
            case filter keep (def : defs) of
              [] -> Nothing
              [d] | not (keep def) -> Just (Can.Decl d)
              d : ds -> Just (Can.DeclRec d ds)
      )
    & Graph.Sorted
    & Can.Decls


updateDecls :: Module.Canonical -> Can.Decls -> Can.Decls
updateDecls fileName (Can.Decls (Graph.Sorted decls)) =
  decls
    & fmap (\decl ->
        case decl of
          Can.Decl def ->
            Can.Decl (updateDefs fileName def)

          Can.DeclRec def remainingDefs ->
            Can.DeclRec
              (updateDefs fileName def)
              (map (updateDefs fileName) remainingDefs)
      )
    & Graph.Sorted
    & Can.Decls


-- @LAMDERA the annotation of Lamdera.Effect.unsafeCoerce, `a -> b`
unsafeCoerceAnnotation :: I.Annotation
unsafeCoerceAnnotation =
  Convert.toAnnotation (Convert.envFromInterfaces Map.empty) $
    Forall
      (Map.fromList [("a", ()), ("b", ())])
      (TLambda (TVar "a") (TVar "b"))


updateExpr :: Module.Canonical -> Name.Name -> Can.Expr -> Can.Expr
updateExpr fileName functionName (Reporting.Annotation.At location_ expr_) =
    let replaceCall location param params =
          Can.Call
            (Reporting.Annotation.At location
              (Can.VarForeign
                (Module.Canonical (Name "lamdera" "core") "Lamdera.Effect")
                "unsafeCoerce"
                unsafeCoerceAnnotation
              )
            ) param params

        go = updateExpr fileName functionName
    in
    (case expr_ of
      Can.Call (Reporting.Annotation.At location
          (Can.VarTopLevel (Module.Canonical (Name "author" "project") "LamderaHelpers") "unsafeCoerce")
        ) param params ->
          replaceCall location param params

      Can.Call (Reporting.Annotation.At location
          (Can.VarTopLevel (Module.Canonical (Name "author" "project") "LamderaCheckBoth") "unsafeCoerce")
        ) param params ->
          replaceCall location param params

      Can.Call (Reporting.Annotation.At location
          (Can.VarTopLevel (Module.Canonical (Name "author" "project") "LamderaGenerated") "unsafeCoerce")
        ) param params ->
          replaceCall location param params

      -- The recursive rest. Might be worth looking at revisiting recursion schemes again, esp if error messages have improved
      Can.List exprs -> Can.List (fmap go exprs)
      Can.Negate expr -> Can.Negate (go expr)
      Can.Binop name canonical name2 annotation expr expr2 -> Can.Binop name canonical name2 annotation (go expr) (go expr2)
      Can.Lambda pattern patterns expr -> Can.Lambda pattern patterns (go expr)
      Can.Call expr param params -> Can.Call (go expr) (go param) (fmap go params)
      Can.If exprs expr ->
        Can.If
          (fmap
            (\(first, second) ->
              (go first
              , go second
              )
            )
            exprs
          )
          (go expr)
      Can.LetValue value expr ->
        Can.LetValue
          (updateValue fileName value)
          (go expr)
      Can.LetFunc func expr ->
        Can.LetFunc
          (updateFunc fileName func)
          (go expr)
      Can.LetRec func funcs expr ->
        Can.LetRec
          (updateFunc fileName func)
          (fmap (updateFunc fileName) funcs)
          (go expr)
      Can.LetDestruct pattern expr expr2 ->
        Can.LetDestruct
          pattern
          (go expr)
          (go expr2)
      Can.Case expr branch branches ->
        let
          goBranch (Can.Branch pattern caseExpr) =
            Can.Branch pattern (go caseExpr)
        in
        Can.Case
          (go expr)
          (goBranch branch)
          (fmap goBranch branches)
      Can.Access expr name -> Can.Access (go expr) name
      Can.Update name expr fieldUpdates ->
        Can.Update
          name
          (go expr)
          (F.map
            (\(Can.Update_ region expr__) ->
                Can.Update_ region (updateExpr fileName functionName expr__)
            )
            fieldUpdates
          )
      Can.Record fields -> Can.Record (F.map (\field -> updateExpr fileName functionName field) fields)
      Can.Pair expr expr2 ->
        Can.Pair
          (go expr)
          (go expr2)
      Can.Triple expr expr2 expr3 ->
        Can.Triple
          (go expr)
          (go expr2)
          (go expr3)
      -- the rest have no expressions in them
      _ -> expr_
  )
  & Reporting.Annotation.At location_


updateDefs :: Module.Canonical -> Can.Def -> Can.Def
updateDefs fileName def =
    case def of
      Can.DValue_ name expr ->
        Can.DValue_ name (updateExpr fileName (Reporting.Annotation.toValue name) expr)

      Can.DValue name expr type_ annotation ->
        Can.DValue name (updateExpr fileName (Reporting.Annotation.toValue name) expr) type_ annotation

      Can.DFunc_ name pattern patterns expr ->
        Can.DFunc_ name pattern patterns (updateExpr fileName (Reporting.Annotation.toValue name) expr)

      Can.DFunc name arg args expr type_ annotation ->
        Can.DFunc name arg args (updateExpr fileName (Reporting.Annotation.toValue name) expr) type_ annotation


updateValue :: Module.Canonical -> Can.Value -> Can.Value
updateValue fileName value =
    case value of
      Can.Value_ name expr ->
        Can.Value_ name (updateExpr fileName (Reporting.Annotation.toValue name) expr)

      Can.Value name expr type_ annotation ->
        Can.Value name (updateExpr fileName (Reporting.Annotation.toValue name) expr) type_ annotation


updateFunc :: Module.Canonical -> Can.Func -> Can.Func
updateFunc fileName func =
    case func of
      Can.Func_ name pattern patterns expr ->
        Can.Func_ name pattern patterns (updateExpr fileName (Reporting.Annotation.toValue name) expr)

      Can.Func name arg args expr type_ annotation ->
        Can.Func name arg args (updateExpr fileName (Reporting.Annotation.toValue name) expr) type_ annotation
