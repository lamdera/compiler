{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lamdera.Evergreen.ModifyAST (update) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Name as Name
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import NeatInterpolation

import qualified Data.Utf8
import AST.Canonical
import Elm.Package
import qualified AST.Canonical as Can
import qualified Elm.ModuleName as Module
import qualified Reporting.Annotation
import qualified Data.ByteString.Builder as B

import Lamdera
import StandaloneInstances


{-|

Finds any local vars `unsafeCoerce` within the `LamderaGenerated` module
and replaces them with a foreign var `Lamdera.Effect.unsafeCoerce`.

This allows us to use `unsafeCoerce` in the generated code without exposing
it in the lamdera/core module, so it cannot be used in regular user code.

-}
update :: Can.Module -> Can.Module
update canonical =
  let
    moduleName :: Module.Canonical = (Can._name canonical)
    decls :: Can.Decls = (Can._decls canonical)
    newDecls :: Can.Decls = updateDecls moduleName decls
  in
  canonical { Can._decls = newDecls }


updateDecls :: Module.Canonical -> Can.Decls -> Can.Decls
updateDecls fileName decls =
  case fileName of
    Module.Canonical (Name "author" "project") "LamderaGenerated" ->
      case decls of
        Can.Declare def nextDecl ->
          Can.Declare (updateDefs fileName def) (updateDecls fileName nextDecl)

        Can.DeclareRec def remainingDefs nextDecl ->
          Can.DeclareRec
            (updateDefs fileName def)
            (map (updateDefs fileName) remainingDefs)
            (updateDecls fileName nextDecl)

        Can.SaveTheEnvironment ->
          Can.SaveTheEnvironment

    _ ->
        decls


updateExpr :: Module.Canonical -> Name.Name -> Can.Expr -> Can.Expr
updateExpr fileName functionName (Reporting.Annotation.At location_ expr_) =
    (case expr_ of
      Can.VarLocal name -> Can.VarLocal name
      Can.VarTopLevel canonical name -> Can.VarTopLevel canonical name
      Can.VarKernel name name2 -> Can.VarKernel name name2
      Can.VarForeign canonical name annotation -> Can.VarForeign canonical name annotation
      Can.VarCtor ctorOpts canonical name zeroBased annotation -> Can.VarCtor ctorOpts canonical name zeroBased annotation
      Can.VarDebug canonical name annotation -> Can.VarDebug canonical name annotation
      Can.VarOperator name canonical name2 annotation -> Can.VarOperator name canonical name2 annotation
      Can.Chr string -> Can.Chr string
      Can.Str string -> Can.Str string
      Can.Int int -> Can.Int int
      Can.Float float -> Can.Float float
      Can.List exprs -> Can.List (fmap (updateExpr fileName functionName) exprs)
      Can.Negate expr -> Can.Negate ((updateExpr fileName functionName) expr)
      Can.Binop name canonical name2 annotation expr expr2 -> Can.Binop name canonical name2 annotation ((updateExpr fileName functionName) expr) ((updateExpr fileName functionName) expr2)
      Can.Lambda patterns expr -> Can.Lambda patterns ((updateExpr fileName functionName) expr)
      Can.Call (Reporting.Annotation.At location (Can.VarLocal "unsafeCoerce") ) params ->
        Can.Call
          (Reporting.Annotation.At
            location
            (Can.VarForeign
              (Module.Canonical (Name "lamdera" "core") "Lamdera.Effect")
              "unsafeCoerce"
              (Forall Map.empty (TLambda (TVar "a") (TVar "b")))
            )
          )
          params
      Can.Call expr exprs -> Can.Call ((updateExpr fileName functionName) expr) (fmap (updateExpr fileName functionName) exprs)
      Can.If exprs expr ->
        Can.If
          (fmap
            (\(first, second) ->
              ((updateExpr fileName functionName) first
              , (updateExpr fileName functionName) second
              )
            )
            exprs
          )
          ((updateExpr fileName functionName) expr)
      Can.Let def expr ->
        Can.Let
          (updateDefs fileName def)
          ((updateExpr fileName functionName) expr)
      Can.LetRec defs expr ->
        Can.LetRec
          (fmap (updateDefs fileName) defs)
          ((updateExpr fileName functionName) expr)
      Can.LetDestruct pattern expr expr2 ->
        Can.LetDestruct
          pattern
          ((updateExpr fileName functionName) expr)
          ((updateExpr fileName functionName) expr2)
      Can.Case expr caseBranches ->
        Can.Case
          ((updateExpr fileName functionName) expr)
          (fmap
            (\(Can.CaseBranch pattern caseExpr) ->
                Can.CaseBranch pattern ((updateExpr fileName functionName) caseExpr)
            )
            caseBranches
          )
      Can.Accessor name -> Can.Accessor name
      Can.Access expr name -> Can.Access ((updateExpr fileName functionName) expr) name
      Can.Update name expr fieldUpdates ->
        Can.Update
          name
          ((updateExpr fileName functionName) expr)
          (fmap
            (\(Can.FieldUpdate region expr__) ->
                Can.FieldUpdate region (updateExpr fileName functionName expr__)
            )
            fieldUpdates
          )
      Can.Record fields -> Can.Record (fmap (\field -> updateExpr fileName functionName field) fields)
      Can.Unit -> Can.Unit
      Can.Tuple expr expr2 maybeExpr ->
        Can.Tuple
          ((updateExpr fileName functionName) expr)
          ((updateExpr fileName functionName) expr2)
          (fmap (updateExpr fileName functionName) maybeExpr)
      Can.Shader shaderSource shaderTypes -> Can.Shader shaderSource shaderTypes
  )
  & Reporting.Annotation.At location_


updateDefs :: Module.Canonical -> Can.Def -> Can.Def
updateDefs fileName def =
    case def of
      Can.Def name patterns expr ->
        Can.Def
          name
          patterns
          ((updateExpr fileName (Reporting.Annotation.toValue name)) expr)

      Can.TypedDef name freeVars patterns expr type_ ->
        Can.TypedDef
          name
          freeVars
          patterns
          ((updateExpr fileName (Reporting.Annotation.toValue name)) expr)
          type_
