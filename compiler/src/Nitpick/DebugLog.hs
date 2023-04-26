{-# LANGUAGE OverloadedStrings #-}

module Nitpick.DebugLog
  ( hasUselessDebugLogs
  )
  where


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
import qualified Reporting.Error as E
import qualified Reporting.Annotation as A
import qualified Data.NonEmptyList as NE

import Lamdera
import StandaloneInstances

hasUselessDebugLogs :: Can.Module -> Either E.Error ()
hasUselessDebugLogs canonical =
    case updateDecls (Can._name canonical) (Can._decls canonical) of
        first : rest -> debugPass "errors" (NE.List first rest) Left $ E.LamderaBadDebugLog $ NE.List first rest
        [] -> Right ()

updateDecls :: Module.Canonical -> Can.Decls -> [A.Region]
updateDecls fileName decls =
    case decls of
        Can.Declare def nextDecl ->
            updateDefs fileName def ++ updateDecls fileName nextDecl

        Can.DeclareRec def remainingDefs nextDecl ->
            updateDefs fileName def ++ updateDecls fileName nextDecl ++ concatMap (updateDefs fileName) remainingDefs

        Can.SaveTheEnvironment ->
            []



updateDefs :: Module.Canonical -> Can.Def -> [A.Region]
updateDefs fileName def =
    case def of
        Can.Def name patterns expr ->
            updateExpr fileName (Reporting.Annotation.toValue name) expr

        Can.TypedDef name freeVars patterns expr type_ ->
            updateExpr fileName (Reporting.Annotation.toValue name) expr


updateExpr :: Module.Canonical -> Name.Name -> Can.Expr -> [A.Region]
updateExpr fileName functionName (Reporting.Annotation.At _ expr) =
    case expr of
        Can.VarLocal name ->
            []

        Can.VarTopLevel canonical name ->
            []

        Can.VarKernel name name2 ->
            []

        Can.VarForeign canonical name annotation ->
            []

        Can.VarCtor ctorOpts canonical name zeroBased annotation ->
            []

        Can.VarDebug canonical name annotation ->
            []

        Can.VarOperator name canonical name2 annotation ->
            []

        Can.Chr string ->
            []

        Can.Str string ->
            []

        Can.Int int ->
            []

        Can.Float float ->
            []

        Can.List exprs ->
            concatMap (updateExpr fileName functionName) exprs

        Can.Negate expr ->
            updateExpr fileName functionName expr

        Can.Binop name canonical name2 annotation expr expr2 ->
            updateExpr fileName functionName expr ++ updateExpr fileName functionName expr2

        Can.Lambda patterns expr ->
            updateExpr fileName functionName expr

        Can.Call expr exprs ->
            updateExpr fileName functionName expr ++ concatMap (updateExpr fileName functionName) exprs

        Can.If exprs expr ->
            updateExpr fileName functionName expr
                ++ concatMap
                    (\(first, second) ->
                        updateExpr fileName functionName first ++ updateExpr fileName functionName second
                    )
                    exprs

        Can.Let def expr ->
            updateExpr fileName functionName expr ++ updateDefs fileName def

        Can.LetRec defs expr ->
            updateExpr fileName functionName expr ++ concatMap (updateDefs fileName) defs

        Can.LetDestruct
            (Reporting.Annotation.At (A.Region start _) Can.PAnything)
            (Reporting.Annotation.At (A.Region _ end) (Can.Call (Reporting.Annotation.At _ (Can.VarDebug _ "log" annotation )) [ firstParam ]))
            expr ->
            [ A.Region start end ] ++ updateExpr fileName functionName expr

        Can.LetDestruct pattern expr expr2 ->
            updateExpr fileName functionName expr ++ updateExpr fileName functionName expr2

        Can.Case expr caseBranches ->
            updateExpr fileName functionName expr
                ++ concatMap
                    (\(Can.CaseBranch _ caseExpr) -> updateExpr fileName functionName caseExpr)
                    caseBranches

        Can.Accessor name ->
            []

        Can.Access expr name ->
            updateExpr fileName functionName expr

        Can.Update name expr fieldUpdates ->
            updateExpr fileName functionName expr
                ++ concatMap (\(Can.FieldUpdate _ expr) -> updateExpr fileName functionName expr) (Map.elems fieldUpdates)

        Can.Record fields ->
            concatMap (\field -> updateExpr fileName functionName field) (Map.elems fields)

        Can.Unit ->
            []

        Can.Tuple expr expr2 maybeExpr ->
            updateExpr fileName functionName expr
                ++ updateExpr fileName functionName expr2
                ++ concatMap (updateExpr fileName functionName) maybeExpr


        Can.Shader shaderSource shaderTypes ->
            []