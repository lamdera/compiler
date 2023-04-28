{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Nitpick.DebugLog
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
    case checkDecls (Can._name canonical) (Can._decls canonical) of
        first : rest -> debugPass "errors" (NE.List first rest) Left $ E.LamderaBadDebugLog $ NE.List first rest
        [] -> Right ()

checkDecls :: Module.Canonical -> Can.Decls -> [A.Region]
checkDecls fileName decls =
    case decls of
        Can.Declare def nextDecl ->
            checkDefs fileName def ++ checkDecls fileName nextDecl

        Can.DeclareRec def remainingDefs nextDecl ->
            checkDefs fileName def ++ checkDecls fileName nextDecl ++ concatMap (checkDefs fileName) remainingDefs

        Can.SaveTheEnvironment ->
            []



checkDefs :: Module.Canonical -> Can.Def -> [A.Region]
checkDefs fileName def =
    case def of
        Can.Def name patterns expr ->
            checkExpr fileName (Reporting.Annotation.toValue name) expr

        Can.TypedDef name freeVars patterns expr type_ ->
            checkExpr fileName (Reporting.Annotation.toValue name) expr


checkExpr :: Module.Canonical -> Name.Name -> Can.Expr -> [A.Region]
checkExpr fileName functionName (Reporting.Annotation.At _ expr) =
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
            concatMap (checkExpr fileName functionName) exprs

        Can.Negate expr ->
            checkExpr fileName functionName expr

        Can.Binop name canonical name2 annotation expr expr2 ->
            checkExpr fileName functionName expr ++ checkExpr fileName functionName expr2

        Can.Lambda patterns expr ->
            checkExpr fileName functionName expr

        Can.Call expr exprs ->
            checkExpr fileName functionName expr ++ concatMap (checkExpr fileName functionName) exprs

        Can.If exprs expr ->
            checkExpr fileName functionName expr
                ++ concatMap
                    (\(first, second) ->
                        checkExpr fileName functionName first ++ checkExpr fileName functionName second
                    )
                    exprs

        Can.Let def expr ->
            checkExpr fileName functionName expr ++ checkDefs fileName def

        Can.LetRec defs expr ->
            checkExpr fileName functionName expr ++ concatMap (checkDefs fileName) defs

        Can.LetDestruct
            (Reporting.Annotation.At (A.Region start _) Can.PAnything)
            (Reporting.Annotation.At (A.Region _ end) (Can.Call (Reporting.Annotation.At _ (Can.VarDebug _ "log" annotation )) [ firstParam ]))
            expr ->
            [ A.Region start end ] ++ checkExpr fileName functionName expr

        Can.LetDestruct pattern expr expr2 ->
            checkExpr fileName functionName expr ++ checkExpr fileName functionName expr2

        Can.Case expr caseBranches ->
            checkExpr fileName functionName expr
                ++ concatMap
                    (\(Can.CaseBranch _ caseExpr) -> checkExpr fileName functionName caseExpr)
                    caseBranches

        Can.Accessor name ->
            []

        Can.Access expr name ->
            checkExpr fileName functionName expr

        Can.Update name expr fieldUpdates ->
            checkExpr fileName functionName expr
                ++ concatMap (\(Can.FieldUpdate _ expr) -> checkExpr fileName functionName expr) (Map.elems fieldUpdates)

        Can.Record fields ->
            concatMap (\field -> checkExpr fileName functionName field) (Map.elems fields)

        Can.Unit ->
            []

        Can.Tuple expr expr2 maybeExpr ->
            checkExpr fileName functionName expr
                ++ checkExpr fileName functionName expr2
                ++ concatMap (checkExpr fileName functionName) maybeExpr


        Can.Shader shaderSource shaderTypes ->
            []