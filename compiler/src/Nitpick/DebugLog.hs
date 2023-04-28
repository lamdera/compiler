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
    case checkDecls (Can._decls canonical) of
        first : rest -> Left $ E.LamderaBadDebugLog $ NE.List first rest
        [] -> Right ()

checkDecls :: Can.Decls -> [A.Region]
checkDecls decls =
    case decls of
        Can.Declare def nextDecl ->
            checkDefs def ++ checkDecls nextDecl

        Can.DeclareRec def remainingDefs nextDecl ->
            checkDefs def ++ checkDecls nextDecl ++ concatMap (checkDefs) remainingDefs

        Can.SaveTheEnvironment ->
            []



checkDefs :: Can.Def -> [A.Region]
checkDefs def =
    case def of
        Can.Def name patterns expr ->
            checkExpr (Reporting.Annotation.toValue name) expr

        Can.TypedDef name freeVars patterns expr type_ ->
            checkExpr (Reporting.Annotation.toValue name) expr


checkExpr :: Name.Name -> Can.Expr -> [A.Region]
checkExpr functionName (Reporting.Annotation.At _ expr) =
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
            concatMap (checkExpr functionName) exprs

        Can.Negate expr ->
            checkExpr functionName expr

        Can.Binop name canonical name2 annotation expr expr2 ->
            checkExpr functionName expr ++ checkExpr functionName expr2

        Can.Lambda patterns expr ->
            checkExpr functionName expr

        Can.Call expr exprs ->
            checkExpr functionName expr ++ concatMap (checkExpr functionName) exprs

        Can.If exprs expr ->
            checkExpr functionName expr
                ++ concatMap
                    (\(first, second) ->
                        checkExpr functionName first ++ checkExpr functionName second
                    )
                    exprs

        Can.Let def expr ->
            checkExpr functionName expr ++ checkDefs def

        Can.LetRec defs expr ->
            checkExpr functionName expr ++ concatMap (checkDefs) defs

        Can.LetDestruct
            (Reporting.Annotation.At (A.Region start _) Can.PAnything)
            (Reporting.Annotation.At (A.Region _ end) (Can.Call (Reporting.Annotation.At _ (Can.VarDebug _ "log" annotation )) [ firstParam ]))
            expr ->
            [ A.Region start end ] ++ checkExpr functionName expr

        Can.LetDestruct pattern expr expr2 ->
            checkExpr functionName expr ++ checkExpr functionName expr2

        Can.Case expr caseBranches ->
            checkExpr functionName expr
                ++ concatMap
                    (\(Can.CaseBranch _ caseExpr) -> checkExpr functionName caseExpr)
                    caseBranches

        Can.Accessor name ->
            []

        Can.Access expr name ->
            checkExpr functionName expr

        Can.Update name expr fieldUpdates ->
            checkExpr functionName expr
                ++ concatMap (\(Can.FieldUpdate _ expr) -> checkExpr functionName expr) (Map.elems fieldUpdates)

        Can.Record fields ->
            concatMap (\field -> checkExpr functionName field) (Map.elems fields)

        Can.Unit ->
            []

        Can.Tuple expr expr2 maybeExpr ->
            checkExpr functionName expr
                ++ checkExpr functionName expr2
                ++ concatMap (checkExpr functionName) maybeExpr


        Can.Shader shaderSource shaderTypes ->
            []