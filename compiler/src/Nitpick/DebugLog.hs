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

import Lamdera
import StandaloneInstances

hasUselessDebugLogs :: Can.Module -> Either E.Error ()
hasUselessDebugLogs canonical =
    updateDecls (Can._name canonical) (Can._decls canonical)

updateDecls :: Module.Canonical -> Can.Decls -> Either E.Error ()
updateDecls fileName decls =
    case fileName of
        Module.Canonical (Name "author" "project") "LocalDev" ->
            Right ()

        Module.Canonical (Name "author" "project") _ ->
            case decls of
                Can.Declare def nextDecl ->
                    case updateDefs fileName def of
                        Left e -> Left e
                        Right () -> updateDecls fileName nextDecl

                Can.DeclareRec def remainingDefs nextDecl ->
                    case updateDefs fileName def of
                        Left e -> Left e
                        Right () ->
                            case updateDecls fileName nextDecl of
                                Left e -> Left e
                                Right () -> firstError (map (updateDefs fileName) remainingDefs)

                Can.SaveTheEnvironment ->
                    Right ()

        _ ->
            Right ()

updateDefs :: Module.Canonical -> Can.Def -> Either E.Error ()
updateDefs fileName def =
    case def of
        Can.Def name patterns expr ->
            updateExpr fileName (Reporting.Annotation.toValue name) expr

        Can.TypedDef name freeVars patterns expr type_ ->
            updateExpr fileName (Reporting.Annotation.toValue name) expr


firstError :: [Either E.Error ()] -> Either E.Error ()
firstError results =
    case results of
        Left e : _ -> Left e

        Right () : rest -> firstError rest

        [] -> Right ()


updateExpr :: Module.Canonical -> Name.Name -> Can.Expr -> Either E.Error ()
updateExpr fileName functionName (Reporting.Annotation.At _ expr) =
    case expr of
        Can.VarLocal name ->
            Right ()

        Can.VarTopLevel canonical name ->
            Right ()

        Can.VarKernel name name2 ->
            Right ()

        Can.VarForeign canonical name annotation ->
            Right ()

        Can.VarCtor ctorOpts canonical name zeroBased annotation ->
            Right ()

        Can.VarDebug canonical name annotation ->
            Right ()

        Can.VarOperator name canonical name2 annotation ->
            Right ()

        Can.Chr string ->
            Right ()

        Can.Str string ->
            Right ()

        Can.Int int ->
            Right ()

        Can.Float float ->
            Right ()

        Can.List exprs ->
            firstError (fmap (updateExpr fileName functionName) exprs)

        Can.Negate expr ->
            updateExpr fileName functionName expr

        Can.Binop name canonical name2 annotation expr expr2 ->
            case updateExpr fileName functionName expr of
                Left e -> Left e
                Right () -> updateExpr fileName functionName expr2

        Can.Lambda patterns expr ->
            updateExpr fileName functionName expr

        Can.Call expr exprs ->
            case updateExpr fileName functionName expr of
                Left e -> Left e
                Right () -> firstError (fmap (updateExpr fileName functionName) exprs)

        Can.If exprs expr ->
            case (updateExpr fileName functionName) expr of
                Left e -> Left e
                Right () ->
                    firstError
                        (fmap
                            (\(first, second) ->
                                case updateExpr fileName functionName first of
                                    Left e -> Left e
                                    Right () -> updateExpr fileName functionName second
                            )
                            exprs)

        Can.Let def expr ->
            case updateExpr fileName functionName expr of
                Left e -> Left e
                Right () ->
                    updateDefs fileName def

        Can.LetRec defs expr ->
            case updateExpr fileName functionName expr of
                Left e -> Left e
                Right () -> firstError (fmap (updateDefs fileName) defs)

        Can.LetDestruct
            _
            (Reporting.Annotation.At location (Can.Call (Reporting.Annotation.At _ (Can.VarDebug _ "log" annotation )) [ firstParam ]))
            _ ->
            Left (E.BadDebugLog location)

        Can.LetDestruct pattern expr expr2 ->
            case updateExpr fileName functionName expr of
                Left e -> Left e
                Right () -> updateExpr fileName functionName expr2

        Can.Case expr caseBranches ->
            case updateExpr fileName functionName expr of
                Left e -> Left e
                Right () ->
                    firstError
                        (fmap
                            (\(Can.CaseBranch _ caseExpr) -> updateExpr fileName functionName caseExpr)
                            caseBranches)


        Can.Accessor name ->
            Right ()

        Can.Access expr name ->
            updateExpr fileName functionName expr

        Can.Update name expr fieldUpdates ->
            case updateExpr fileName functionName expr of
                Left e -> Left e
                Right () ->
                    firstError (fmap (\(Can.FieldUpdate _ expr) -> updateExpr fileName functionName expr) (Map.elems fieldUpdates))

        Can.Record fields ->
            firstError (fmap (\field -> updateExpr fileName functionName field) (Map.elems fields))

        Can.Unit ->
            Right ()

        Can.Tuple expr expr2 maybeExpr ->
            case updateExpr fileName functionName expr of
                Left e -> Left e
                Right () ->
                    case updateExpr fileName functionName expr2 of
                        Left e -> Left e
                        Right () ->
                            case fmap (updateExpr fileName functionName) maybeExpr of
                                Just (Left e) -> Left e
                                _ -> Right ()


        Can.Shader shaderSource shaderTypes ->
            Right ()