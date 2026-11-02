{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Nitpick.DebugLog (hasDebug, hasUselessDebugLogs) where

import qualified Lamdera.Lexical as Lexical
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified AST.Prim.Name as Name
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import NeatInterpolation

import qualified Data.Utf8
import Elm.Package
import qualified AST.Canonical as Can
import qualified AST.Typed as Tpd
import qualified AST.Prim.Fields as F
import qualified Data.NonEmptyMap as NEM
import qualified Graph
import qualified AST.Optimized as Opt
import qualified Elm.ModuleName as Module
import qualified Reporting.Annotation
import qualified Data.ByteString.Builder as B
import qualified Reporting.Error as E
import qualified Reporting.Annotation as A
import qualified Data.NonEmptyList as NE

import Lamdera
import StandaloneInstances


hasDebug :: Opt.Expr -> Bool -> Bool
hasDebug expression original = do
    -- @TODO Replace with global that activates for lamdera deploy and check
    let ignoreDebugLog = unsafePerformIO $ Lamdera.isCheckMode
    case expression of
        Opt.VarDebug name _ _ _ | ignoreDebugLog -> name /= "log"
        _                                        -> original


-- @LAMDERA Elm 0.19.4: runs on the type checked module (AST.Typed), whose decls are
-- the canonical ones
hasUselessDebugLogs :: Tpd.Module -> Either E.Error ()
hasUselessDebugLogs typed =
    case checkDecls (Tpd._decls typed) of
        first : rest -> Left $ E.LamderaBadDebugLog $ NE.List first rest
        [] -> Right ()


checkDecls :: Can.Decls -> [A.Region]
checkDecls (Can.Decls (Graph.Sorted decls)) =
    concatMap
        (\decl ->
            case decl of
                Can.Decl def ->
                    checkDefs def

                Can.DeclRec def remainingDefs ->
                    checkDefs def ++ concatMap checkDefs remainingDefs
        )
        decls


checkDefs :: Can.Def -> [A.Region]
checkDefs def =
    case def of
        Can.DValue_ name expr -> checkExpr (Reporting.Annotation.toValue name) expr
        Can.DValue name expr _ _ -> checkExpr (Reporting.Annotation.toValue name) expr
        Can.DFunc_ name _ _ expr -> checkExpr (Reporting.Annotation.toValue name) expr
        Can.DFunc name _ _ expr _ _ -> checkExpr (Reporting.Annotation.toValue name) expr


checkValue :: Can.Value -> [A.Region]
checkValue value =
    case value of
        Can.Value_ name expr -> checkExpr (Reporting.Annotation.toValue name) expr
        Can.Value name expr _ _ -> checkExpr (Reporting.Annotation.toValue name) expr


checkFunc :: Can.Func -> [A.Region]
checkFunc func =
    case func of
        Can.Func_ name _ _ expr -> checkExpr (Reporting.Annotation.toValue name) expr
        Can.Func name _ _ expr _ _ -> checkExpr (Reporting.Annotation.toValue name) expr


checkExpr :: Name.Name -> Can.Expr -> [A.Region]
checkExpr functionName (Reporting.Annotation.At _ expr) =
    case expr of
        Can.List exprs ->
            concatMap (checkExpr functionName) exprs

        Can.Negate expr_ ->
            checkExpr functionName expr_

        Can.Binop _ _ _ _ expr_ expr2 ->
            checkExpr functionName expr_ ++ checkExpr functionName expr2

        Can.Lambda _ _ expr_ ->
            checkExpr functionName expr_

        Can.Call expr_ first rest ->
            checkExpr functionName expr_ ++ concatMap (checkExpr functionName) (first : rest)

        Can.If exprs expr_ ->
            checkExpr functionName expr_
                ++ concatMap
                    (\(first, second) ->
                        checkExpr functionName first ++ checkExpr functionName second
                    )
                    exprs

        Can.LetValue value expr_ ->
            checkExpr functionName expr_ ++ checkValue value

        Can.LetFunc func expr_ ->
            checkExpr functionName expr_ ++ checkFunc func

        Can.LetRec func funcs expr_ ->
            checkExpr functionName expr_ ++ concatMap checkFunc (func : funcs)

        Can.LetDestruct
            (Reporting.Annotation.At (A.Region start _) Can.PAnything)
            (Reporting.Annotation.At (A.Region _ end) (Can.Call (Reporting.Annotation.At _ (Can.VarDebug _ "log" _)) _ []))
            expr_ ->
            [ A.Region start end ] ++ checkExpr functionName expr_

        Can.LetDestruct _ expr_ expr2 ->
            checkExpr functionName expr_ ++ checkExpr functionName expr2

        Can.Case expr_ branch branches ->
            checkExpr functionName expr_
                ++ concatMap
                    (\(Can.Branch _ caseExpr) -> checkExpr functionName caseExpr)
                    (branch : branches)

        Can.Access expr_ _ ->
            checkExpr functionName expr_

        Can.Update _ expr_ fieldUpdates ->
            checkExpr functionName expr_
                ++ concatMap (\(Can.Update_ _ e) -> checkExpr functionName e) (Lexical.elems (NEM.toDict (F.toNEM fieldUpdates)))

        Can.Record fields ->
            concatMap (\field -> checkExpr functionName field) (Lexical.elems (NEM.toDict (F.toNEM fields)))

        Can.Pair expr_ expr2 ->
            checkExpr functionName expr_
                ++ checkExpr functionName expr2

        Can.Triple expr_ expr2 expr3 ->
            checkExpr functionName expr_
                ++ checkExpr functionName expr2
                ++ checkExpr functionName expr3

        -- the rest have no expressions in them
        _ ->
            []
