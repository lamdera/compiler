{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Lamdera.UiSourceMap
    (updateDecls)
    where

import qualified Data.Map as Map
import qualified Data.Name as Name

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import AST.Canonical
import qualified AST.Optimized as Opt
import qualified Canonicalize.Module as Canonicalize
import qualified Elm.Float
import qualified Elm.Interface as I
import qualified Elm.ModuleName as Module
import Elm.Package
import qualified Elm.Package as Pkg
import qualified Nitpick.PatternMatches as PatternMatches
import qualified Optimize.Module as Optimize
import qualified Reporting.Annotation
import qualified Reporting.Error as E
import qualified Reporting.Result as R
import qualified Reporting.Render.Type.Localizer as Localizer
import qualified Type.Constrain.Module as Type
import qualified Type.Solve as Type

-- import System.IO.Unsafe (unsafePerformIO)


-- @DEPRECATED for alpha12 release migration only
import qualified Lamdera.Wire2.Core
import qualified Lamdera.Wire2.Interfaces

import qualified Lamdera.Wire3.Core
import qualified Lamdera.Wire3.Interfaces
import qualified Lamdera.Wire3.Helpers as Lamdera.Wire
import Lamdera
import qualified CanSer.CanSer as ToSource
import qualified Data.Text as T
import qualified Data.Utf8

newAttributes location =
    let
        a = Reporting.Annotation.At location
    in
    (a (List
          [ (a (Call
                  (a (VarForeign
                        (Module.Canonical (Name "mdgriffith" "elm-ui") "Element.Background")
                        "color"
                        (Forall
                           (Map.fromList [("decorative", ()), ("msg", ())])
                           (TLambda
                              (TType (Module.Canonical (Name "mdgriffith" "elm-ui") "Element") "Color" [])
                              (TType (Module.Canonical (Name "mdgriffith" "elm-ui") "Element") "Attr" [TVar "decorative", TVar "msg"])))))
                  [ (a (Call
                          (a (VarForeign
                                (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                                "rgb255"
                                (Forall
                                   (Map.fromList [])
                                   (TLambda
                                      (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])
                                      (TLambda
                                         (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])
                                         (TLambda
                                            (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])
                                            (TType (Module.Canonical (Name "mdgriffith" "elm-ui") "Element") "Color" [])))))))
                          [(a (Int 50)), (a (Int 200)), (a (Int 100))]))
                  ]))
          ]))

updateExpr :: Can.Expr -> Can.Expr
updateExpr (Reporting.Annotation.At location expr) =
    (case expr of
        Can.VarLocal name ->
            Can.VarLocal name

        Can.VarTopLevel canonical name ->
            Can.VarTopLevel canonical name

        Can.VarKernel name name2 ->
            Can.VarKernel name name2

        Can.VarForeign canonical name annotation ->
            Can.VarForeign canonical name annotation

        Can.VarCtor ctorOpts canonical name zeroBased annotation ->
            Can.VarCtor ctorOpts canonical name zeroBased annotation

        Can.VarDebug canonical name annotation ->
            Can.VarDebug canonical name annotation

        Can.VarOperator name canonical name2 annotation ->
            Can.VarOperator name canonical name2 annotation

        Can.Chr string ->
            Can.Chr string

        Can.Str string ->
            Can.Str string

        Can.Int int ->
            Can.Int int

        Can.Float float ->
            Can.Float float

        Can.List exprs ->
            Can.List (fmap updateExpr exprs)

        Can.Negate expr ->
            Can.Negate (updateExpr expr)

        Can.Binop name canonical name2 annotation expr expr2 ->
            Can.Binop name canonical name2 annotation (updateExpr expr) (updateExpr expr2)

        Can.Lambda patterns expr ->
            Can.Lambda patterns (updateExpr expr)

        Can.Call
            (Reporting.Annotation.At
                location
                (Can.VarForeign
                    (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                    "el"
                    annotation
                )
            )
            (firstParam : rest) ->
            let
                {-| List.append firstParam newAttributes  -}
                finalAttributes :: Can.Expr
                finalAttributes =
                    firstParam --newAttributes location
--                        Reporting.Annotation.At
--                            location
--                            (Can.Call
--                                listAppend
--                                [ newAttributes
--                                , Reporting.Annotation.At
--                                  location
--                                  (Can.List [])
--                                --newAttributes
--                                ]
--                            )
                elmUi =
                    Name "mdgriffith" "elm-ui"
            in
            Can.Call
                (Reporting.Annotation.At
                    location
                    (Can.VarForeign (Module.Canonical elmUi "Element") "el" annotation)
                )
                (finalAttributes : fmap updateExpr rest)
                & debugHaskell "Note"

        Can.Call expr exprs ->
            Can.Call (updateExpr expr) (fmap updateExpr exprs)

        Can.If exprs expr ->
            Can.If
                (fmap (\(first, second) -> (updateExpr first, updateExpr second)) exprs)
                (updateExpr expr)

        Can.Let def expr ->
            Can.Let def (updateExpr expr)

        Can.LetRec defs expr ->
            Can.LetRec defs (updateExpr expr)

        Can.LetDestruct pattern expr expr2 ->
            Can.LetDestruct pattern (updateExpr expr) (updateExpr expr2)

        Can.Case expr caseBranches ->
            Can.Case
                (updateExpr expr)
                (fmap
                    (\(Can.CaseBranch pattern caseExpr) ->
                        Can.CaseBranch pattern (updateExpr caseExpr)
                    )
                    caseBranches
                )

        Can.Accessor name ->
            Can.Accessor name

        Can.Access expr name ->
            Can.Access (updateExpr expr) name

        Can.Update name expr fieldUpdates ->
            Can.Update name (updateExpr expr) fieldUpdates

        Can.Record fields ->
            Can.Record fields

        Can.Unit ->
            Can.Unit

        Can.Tuple expr expr2 maybeExpr ->
            Can.Tuple (updateExpr expr) (updateExpr expr2) (fmap updateExpr maybeExpr)

        Can.Shader shaderSource shaderTypes ->
            Can.Shader shaderSource shaderTypes
    )
    & Reporting.Annotation.At location

updateDefs :: Can.Def -> Can.Def
updateDefs def =
    case def of
        Can.Def name patterns expr ->
            Can.Def name patterns (updateExpr expr)

        Can.TypedDef name freeVars patterns expr type_ ->
            Can.TypedDef name freeVars patterns (updateExpr expr) type_


updateDecls :: Can.Decls -> Can.Decls
updateDecls decls =
  -- error "todo!"
    case decls of
        Can.Declare def nextDecl ->
            Can.Declare (updateDefs def) (updateDecls nextDecl)

        Can.DeclareRec def remainingDefs nextDecl ->
            Can.DeclareRec
                (updateDefs def)
                (map updateDefs remainingDefs)
                (updateDecls nextDecl)

        Can.SaveTheEnvironment ->
            Can.SaveTheEnvironment