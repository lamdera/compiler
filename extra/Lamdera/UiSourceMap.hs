{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Lamdera.UiSourceMap
    (updateDecls, src)
    where

import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Text.Encoding as T

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
import NeatInterpolation
import qualified Data.ByteString.Builder as B


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

newAttributes fileName functionName location originalAttributes =
    let
        a = Reporting.Annotation.At location
    in
    (a (Call
          (a (VarForeign
                (Module.Canonical (Name "elm" "core") "List")
                "append"
                (Forall
                   (Map.fromList [("a", ())])
                   (TLambda
                      (TType (Module.Canonical (Name "elm" "core") "List") "List" [TVar "a"])
                      (TLambda
                         (TType (Module.Canonical (Name "elm" "core") "List") "List" [TVar "a"])
                         (TType (Module.Canonical (Name "elm" "core") "List") "List" [TVar "a"]))))))
          [ originalAttributes
          , newAttributesHelper fileName functionName location
          ]))

jsPropertyName = "triggerUrl123"


moduleToFilePath :: Module.Canonical -> String
moduleToFilePath ((Module.Canonical pkg moduleName)) =
    moduleName & Name.toText & T.replace "." "/" & (\v -> v <> ".elm") & T.unpack


newAttributesHelper :: Module.Canonical -> Name.Name -> Reporting.Annotation.Region -> Can.Expr
newAttributesHelper module_ functionName location =
    let
        (Reporting.Annotation.Region (Reporting.Annotation.Position row column) _) =
            location

        lineNumber =
            Name.toChars functionName
                ++ "," ++ (moduleToFilePath module_)
                ++ ":" ++ show row
                ++ ":" ++ show column

                & Data.Utf8.fromChars

        a =
            Reporting.Annotation.At location
    in
    (a (List
      [ (a (Binop
              "|>"
              (Module.Canonical (Name "elm" "core") "Basics")
              "apR"
              (Forall (Map.fromList [("a", ()), ("b", ())]) (TLambda (TVar "a") (TLambda (TLambda (TVar "a") (TVar "b")) (TVar "b"))))
              (a (Call
                    (a (VarForeign
                          (Module.Canonical (Name "elm" "html") "Html.Events")
                          "on"
                          (Forall
                             (Map.fromList [("msg", ())])
                             (TLambda
                                (TType (Module.Canonical (Name "elm" "core") "String") "String" [])
                                (TLambda
                                   (TType (Module.Canonical (Name "elm" "json") "Json.Decode") "Decoder" [TVar "msg"])
                                   (TAlias
                                      (Module.Canonical (Name "elm" "html") "Html")
                                      "Attribute"
                                      [("msg", TVar "msg")]
                                      (Filled (TType (Module.Canonical (Name "elm" "virtual-dom") "VirtualDom") "Attribute" [TVar "msg"]))))))))
                    [ (a (Str "dblclick"))
                    , (a (Binop
                            "|>"
                            (Module.Canonical (Name "elm" "core") "Basics")
                            "apR"
                            (Forall (Map.fromList [("a", ()), ("b", ())]) (TLambda (TVar "a") (TLambda (TLambda (TVar "a") (TVar "b")) (TVar "b"))))
                            (a (Call
                                  (a (VarForeign
                                        (Module.Canonical (Name "elm" "json") "Json.Decode")
                                        "field"
                                        (Forall
                                           (Map.fromList [("a", ())])
                                           (TLambda
                                              (TType (Module.Canonical (Name "elm" "core") "String") "String" [])
                                              (TLambda
                                                 (TType (Module.Canonical (Name "elm" "json") "Json.Decode") "Decoder" [TVar "a"])
                                                 (TType (Module.Canonical (Name "elm" "json") "Json.Decode") "Decoder" [TVar "a"]))))))
                                  [ (a (Str jsPropertyName))
                                  , (a (VarForeign
                                          (Module.Canonical (Name "elm" "json") "Json.Decode")
                                          "int"
                                          (Forall
                                             (Map.fromList [])
                                             (TType
                                                (Module.Canonical (Name "elm" "json") "Json.Decode")
                                                "Decoder"
                                                [TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []]))))
                                  ]))
                            (a (Call
                                  (a (VarForeign
                                        (Module.Canonical (Name "elm" "json") "Json.Decode")
                                        "andThen"
                                        (Forall
                                           (Map.fromList [("a", ()), ("b", ())])
                                           (TLambda
                                              (TLambda (TVar "a") (TType (Module.Canonical (Name "elm" "json") "Json.Decode") "Decoder" [TVar "b"]))
                                              (TLambda
                                                 (TType (Module.Canonical (Name "elm" "json") "Json.Decode") "Decoder" [TVar "a"])
                                                 (TType (Module.Canonical (Name "elm" "json") "Json.Decode") "Decoder" [TVar "b"]))))))
                                  [ (a (Lambda
                                          [(a (PAnything))]
                                          (a (Call
                                                (a (VarForeign
                                                      (Module.Canonical (Name "elm" "json") "Json.Decode")
                                                      "fail"
                                                      (Forall
                                                         (Map.fromList [("a", ())])
                                                         (TLambda
                                                            (TType (Module.Canonical (Name "elm" "core") "String") "String" [])
                                                            (TType (Module.Canonical (Name "elm" "json") "Json.Decode") "Decoder" [TVar "a"])))))
                                                [(a (Str ""))]))))
                                  ]))))
                    ]))
              (a (VarForeign
                    (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                    "htmlAttribute"
                    (Forall
                       (Map.fromList [("msg", ())])
                       (TLambda
                          (TAlias
                             (Module.Canonical (Name "elm" "html") "Html")
                             "Attribute"
                             [("msg", TVar "msg")]
                             (Filled (TType (Module.Canonical (Name "elm" "virtual-dom") "VirtualDom") "Attribute" [TVar "msg"])))
                          (TAlias
                             (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                             "Attribute"
                             [("msg", TVar "msg")]
                             (Filled (TType (Module.Canonical (Name "mdgriffith" "elm-ui") "Internal.Model") "Attribute" [TUnit, TVar "msg"])))))))))
      , (a (Binop
              "|>"
              (Module.Canonical (Name "elm" "core") "Basics")
              "apR"
              (Forall (Map.fromList [("a", ()), ("b", ())]) (TLambda (TVar "a") (TLambda (TLambda (TVar "a") (TVar "b")) (TVar "b"))))
              (a (Call
                    (a (VarForeign
                          (Module.Canonical (Name "elm" "html") "Html.Attributes")
                          "attribute"
                          (Forall
                             (Map.fromList [("msg", ())])
                             (TLambda
                                (TType (Module.Canonical (Name "elm" "core") "String") "String" [])
                                (TLambda
                                   (TType (Module.Canonical (Name "elm" "core") "String") "String" [])
                                   (TAlias
                                      (Module.Canonical (Name "elm" "html") "Html")
                                      "Attribute"
                                      [("msg", TVar "msg")]
                                      (Filled (TType (Module.Canonical (Name "elm" "virtual-dom") "VirtualDom") "Attribute" [TVar "msg"]))))))))
                    [(a (Str "line-number-attribute")), (a (Str lineNumber))]))
              (a (VarForeign
                    (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                    "htmlAttribute"
                    (Forall
                       (Map.fromList [("msg", ())])
                       (TLambda
                          (TAlias
                             (Module.Canonical (Name "elm" "html") "Html")
                             "Attribute"
                             [("msg", TVar "msg")]
                             (Filled (TType (Module.Canonical (Name "elm" "virtual-dom") "VirtualDom") "Attribute" [TVar "msg"])))
                          (TAlias
                             (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                             "Attribute"
                             [("msg", TVar "msg")]
                             (Filled (TType (Module.Canonical (Name "mdgriffith" "elm-ui") "Internal.Model") "Attribute" [TUnit, TVar "msg"])))))))))
      ]))

updateExpr :: Module.Canonical -> Name.Name -> Can.Expr -> Can.Expr
updateExpr fileName functionName (Reporting.Annotation.At location expr) =
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
            Can.List (fmap (updateExpr fileName functionName) exprs)

        Can.Negate expr ->
            Can.Negate ((updateExpr fileName functionName) expr)

        Can.Binop name canonical name2 annotation expr expr2 ->
            Can.Binop name canonical name2 annotation ((updateExpr fileName functionName) expr) ((updateExpr fileName functionName) expr2)

        Can.Lambda patterns expr ->
            Can.Lambda patterns ((updateExpr fileName functionName) expr)

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
            Can.Call
                (Reporting.Annotation.At
                    location
                    (Can.VarForeign
                        (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                        "el"
                        annotation
                    )
                )
                (newAttributes fileName functionName location firstParam : fmap (updateExpr fileName functionName) rest)

        Can.Call
            (Reporting.Annotation.At
                location
                (Can.VarForeign
                    (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                    "paragraph"
                    annotation
                )
            )
            (firstParam : rest) ->
            Can.Call
                (Reporting.Annotation.At
                    location
                    (Can.VarForeign
                        (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                        "paragraph"
                        annotation
                    )
                )
                (newAttributes fileName functionName location firstParam : fmap (updateExpr fileName functionName) rest)

        Can.Call
            (Reporting.Annotation.At
                location
                (Can.VarForeign
                    (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                    "wrappedRow"
                    annotation
                )
            )
            (firstParam : rest) ->
            Can.Call
                (Reporting.Annotation.At
                    location
                    (Can.VarForeign
                        (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                        "wrappedRow"
                        annotation
                    )
                )
                (newAttributes fileName functionName location firstParam : fmap (updateExpr fileName functionName) rest)

        Can.Call
            (Reporting.Annotation.At
                location
                (Can.VarForeign
                    (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                    "column"
                    annotation
                )
            )
            (firstParam : rest) ->
            Can.Call
                (Reporting.Annotation.At
                    location
                    (Can.VarForeign
                        (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                        "column"
                        annotation
                    )
                )
                (newAttributes fileName functionName location firstParam : fmap (updateExpr fileName functionName) rest)

        Can.Call
            (Reporting.Annotation.At
                location
                (Can.VarForeign
                    (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                    "row"
                    annotation
                )
            )
            (firstParam : rest) ->
            Can.Call
                (Reporting.Annotation.At
                    location
                    (Can.VarForeign
                        (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                        "row"
                        annotation
                    )
                )
                (newAttributes fileName functionName location firstParam : fmap (updateExpr fileName functionName) rest)

        Can.Call expr exprs ->
            Can.Call ((updateExpr fileName functionName) expr) (fmap (updateExpr fileName functionName) exprs)

        Can.If exprs expr ->
            Can.If
                (fmap (\(first, second) -> ((updateExpr fileName functionName) first, (updateExpr fileName functionName) second)) exprs)
                ((updateExpr fileName functionName) expr)

        Can.Let def expr ->
            Can.Let def ((updateExpr fileName functionName) expr)

        Can.LetRec defs expr ->
            Can.LetRec defs ((updateExpr fileName functionName) expr)

        Can.LetDestruct pattern expr expr2 ->
            Can.LetDestruct pattern ((updateExpr fileName functionName) expr) ((updateExpr fileName functionName) expr2)

        Can.Case expr caseBranches ->
            Can.Case
                ((updateExpr fileName functionName) expr)
                (fmap
                    (\(Can.CaseBranch pattern caseExpr) ->
                        Can.CaseBranch pattern ((updateExpr fileName functionName) caseExpr)
                    )
                    caseBranches
                )

        Can.Accessor name ->
            Can.Accessor name

        Can.Access expr name ->
            Can.Access ((updateExpr fileName functionName) expr) name

        Can.Update name expr fieldUpdates ->
            Can.Update name ((updateExpr fileName functionName) expr) fieldUpdates

        Can.Record fields ->
            Can.Record fields

        Can.Unit ->
            Can.Unit

        Can.Tuple expr expr2 maybeExpr ->
            Can.Tuple ((updateExpr fileName functionName) expr) ((updateExpr fileName functionName) expr2) (fmap (updateExpr fileName functionName) maybeExpr)

        Can.Shader shaderSource shaderTypes ->
            Can.Shader shaderSource shaderTypes
    )
    & Reporting.Annotation.At location

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


updateDecls :: Module.Canonical -> Can.Decls -> Can.Decls
updateDecls fileName decls =
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


src :: B.Builder
src =
  [text|

function getNodesWithLineNumber123(node) {
    let list = [];
    if (node.parentNode) {
        list = getNodesWithLineNumber123(node.parentNode);
    }
    if (node.attributes) {
        let attribute = node.attributes.getNamedItem("line-number-attribute");
        if (attribute) {
            let components = attribute.value.split(",");
            return [{ functionName : components[0], path: components[1] }].concat(list);
        }
    }
    return list;
}

var openedFileRecently123 = false;

Object.defineProperty(
    MouseEvent.prototype, 'triggerUrl123',
    { get: function()
        {
            if (openedFileRecently123 === false)
            {
                openedFileRecently123 = true;
                setTimeout(function() { openedFileRecently123 = false; }, 100);
                let nodes = getNodesWithLineNumber123(this.target);

                if (nodes.length > 1) {
                    let backgroundDiv = document.createElement("div");
                    backgroundDiv.style.left = "0px";
                    backgroundDiv.style.top = "0px";
                    backgroundDiv.style.position = "fixed";
                    backgroundDiv.style.width = "100%";
                    backgroundDiv.style.height = "100%";
                    backgroundDiv.onclick = function() { backgroundDiv.remove(); };

                    let div = document.createElement("div");
                    div.style.left = this.clientX + "px";
                    div.style.top = this.clientY + "px";
                    div.style.position = "absolute";
                    div.style.padding = "4px";
                    div.style.display = "flex";
                    div.style.flexDirection = "column";
                    div.style.background = "rgb(46, 51, 53)";
                    div.style.borderRadius = "5px";
                    div.style.color = "white";

                    nodes.forEach(node => {
                        let splitPath = node.path.split(":");
                        let moduleName = splitPath[0].substring(0,splitPath[0].length-3);
                        let button = document.createElement("button");
                        button.textContent = moduleName + node.functionName + ":" + splitPath[1];
                        button.style.padding = "4px";
                        button.style.textAlign = "right";
                        button.onclick = function() {
                            backgroundDiv.remove();
                            let xmlHttpReq = new XMLHttpRequest();
                            xmlHttpReq.open("GET", "/_x/editor/src/" + node.path, false);
                            xmlHttpReq.send(null);
                        };
                        div.appendChild(button);
                    });

                    backgroundDiv.appendChild(div);
                    document.body.appendChild(backgroundDiv);
                }
                else if (nodes.length === 1) {
                    let xmlHttpReq = new XMLHttpRequest();
                    xmlHttpReq.open("GET", "/_x/editor/src/" + nodes[0], false);
                    xmlHttpReq.send(null);
                }
            }
        }
    }
);
  |]
  & T.encodeUtf8Builder
