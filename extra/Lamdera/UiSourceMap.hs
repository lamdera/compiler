{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Lamdera.UiSourceMap
    (updateDecls, src)
    where

import qualified Data.Map as Map
import qualified Data.Set as Set
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


updateDecls :: Module.Canonical -> Can.Decls -> Can.Decls
updateDecls fileName decls =
    case fileName of
        Module.Canonical (Name "author" "project") "LocalDev" ->
            decls

        Module.Canonical (Name "author" "project") _ ->
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





newAttributes isElmUi fileName functionName location originalAttributes =
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
          [ updateExpr fileName functionName originalAttributes
          , newAttributesHelper isElmUi fileName functionName location
          ]))


moduleToFilePath :: Module.Canonical -> String
moduleToFilePath ((Module.Canonical pkg moduleName)) =
    moduleName & Name.toText & T.replace "." "/" & (\v -> v <> ".elm") & T.unpack


newAttributesHelper :: Bool -> Module.Canonical -> Name.Name -> Reporting.Annotation.Region -> Can.Expr
newAttributesHelper isElmUi module_ functionName location =
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
    if isElmUi then
        (a (List
              [ (a (Call
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
                                     (Filled (TType (Module.Canonical (Name "mdgriffith" "elm-ui") "Internal.Model") "Attribute" [TUnit, TVar "msg"])))))))
                      [ (a (Call
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
                      ]))
              ]))
    else
        (a (List
              [ (a (Call
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
              ]))

htmlNodes =
    Set.fromList
        [ "h1"
        , "h2"
        , "h3"
        , "h4"
        , "h5"
        , "h6"
        , "div"
        , "p"
        , "hr"
        , "pre"
        , "blockquote"
        , "span"
        , "a"
        , "code"
        , "em"
        , "strong"
        , "i"
        , "b"
        , "u"
        , "sub"
        , "sup"
        , "br"
        , "ol"
        , "ul"
        , "li"
        , "dl"
        , "dt"
        , "dd"
        , "img"
        , "iframe"
        , "canvas"
        , "math"
        , "form"
        , "input"
        , "textarea"
        , "button"
        , "select"
        , "option"
        , "section"
        , "nav"
        , "article"
        , "aside"
        , "header"
        , "footer"
        , "address"
        , "main_"
        , "figure"
        , "figcaption"
        , "table"
        , "caption"
        , "colgroup"
        , "col"
        , "tbody"
        , "thead"
        , "tfoot"
        , "tr"
        , "td"
        , "th"
        , "fieldset"
        , "legend"
        , "label"
        , "datalist"
        , "optgroup"
        , "output"
        , "progress"
        , "meter"
        , "audio"
        , "video"
        , "source"
        , "track"
        , "embed"
        , "object"
        , "param"
        , "ins"
        , "del"
        , "small"
        , "cite"
        , "dfn"
        , "abbr"
        , "time"
        , "var"
        , "samp"
        , "kbd"
        , "s"
        , "q"
        , "mark"
        , "ruby"
        , "rt"
        , "rp"
        , "bdi"
        , "bdo"
        , "wbr"
        , "details"
        , "summary"
        , "menuitem"
        , "menu"
        ]

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
                    (Module.Canonical (Name "elm" "html") moduleName)
                    functionName_
                    annotation
                )
            )
            (firstParam : rest) ->
            let
                expr =
                    (Reporting.Annotation.At
                        location
                        (Can.VarForeign
                            (Module.Canonical (Name "elm" "html") moduleName)
                            functionName_
                            annotation
                        )
                    )
            in
            if Set.member functionName_ htmlNodes && moduleName == "Html" then
                Can.Call
                    expr
                    (newAttributes False fileName functionName location firstParam
                        : fmap (updateExpr fileName functionName) rest)
            else
                Can.Call
                    ((updateExpr fileName functionName) expr)
                    (fmap (updateExpr fileName functionName) (firstParam : rest))

        Can.Call
            (Reporting.Annotation.At
                location
                (Can.VarForeign
                    (Module.Canonical (Name "mdgriffith" "elm-ui") moduleName)
                    functionName_
                    annotation
                )
            )
            (firstParam : rest) ->
            let
                expr =
                    (Reporting.Annotation.At
                        location
                        (Can.VarForeign
                            (Module.Canonical (Name "mdgriffith" "elm-ui") moduleName)
                            functionName_
                            annotation
                        )
                    )

                isElement =
                    (functionName_ == "el"
                        || functionName_ == "row"
                        || functionName_ == "column"
                        || functionName_ == "wrappedRow"
                        || functionName_ == "paragraph"
                        || functionName_ == "textColumn"
                        || functionName_ == "table"
                        || functionName_ == "indexedTable"
                        || functionName_ == "link"
                        || functionName_ == "newTabLink"
                        || functionName_ == "download"
                        || functionName_ == "downloadAs"
                        || functionName_ == "image"
                    )
                        && moduleName == "Element"

                isKeyed =
                    (functionName_ == "el"
                        || functionName_ == "row"
                        || functionName_ == "column"
                    )
                        && moduleName == "Element.Keyed"

                isInput =
                    (functionName_ == "button"
                        || functionName_ == "checkbox"
                        || functionName_ == "text"
                        || functionName_ == "multiline"
                        || functionName_ == "username"
                        || functionName_ == "newPassword"
                        || functionName_ == "currentPassword"
                        || functionName_ == "email"
                        || functionName_ == "search"
                        || functionName_ == "spellChecked"
                        || functionName_ == "slider"
                        || functionName_ == "radio"
                        || functionName_ == "radioRow"
                    )
                        && moduleName == "Element.Input"
            in
            if isElement || isKeyed || isInput then
                Can.Call
                    expr
                    (newAttributes True fileName functionName location firstParam
                        : fmap (updateExpr fileName functionName) rest)
            else
                Can.Call
                    ((updateExpr fileName functionName) expr)
                    (fmap (updateExpr fileName functionName) (firstParam : rest))

        Can.Call expr exprs ->
            Can.Call ((updateExpr fileName functionName) expr) (fmap (updateExpr fileName functionName) exprs)

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

        Can.Accessor name ->
            Can.Accessor name

        Can.Access expr name ->
            Can.Access ((updateExpr fileName functionName) expr) name

        Can.Update name expr fieldUpdates ->
            Can.Update
                name
                ((updateExpr fileName functionName) expr)
                (fmap
                    (\(Can.FieldUpdate region expr) ->
                        Can.FieldUpdate region (updateExpr fileName functionName expr)
                    )
                    fieldUpdates
                )

        Can.Record fields ->
            Can.Record (fmap (\field -> updateExpr fileName functionName field) fields)

        Can.Unit ->
            Can.Unit

        Can.Tuple expr expr2 maybeExpr ->
            Can.Tuple
                ((updateExpr fileName functionName) expr)
                ((updateExpr fileName functionName) expr2)
                (fmap (updateExpr fileName functionName) maybeExpr)

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


{-|123 is used as a suffix to reduce the chances of a name collision-}
src :: B.Builder
src =
  [text|

var mouseX123 = 0;
var mouseY123 = 0;
var backgroundDiv123 = null;
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

window.addEventListener(
    "mousemove",
    function (event) {
        mouseX123 = event.clientX;
        mouseY123 = event.clientY;
    });

window.addEventListener(
    "keydown",
    function(event) {
        if (event.ctrlKey && event.altKey && event.keyCode == 88)
        {
            let target = document.elementFromPoint(mouseX123, mouseY123);

            let nodes = getNodesWithLineNumber123(target);

            if (nodes.length > 0) {
                if (backgroundDiv123) {
                    backgroundDiv123.remove();
                }

                backgroundDiv123 = document.createElement("div");
                backgroundDiv123.style.left = "0px";
                backgroundDiv123.style.top = "0px";
                backgroundDiv123.style.position = "fixed";
                backgroundDiv123.style.width = "100%";
                backgroundDiv123.style.height = "100%";
                backgroundDiv123.onclick = function() { backgroundDiv123.remove(); };

                let div = document.createElement("div");
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
                        backgroundDiv123.remove();
                        let xmlHttpReq = new XMLHttpRequest();
                        xmlHttpReq.open("GET", "/_x/editor/src/" + node.path, true);
                        xmlHttpReq.send(null);
                    };
                    div.appendChild(button);
                });

                backgroundDiv123.appendChild(div);
                document.body.appendChild(backgroundDiv123);

                div.style.left = Math.min(mouseX123, window.innerWidth - div.offsetWidth) + "px";
                div.style.top = Math.min(mouseY123, window.innerHeight - div.offsetHeight) + "px";
            }
        }
    });

  |]
  & T.encodeUtf8Builder
