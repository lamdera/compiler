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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import NeatInterpolation

import AST.Canonical
import Elm.Package
import qualified AST.Canonical as Can
import qualified Elm.ModuleName as Module
import qualified Reporting.Annotation
import qualified Data.ByteString.Builder as B

import Lamdera
import StandaloneInstances()
import qualified Elm.String as ES
import qualified Data.List as List
import qualified Lamdera.String


updateDecls :: FilePath -> Module.Canonical -> Can.Decls -> Can.Decls
updateDecls fileName moduleName decls =
    case moduleName of
        Module.Canonical (Name "author" "project") "Lamdera.Live" ->
            decls

        Module.Canonical (Name "author" "project") _ ->
            case decls of
                Can.Declare def nextDecl ->
                    Can.Declare (updateDefs fileName moduleName def) (updateDecls fileName moduleName nextDecl)

                Can.DeclareRec def remainingDefs nextDecl ->
                    Can.DeclareRec
                        (updateDefs fileName moduleName def)
                        (map (updateDefs fileName moduleName) remainingDefs)
                        (updateDecls fileName moduleName nextDecl)

                Can.SaveTheEnvironment ->
                    Can.SaveTheEnvironment

        _ ->
            decls




newAttributes :: Bool
                -> FilePath
                -> Module.Canonical
                -> Name.Name
                -> Reporting.Annotation.Region
                -> Expr
                -> Reporting.Annotation.Located Expr_
newAttributes isElmUi fileName moduleName functionName location originalAttributes =
    let
        a = Reporting.Annotation.At location
    in
    a (Call
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
          [ updateExpr fileName moduleName functionName originalAttributes
          , newAttributesHelper isElmUi fileName moduleName functionName location
          ])


propertyName :: ES.String
propertyName =
    "lamderaSource"


propertyNameText :: Text
propertyNameText =
    T.pack (ES.toChars propertyName)


newAttributesHelper :: Bool -> FilePath -> Module.Canonical -> Name.Name -> Reporting.Annotation.Region -> Can.Expr
newAttributesHelper isElmUi fileName (Module.Canonical _ moduleName) functionName location =
    let
        (Reporting.Annotation.Region (Reporting.Annotation.Position row column) _) =
            location

        propertyValue =
            [ Name.toChars moduleName
            , Name.toChars functionName
            , show row
            , show column
            , fileName
            ]
            & List.intercalate ","
            & Lamdera.String.fromChars

        a =
            Reporting.Annotation.At location

        propertyCall =
            a (Call
                (a (VarForeign
                      (Module.Canonical (Name "elm" "html") "Html.Attributes")
                      "property"
                      (Forall
                         (Map.fromList [("msg", ())])
                         (TLambda
                            (TType (Module.Canonical (Name "elm" "core") "String") "String" [])
                            (TLambda
                               (TType (Module.Canonical (Name "elm" "json") "Json.Encode") "Value" [])
                               (TAlias
                                  (Module.Canonical (Name "elm" "html") "Html")
                                  "Attribute"
                                  [("msg", TVar "msg")]
                                  (Filled (TType (Module.Canonical (Name "elm" "virtual-dom") "VirtualDom") "Attribute" [TVar "msg"]))))))))
                [ a (Str propertyName)
                , a (Call
                      (a (VarForeign
                            (Module.Canonical (Name "elm" "json") "Json.Encode")
                            "string"
                            (Forall
                               Map.empty
                               (TLambda
                                  (TType (Module.Canonical (Name "elm" "core") "String") "String" [])
                                  (TType (Module.Canonical (Name "elm" "json") "Json.Encode") "Value" [])))))
                      [a (Str propertyValue)])
                ])
    in
    if isElmUi then
        a (List
              [ a (Call
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
                      [ propertyCall ])
              ])
    else
        a (List [ propertyCall ])

htmlNodes :: Set.Set Name.Name
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

updateExpr :: FilePath -> Module.Canonical -> Name.Name -> Can.Expr -> Can.Expr
updateExpr fileName moduleName functionName (Reporting.Annotation.At location_ expr_) =
    (case expr_ of
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
            Can.List (fmap (updateExpr fileName moduleName functionName) exprs)

        Can.Negate expr ->
            Can.Negate (updateExpr fileName moduleName functionName expr)

        Can.Binop name canonical name2 annotation expr expr2 ->
            Can.Binop name canonical name2 annotation (updateExpr fileName moduleName functionName expr) (updateExpr fileName moduleName functionName expr2)

        Can.Lambda patterns expr ->
            Can.Lambda patterns (updateExpr fileName moduleName functionName expr)

        Can.Call
            (Reporting.Annotation.At
                location
                (Can.VarForeign
                    (Module.Canonical (Name "elm" "html") htmlModuleName)
                    functionName_
                    annotation
                )
            )
            (firstParam : rest) ->
            let
                expr =
                    Reporting.Annotation.At
                        location
                        (Can.VarForeign
                            (Module.Canonical (Name "elm" "html") htmlModuleName)
                            functionName_
                            annotation
                        )
            in
            if Set.member functionName_ htmlNodes && htmlModuleName == "Html" then
                Can.Call
                    expr
                    (newAttributes False fileName moduleName functionName location firstParam
                        : fmap (updateExpr fileName moduleName functionName) rest)
            else
                Can.Call
                    (updateExpr fileName moduleName functionName expr)
                    (fmap (updateExpr fileName moduleName functionName) (firstParam : rest))

        Can.Call
            (Reporting.Annotation.At
                location
                (Can.VarForeign
                    (Module.Canonical (Name "mdgriffith" "elm-ui") elmUiModuleName)
                    functionName_
                    annotation
                )
            )
            (firstParam : rest) ->
            let
                expr =
                    Reporting.Annotation.At
                        location
                        (Can.VarForeign
                            (Module.Canonical (Name "mdgriffith" "elm-ui") elmUiModuleName)
                            functionName_
                            annotation
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
                        && elmUiModuleName == "Element"

                isKeyed =
                    (functionName_ == "el"
                        || functionName_ == "row"
                        || functionName_ == "column"
                    )
                        && elmUiModuleName == "Element.Keyed"

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
                        && elmUiModuleName == "Element.Input"
            in
            if isElement || isKeyed || isInput then
                Can.Call
                    expr
                    (newAttributes True fileName moduleName functionName location firstParam
                        : fmap (updateExpr fileName moduleName functionName) rest)
            else
                Can.Call
                    (updateExpr fileName moduleName functionName expr)
                    (fmap (updateExpr fileName moduleName functionName) (firstParam : rest))

        Can.Call expr exprs ->
            Can.Call (updateExpr fileName moduleName functionName expr) (fmap (updateExpr fileName moduleName functionName) exprs)

        Can.If exprs expr ->
            Can.If
                (fmap
                    (\(first, second) ->
                        ( updateExpr fileName moduleName functionName first
                        , updateExpr fileName moduleName functionName second
                        )
                    )
                    exprs
                )
                (updateExpr fileName moduleName functionName expr)

        Can.Let def expr ->
            Can.Let
                (updateDefs fileName moduleName def)
                (updateExpr fileName moduleName functionName expr)

        Can.LetRec defs expr ->
            Can.LetRec
                (fmap (updateDefs fileName moduleName) defs)
                (updateExpr fileName moduleName functionName expr)

        Can.LetDestruct pattern expr expr2 ->
            Can.LetDestruct
                pattern
                (updateExpr fileName moduleName functionName expr)
                (updateExpr fileName moduleName functionName expr2)

        Can.Case expr caseBranches ->
            Can.Case
                (updateExpr fileName moduleName functionName expr)
                (fmap
                    (\(Can.CaseBranch pattern caseExpr) ->
                        Can.CaseBranch pattern (updateExpr fileName moduleName functionName caseExpr)
                    )
                    caseBranches
                )

        Can.Accessor name ->
            Can.Accessor name

        Can.Access expr name ->
            Can.Access (updateExpr fileName moduleName functionName expr) name

        Can.Update name expr fieldUpdates ->
            Can.Update
                name
                (updateExpr fileName moduleName functionName expr)
                (fmap
                    (\(Can.FieldUpdate region expr__) ->
                        Can.FieldUpdate region (updateExpr fileName moduleName functionName expr__)
                    )
                    fieldUpdates
                )

        Can.Record fields ->
            Can.Record (fmap (updateExpr fileName moduleName functionName) fields)

        Can.Unit ->
            Can.Unit

        Can.Tuple expr expr2 maybeExpr ->
            Can.Tuple
                (updateExpr fileName moduleName functionName expr)
                (updateExpr fileName moduleName functionName expr2)
                (fmap (updateExpr fileName moduleName functionName) maybeExpr)

        Can.Shader shaderSource shaderTypes ->
            Can.Shader shaderSource shaderTypes
    )
    & Reporting.Annotation.At location_

updateDefs :: FilePath -> Module.Canonical -> Can.Def -> Can.Def
updateDefs fileName moduleName def =
    case def of
        Can.Def name patterns expr ->
            Can.Def
                name
                patterns
                (updateExpr fileName moduleName (Reporting.Annotation.toValue name) expr)

        Can.TypedDef name freeVars patterns expr type_ ->
            Can.TypedDef
                name
                freeVars
                patterns
                (updateExpr fileName moduleName (Reporting.Annotation.toValue name) expr)
                type_


src :: B.Builder
src =
  [text|
;(function() {
var propertyName = "$propertyNameText";
var mouseX = 0;
var mouseY = 0;
var backgroundDiv = null;
function getNodesWithLineNumber(targets) {
    return targets
        .map(target => {
            let property = target[propertyName];
            if (property === undefined) {
                return null;
            }
            let [moduleName, functionName, row, column, ...fileName] = property.split(",");
            return {fileName: fileName.join(","), moduleName, functionName, row, column};
        })
        .filter(Boolean);
}

window.addEventListener(
    "mousemove",
    function (event) {
        mouseX = event.clientX;
        mouseY = event.clientY;
    });

window.addEventListener(
    "keydown",
    function(event) {
        if (event.ctrlKey && event.altKey && event.keyCode == 88) // x
        {
            let targets = document.elementsFromPoint(mouseX, mouseY);
            let nodes = getNodesWithLineNumber(targets);

            if (nodes.length > 0) {
                if (backgroundDiv) { backgroundDiv.remove(); }

                backgroundDiv = document.createElement("div");
                backgroundDiv.style.setProperty("left", "0px", "important");
                backgroundDiv.style.setProperty("top", "0px", "important");
                backgroundDiv.style.setProperty("position", "fixed", "important");
                backgroundDiv.style.setProperty("width", "100%", "important");
                backgroundDiv.style.setProperty("height", "100%", "important");
                backgroundDiv.onclick = function() { backgroundDiv.remove(); };

                let div = document.createElement("div");
                div.style.setProperty("position", "absolute", "important");
                div.style.setProperty("padding", "4px", "important");
                div.style.setProperty("display", "flex", "important");
                div.style.setProperty("flex-direction", "column", "important");
                div.style.setProperty("background", "rgb(46, 51, 53)", "important");
                div.style.setProperty("border-radius", "5px", "important");
                div.style.setProperty("border-radius", "5px", "important");
                div.style.setProperty("font-size", "13px", "important");
                div.style.setProperty("font-family", 'system-ui, "Helvetica Neue", sans-serif', "important");

                nodes.forEach(node => {
                    let button = document.createElement("button");
                    button.textContent = node.moduleName + "." + node.functionName + ":" + node.row;
                    button.style.setProperty("padding", "4px", "important");
                    button.style.setProperty("text-align", "right", "important");
                    button.style.setProperty("border", "none", "important");
                    button.style.setProperty("background", "rgb(46, 51, 53)", "important");
                    button.style.setProperty("color", "rgb(238, 238, 238)", "important");
                    button.addEventListener("mouseenter", function(){ this.style.setProperty("background", "rgb(65, 65, 65)", "important") });
                    button.addEventListener("mouseleave", function(){ this.style.setProperty("background", "rgb(46, 51, 53)", "important") });
                    button.onclick = function() {
                        backgroundDiv.remove();
                        fetch("/_x/editor/" + node.fileName + "?row=" + node.row + "&column=" + node.column);
                    };
                    div.appendChild(button);
                });

                backgroundDiv.appendChild(div);
                document.body.appendChild(backgroundDiv);

                div.style.setProperty("left", Math.min(mouseX, window.innerWidth - div.offsetWidth) + "px", "important");
                div.style.setProperty("top", Math.min(mouseY, window.innerHeight - div.offsetHeight) + "px", "important");
            }
        }
    });
}());
  |]
  & T.encodeUtf8Builder
