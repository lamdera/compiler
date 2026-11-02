{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Lamdera.UiSourceMap
    (updateDecls, src, openEditorSrc)
    where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified AST.Prim.Name as Name
import qualified AST.Prim.Module as ModuleName_
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import NeatInterpolation

-- @LAMDERA the annotations below are written as Elm 0.19.3 types and converted, see
-- Lamdera.Compat.Convert
import Lamdera.Compat.Canonical (Annotation(..), Type(..), AliasType(..))
import qualified Lamdera.Compat.Convert as Convert
import Elm.Package
import qualified AST.Canonical as Can
import qualified Elm.Interface as I
import qualified Graph
import qualified AST.Prim.Fields as F
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

        Module.Canonical (Name "author" "project") "Lamdera.Repl" ->
            decls

        Module.Canonical (Name "author" "project") _ ->
            -- @LAMDERA Elm 0.19.4 Decls are a sorted list of Decl
            case decls of
                Can.Decls (Graph.Sorted ds) ->
                    Can.Decls $ Graph.Sorted $ fmap
                        (\decl ->
                            case decl of
                                Can.Decl def ->
                                    Can.Decl (updateDefs fileName moduleName def)

                                Can.DeclRec def remainingDefs ->
                                    Can.DeclRec
                                        (updateDefs fileName moduleName def)
                                        (map (updateDefs fileName moduleName) remainingDefs)
                        )
                        ds

        _ ->
            decls




newAttributes :: Bool
                -> FilePath
                -> Module.Canonical
                -> Name.Name
                -> Reporting.Annotation.Region
                -> Can.Expr
                -> Reporting.Annotation.Located Can.Expr_
newAttributes isElmUi fileName moduleName functionName location originalAttributes =
    let
        a = Reporting.Annotation.At location
    in
    a (Can.Binop
        "::"
        (Module.Canonical (Name "elm" "core") "List")
        "cons"
        (annotation $ Forall
           (Map.fromList [("a", ())])
           (TLambda
              (TVar "a")
              (TLambda
                 (TType (Module.Canonical (Name "elm" "core") "List") "List" [TVar "a"])
                 (TType (Module.Canonical (Name "elm" "core") "List") "List" [TVar "a"]))))
        (newProperty isElmUi fileName moduleName functionName location)
        (updateExpr fileName moduleName functionName originalAttributes))


propertyName :: ES.String
propertyName =
    "lamderaSource"


propertyNameText :: Text
propertyNameText =
    T.pack (ES.toChars propertyName)


newProperty :: Bool -> FilePath -> Module.Canonical -> Name.Name -> Reporting.Annotation.Region -> Can.Expr
newProperty isElmUi fileName (Module.Canonical _ moduleName) functionName location =
    let
        !(Reporting.Annotation.Region cursor _) =
            location

        -- Editor coordinates (1-based), as Elm 0.19.1 gave and the editor link expects.
        -- Since 0.19.2 toRowCol is 0-based (see Reporting.Annotation.toEditorRowCol).
        (row, column) = Reporting.Annotation.toEditorRowCol cursor

        propertyValue =
            [ ModuleName_.toChars moduleName
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
            a (Can.Call
                (a (Can.VarForeign
                      (Module.Canonical (Name "elm" "html") "Html.Attributes")
                      "property"
                      (annotation $ Forall
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
                (a (Can.Str propertyName))
                [ a (Can.Call
                      (a (Can.VarForeign
                            (Module.Canonical (Name "elm" "json") "Json.Encode")
                            "string"
                            (annotation $ Forall
                               Map.empty
                               (TLambda
                                  (TType (Module.Canonical (Name "elm" "core") "String") "String" [])
                                  (TType (Module.Canonical (Name "elm" "json") "Json.Encode") "Value" [])))))
                      (a (Can.Str propertyValue))
                      [])
                ])
    in
    if isElmUi then
        a (Can.Call
            (a (Can.VarForeign
                  (Module.Canonical (Name "mdgriffith" "elm-ui") "Element")
                  "htmlAttribute"
                  (annotation $ Forall
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
            propertyCall
            [])
    else
        propertyCall

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

-- @LAMDERA The annotations of the values the source map adds, written as Elm 0.19.3
-- types. This runs after type checking, so the annotations only need to be well formed.
annotation :: Annotation -> I.Annotation
annotation =
    Convert.toAnnotation (Convert.envFromInterfaces Map.empty)


updateExpr :: FilePath -> Module.Canonical -> Name.Name -> Can.Expr -> Can.Expr
updateExpr fileName moduleName functionName (Reporting.Annotation.At location_ expr_) =
    let
        go = updateExpr fileName moduleName functionName
    in
    (case expr_ of
        Can.List exprs ->
            Can.List (fmap go exprs)

        Can.Negate expr ->
            Can.Negate (go expr)

        Can.Binop name canonical name2 annotation_ expr expr2 ->
            Can.Binop name canonical name2 annotation_ (go expr) (go expr2)

        Can.Lambda pattern patterns expr ->
            Can.Lambda pattern patterns (go expr)

        Can.Call
            (Reporting.Annotation.At
                location
                (Can.VarForeign
                    (Module.Canonical (Name "elm" "html") htmlModuleName)
                    functionName_
                    annotation_
                )
            )
            firstParam
            rest ->
            let
                expr =
                    Reporting.Annotation.At
                        location
                        (Can.VarForeign
                            (Module.Canonical (Name "elm" "html") htmlModuleName)
                            functionName_
                            annotation_
                        )
            in
            if Set.member functionName_ htmlNodes && htmlModuleName == "Html" then
                Can.Call
                    expr
                    (newAttributes False fileName moduleName functionName location firstParam)
                    (fmap go rest)
            else
                Can.Call
                    (go expr)
                    (go firstParam)
                    (fmap go rest)

        Can.Call
            (Reporting.Annotation.At
                location
                (Can.VarForeign
                    (Module.Canonical (Name "mdgriffith" "elm-ui") elmUiModuleName)
                    functionName_
                    annotation_
                )
            )
            firstParam
            rest ->
            let
                expr =
                    Reporting.Annotation.At
                        location
                        (Can.VarForeign
                            (Module.Canonical (Name "mdgriffith" "elm-ui") elmUiModuleName)
                            functionName_
                            annotation_
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
                    (newAttributes True fileName moduleName functionName location firstParam)
                    (fmap go rest)
            else
                Can.Call
                    (go expr)
                    (go firstParam)
                    (fmap go rest)

        Can.Call expr firstParam rest ->
            Can.Call (go expr) (go firstParam) (fmap go rest)

        Can.If exprs expr ->
            Can.If
                (fmap
                    (\(first, second) ->
                        ( go first
                        , go second
                        )
                    )
                    exprs
                )
                (go expr)

        Can.LetValue value expr ->
            Can.LetValue
                (updateValue fileName moduleName value)
                (go expr)

        Can.LetFunc func expr ->
            Can.LetFunc
                (updateFunc fileName moduleName func)
                (go expr)

        Can.LetRec func funcs expr ->
            Can.LetRec
                (updateFunc fileName moduleName func)
                (fmap (updateFunc fileName moduleName) funcs)
                (go expr)

        Can.LetDestruct pattern expr expr2 ->
            Can.LetDestruct
                pattern
                (go expr)
                (go expr2)

        Can.Case expr branch branches ->
            let
                goBranch (Can.Branch pattern caseExpr) =
                    Can.Branch pattern (go caseExpr)
            in
            Can.Case
                (go expr)
                (goBranch branch)
                (fmap goBranch branches)

        Can.Access expr name ->
            Can.Access (go expr) name

        Can.Update name expr fieldUpdates ->
            Can.Update
                name
                (go expr)
                (F.map
                    (\(Can.Update_ region expr__) ->
                        Can.Update_ region (go expr__)
                    )
                    fieldUpdates
                )

        Can.Record fields ->
            Can.Record (F.map go fields)

        Can.Pair expr expr2 ->
            Can.Pair
                (go expr)
                (go expr2)

        Can.Triple expr expr2 expr3 ->
            Can.Triple
                (go expr)
                (go expr2)
                (go expr3)

        -- everything else has no expressions in it
        _ ->
            expr_
    )
    & Reporting.Annotation.At location_


updateDefs :: FilePath -> Module.Canonical -> Can.Def -> Can.Def
updateDefs fileName moduleName def =
    case def of
        Can.DValue_ name expr ->
            Can.DValue_ name (updateExpr fileName moduleName (Reporting.Annotation.toValue name) expr)

        Can.DValue name expr type_ annotation_ ->
            Can.DValue name (updateExpr fileName moduleName (Reporting.Annotation.toValue name) expr) type_ annotation_

        Can.DFunc_ name pattern patterns expr ->
            Can.DFunc_ name pattern patterns (updateExpr fileName moduleName (Reporting.Annotation.toValue name) expr)

        Can.DFunc name arg args expr type_ annotation_ ->
            Can.DFunc name arg args (updateExpr fileName moduleName (Reporting.Annotation.toValue name) expr) type_ annotation_


updateValue :: FilePath -> Module.Canonical -> Can.Value -> Can.Value
updateValue fileName moduleName value =
    case value of
        Can.Value_ name expr ->
            Can.Value_ name (updateExpr fileName moduleName (Reporting.Annotation.toValue name) expr)

        Can.Value name expr type_ annotation_ ->
            Can.Value name (updateExpr fileName moduleName (Reporting.Annotation.toValue name) expr) type_ annotation_


updateFunc :: FilePath -> Module.Canonical -> Can.Func -> Can.Func
updateFunc fileName moduleName func =
    case func of
        Can.Func_ name pattern patterns expr ->
            Can.Func_ name pattern patterns (updateExpr fileName moduleName (Reporting.Annotation.toValue name) expr)

        Can.Func name arg args expr type_ annotation_ ->
            Can.Func name arg args (updateExpr fileName moduleName (Reporting.Annotation.toValue name) expr) type_ annotation_


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
                        $openEditorSrc
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


openEditorSrc :: Text
openEditorSrc =
  [text|
let url = new URL("/_x/editor/", window.top.location);
url.pathname += node.fileName;
url.searchParams.append("row", node.row);
url.searchParams.append("column", node.column);
fetch(url)
    .then(response => response.ok ? undefined : response.text().then(message => alert(message)))
    .catch(error => alert(error.message));
  |]
