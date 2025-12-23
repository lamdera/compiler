module Compiler.Reporting.Error.Syntax exposing
    ( Case(..)
    , CustomType(..)
    , Decl(..)
    , DeclDef(..)
    , DeclType(..)
    , Def(..)
    , Destruct(..)
    , Error(..)
    , Escape(..)
    , Exposing(..)
    , Expr(..)
    , Func(..)
    , If(..)
    , Let(..)
    , Module(..)
    , Number(..)
    , PList(..)
    , PRecord(..)
    , PTuple(..)
    , Pattern(..)
    , Port(..)
    , Record(..)
    , Space(..)
    , TChar(..)
    , TList(..)
    , TRecord(..)
    , TString(..)
    , TTuple(..)
    , Tuple(..)
    , Type(..)
    , TypeAlias(..)
    , toReport
    )

import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Parse.Primitives as P
import Compiler.Parse.Symbol as Symbol
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D exposing (d, da)
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Report as Report
import Extra.Type.List as MList
import Extra.Type.String as SE
import Hex



-- ALL SYNTAX ERRORS


type Error
    = ModuleNameUnspecified ModuleName.Raw
    | ModuleNameMismatch ModuleName.Raw (A.Located ModuleName.Raw)
    | UnexpectedPort A.Region
    | NoPorts A.Region
    | NoPortsInPackage (A.Located Name.Name)
    | NoPortModulesInPackage A.Region
    | NoEffectsOutsideKernel A.Region
    | ParseError Module



-- MODULE


type Module
    = ModuleSpace Space P.Row P.Col
    | ModuleBadEnd P.Row P.Col
      --
    | ModuleProblem P.Row P.Col
    | ModuleName P.Row P.Col
    | ModuleExposing Exposing P.Row P.Col
      --
    | PortModuleProblem P.Row P.Col
    | PortModuleName P.Row P.Col
    | PortModuleExposing Exposing P.Row P.Col
      --
    | Effect P.Row P.Col
      --
    | FreshLine P.Row P.Col
      --
    | ImportStart P.Row P.Col
    | ImportName P.Row P.Col
    | ImportAs P.Row P.Col
    | ImportAlias P.Row P.Col
    | ImportExposing P.Row P.Col
    | ImportExposingList Exposing P.Row P.Col
    | ImportEnd P.Row P.Col -- different based on col=1 or if greater
      --
    | ImportIndentName P.Row P.Col
    | ImportIndentAlias P.Row P.Col
    | ImportIndentExposingList P.Row P.Col
      --
    | Infix P.Row P.Col
      --
    | Declarations Decl


type Exposing
    = ExposingSpace Space P.Row P.Col
    | ExposingStart P.Row P.Col
    | ExposingValue P.Row P.Col
    | ExposingOperator P.Row P.Col
    | ExposingOperatorReserved Symbol.BadOperator P.Row P.Col
    | ExposingOperatorRightParen P.Row P.Col
    | ExposingTypePrivacy P.Row P.Col
    | ExposingEnd P.Row P.Col
      --
    | ExposingIndentEnd P.Row P.Col
    | ExposingIndentValue P.Row P.Col



-- DECLARATIONS


type Decl
    = DeclStart P.Row P.Col
    | DeclSpace Space P.Row P.Col
      --
    | Port Port P.Row P.Col
    | DeclType DeclType P.Row P.Col
    | DeclDef Name.Name DeclDef P.Row P.Col
      --
    | DeclFreshLineAfterDocComment P.Row P.Col


type DeclDef
    = DeclDefSpace Space P.Row P.Col
    | DeclDefEquals P.Row P.Col
    | DeclDefType Type P.Row P.Col
    | DeclDefArg Pattern P.Row P.Col
    | DeclDefBody Expr P.Row P.Col
    | DeclDefNameRepeat P.Row P.Col
    | DeclDefNameMatch Name.Name P.Row P.Col
      --
    | DeclDefIndentType P.Row P.Col
    | DeclDefIndentEquals P.Row P.Col
    | DeclDefIndentBody P.Row P.Col


type Port
    = PortSpace Space P.Row P.Col
    | PortName P.Row P.Col
    | PortColon P.Row P.Col
    | PortType Type P.Row P.Col
    | PortIndentName P.Row P.Col
    | PortIndentColon P.Row P.Col
    | PortIndentType P.Row P.Col



-- TYPE DECLARATIONS


type DeclType
    = DT_Space Space P.Row P.Col
    | DT_Name P.Row P.Col
    | DT_Alias TypeAlias P.Row P.Col
    | DT_Union CustomType P.Row P.Col
      --
    | DT_IndentName P.Row P.Col


type TypeAlias
    = AliasSpace Space P.Row P.Col
    | AliasName P.Row P.Col
    | AliasEquals P.Row P.Col
    | AliasBody Type P.Row P.Col
      --
    | AliasIndentEquals P.Row P.Col
    | AliasIndentBody P.Row P.Col


type CustomType
    = CT_Space Space P.Row P.Col
    | CT_Name P.Row P.Col
    | CT_Equals P.Row P.Col
    | CT_Bar P.Row P.Col
    | CT_Variant P.Row P.Col
    | CT_VariantArg Type P.Row P.Col
      --
    | CT_IndentEquals P.Row P.Col
    | CT_IndentBar P.Row P.Col
    | CT_IndentAfterBar P.Row P.Col
    | CT_IndentAfterEquals P.Row P.Col



-- EXPRESSIONS


type Expr
    = Let Let P.Row P.Col
    | Case Case P.Row P.Col
    | If If P.Row P.Col
    | CList TList P.Row P.Col
    | Record Record P.Row P.Col
    | Tuple Tuple P.Row P.Col
    | Func Func P.Row P.Col
      --
    | Dot P.Row P.Col
    | Access P.Row P.Col
    | OperatorRight Name.Name P.Row P.Col
    | OperatorReserved Symbol.BadOperator P.Row P.Col
      --
    | Start P.Row P.Col
    | CChar TChar P.Row P.Col
    | CString TString P.Row P.Col
    | Number Number P.Row P.Col
    | Space Space P.Row P.Col
    | EndlessShader P.Row P.Col
    | IndentOperatorRight Name.Name P.Row P.Col


type Record
    = RecordOpen P.Row P.Col
    | RecordEnd P.Row P.Col
    | RecordField P.Row P.Col
    | RecordEquals P.Row P.Col
    | RecordExpr Expr P.Row P.Col
    | RecordSpace Space P.Row P.Col
      --
    | RecordIndentOpen P.Row P.Col
    | RecordIndentEnd P.Row P.Col
    | RecordIndentField P.Row P.Col
    | RecordIndentEquals P.Row P.Col
    | RecordIndentExpr P.Row P.Col


type Tuple
    = TupleExpr Expr P.Row P.Col
    | TupleSpace Space P.Row P.Col
    | TupleEnd P.Row P.Col
    | TupleOperatorClose P.Row P.Col
    | TupleOperatorReserved Symbol.BadOperator P.Row P.Col
      --
    | TupleIndentExpr1 P.Row P.Col
    | TupleIndentExprN P.Row P.Col
    | TupleIndentEnd P.Row P.Col


type TList
    = ListSpace Space P.Row P.Col
    | ListOpen P.Row P.Col
    | ListExpr Expr P.Row P.Col
    | ListEnd P.Row P.Col
      --
    | ListIndentOpen P.Row P.Col
    | ListIndentEnd P.Row P.Col
    | ListIndentExpr P.Row P.Col


type Func
    = FuncSpace Space P.Row P.Col
    | FuncArg Pattern P.Row P.Col
    | FuncBody Expr P.Row P.Col
    | FuncArrow P.Row P.Col
      --
    | FuncIndentArg P.Row P.Col
    | FuncIndentArrow P.Row P.Col
    | FuncIndentBody P.Row P.Col


type Case
    = CaseSpace Space P.Row P.Col
    | CaseOf P.Row P.Col
    | CasePattern Pattern P.Row P.Col
    | CaseArrow P.Row P.Col
    | CaseExpr Expr P.Row P.Col
    | CaseBranch Expr P.Row P.Col
      --
    | CaseIndentOf P.Row P.Col
    | CaseIndentExpr P.Row P.Col
    | CaseIndentPattern P.Row P.Col
    | CaseIndentArrow P.Row P.Col
    | CaseIndentBranch P.Row P.Col
    | CasePatternAlignment Int P.Row P.Col


type If
    = IfSpace Space P.Row P.Col
    | IfThen P.Row P.Col
    | IfElse P.Row P.Col
    | IfElseBranchStart P.Row P.Col
      --
    | IfCondition Expr P.Row P.Col
    | IfThenBranch Expr P.Row P.Col
    | IfElseBranch Expr P.Row P.Col
      --
    | IfIndentCondition P.Row P.Col
    | IfIndentThen P.Row P.Col
    | IfIndentThenBranch P.Row P.Col
    | IfIndentElseBranch P.Row P.Col
    | IfIndentElse P.Row P.Col


type Let
    = LetSpace Space P.Row P.Col
    | LetIn P.Row P.Col
    | LetDefAlignment P.Row P.Col
    | LetDefName P.Row P.Col
    | LetDef Name.Name Def P.Row P.Col
    | LetDestruct Destruct P.Row P.Col
    | LetBody Expr P.Row P.Col
    | LetIndentDef P.Row P.Col
    | LetIndentIn P.Row P.Col
    | LetIndentBody P.Row P.Col


type Def
    = DefSpace Space P.Row P.Col
    | DefType Type P.Row P.Col
    | DefNameRepeat P.Row P.Col
    | DefNameMatch Name.Name P.Row P.Col
    | DefArg Pattern P.Row P.Col
    | DefEquals P.Row P.Col
    | DefBody Expr P.Row P.Col
    | DefIndentEquals P.Row P.Col
    | DefIndentType P.Row P.Col
    | DefIndentBody P.Row P.Col
    | DefAlignment Int P.Row P.Col


type Destruct
    = DestructSpace Space P.Row P.Col
    | DestructPattern Pattern P.Row P.Col
    | DestructEquals P.Row P.Col
    | DestructBody Expr P.Row P.Col
    | DestructIndentEquals P.Row P.Col
    | DestructIndentBody P.Row P.Col



-- PATTERNS


type Pattern
    = PRecord PRecord P.Row P.Col
    | PTuple PTuple P.Row P.Col
    | PList PList P.Row P.Col
      --
    | PStart P.Row P.Col
    | PChar TChar P.Row P.Col
    | PString TString P.Row P.Col
    | PNumber Number P.Row P.Col
    | PFloat Int P.Row P.Col
    | PAlias P.Row P.Col
    | PWildcardNotVar Name.Name Int P.Row P.Col
    | PSpace Space P.Row P.Col
      --
    | PIndentStart P.Row P.Col
    | PIndentAlias P.Row P.Col


type PRecord
    = PRecordOpen P.Row P.Col
    | PRecordEnd P.Row P.Col
    | PRecordField P.Row P.Col
    | PRecordSpace Space P.Row P.Col
      --
    | PRecordIndentOpen P.Row P.Col
    | PRecordIndentEnd P.Row P.Col
    | PRecordIndentField P.Row P.Col


type PTuple
    = PTupleOpen P.Row P.Col
    | PTupleEnd P.Row P.Col
    | PTupleExpr Pattern P.Row P.Col
    | PTupleSpace Space P.Row P.Col
      --
    | PTupleIndentEnd P.Row P.Col
    | PTupleIndentExpr1 P.Row P.Col
    | PTupleIndentExprN P.Row P.Col


type PList
    = PListOpen P.Row P.Col
    | PListEnd P.Row P.Col
    | PListExpr Pattern P.Row P.Col
    | PListSpace Space P.Row P.Col
      --
    | PListIndentOpen P.Row P.Col
    | PListIndentEnd P.Row P.Col
    | PListIndentExpr P.Row P.Col



-- TYPES


type Type
    = TRecord TRecord P.Row P.Col
    | TTuple TTuple P.Row P.Col
      --
    | TStart P.Row P.Col
    | TSpace Space P.Row P.Col
      --
    | TIndentStart P.Row P.Col


type TRecord
    = TRecordOpen P.Row P.Col
    | TRecordEnd P.Row P.Col
      --
    | TRecordField P.Row P.Col
    | TRecordColon P.Row P.Col
    | TRecordType Type P.Row P.Col
      --
    | TRecordSpace Space P.Row P.Col
      --
    | TRecordIndentOpen P.Row P.Col
    | TRecordIndentField P.Row P.Col
    | TRecordIndentColon P.Row P.Col
    | TRecordIndentType P.Row P.Col
    | TRecordIndentEnd P.Row P.Col


type TTuple
    = TTupleOpen P.Row P.Col
    | TTupleEnd P.Row P.Col
    | TTupleType Type P.Row P.Col
    | TTupleSpace Space P.Row P.Col
      --
    | TTupleIndentType1 P.Row P.Col
    | TTupleIndentTypeN P.Row P.Col
    | TTupleIndentEnd P.Row P.Col



-- LITERALS


type TChar
    = CharEndless
    | CharEscape Escape
    | CharNotString Int


type TString
    = StringEndless_Single
    | StringEndless_Multi
    | StringEscape Escape


type Escape
    = EscapeUnknown
    | BadUnicodeFormat Int
    | BadUnicodeCode Int
    | BadUnicodeLength Int Int Int


type Number
    = NumberEnd
    | NumberDot Int
    | NumberHexDigit
    | NumberNoLeadingZero



-- MISC


type Space
    = HasTab
    | EndlessMultiComment



-- TO REPORT


toReport : Code.Source -> Error -> Report.Report
toReport source err =
    case err of
        ModuleNameUnspecified name ->
            let
                region =
                    toRegion 1 1
            in
            Report.Report "MODULE NAME MISSING" region <|
                D.stack
                    [ D.reflow <|
                        "I need the module name to be declared at the top of this file, like this:"
                    , D.indent 4 <|
                        D.fillSep <|
                            [ D.cyanS "module", D.fromName name, D.cyanS "exposing", d "(..)" ]
                    , D.reflow <|
                        "Try adding that as the first line of your file!"
                    , D.toSimpleNote <|
                        "It is best to replace (..) with an explicit list of types and"
                            ++ " functions you want to expose. When you know a value is only used"
                            ++ " within this module, you can refactor without worrying about uses"
                            ++ " elsewhere. Limiting exposed values can also speed up compilation"
                            ++ " because I can skip a bunch of work if I see that the exposed API"
                            ++ " has not changed."
                    ]

        ModuleNameMismatch expectedName (A.At region actualName) ->
            Report.Report "MODULE NAME MISMATCH" region <|
                Code.toSnippet source
                    region
                    Nothing
                    ( d "It looks like this module name is out of sync:"
                    , D.stack
                        [ D.reflow <|
                            "I need it to match the file path, so I was expecting to see `"
                                ++ ModuleName.toChars expectedName
                                ++ "` here. Make the following change, and you should be all set!"
                        , D.indent 4 <|
                            da [ D.dullyellow (D.fromName actualName), d " -> ", D.green (D.fromName expectedName) ]
                        , D.toSimpleNote <|
                            "I require that module names correspond to file paths. This makes it much"
                                ++ " easier to explore unfamiliar codebases! So if you want to keep the current"
                                ++ " module name, try renaming the file instead."
                        ]
                    )

        UnexpectedPort region ->
            Report.Report "UNEXPECTED PORTS" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "You are declaring ports in a normal module."
                    , D.stack
                        [ D.fillSep
                            [ D.reflow "Switch this to say"
                            , D.cyanS "port module"
                            , D.reflow "instead, marking that this module contains port declarations."
                            ]
                        , D.link "Note"
                            "Ports are not a traditional FFI for calling JS functions directly. They need a different mindset! Read"
                            "ports"
                            "to learn the syntax and how to use it effectively."
                        ]
                    )

        NoPorts region ->
            Report.Report "NO PORTS" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "This module does not declare any ports, but it says it will:"
                    , D.fillSep
                        [ D.reflow "Switch this to"
                        , D.cyanS "module"
                        , D.reflow "and you should be all set!"
                        ]
                    )

        NoPortsInPackage (A.At region _) ->
            Report.Report "PACKAGES CANNOT HAVE PORTS" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "Packages cannot declare any ports, so I am getting stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "Remove this port declaration."
                        , noteForPortsInPackage
                        ]
                    )

        NoPortModulesInPackage region ->
            Report.Report "PACKAGES CANNOT HAVE PORTS" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "Packages cannot declare any ports, so I am getting stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.reflow "Remove the"
                            , D.cyanS "port"
                            , D.reflow "keyword and I"
                            , D.reflow "should be able to continue."
                            ]
                        , noteForPortsInPackage
                        ]
                    )

        NoEffectsOutsideKernel region ->
            Report.Report "INVALID EFFECT MODULE" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "It is not possible to declare an `effect module` outside the @elm organization,"
                            ++ " so I am getting stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "Switch to a normal module declaration."
                        , D.toSimpleNote <|
                            "Effect modules are designed to allow certain core functionality to be"
                                ++ " defined separately from the compiler. So the @elm organization has access to"
                                ++ " this so that certain changes, extensions, and fixes can be introduced without"
                                ++ " needing to release new Elm binaries. For example, we want to make it possible"
                                ++ " to test effects, but this may require changes to the design of effect modules."
                                ++ " By only having them defined in the @elm organization, that kind of design work"
                                ++ " can proceed much more smoothly."
                        ]
                    )

        ParseError modul ->
            toParseErrorReport source modul


noteForPortsInPackage : D.Doc
noteForPortsInPackage =
    D.stack
        [ D.toSimpleNote <|
            "One of the major goals of the package ecosystem is to be completely written"
                ++ " in Elm. This means when you install an Elm package, you can be sure you are safe"
                ++ " from security issues on install and that you are not going to get any runtime"
                ++ " exceptions coming from your new dependency. This design also sets the ecosystem"
                ++ " up to target other platforms more easily (like mobile phones, WebAssembly, etc.)"
                ++ " since no community code explicitly depends on JavaScript even existing."
        , D.reflow <|
            "Given that overall goal, allowing ports in packages would lead to some pretty"
                ++ " surprising behavior. If ports were allowed in packages, you could install a"
                ++ " package but not realize that it brings in an indirect dependency that defines a"
                ++ " port. Now you have a program that does not work and the fix is to realize that"
                ++ " some JavaScript needs to be added for a dependency you did not even know about."
                ++ " That would be extremely frustrating! \"So why not allow the package author to"
                ++ " include the necessary JS code as well?\" Now we are back in conflict with our"
                ++ " overall goal to keep all community packages free from runtime exceptions."
        ]


toParseErrorReport : Code.Source -> Module -> Report.Report
toParseErrorReport source modul =
    case modul of
        ModuleSpace space row col ->
            toSpaceReport source space row col

        ModuleBadEnd row col ->
            if col == 1 then
                toDeclStartReport source row col

            else
                toWeirdEndReport source row col

        ModuleProblem row col ->
            let
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED MODULE DECLARATION" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I am parsing an `module` declaration, but I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "Here are some examples of valid `module` declarations:"
                        , D.indent 4 <|
                            D.vcat
                                [ D.fillSep [ D.cyanS "module", D.fromChars "Main", D.cyanS "exposing", D.fromChars "(..)" ]
                                , D.fillSep [ D.cyanS "module", D.fromChars "Dict", D.cyanS "exposing", D.fromChars "(Dict, empty, get)" ]
                                ]
                        , D.reflow <|
                            "I generally recommend using an explicit exposing list. I can skip compiling a bunch"
                                ++ " of files when the public interface of a module stays the same, so exposing fewer"
                                ++ " values can help improve compile times!"
                        ]
                    )

        ModuleName row col ->
            let
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING MODULE NAME" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was parsing an `module` declaration until I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see the module name next, like in these examples:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep [ D.cyanS "module", D.fromChars "Dict", D.cyanS "exposing", D.fromChars "(..)" ]
                                , D.fillSep [ D.cyanS "module", D.fromChars "Maybe", D.cyanS "exposing", D.fromChars "(..)" ]
                                , D.fillSep [ D.cyanS "module", D.fromChars "Html.Attributes", D.cyanS "exposing", D.fromChars "(..)" ]
                                , D.fillSep [ D.cyanS "module", D.fromChars "Json.Decode", D.cyanS "exposing", D.fromChars "(..)" ]
                                ]
                        , D.reflow <|
                            "Notice that the module names all start with capital letters. That is required!"
                        ]
                    )

        ModuleExposing exposing_ row col ->
            toExposingReport source exposing_ row col

        PortModuleProblem row col ->
            let
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PORT MODULE DECLARATION" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I am parsing an `port module` declaration, but I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "Here are some examples of valid `port module` declarations:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep [ D.cyanS "port", D.cyanS "module", D.fromChars "WebSockets", D.cyanS "exposing", D.fromChars "(send, listen, keepAlive)" ]
                                , D.fillSep [ D.cyanS "port", D.cyanS "module", D.fromChars "Maps", D.cyanS "exposing", D.fromChars "(Location, goto)" ]
                                ]
                        , D.link "Note" "Read" "ports" "for more help."
                        ]
                    )

        PortModuleName row col ->
            let
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING MODULE NAME" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was parsing an `module` declaration until I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see the module name next, like in these examples:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep [ D.cyanS "port", D.cyanS "module", D.fromChars "WebSockets", D.cyanS "exposing", D.fromChars "(send, listen, keepAlive)" ]
                                , D.fillSep [ D.cyanS "port", D.cyanS "module", D.fromChars "Maps", D.cyanS "exposing", D.fromChars "(Location, goto)" ]
                                ]
                        , D.reflow <|
                            "Notice that the module names start with capital letters. That is required!"
                        ]
                    )

        PortModuleExposing exposing_ row col ->
            toExposingReport source exposing_ row col

        Effect row col ->
            let
                region =
                    toRegion row col
            in
            Report.Report "BAD MODULE DECLARATION" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I cannot parse this module declaration:"
                    , D.reflow <|
                        "This type of module is reserved for the @elm organization. It is used to"
                            ++ " define certain effects, avoiding building them into the compiler."
                    )

        FreshLine row col ->
            let
                region =
                    toRegion row col

                toBadFirstLineReport keyword =
                    Report.Report "TOO MUCH INDENTATION" region <|
                        Code.toSnippet source region Nothing <|
                            ( D.reflow <|
                                "This `"
                                    ++ keyword
                                    ++ "` should not have any spaces before it:"
                            , D.reflow <|
                                "Delete the spaces before `"
                                    ++ keyword
                                    ++ "` until there are none left!"
                            )
            in
            case Code.whatIsNext source row col of
                Code.Keyword "module" ->
                    toBadFirstLineReport "module"

                Code.Keyword "import" ->
                    toBadFirstLineReport "import"

                Code.Keyword "type" ->
                    toBadFirstLineReport "type"

                Code.Keyword "port" ->
                    toBadFirstLineReport "port"

                _ ->
                    Report.Report "SYNTAX PROBLEM" region <|
                        Code.toSnippet source region Nothing <|
                            ( D.reflow <|
                                "I got stuck here:"
                            , D.stack
                                [ D.reflow <|
                                    "I am not sure what is going on, but I recommend starting an Elm"
                                        ++ " file with the following lines:"
                                , D.indent 4 <|
                                    D.vcat <|
                                        [ D.fillSep [ D.cyanS "import", D.fromChars "Html" ]
                                        , D.fromChars ""
                                        , D.fromChars "main ="
                                        , D.hcat [ D.fromChars "  Html.text ", D.dullyellowS "\"Hello!\"" ]
                                        ]
                                , D.reflow <|
                                    "You should be able to copy those lines directly into your file. Check out the"
                                        ++ " examples at <https://elm-lang.org/examples> for more help getting started!"
                                , D.toSimpleNote
                                    "This can also happen when something is indented too much!"
                                ]
                            )

        ImportStart row col ->
            toImportReport source row col

        ImportName row col ->
            let
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING IMPORT NAME" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was parsing an `import` until I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see a module name next, like in these examples:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep [ D.cyanS "import", D.fromChars "Dict" ]
                                , D.fillSep [ D.cyanS "import", D.fromChars "Maybe" ]
                                , D.fillSep [ D.cyanS "import", D.fromChars "Html.Attributes", D.cyanS "as", D.fromChars "A" ]
                                , D.fillSep [ D.cyanS "import", D.fromChars "Json.Decode", D.cyanS "exposing", D.fromChars "(..)" ]
                                ]
                        , D.reflow <|
                            "Notice that the module names all start with capital letters. That is required!"
                        , D.reflowLink "Read" "imports" "to learn more."
                        ]
                    )

        ImportAs row col ->
            toImportReport source row col

        ImportAlias row col ->
            let
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING IMPORT ALIAS" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was parsing an `import` until I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see an alias next, like in these examples:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep [ D.cyanS "import", D.fromChars "Html.Attributes", D.cyanS "as", D.fromChars "Attr" ]
                                , D.fillSep [ D.cyanS "import", D.fromChars "WebGL.Texture", D.cyanS "as", D.fromChars "Texture" ]
                                , D.fillSep [ D.cyanS "import", D.fromChars "Json.Decode", D.cyanS "as", D.fromChars "D" ]
                                ]
                        , D.reflow <|
                            "Notice that the alias always starts with a capital letter. That is required!"
                        , D.reflowLink "Read" "imports" "to learn more."
                        ]
                    )

        ImportExposing row col ->
            toImportReport source row col

        ImportExposingList exposing_ row col ->
            toExposingReport source exposing_ row col

        ImportEnd row col ->
            toImportReport source row col

        ImportIndentName row col ->
            toImportReport source row col

        ImportIndentAlias row col ->
            toImportReport source row col

        ImportIndentExposingList row col ->
            let
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IMPORT" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was parsing an `import` until I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see the list of exposed values next. For example, here"
                                ++ " are two ways to expose values from the `Html` module:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep [ D.cyanS "import", D.fromChars "Html", D.cyanS "exposing", D.fromChars "(..)" ]
                                , D.fillSep [ D.cyanS "import", D.fromChars "Html", D.cyanS "exposing", D.fromChars "(Html, div, text)" ]
                                ]
                        , D.reflow <|
                            "I generally recommend the second style. It is more explicit, making it"
                                ++ " much easier to figure out where values are coming from in large projects!"
                        ]
                    )

        Infix row col ->
            let
                region =
                    toRegion row col
            in
            Report.Report "BAD INFIX" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "Something went wrong in this infix operator declaration:"
                    , D.reflow <|
                        "This feature is used by the @elm organization to define the"
                            ++ " languages built-in operators."
                    )

        Declarations decl ->
            toDeclarationsReport source decl



-- WEIRD END


toWeirdEndReport : Code.Source -> P.Row -> P.Col -> Report.Report
toWeirdEndReport source row col =
    case Code.whatIsNext source row col of
        Code.Keyword keyword ->
            let
                region =
                    toKeywordRegion row col keyword
            in
            Report.Report "RESERVED WORD" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I got stuck on this reserved word:"
                    , D.reflow <|
                        "The name `"
                            ++ keyword
                            ++ "` is reserved, so try using a different name?"
                    )

        Code.Operator op ->
            let
                region =
                    toKeywordRegion row col op
            in
            Report.Report "UNEXPECTED SYMBOL" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I ran into an unexpected symbol:"
                    , D.reflow <|
                        "I was not expecting to see a "
                            ++ op
                            ++ " here. Try deleting it? Maybe"
                            ++ " I can give a better hint from there?"
                    )

        Code.Close term bracket ->
            let
                region =
                    toRegion row col
            in
            Report.Report ("UNEXPECTED " ++ String.map Char.toUpper term) region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I ran into an unexpected "
                            ++ term
                            ++ ":"
                    , D.reflow <|
                        "This "
                            ++ String.cons bracket " does not match up with an earlier open "
                            ++ term
                            ++ ". Try deleting it?"
                    )

        Code.Lower c cs ->
            let
                region =
                    toKeywordRegion row col (String.cons c cs)
            in
            Report.Report "UNEXPECTED NAME" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I got stuck on this name:"
                    , D.reflow <|
                        "It is confusing me a lot! Normally I can give fairly specific hints, but"
                            ++ " something is really tripping me up this time."
                    )

        Code.Upper c cs ->
            let
                region =
                    toKeywordRegion row col (String.cons c cs)
            in
            Report.Report "UNEXPECTED NAME" region <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow <|
                        "I got stuck on this name:"
                    , D.reflow <|
                        "It is confusing me a lot! Normally I can give fairly specific hints, but"
                            ++ " something is really tripping me up this time."
                    )

        Code.Other maybeChar ->
            let
                region =
                    toRegion row col
            in
            case maybeChar of
                Just ';' ->
                    Report.Report "UNEXPECTED SEMICOLON" region <|
                        Code.toSnippet source region Nothing <|
                            ( D.reflow <|
                                "I got stuck on this semicolon:"
                            , D.stack
                                [ D.reflow "Try removing it?"
                                , D.toSimpleNote <|
                                    "Some languages require semicolons at the end of each statement. These are"
                                        ++ " often called C-like languages, and they usually share a lot of language design"
                                        ++ " choices. (E.g. side-effects, for loops, etc.) Elm manages effects with commands"
                                        ++ " and subscriptions instead, so there is no special syntax for \"statements\" and"
                                        ++ " therefore no need to use semicolons to separate them. I think this will make"
                                        ++ " more sense as you work through <https://guide.elm-lang.org> though!"
                                ]
                            )

                Just ',' ->
                    Report.Report "UNEXPECTED COMMA" region <|
                        Code.toSnippet source region Nothing <|
                            ( D.reflow <|
                                "I got stuck on this comma:"
                            , D.stack
                                [ D.reflow <|
                                    "I do not think I am parsing a list or tuple right now. Try deleting the comma?"
                                , D.toSimpleNote <|
                                    "If this is supposed to be part of a list, the problem may be a bit earlier."
                                        ++ " Perhaps the opening [ is missing? Or perhaps some value in the list has an extra"
                                        ++ " closing ] that is making me think the list ended earlier? The same kinds of"
                                        ++ " things could be going wrong if this is supposed to be a tuple."
                                ]
                            )

                Just '`' ->
                    Report.Report "UNEXPECTED CHARACTER" region <|
                        Code.toSnippet source region Nothing <|
                            ( D.reflow <|
                                "I got stuck on this character:"
                            , D.stack
                                [ D.reflow <|
                                    "It is not used for anything in Elm syntax. It is used for multi-line strings in"
                                        ++ " some languages though, so if you want a string that spans multiple lines, you"
                                        ++ " can use Elm's multi-line string syntax like this:"
                                , D.indent 4 <|
                                    D.vcat <|
                                        [ D.dullyellowS "\"\"\""
                                        , D.dullyellowS "# Multi-line Strings"
                                        , D.dullyellowS ""
                                        , D.dullyellowS "- start with triple double quotes"
                                        , D.dullyellowS "- write whatever you want"
                                        , D.dullyellowS "- no need to escape newlines or double quotes"
                                        , D.dullyellowS "- end with triple double quotes"
                                        , D.dullyellowS "\"\"\""
                                        ]
                                , D.reflow <|
                                    "Otherwise I do not know what is going on! Try removing the character?"
                                ]
                            )

                Just '$' ->
                    Report.Report "UNEXPECTED SYMBOL" region <|
                        Code.toSnippet source region Nothing <|
                            ( D.reflow <|
                                "I got stuck on this dollar sign:"
                            , D.reflow <|
                                "It is not used for anything in Elm syntax. Are you coming from a language where"
                                    ++ " dollar signs can be used in variable names? If so, try a name that (1) starts"
                                    ++ " with a letter and (2) only contains letters, numbers, and underscores."
                            )

                Just c ->
                    if MList.elem c [ '#', '@', '!', '%', '~' ] then
                        Report.Report "UNEXPECTED SYMBOL" region <|
                            Code.toSnippet source region Nothing <|
                                ( D.reflow <|
                                    "I got stuck on this symbol:"
                                , D.reflow <|
                                    "It is not used for anything in Elm syntax. Try removing it?"
                                )

                    else
                        toWeirdEndReportOther source region

                _ ->
                    toWeirdEndReportOther source region


toWeirdEndReportOther : Code.Source -> A.Region -> Report.Report
toWeirdEndReportOther source region =
    Report.Report "SYNTAX PROBLEM" region <|
        Code.toSnippet source region Nothing <|
            ( D.reflow <|
                "I got stuck here:"
            , D.reflow <|
                "Whatever I am running into is confusing me a lot! Normally I can give fairly"
                    ++ " specific hints, but something is really tripping me up this time."
            )



-- IMPORTS


toImportReport : Code.Source -> P.Row -> P.Col -> Report.Report
toImportReport source row col =
    let
        region =
            toRegion row col
    in
    Report.Report "UNFINISHED IMPORT" region <|
        Code.toSnippet source region Nothing <|
            ( D.reflow <|
                "I am partway through parsing an import, but I got stuck here:"
            , D.stack
                [ D.reflow <|
                    "Here are some examples of valid `import` declarations:"
                , D.indent 4 <|
                    D.vcat <|
                        [ D.fillSep [ D.cyanS "import", D.fromChars "Html" ]
                        , D.fillSep [ D.cyanS "import", D.fromChars "Html", D.cyanS "as", D.fromChars "H" ]
                        , D.fillSep [ D.cyanS "import", D.fromChars "Html", D.cyanS "as", D.fromChars "H", D.cyanS "exposing", D.fromChars "(..)" ]
                        , D.fillSep [ D.cyanS "import", D.fromChars "Html", D.cyanS "exposing", D.fromChars "(Html, div, text)" ]
                        ]
                , D.reflow <|
                    "You are probably trying to import a different module, but try to make it look like one of these examples!"
                , D.reflowLink "Read" "imports" "to learn more."
                ]
            )



-- EXPOSING


toExposingReport : Code.Source -> Exposing -> P.Row -> P.Col -> Report.Report
toExposingReport source exposing_ startRow startCol =
    case exposing_ of
        ExposingSpace space row col ->
            toSpaceReport source space row col

        ExposingStart row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN EXPOSING" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I want to parse exposed values, but I am getting stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "Exposed values are always surrounded by parentheses."
                            , D.reflow "So try adding a"
                            , D.greenS "("
                            , D.fromChars "here?"
                            ]
                        , D.toSimpleNote "Here are some valid examples of `exposing` for reference:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep [ D.cyanS "import", D.fromChars "Html", D.cyanS "exposing", D.fromChars "(..)" ]
                                , D.fillSep [ D.cyanS "import", D.fromChars "Html", D.cyanS "exposing", D.fromChars "(Html, div, text)" ]
                                ]
                        , D.reflow <|
                            "If you are getting tripped up, you can just expose everything for now. It should"
                                ++ " get easier to make an explicit exposing list as you see more examples in the wild."
                        ]
                    )

        ExposingValue row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I got stuck on this reserved word:"
                            , D.reflow <|
                                "It looks like you are trying to expose `"
                                    ++ keyword
                                    ++ "` but that is a reserved word. Is there a typo?"
                            )

                Code.Operator op ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col op
                    in
                    Report.Report "UNEXPECTED SYMBOL" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I got stuck on this symbol:"
                            , D.stack
                                [ D.reflow <|
                                    "If you are trying to expose an operator, add parentheses around it like this:"
                                , D.indent 4 <| D.hcat [ D.dullyellowS op, D.fromChars " -> ", D.greenS ("(" ++ op ++ ")") ]
                                ]
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN EXPOSING" region <|
                        Code.toSnippet source
                            surroundings
                            (Just region)
                            ( D.reflow <|
                                "I got stuck while parsing these exposed values:"
                            , D.stack
                                [ D.reflow <|
                                    "I do not have an exact recommendation, so here are some valid examples"
                                        ++ " of `exposing` for reference:"
                                , D.indent 4 <|
                                    D.vcat <|
                                        [ D.fillSep [ D.cyanS "import", D.fromChars "Html", D.cyanS "exposing", D.fromChars "(..)" ]
                                        , D.fillSep [ D.cyanS "import", D.fromChars "Basics", D.cyanS "exposing", D.fromChars "(Int, Float, Bool(..), (+), not, sqrt)" ]
                                        ]
                                , D.reflow <|
                                    "These examples show how to expose types, variants, operators, and functions. Everything"
                                        ++ " should be some permutation of these examples, just with different names."
                                ]
                            )

        ExposingOperator row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN EXPOSING" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw an open parenthesis, so I was expecting an operator next:"
                    , D.fillSep <|
                        [ D.reflow "It is possible to expose operators, so I was expecting"
                        , D.reflow "to see something like"
                        , D.dullyellowS "(+)"
                        , D.fromChars "or"
                        , D.dullyellowS "(|=)"
                        , D.fromChars "or"
                        , D.dullyellowS "(||)"
                        , D.reflow "after I saw that open parenthesis."
                        ]
                    )

        ExposingOperatorReserved op row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "RESERVED SYMBOL" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I cannot expose this as an operator:"
                    , case op of
                        Symbol.BadDot ->
                            D.reflow "Try getting rid of this entry? Maybe I can give you a better hint after that?"

                        Symbol.BadPipe ->
                            D.fillSep [ D.reflow "Maybe you want", D.dullyellowS "(||)", D.fromChars "instead?" ]

                        Symbol.BadArrow ->
                            D.reflow "Try getting rid of this entry? Maybe I can give you a better hint after that?"

                        Symbol.BadEquals ->
                            D.fillSep [ D.reflow "Maybe you want", D.dullyellowS "(==)", D.fromChars "instead?" ]

                        Symbol.BadHasType ->
                            D.fillSep [ D.reflow "Maybe you want", D.dullyellowS "(::)", D.fromChars "instead?" ]
                    )

        ExposingOperatorRightParen row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN EXPOSING" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "It looks like you are exposing an operator, but I got stuck here:"
                    , D.fillSep <|
                        [ D.reflow "I was expecting to see the closing parenthesis immediately"
                        , D.reflow "after the operator. Try adding a"
                        , D.greenS ")"
                        , D.reflow "right here?"
                        ]
                    )

        ExposingEnd row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED EXPOSING" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was partway through parsing exposed values, but I got stuck here:"
                    , D.reflow <|
                        "Maybe there is a comma missing before this?"
                    )

        ExposingTypePrivacy row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "PROBLEM EXPOSING CUSTOM TYPE VARIANTS" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "It looks like you are trying to expose the variants of a custom type:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "You need to write something like"
                            , D.dullyellowS "Status(..)"
                            , D.fromChars "or"
                            , D.dullyellowS "Entity(..)"
                            , D.reflow "though. It is all or nothing, otherwise `case`"
                            , D.reflow "expressions could miss a variant and crash!"
                            ]
                        , D.toSimpleNote <|
                            "It is often best to keep the variants hidden! If someone pattern matches on"
                                ++ " the variants, it is a MAJOR change if any new variants are added. Suddenly"
                                ++ " their `case` expressions do not cover all variants! So if you do not need"
                                ++ " people to pattern match, keep the variants hidden and expose functions to"
                                ++ " construct values of this type. This way you can add new variants as a MINOR change!"
                        ]
                    )

        ExposingIndentEnd row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED EXPOSING" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was partway through parsing exposed values, but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I was expecting a closing parenthesis."
                            , D.reflow "Try adding a"
                            , D.greenS ")"
                            , D.reflow "right here?"
                            ]
                        , D.toSimpleNote <|
                            "I can get confused when there is not enough indentation, so if you already"
                                ++ " have a closing parenthesis, it probably just needs some spaces in front of it."
                        ]
                    )

        ExposingIndentValue row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED EXPOSING" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was partway through parsing exposed values, but I got stuck here:"
                    , D.reflow <|
                        "I was expecting another value to expose."
                    )



-- SPACES


toSpaceReport : Code.Source -> Space -> P.Row -> P.Col -> Report.Report
toSpaceReport source space row col =
    case space of
        HasTab ->
            let
                region =
                    toRegion row col
            in
            Report.Report "NO TABS" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I ran into a tab, but tabs are not allowed in Elm files."
                    , D.reflow <|
                        "Replace the tab with spaces."
                    )

        EndlessMultiComment ->
            let
                region =
                    toWiderRegion row col 2
            in
            Report.Report "ENDLESS COMMENT" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I cannot find the end of this multi-line comment:"
                    , D.stack
                        -- "{-"
                        [ D.reflow "Add a -} somewhere after this to end the comment."
                        , D.toSimpleHint <|
                            "Multi-line comments can be nested in Elm, so {- {- -} -} is a comment"
                                ++ " that happens to contain another comment. Like parentheses and curly braces,"
                                ++ " the start and end markers must always be balanced. Maybe that is the problem?"
                        ]
                    )



-- DECLARATIONS


toRegion : P.Row -> P.Col -> A.Region
toRegion row col =
    let
        pos =
            A.Position row col
    in
    A.Region pos pos


toWiderRegion : P.Row -> P.Col -> Int -> A.Region
toWiderRegion row col extra =
    A.Region
        (A.Position row col)
        (A.Position row (col + extra))


toKeywordRegion : P.Row -> P.Col -> String -> A.Region
toKeywordRegion row col keyword =
    A.Region
        (A.Position row col)
        (A.Position row (col + String.length keyword))


toDeclarationsReport : Code.Source -> Decl -> Report.Report
toDeclarationsReport source decl =
    case decl of
        DeclStart row col ->
            toDeclStartReport source row col

        DeclSpace space row col ->
            toSpaceReport source space row col

        Port port_ row col ->
            toPortReport source port_ row col

        DeclType declType row col ->
            toDeclTypeReport source declType row col

        DeclDef name declDef row col ->
            toDeclDefReport source name declDef row col

        DeclFreshLineAfterDocComment row col ->
            let
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING DECLARATION" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I just saw a doc comment, but then I got stuck here:"
                    , D.reflow <|
                        "I was expecting to see the corresponding declaration next, starting on a fresh"
                            ++ " line with no indentation."
                    )


toDeclStartReport : Code.Source -> P.Row -> P.Col -> Report.Report
toDeclStartReport source row col =
    case Code.whatIsNext source row col of
        Code.Close term bracket ->
            let
                region =
                    toRegion row col
            in
            Report.Report ("STRAY " ++ String.map Char.toUpper term) region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was not expecting to see a "
                            ++ term
                            ++ " here:"
                    , D.reflow <|
                        "This "
                            ++ String.cons bracket " does not match up with an earlier open "
                            ++ term
                            ++ ". Try deleting it?"
                    )

        Code.Keyword keyword ->
            let
                region =
                    toKeywordRegion row col keyword
            in
            Report.Report "RESERVED WORD" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was not expecting to run into the `"
                            ++ keyword
                            ++ "` keyword here:"
                    , case keyword of
                        "import" ->
                            D.reflow <|
                                "It is reserved for declaring imports at the top of your module. If you want"
                                    ++ " another import, try moving it up top with the other imports. If you want to"
                                    ++ " define a value or function, try changing the name to something else!"

                        "case" ->
                            D.stack
                                [ D.reflow <|
                                    "It is reserved for writing `case` expressions. Try using a different name?"
                                , D.toSimpleNote <|
                                    "If you are trying to write a `case` expression, it needs to be part of a"
                                        ++ " definition. So you could write something like this instead:"
                                , D.indent 4 <|
                                    D.vcat <|
                                        [ D.indent 0 <| D.fillSep [ D.fromChars "getWidth", D.fromChars "maybeWidth", D.fromChars "=" ]
                                        , D.indent 2 <| D.fillSep [ D.cyanS "case", D.fromChars "maybeWidth", D.cyanS "of" ]
                                        , D.indent 4 <| D.fillSep [ D.blue "Just", D.fromChars "width", D.fromChars "->" ]
                                        , D.indent 6 <| D.fillSep [ D.fromChars "width", D.fromChars "+", D.dullyellowS "200" ]
                                        , D.fromChars ""
                                        , D.indent 4 <| D.fillSep [ D.blue "Nothing", D.fromChars "->" ]
                                        , D.indent 6 <| D.fillSep [ D.dullyellowS "400" ]
                                        ]
                                , D.reflow <|
                                    "This defines a `getWidth` function that you can use elsewhere in your program."
                                ]

                        "if" ->
                            D.stack
                                [ D.reflow <|
                                    "It is reserved for writing `if` expressions. Try using a different name?"
                                , D.toSimpleNote <|
                                    "If you are trying to write an `if` expression, it needs to be part of a"
                                        ++ " definition. So you could write something like this instead:"
                                , D.indent 4 <|
                                    D.vcat <|
                                        [ D.fromChars "greet name ="
                                        , D.fillSep <|
                                            [ D.fromChars " "
                                            , D.cyanS "if"
                                            , D.fromChars "name"
                                            , D.fromChars "=="
                                            , D.dullyellowS "\"Abraham Lincoln\""
                                            , D.cyanS "then"
                                            , D.dullyellowS "\"Greetings Mr. President.\""
                                            , D.cyanS "else"
                                            , D.dullyellowS "\"Hey!\""
                                            ]
                                        ]
                                , D.reflow <|
                                    "This defines a `reviewPowerLevel` function that you can use elsewhere in your program."
                                ]

                        _ ->
                            D.reflow <|
                                "It is a reserved word. Try changing the name to something else?"
                    )

        Code.Upper c cs ->
            let
                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED CAPITAL LETTER" region <|
                Code.toSnippet source
                    region
                    Nothing
                    ( D.reflow <|
                        "Declarations always start with a lower-case letter, so I am getting stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "Try a name like"
                            , D.greenS (String.cons (Char.toLower c) cs)
                            , D.fromChars "instead?"
                            ]
                        , D.toSimpleNote
                            "Here are a couple valid declarations for reference:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fromChars "greet : String -> String"
                                , D.fromChars "greet name ="
                                , D.hcat [ D.fromChars "  ", D.dullyellowS "\"Hello \"", D.fromChars " ++ name ++ ", D.dullyellowS "\"!\"" ]
                                , D.fromChars ""
                                , D.hcat [ D.cyanS "type", D.fromChars " User = Anonymous | LoggedIn String" ]
                                ]
                        , D.reflow <|
                            "Notice that they always start with a lower-case letter. Capitalization matters!"
                        ]
                    )

        Code.Other (Just char) ->
            if MList.elem char [ '(', '{', '[', '+', '-', '*', '/', '^', '&', '|', '"', '\'', '!', '@', '#', '$', '%' ] then
                let
                    region =
                        toRegion row col
                in
                Report.Report "UNEXPECTED SYMBOL" region <|
                    Code.toSnippet source region Nothing <|
                        ( D.reflow <|
                            "I am getting stuck because this line starts with the "
                                ++ String.fromChar char
                                ++ " symbol:"
                        , D.stack
                            [ D.reflow <|
                                "When a line has no spaces at the beginning, I expect it to be a declaration like one of these:"
                            , D.indent 4 <|
                                D.vcat <|
                                    [ D.fromChars "greet : String -> String"
                                    , D.fromChars "greet name ="
                                    , D.hcat [ D.fromChars "  ", D.dullyellowS "\"Hello \"", D.fromChars " ++ name ++ ", D.dullyellowS "\"!\"" ]
                                    , D.fromChars ""
                                    , D.hcat [ D.cyanS "type", D.fromChars " User = Anonymous | LoggedIn String" ]
                                    ]
                            , D.reflow <|
                                "If this is not supposed to be a declaration, try adding some spaces before it?"
                            ]
                        )

            else
                toDeclStartReportOther source row col

        _ ->
            toDeclStartReportOther source row col


toDeclStartReportOther : Code.Source -> P.Row -> P.Col -> Report.Report
toDeclStartReportOther source row col =
    let
        region =
            toRegion row col
    in
    Report.Report "WEIRD DECLARATION" region <|
        Code.toSnippet source region Nothing <|
            ( D.reflow <|
                "I am trying to parse a declaration, but I am getting stuck here:"
            , D.stack
                [ D.reflow <|
                    "When a line has no spaces at the beginning, I expect it to be a declaration like one of these:"
                , D.indent 4 <|
                    D.vcat <|
                        [ D.fromChars "greet : String -> String"
                        , D.fromChars "greet name ="
                        , D.hcat [ D.fromChars "  ", D.dullyellowS "\"Hello \"", D.fromChars " ++ name ++ ", D.dullyellowS "\"!\"" ]
                        , D.fromChars ""
                        , D.hcat [ D.cyanS "type", D.fromChars " User = Anonymous | LoggedIn String" ]
                        ]
                , D.reflow <|
                    "Try to make your declaration look like one of those? Or if this is not"
                        ++ " supposed to be a declaration, try adding some spaces before it?"
                ]
            )



-- PORT


toPortReport : Code.Source -> Port -> P.Row -> P.Col -> Report.Report
toPortReport source port_ startRow startCol =
    case port_ of
        PortSpace space row col ->
            toSpaceReport source space row col

        PortName row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I cannot handle ports with names like this:"
                            , D.reflow <|
                                "You are trying to make a port named `"
                                    ++ keyword
                                    ++ "` but that is a reserved word. Try using some other name?"
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "PORT PROBLEM" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I just saw the start of a `port` declaration, but then I got stuck here:"
                            , D.stack
                                [ D.fillSep
                                    [ D.reflow "I was expecting to see a name like"
                                    , D.dullyellowS "send"
                                    , D.fromChars "or"
                                    , D.dullyellowS "receive"
                                    , D.fromChars "next."
                                    , D.reflow "Something that starts with a lower-case letter."
                                    ]
                                , portNote
                                ]
                            )

        PortColon row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "PORT PROBLEM" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw the start of a `port` declaration, but then I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see a colon next. And then a type that tells me"
                                ++ " what type of values are going to flow through."
                        , portNote
                        ]
                    )

        PortType tipe row col ->
            toTypeReport source TC_Port tipe row col

        PortIndentName row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PORT" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw the start of a `port` declaration, but then I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.reflow "I was expecting to see a name like"
                            , D.dullyellowS "send"
                            , D.fromChars "or"
                            , D.dullyellowS "receive"
                            , D.fromChars "next."
                            , D.reflow "Something that starts with a lower-case letter."
                            ]
                        , portNote
                        ]
                    )

        PortIndentColon row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PORT" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw the start of a `port` declaration, but then I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see a colon next. And then a type that tells me"
                                ++ " what type of values are going to flow through."
                        , portNote
                        ]
                    )

        PortIndentType row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PORT" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw the start of a `port` declaration, but then I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see a type next. Here are examples of outgoing and"
                                ++ " incoming ports for reference:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.fillSep [ D.cyanS "port", D.fromChars "send", D.fromChars ":", D.fromChars "String -> Cmd msg" ]
                                , D.fillSep [ D.cyanS "port", D.fromChars "receive", D.fromChars ":", D.fromChars "(String -> msg) -> Sub msg" ]
                                ]
                        , D.reflow <|
                            "The first line defines a `send` port so you can send strings out to JavaScript."
                                ++ " Maybe you send them on a WebSocket or put them into IndexedDB. The second line"
                                ++ " defines a `receive` port so you can receive strings from JavaScript. Maybe you"
                                ++ " get receive messages when new WebSocket messages come in or when an entry in"
                                ++ " IndexedDB changes for some external reason."
                        ]
                    )


portNote : D.Doc
portNote =
    D.stack
        [ D.toSimpleNote <|
            "Here are some example `port` declarations for reference:"
        , D.indent 4 <|
            D.vcat <|
                [ D.fillSep [ D.cyanS "port", D.fromChars "send", D.fromChars ":", D.fromChars "String -> Cmd msg" ]
                , D.fillSep [ D.cyanS "port", D.fromChars "receive", D.fromChars ":", D.fromChars "(String -> msg) -> Sub msg" ]
                ]
        , D.reflow <|
            "The first line defines a `send` port so you can send strings out to JavaScript."
                ++ " Maybe you send them on a WebSocket or put them into IndexedDB. The second line"
                ++ " defines a `receive` port so you can receive strings from JavaScript. Maybe you"
                ++ " get receive messages when new WebSocket messages come in or when the IndexedDB"
                ++ " is changed for some external reason."
        ]



-- DECL TYPE


toDeclTypeReport : Code.Source -> DeclType -> P.Row -> P.Col -> Report.Report
toDeclTypeReport source declType startRow startCol =
    case declType of
        DT_Space space row col ->
            toSpaceReport source space row col

        DT_Name row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "EXPECTING TYPE NAME" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I think I am parsing a type declaration, but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I was expecting a name like"
                            , D.dullyellowS "Status"
                            , D.fromChars "or"
                            , D.dullyellowS "Style"
                            , D.reflow "next. Just make sure it is a name that starts with a capital letter!"
                            ]
                        , customTypeNote
                        ]
                    )

        DT_Alias typeAlias row col ->
            toTypeAliasReport source typeAlias row col

        DT_Union customType row col ->
            toCustomTypeReport source customType row col

        DT_IndentName row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "EXPECTING TYPE NAME" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I think I am parsing a type declaration, but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I was expecting a name like"
                            , D.dullyellowS "Status"
                            , D.fromChars "or"
                            , D.dullyellowS "Style"
                            , D.reflow "next. Just make sure it is a name that starts with a capital letter!"
                            ]
                        , customTypeNote
                        ]
                    )


toTypeAliasReport : Code.Source -> TypeAlias -> P.Row -> P.Col -> Report.Report
toTypeAliasReport source typeAlias startRow startCol =
    case typeAlias of
        AliasSpace space row col ->
            toSpaceReport source space row col

        AliasName row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "EXPECTING TYPE ALIAS NAME" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a type alias, but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I was expecting a name like"
                            , D.dullyellowS "Person"
                            , D.fromChars "or"
                            , D.dullyellowS "Point"
                            , D.reflow "next. Just make sure it is a name that starts with a capital letter!"
                            ]
                        , typeAliasNote
                        ]
                    )

        AliasEquals row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I ran into a reserved word unexpectedly while parsing this type alias:"
                            , D.stack
                                [ D.reflow <|
                                    "It looks like you are trying use `"
                                        ++ keyword
                                        ++ "` as a type variable, but it is a reserved word. Try using a different name?"
                                , typeAliasNote
                                ]
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN TYPE ALIAS" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I am partway through parsing a type alias, but I got stuck here:"
                            , D.stack
                                [ D.reflow <|
                                    "I was expecting to see a type variable or an equals sign next."
                                , typeAliasNote
                                ]
                            )

        AliasBody tipe row col ->
            toTypeReport source TC_TypeAlias tipe row col

        AliasIndentEquals row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED TYPE ALIAS" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a type alias, but I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see a type variable or an equals sign next."
                        , typeAliasNote
                        ]
                    )

        AliasIndentBody row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED TYPE ALIAS" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a type alias, but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I was expecting to see a type next. Something as simple"
                            , D.fromChars "as"
                            , D.dullyellowS "Int"
                            , D.fromChars "or"
                            , D.dullyellowS "Float"
                            , D.fromChars "would"
                            , D.fromChars "work!"
                            ]
                        , typeAliasNote
                        ]
                    )


typeAliasNote : D.Doc
typeAliasNote =
    D.stack
        [ D.toSimpleNote <|
            "Here is an example of a valid `type alias` for reference:"
        , D.vcat <|
            [ D.indent 4 <| D.fillSep [ D.cyanS "type", D.cyanS "alias", D.fromChars "Person", D.fromChars "=" ]
            , D.indent 6 <|
                D.vcat <|
                    [ D.fromChars "{ name : String"
                    , D.fromChars ", age : Int"
                    , D.fromChars ", height : Float"
                    , D.fromChars "}"
                    ]
            ]
        , D.reflow <|
            "This would let us use `Person` as a shorthand for that record type. Using this"
                ++ " shorthand makes type annotations much easier to read, and makes changing code"
                ++ " easier if you decide later that there is more to a person than age and height!"
        ]


toCustomTypeReport : Code.Source -> CustomType -> P.Row -> P.Col -> Report.Report
toCustomTypeReport source customType startRow startCol =
    case customType of
        CT_Space space row col ->
            toSpaceReport source space row col

        CT_Name row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "EXPECTING TYPE NAME" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I think I am parsing a type declaration, but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I was expecting a name like"
                            , D.dullyellowS "Status"
                            , D.fromChars "or"
                            , D.dullyellowS "Style"
                            , D.reflow "next. Just make sure it is a name that starts with a capital letter!"
                            ]
                        , customTypeNote
                        ]
                    )

        CT_Equals row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I ran into a reserved word unexpectedly while parsing this custom type:"
                            , D.stack
                                [ D.reflow <|
                                    "It looks like you are trying use `"
                                        ++ keyword
                                        ++ "` as a type variable, but it is a reserved word. Try using a different name?"
                                , customTypeNote
                                ]
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN CUSTOM TYPE" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I am partway through parsing a custom type, but I got stuck here:"
                            , D.stack
                                [ D.reflow <|
                                    "I was expecting to see a type variable or an equals sign next."
                                , customTypeNote
                                ]
                            )

        CT_Bar row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN CUSTOM TYPE" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a custom type, but I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see a vertical bar like | next."
                        , customTypeNote
                        ]
                    )

        CT_Variant row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN CUSTOM TYPE" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a custom type, but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I was expecting to see a variant name next."
                            , D.reflow "Something like"
                            , D.dullyellowS "Success"
                            , D.fromChars "or"
                            , D.hcat [ D.dullyellowS "Sandwich", D.fromChars "." ]
                            , D.reflow "Any name that starts with a capital letter really!"
                            ]
                        , customTypeNote
                        ]
                    )

        CT_VariantArg tipe row col ->
            toTypeReport source TC_CustomType tipe row col

        CT_IndentEquals row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED CUSTOM TYPE" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a custom type, but I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see a type variable or an equals sign next."
                        , customTypeNote
                        ]
                    )

        CT_IndentBar row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED CUSTOM TYPE" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a custom type, but I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see a vertical bar like | next."
                        , customTypeNote
                        ]
                    )

        CT_IndentAfterBar row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED CUSTOM TYPE" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a custom type, but I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I just saw a vertical bar, so I was expecting to see another variant defined next."
                        , customTypeNote
                        ]
                    )

        CT_IndentAfterEquals row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED CUSTOM TYPE" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a custom type, but I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I just saw an equals sign, so I was expecting to see the first variant defined next."
                        , customTypeNote
                        ]
                    )


customTypeNote : D.Doc
customTypeNote =
    D.stack
        [ D.toSimpleNote <|
            "Here is an example of a valid `type` declaration for reference:"
        , D.vcat <|
            [ D.indent 4 <| D.fillSep [ D.cyanS "type", D.fromChars "Status" ]
            , D.indent 6 <| D.fillSep [ D.fromChars "=", D.fromChars "Failure" ]
            , D.indent 6 <| D.fillSep [ D.fromChars "|", D.fromChars "Waiting" ]
            , D.indent 6 <| D.fillSep [ D.fromChars "|", D.fromChars "Success", D.fromChars "String" ]
            ]
        , D.reflow <|
            "This defines a new `Status` type with three variants. This could be useful if"
                ++ " we are waiting for an HTTP request. Maybe we start with `Waiting` and then"
                ++ " switch to `Failure` or `Success \"message from server\"` depending on how"
                ++ " things go. Notice that the Success variant has some associated data, allowing"
                ++ " us to store a String if the request goes well!"
        ]



-- DECL DEF


toDeclDefReport : Code.Source -> Name.Name -> DeclDef -> P.Row -> P.Col -> Report.Report
toDeclDefReport source name declDef startRow startCol =
    case declDef of
        DeclDefSpace space row col ->
            toSpaceReport source space row col

        DeclDefEquals row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.fillSep
                                [ D.fromChars "The"
                                , D.fromChars "name"
                                , D.hcat [ D.fromChars "`", D.cyanS keyword, D.fromChars "`" ]
                                , D.reflow "is reserved in Elm, so it cannot"
                                , D.reflow "be used as an argument here:"
                                ]
                            , D.stack
                                [ D.reflow <|
                                    "Try renaming it to something else."
                                , case keyword of
                                    "as" ->
                                        D.toFancyNote
                                            [ D.reflow "This keyword is reserved for pattern matches like"
                                            , D.fromChars "((x,y)"
                                            , D.cyanS "as"
                                            , D.reflow "point) where you want to name a tuple and"
                                            , D.reflow "the values it contains."
                                            ]

                                    _ ->
                                        D.toSimpleNote <|
                                            "The `"
                                                ++ keyword
                                                ++ "` keyword has a special meaning in Elm, so it can only be used in certain situations."
                                ]
                            )

                Code.Operator "->" ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toWiderRegion row col 2
                    in
                    Report.Report "MISSING COLON?" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was not expecting to see an arrow here:"
                            , D.stack
                                [ D.fillSep
                                    [ D.reflow "This usually means a"
                                    , D.greenS ":"
                                    , D.reflow "is missing a bit earlier in"
                                    , D.reflow "a type annotation. It could be something else though, so"
                                    , D.reflow "here is a valid definition for reference:"
                                    ]
                                , D.indent 4 <|
                                    D.vcat <|
                                        [ D.fromChars "greet : String -> String"
                                        , D.fromChars "greet name ="
                                        , D.hcat [ D.fromChars "  ", D.dullyellowS "\"Hello \"", D.fromChars " ++ name ++ ", D.dullyellowS "\"!\"" ]
                                        ]
                                , D.reflow <|
                                    "Try to use that format with your `"
                                        ++ name
                                        ++ "` definition!"
                                ]
                            )

                Code.Operator op ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col op
                    in
                    Report.Report "UNEXPECTED SYMBOL" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was not expecting to see this symbol here:"
                            , D.stack
                                [ D.reflow <|
                                    "I am not sure what is going wrong exactly, so here is a valid"
                                        ++ " definition (with an optional type annotation) for reference:"
                                , D.indent 4 <|
                                    D.vcat <|
                                        [ D.fromChars "greet : String -> String"
                                        , D.fromChars "greet name ="
                                        , D.hcat [ D.fromChars "  ", D.dullyellowS "\"Hello \"", D.fromChars " ++ name ++ ", D.dullyellowS "\"!\"" ]
                                        ]
                                , D.reflow <|
                                    "Try to use that format with your `"
                                        ++ name
                                        ++ "` definition!"
                                ]
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN DEFINITION" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I got stuck while parsing the `"
                                    ++ name
                                    ++ "` definition:"
                            , D.stack
                                [ D.reflow <|
                                    "I am not sure what is going wrong exactly, so here is a valid"
                                        ++ " definition (with an optional type annotation) for reference:"
                                , D.indent 4 <|
                                    D.vcat <|
                                        [ D.fromChars "greet : String -> String"
                                        , D.fromChars "greet name ="
                                        , D.hcat [ D.fromChars "  ", D.dullyellowS "\"Hello \"", D.fromChars " ++ name ++ ", D.dullyellowS "\"!\"" ]
                                        ]
                                , D.reflow <|
                                    "Try to use that format!"
                                ]
                            )

        DeclDefType tipe row col ->
            toTypeReport source (TC_Annotation name) tipe row col

        DeclDefArg pattern row col ->
            toPatternReport source PArg pattern row col

        DeclDefBody expr row col ->
            toExprReport source (InDef name startRow startCol) expr row col

        DeclDefNameRepeat row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "EXPECTING DEFINITION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw the type annotation for `"
                            ++ name
                            ++ "` so I was expecting to see its definition here:"
                    , D.stack
                        [ D.reflow <|
                            "Type annotations always appear directly above the relevant"
                                ++ " definition, without anything else in between. (Not even doc comments!)"
                        , declDefNote
                        ]
                    )

        DeclDefNameMatch defName row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "NAME MISMATCH" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw a type annotation for `"
                            ++ name
                            ++ "`, but it is followed by a definition for `"
                            ++ defName
                            ++ "`:"
                    , D.stack
                        [ D.reflow <|
                            "These names do not match! Is there a typo?"
                        , D.indent 4 <|
                            D.fillSep <|
                                [ D.dullyellowS defName, D.fromChars "->", D.greenS name ]
                        ]
                    )

        DeclDefIndentType row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck while parsing the `"
                            ++ name
                            ++ "` type annotation:"
                    , D.stack
                        [ D.reflow <|
                            "I just saw a colon, so I am expecting to see a type next."
                        , declDefNote
                        ]
                    )

        DeclDefIndentEquals row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck while parsing the `"
                            ++ name
                            ++ "` definition:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see an argument or an equals sign next."
                        , declDefNote
                        ]
                    )

        DeclDefIndentBody row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck while parsing the `"
                            ++ name
                            ++ "` definition:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see an expression next. What is it equal to?"
                        , declDefNote
                        ]
                    )


declDefNote : D.Doc
declDefNote =
    D.stack
        [ D.reflow <|
            "Here is a valid definition (with a type annotation) for reference:"
        , D.indent 4 <|
            D.vcat <|
                [ D.fromChars "greet : String -> String"
                , D.fromChars "greet name ="
                , D.hcat [ D.fromChars "  ", D.dullyellowS "\"Hello \"", D.fromChars " ++ name ++ ", D.dullyellowS "\"!\"" ]
                ]
        , D.reflow <|
            "The top line (called a \"type annotation\") is optional. You can leave it off"
                ++ " if you want. As you get more comfortable with Elm and as your project grows,"
                ++ " it becomes more and more valuable to add them though! They work great as"
                ++ " compiler-verified documentation, and they often improve error messages!"
        ]



-- CONTEXT


type Context
    = InNode Node P.Row P.Col Context
    | InDef Name.Name P.Row P.Col
    | InDestruct P.Row P.Col


type Node
    = NRecord
    | NParens
    | NList
    | NFunc
    | NCond
    | NThen
    | NElse
    | NCase
    | NBranch


getDefName : Context -> Maybe Name.Name
getDefName context =
    case context of
        InDestruct _ _ ->
            Nothing

        InDef name _ _ ->
            Just name

        InNode _ _ _ c ->
            getDefName c


isWithin : Node -> Context -> Bool
isWithin desiredNode context =
    case context of
        InDestruct _ _ ->
            False

        InDef _ _ _ ->
            False

        InNode actualNode _ _ _ ->
            desiredNode == actualNode



-- EXPR REPORTS


toExprReport : Code.Source -> Context -> Expr -> P.Row -> P.Col -> Report.Report
toExprReport source context expr startRow startCol =
    case expr of
        Let let_ row col ->
            toLetReport source context let_ row col

        Case case_ row col ->
            toCaseReport source context case_ row col

        If if_ row col ->
            toIfReport source context if_ row col

        CList list row col ->
            toListReport source context list row col

        Record record row col ->
            toRecordReport source context record row col

        Tuple tuple row col ->
            toTupleReport source context tuple row col

        Func func row col ->
            toFuncReport source context func row col

        Dot row col ->
            let
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING RECORD ACCESSOR" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was expecting to see a record accessor here:"
                    , D.fillSep
                        [ D.reflow "Something like"
                        , D.dullyellowS ".name"
                        , D.fromChars "or"
                        , D.dullyellowS ".price"
                        , D.reflow "that accesses a value from a record."
                        ]
                    )

        Access row col ->
            let
                region =
                    toRegion row col
            in
            Report.Report "EXPECTING RECORD ACCESSOR" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I am trying to parse a record accessor here:"
                    , D.stack
                        [ D.fillSep
                            [ D.reflow "Something like"
                            , D.dullyellowS ".name"
                            , D.fromChars "or"
                            , D.dullyellowS ".price"
                            , D.reflow "that accesses a value from a record."
                            ]
                        , D.toSimpleNote <|
                            "Record field names must start with a lower case letter!"
                        ]
                    )

        OperatorRight op row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col

                isMath =
                    MList.elem op [ "-", "+", "*", "/", "^" ]
            in
            Report.Report "MISSING EXPRESSION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw a "
                            ++ op
                            ++ " "
                            ++ (if isMath then
                                    "sign"

                                else
                                    "operator"
                               )
                            ++ ", so I am getting stuck here:"
                    , if isMath then
                        D.fillSep
                            [ D.reflow "I was expecting to see an expression next."
                            , D.reflow "Something like"
                            , D.dullyellowS "42"
                            , D.fromChars "or"
                            , D.dullyellowS "1000"
                            , D.reflow "that makes sense with a"
                            , D.fromName op
                            , D.fromChars "sign."
                            ]

                      else if op == "&&" || op == "||" then
                        D.fillSep
                            [ D.reflow "I was expecting to see an expression next."
                            , D.reflow "Something like"
                            , D.dullyellowS "True"
                            , D.fromChars "or"
                            , D.dullyellowS "False"
                            , D.reflow "that makes sense with boolean logic."
                            ]

                      else if op == "|>" then
                        D.reflow <|
                            "I was expecting to see a function next."

                      else if op == "<|" then
                        D.reflow <|
                            "I was expecting to see an argument next."

                      else
                        D.reflow <|
                            "I was expecting to see an expression next."
                    )

        OperatorReserved operator row col ->
            toOperatorReport source context operator row col

        Start row col ->
            let
                ( contextRow, contextCol, aThing ) =
                    case context of
                        InDestruct r c ->
                            ( r, c, "a definition" )

                        InDef name r c ->
                            ( r, c, "the `" ++ name ++ "` definition" )

                        InNode NRecord r c _ ->
                            ( r, c, "a record" )

                        InNode NParens r c _ ->
                            ( r, c, "some parentheses" )

                        InNode NList r c _ ->
                            ( r, c, "a list" )

                        InNode NFunc r c _ ->
                            ( r, c, "an anonymous function" )

                        InNode NCond r c _ ->
                            ( r, c, "an `if` expression" )

                        InNode NThen r c _ ->
                            ( r, c, "an `if` expression" )

                        InNode NElse r c _ ->
                            ( r, c, "an `if` expression" )

                        InNode NCase r c _ ->
                            ( r, c, "a `case` expression" )

                        InNode NBranch r c _ ->
                            ( r, c, "a `case` expression" )

                surroundings =
                    A.Region (A.Position contextRow contextCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "MISSING EXPRESSION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing "
                            ++ aThing
                            ++ ", but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I was expecting to see an expression like"
                            , D.dullyellowS "42"
                            , D.fromChars "or"
                            , D.hcat [ D.dullyellowS "\"hello\"", D.fromChars "." ]
                            , D.reflow "Once there is something there, I can probably"
                            , D.reflow "give a more specific hint!"
                            ]
                        , D.toSimpleNote <|
                            "This can also happen if I run into reserved words like `let` or `as` unexpectedly."
                                ++ " Or if I run into operators in unexpected spots. Point is, there are a"
                                ++ " couple ways I can get confused and give sort of weird advice!"
                        ]
                    )

        CChar char row col ->
            toCharReport source char row col

        CString string row col ->
            toStringReport source string row col

        Number number row col ->
            toNumberReport source number row col

        Space space row col ->
            toSpaceReport source space row col

        EndlessShader row col ->
            let
                region =
                    toWiderRegion row col 6
            in
            Report.Report "ENDLESS SHADER" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow "I cannot find the end of this shader:"
                    , D.reflow "Add a |] somewhere after this to end the shader."
                    )

        IndentOperatorRight op row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "MISSING EXPRESSION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting to see an expression after this "
                            ++ op
                            ++ " operator:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "You can just put anything for now, like"
                            , D.dullyellowS "42"
                            , D.fromChars "or"
                            , D.hcat [ D.dullyellowS "\"hello\"", D.fromChars "." ]
                            , D.reflow "Once there is something there, I can probably"
                            , D.reflow "give a more specific hint!"
                            ]
                        , D.toSimpleNote <|
                            "I may be getting confused by your indentation? The easiest way to make sure"
                                ++ " this is not an indentation problem is to put the expression on the right of"
                                ++ " the "
                                ++ op
                                ++ " operator on the same line."
                        ]
                    )



-- CHAR


toCharReport : Code.Source -> TChar -> P.Row -> P.Col -> Report.Report
toCharReport source char row col =
    case char of
        CharEndless ->
            let
                region =
                    toRegion row col
            in
            Report.Report "MISSING SINGLE QUOTE" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I thought I was parsing a character, but I got to the end of"
                            ++ " the line without seeing the closing single quote:"
                    , D.reflow <|
                        "Add a closing single quote here!"
                    )

        CharEscape escape ->
            toEscapeReport source escape row col

        CharNotString width ->
            let
                region =
                    toWiderRegion row col width
            in
            Report.Report "NEEDS DOUBLE QUOTES" region <|
                Code.toSnippet source region Nothing <|
                    ( D.fromChars "The following string uses single quotes:"
                    , D.stack
                        [ D.fromChars "Please switch to double quotes instead:"
                        , D.indent 4 <|
                            D.hcat [ D.dullyellowS "'this'", D.fromChars " => ", D.greenS "\"this\"" ]
                        , D.toSimpleNote <|
                            "Elm uses double quotes for strings like \"hello\", whereas it uses single"
                                ++ " quotes for individual characters like 'a' and 'ø'. This distinction helps with"
                                ++ " code like (String.any (\\c -> c == 'X') \"90210\") where you are inspecting"
                                ++ " individual characters."
                        ]
                    )



-- STRING


toStringReport : Code.Source -> TString -> P.Row -> P.Col -> Report.Report
toStringReport source string row col =
    case string of
        StringEndless_Single ->
            let
                region =
                    toRegion row col
            in
            Report.Report "ENDLESS STRING" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I got to the end of the line without seeing the closing double quote:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "Strings look like"
                            , D.greenS "\"this\""
                            , D.reflow "with double"
                            , D.reflow "quotes on each end. Is the closing double"
                            , D.reflow "quote missing in your code?"
                            ]
                        , D.toSimpleNote <|
                            "For a string that spans multiple lines, you can use the multi-line string"
                                ++ " syntax like this:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.dullyellowS "\"\"\""
                                , D.dullyellowS "# Multi-line Strings"
                                , D.dullyellowS ""
                                , D.dullyellowS "- start with triple double quotes"
                                , D.dullyellowS "- write whatever you want"
                                , D.dullyellowS "- no need to escape newlines or double quotes"
                                , D.dullyellowS "- end with triple double quotes"
                                , D.dullyellowS "\"\"\""
                                ]
                        ]
                    )

        StringEndless_Multi ->
            let
                region =
                    toWiderRegion row col 3
            in
            Report.Report "ENDLESS STRING" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I cannot find the end of this multi-line string:"
                    , D.stack
                        [ D.reflow "Add a \"\"\" somewhere after this to end the string."
                        , D.toSimpleNote <|
                            "Here is a valid multi-line string for reference:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.dullyellowS "\"\"\""
                                , D.dullyellowS "# Multi-line Strings"
                                , D.dullyellowS ""
                                , D.dullyellowS "- start with triple double quotes"
                                , D.dullyellowS "- write whatever you want"
                                , D.dullyellowS "- no need to escape newlines or double quotes"
                                , D.dullyellowS "- end with triple double quotes"
                                , D.dullyellowS "\"\"\""
                                ]
                        ]
                    )

        StringEscape escape ->
            toEscapeReport source escape row col



-- ESCAPES


toEscapeReport : Code.Source -> Escape -> P.Row -> P.Col -> Report.Report
toEscapeReport source escape row col =
    case escape of
        EscapeUnknown ->
            let
                region =
                    toWiderRegion row col 2
            in
            Report.Report "UNKNOWN ESCAPE" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "Backslashes always start escaped characters, but I do not recognize this one:"
                    , D.stack
                        [ D.reflow <|
                            "Valid escape characters include:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.dullyellowS "\\n"
                                , D.dullyellowS "\\r"
                                , D.dullyellowS "\\t"
                                , D.dullyellowS "\\\""
                                , D.dullyellowS "\\'"
                                , D.dullyellowS "\\\\"
                                , D.dullyellowS "\\u{003D}"
                                ]
                        , D.reflow <|
                            "Do you want one of those instead? Maybe you need \\\\ to escape a backslash?"
                        , D.toSimpleNote <|
                            "The last style lets encode ANY character by its Unicode code"
                                ++ " point. That means \\u{0009} and \\t are the same. You can use"
                                ++ " that style for anything not covered by the other six escapes!"
                        ]
                    )

        BadUnicodeFormat width ->
            let
                region =
                    toWiderRegion row col width
            in
            Report.Report "BAD UNICODE ESCAPE" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I ran into an invalid Unicode escape:"
                    , D.stack
                        [ D.reflow <|
                            "Here are some examples of valid Unicode escapes:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.dullyellowS "\\u{0041}"
                                , D.dullyellowS "\\u{03BB}"
                                , D.dullyellowS "\\u{6728}"
                                , D.dullyellowS "\\u{1F60A}"
                                ]
                        , D.reflow <|
                            "Notice that the code point is always surrounded by curly braces."
                                ++ " Maybe you are missing the opening or closing curly brace?"
                        ]
                    )

        BadUnicodeCode width ->
            let
                region =
                    toWiderRegion row col width
            in
            Report.Report "BAD UNICODE ESCAPE" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "This is not a valid code point:"
                    , D.reflow <|
                        "The valid code points are between 0 and 10FFFF inclusive."
                    )

        BadUnicodeLength width numDigits badCode ->
            let
                region =
                    toWiderRegion row col width
            in
            Report.Report "BAD UNICODE ESCAPE" region <|
                Code.toSnippet source region Nothing <|
                    if numDigits < 4 then
                        ( D.reflow <|
                            "Every code point needs at least four digits:"
                        , let
                            goodCode =
                                String.repeat (4 - numDigits) "0" ++ String.map Char.toUpper (Hex.toString badCode)

                            suggestion =
                                "\\u{" ++ goodCode ++ "}"
                          in
                          D.fillSep [ D.fromChars "Try", D.greenS suggestion, D.fromChars "instead?" ]
                        )

                    else
                        ( D.reflow <|
                            "This code point has too many digits:"
                        , D.fillSep <|
                            [ D.reflow "Valid code points are between"
                            , D.greenS "\\u{0000}"
                            , D.fromChars "and"
                            , D.hcat [ D.greenS "\\u{10FFFF}", D.fromChars "," ]
                            , D.reflow "so try trimming any leading zeros until"
                            , D.reflow "you have between four and six digits."
                            ]
                        )



-- NUMBERS


toNumberReport : Code.Source -> Number -> P.Row -> P.Col -> Report.Report
toNumberReport source number row col =
    let
        region =
            toRegion row col
    in
    case number of
        NumberEnd ->
            Report.Report "WEIRD NUMBER" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I thought I was reading a number, but I ran into some weird stuff here:"
                    , D.stack
                        [ D.reflow <|
                            "I recognize numbers in the following formats:"
                        , D.indent 4 <| D.vcat [ D.fromChars "42", D.fromChars "3.14", D.fromChars "6.022e23", D.fromChars "0x002B" ]
                        , D.reflow <|
                            "So is there a way to write it like one of those?"
                        ]
                    )

        NumberDot int ->
            Report.Report "WEIRD NUMBER" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "Numbers cannot end with a dot like this:"
                    , D.fillSep
                        [ D.reflow "Switching to"
                        , D.greenS (String.fromInt int)
                        , D.fromChars "or"
                        , D.greenS (String.fromInt int ++ ".0")
                        , D.reflow "will work though!"
                        ]
                    )

        NumberHexDigit ->
            Report.Report "WEIRD HEXIDECIMAL" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I thought I was reading a hexidecimal number until I got here:"
                    , D.stack
                        [ D.reflow <|
                            "Valid hexidecimal digits include 0123456789abcdefABCDEF, so I can"
                                ++ " only recognize things like this:"
                        , D.indent 4 <| D.vcat [ D.fromChars "0x2B", D.fromChars "0x002B", D.fromChars "0x00ffb3" ]
                        ]
                    )

        NumberNoLeadingZero ->
            Report.Report "LEADING ZEROS" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I do not accept numbers with leading zeros:"
                    , D.stack
                        [ D.reflow <|
                            "Just delete the leading zeros and it should work!"
                        , D.toSimpleNote <|
                            "Some languages let you to specify octal numbers by adding a leading zero."
                                ++ " So in C, writing 0111 is the same as writing 73. Some people are used to"
                                ++ " that, but others probably want it to equal 111. Either path is going to"
                                ++ " surprise people from certain backgrounds, so Elm tries to avoid this whole"
                                ++ " situation."
                        ]
                    )



-- OPERATORS


toOperatorReport : Code.Source -> Context -> Symbol.BadOperator -> P.Row -> P.Col -> Report.Report
toOperatorReport source context operator row col =
    case operator of
        Symbol.BadDot ->
            let
                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED SYMBOL" region <|
                Code.toSnippet source region Nothing <|
                    ( D.fromChars "I was not expecting this dot:"
                    , D.reflow <|
                        "Dots are for record access and decimal points, so"
                            ++ " they cannot float around on their own. Maybe"
                            ++ " there is some extra whitespace?"
                    )

        Symbol.BadPipe ->
            let
                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED SYMBOL" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was not expecting this vertical bar:"
                    , D.reflow <|
                        "Vertical bars should only appear in custom type declarations. Maybe you want || instead?"
                    )

        Symbol.BadArrow ->
            let
                region =
                    toWiderRegion row col 2
            in
            Report.Report "UNEXPECTED ARROW" region <|
                Code.toSnippet source region Nothing <|
                    if isWithin NCase context then
                        ( D.reflow <|
                            "I am parsing a `case` expression right now, but this arrow is confusing me:"
                        , D.stack
                            [ D.reflow "Maybe the `of` keyword is missing on a previous line?"
                            , noteForCaseError
                            ]
                        )

                    else if isWithin NBranch context then
                        ( D.reflow <|
                            "I am parsing a `case` expression right now, but this arrow is confusing me:"
                        , D.stack
                            [ D.reflow <|
                                "It makes sense to see arrows around here, so I suspect it is something earlier. Maybe this pattern is indented a bit farther than the previous patterns?"
                            , noteForCaseIndentError
                            ]
                        )

                    else
                        ( D.reflow <|
                            "I was partway through parsing an expression when I got stuck on this arrow:"
                        , D.stack <|
                            [ D.fromChars <|
                                "Arrows should only appear in `case` expressions and anonymous functions.\n"
                                    ++ "Maybe it was supposed to be a > sign instead?"
                            , D.toSimpleNote <|
                                "The syntax for anonymous functions is (\\x -> x + 1) so the arguments all appear"
                                    ++ " after the backslash and before the arrow. Maybe a backslash is missing earlier?"
                            ]
                        )

        Symbol.BadEquals ->
            let
                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED EQUALS" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was not expecting to see this equals sign:"
                    , D.stack
                        [ D.reflow "Maybe you want == instead? To check if two values are equal?"
                        , D.toSimpleNote <|
                            if isWithin NRecord context then
                                "Records look like { x = 3, y = 4 } with the equals sign right"
                                    ++ " after the field name. So maybe you forgot a comma?"

                            else
                                case getDefName context of
                                    Nothing ->
                                        "I may be getting confused by your indentation. I need all definitions to be indented"
                                            ++ " exactly the same amount, so if this is meant to be a new definition, it may have too"
                                            ++ " many spaces in front of it."

                                    Just name ->
                                        "I may be getting confused by your indentation. I think I am still parsing the `"
                                            ++ name
                                            ++ "` definition. Is this supposed to be part of a definition"
                                            ++ " after that? If so, the problem may be a bit before the equals sign. I need all"
                                            ++ " definitions to be indented exactly the same amount, so the problem may be that"
                                            ++ " this new definition has too many spaces in front of it."
                        ]
                    )

        Symbol.BadHasType ->
            let
                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED SYMBOL" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was not expecting to run into the \"has type\" symbol here:"
                    , case getDefName context of
                        Nothing ->
                            D.fillSep
                                [ D.reflow "Maybe you want"
                                , D.greenS "::"
                                , D.fromChars "instead?"
                                , D.reflow "To put something on the front of a list?"
                                ]

                        Just name ->
                            D.stack
                                [ D.fillSep
                                    [ D.reflow "Maybe you want"
                                    , D.greenS "::"
                                    , D.fromChars "instead?"
                                    , D.reflow "To put something on the front of a list?"
                                    ]
                                , D.toSimpleNote <|
                                    "The single colon is reserved for type annotations and record types, but I think"
                                        ++ " I am parsing the definition of `"
                                        ++ name
                                        ++ "` right now."
                                , D.toSimpleNote <|
                                    "I may be getting confused by your indentation. Is this supposed to be part of"
                                        ++ " a type annotation AFTER the `"
                                        ++ name
                                        ++ "` definition? If so,"
                                        ++ " the problem may be a bit before the \"has type\" symbol. I need all definitions to"
                                        ++ " be exactly aligned (with exactly the same indentation) so the problem may be that"
                                        ++ " this new definition is indented a bit too much."
                                ]
                    )



-- CASE


toLetReport : Code.Source -> Context -> Let -> P.Row -> P.Col -> Report.Report
toLetReport source context let_ startRow startCol =
    case let_ of
        LetSpace space row col ->
            toSpaceReport source space row col

        LetIn row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "LET PROBLEM" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was partway through parsing a `let` expression, but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "Based on the indentation, I was expecting to see the"
                            , D.cyanS "in"
                            , D.reflow "keyword next. Is there a typo?"
                            ]
                        , D.toSimpleNote <|
                            "This can also happen if you are trying to define another value within the `let` but"
                                ++ " it is not indented enough. Make sure each definition has exactly the same amount of"
                                ++ " spaces before it. They should line up exactly!"
                        ]
                    )

        LetDefAlignment row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "LET PROBLEM" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was partway through parsing a `let` expression, but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "Based on the indentation, I was expecting to see the"
                            , D.cyanS "in"
                            , D.reflow "keyword next. Is there a typo?"
                            ]
                        , D.toSimpleNote <|
                            "This can also happen if you are trying to define another value within the `let` but"
                                ++ " it is not indented enough. Make sure each definition has exactly the same amount of"
                                ++ " spaces before it. They should line up exactly!"
                        ]
                    )

        LetDefName row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was partway through parsing a `let` expression, but I got stuck here:"
                            , D.reflow <|
                                "It looks like you are trying to use `"
                                    ++ keyword
                                    ++ "` as a variable name, but"
                                    ++ " it is a reserved word! Try using a different name instead."
                            )

                _ ->
                    toUnfinishLetReport source row col startRow startCol <|
                        D.reflow <|
                            "I was expecting the name of a definition next."

        LetDef name def row col ->
            toLetDefReport source name def row col

        LetDestruct destruct row col ->
            toLetDestructReport source destruct row col

        LetBody expr row col ->
            toExprReport source context expr row col

        LetIndentDef row col ->
            toUnfinishLetReport source row col startRow startCol <|
                D.reflow <|
                    "I was expecting a value to be defined here."

        LetIndentIn row col ->
            toUnfinishLetReport source row col startRow startCol <|
                D.fillSep <|
                    [ D.reflow "I was expecting to see the"
                    , D.cyanS "in"
                    , D.reflow "keyword next."
                    , D.reflow "Or maybe more of that expression?"
                    ]

        LetIndentBody row col ->
            toUnfinishLetReport source row col startRow startCol <|
                D.reflow <|
                    "I was expecting an expression next. Tell me what should happen with the value you just defined!"


toUnfinishLetReport : Code.Source -> P.Row -> P.Col -> P.Row -> P.Col -> D.Doc -> Report.Report
toUnfinishLetReport source row col startRow startCol message =
    let
        surroundings =
            A.Region (A.Position startRow startCol) (A.Position row col)

        region =
            toRegion row col
    in
    Report.Report "UNFINISHED LET" region <|
        Code.toSnippet source surroundings (Just region) <|
            ( D.reflow <|
                "I was partway through parsing a `let` expression, but I got stuck here:"
            , D.stack
                [ message
                , D.toSimpleNote <|
                    "Here is an example with a valid `let` expression for reference:"
                , D.indent 4 <|
                    D.vcat <|
                        [ D.indent 0 <| D.fillSep [ D.reflow "viewPerson person =" ]
                        , D.indent 2 <| D.fillSep [ D.cyanS "let" ]
                        , D.indent 4 <| D.fillSep [ D.reflow "fullName =" ]
                        , D.indent 6 <| D.fillSep [ D.reflow "person.firstName ++", D.dullyellowS "\" \"", D.reflow "++ person.lastName" ]
                        , D.indent 2 <| D.fillSep [ D.cyanS "in" ]
                        , D.indent 2 <| D.fillSep [ D.reflow "div [] [ text fullName ]" ]
                        ]
                , D.reflow <|
                    "Here we defined a `viewPerson` function that turns a person into some HTML. We use"
                        ++ " a `let` expression to define the `fullName` we want to show. Notice the indentation! The"
                        ++ " `fullName` is indented more than the `let` keyword, and the actual value of `fullName` is"
                        ++ " indented a bit more than that. That is important!"
                ]
            )


toLetDefReport : Code.Source -> Name.Name -> Def -> P.Row -> P.Col -> Report.Report
toLetDefReport source name def startRow startCol =
    case def of
        DefSpace space row col ->
            toSpaceReport source space row col

        DefType tipe row col ->
            toTypeReport source (TC_Annotation name) tipe row col

        DefNameRepeat row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "EXPECTING DEFINITION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw the type annotation for `"
                            ++ name
                            ++ "` so I was expecting to see its definition here:"
                    , D.stack
                        [ D.reflow <|
                            "Type annotations always appear directly above the relevant"
                                ++ " definition, without anything else in between."
                        , defNote
                        ]
                    )

        DefNameMatch defName row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "NAME MISMATCH" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw a type annotation for `"
                            ++ name
                            ++ "`, but it is followed by a definition for `"
                            ++ defName
                            ++ "`:"
                    , D.stack
                        [ D.reflow <|
                            "These names do not match! Is there a typo?"
                        , D.indent 4 <|
                            D.fillSep <|
                                [ D.dullyellowS defName, D.fromChars "->", D.greenS name ]
                        ]
                    )

        DefArg pattern row col ->
            toPatternReport source PArg pattern row col

        DefEquals row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.fillSep
                                [ D.reflow "The name"
                                , D.hcat [ D.fromChars "`", D.cyanS keyword, D.fromChars "`" ]
                                , D.reflow "is reserved in Elm, so it cannot"
                                , D.reflow "be used as an argument here:"
                                ]
                            , D.stack
                                [ D.reflow <|
                                    "Try renaming it to something else."
                                , case keyword of
                                    "as" ->
                                        D.toFancyNote
                                            [ D.reflow "This keyword is reserved for pattern matches like"
                                            , D.fromChars "((x,y)"
                                            , D.cyanS "as"
                                            , D.reflow "point) where you want to name a tuple and"
                                            , D.reflow "the values it contains."
                                            ]

                                    _ ->
                                        D.toSimpleNote <|
                                            "The `"
                                                ++ keyword
                                                ++ "` keyword has a special meaning in Elm, so it can only be used in certain situations."
                                ]
                            )

                Code.Operator "->" ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toWiderRegion row col 2
                    in
                    Report.Report "MISSING COLON?" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was not expecting to see an arrow here:"
                            , D.stack
                                [ D.fillSep
                                    [ D.reflow "This usually means a"
                                    , D.greenS ":"
                                    , D.reflow "is missing a bit earlier in"
                                    , D.reflow "a type annotation. It could be something else though, so"
                                    , D.reflow "here is a valid definition for reference:"
                                    ]
                                , D.indent 4 <|
                                    D.vcat <|
                                        [ D.fromChars "greet : String -> String"
                                        , D.fromChars "greet name ="
                                        , D.hcat [ D.fromChars "  ", D.dullyellowS "\"Hello \"", D.fromChars " ++ name ++ ", D.dullyellowS "\"!\"" ]
                                        ]
                                , D.reflow <|
                                    "Try to use that format with your `"
                                        ++ name
                                        ++ "` definition!"
                                ]
                            )

                Code.Operator op ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col op
                    in
                    Report.Report "UNEXPECTED SYMBOL" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was not expecting to see this symbol here:"
                            , D.stack
                                [ D.reflow <|
                                    "I am not sure what is going wrong exactly, so here is a valid"
                                        ++ " definition (with an optional type annotation) for reference:"
                                , D.indent 4 <|
                                    D.vcat <|
                                        [ D.fromChars "greet : String -> String"
                                        , D.fromChars "greet name ="
                                        , D.hcat [ D.fromChars "  ", D.dullyellowS "\"Hello \"", D.fromChars " ++ name ++ ", D.dullyellowS "\"!\"" ]
                                        ]
                                , D.reflow <|
                                    "Try to use that format with your `"
                                        ++ name
                                        ++ "` definition!"
                                ]
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN DEFINITION" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I got stuck while parsing the `"
                                    ++ name
                                    ++ "` definition:"
                            , D.stack
                                [ D.reflow <|
                                    "I am not sure what is going wrong exactly, so here is a valid"
                                        ++ " definition (with an optional type annotation) for reference:"
                                , D.indent 4 <|
                                    D.vcat <|
                                        [ D.fromChars "greet : String -> String"
                                        , D.fromChars "greet name ="
                                        , D.hcat [ D.fromChars "  ", D.dullyellowS "\"Hello \"", D.fromChars " ++ name ++ ", D.dullyellowS "\"!\"" ]
                                        ]
                                , D.reflow <|
                                    "Try to use that format!"
                                ]
                            )

        DefBody expr row col ->
            toExprReport source (InDef name startRow startCol) expr row col

        DefIndentEquals row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck while parsing the `"
                            ++ name
                            ++ "` definition:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see an argument or an equals sign next."
                        , defNote
                        ]
                    )

        DefIndentType row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck while parsing the `"
                            ++ name
                            ++ "` type annotation:"
                    , D.stack
                        [ D.reflow <|
                            "I just saw a colon, so I am expecting to see a type next."
                        , defNote
                        ]
                    )

        DefIndentBody row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck while parsing the `"
                            ++ name
                            ++ "` definition:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see an expression next. What is it equal to?"
                        , declDefNote
                        ]
                    )

        DefAlignment indent row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col

                offset =
                    indent - col
            in
            Report.Report "PROBLEM IN DEFINITION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck while parsing the `"
                            ++ name
                            ++ "` definition:"
                    , D.reflow <|
                        "I just saw a type annotation indented "
                            ++ String.fromInt indent
                            ++ " spaces, so I was"
                            ++ " expecting to see the corresponding definition next with the exact same amount"
                            ++ " of indentation. It looks like this line needs "
                            ++ String.fromInt offset
                            ++ " more "
                            ++ (if offset == 1 then
                                    "space"

                                else
                                    "spaces"
                               )
                            ++ "?"
                    )


defNote : D.Doc
defNote =
    D.stack
        [ D.reflow <|
            "Here is a valid definition (with a type annotation) for reference:"
        , D.indent 4 <|
            D.vcat <|
                [ D.fromChars "greet : String -> String"
                , D.fromChars "greet name ="
                , D.hcat [ D.fromChars "  ", D.dullyellowS "\"Hello \"", D.fromChars " ++ name ++ ", D.dullyellowS "\"!\"" ]
                ]
        , D.reflow <|
            "The top line (called a \"type annotation\") is optional. You can leave it off"
                ++ " if you want. As you get more comfortable with Elm and as your project grows,"
                ++ " it becomes more and more valuable to add them though! They work great as"
                ++ " compiler-verified documentation, and they often improve error messages!"
        ]


toLetDestructReport : Code.Source -> Destruct -> P.Row -> P.Col -> Report.Report
toLetDestructReport source destruct startRow startCol =
    case destruct of
        DestructSpace space row col ->
            toSpaceReport source space row col

        DestructPattern pattern row col ->
            toPatternReport source PLet pattern row col

        DestructEquals row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN DEFINITION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck trying to parse this definition:"
                    , case Code.whatIsNext source row col of
                        Code.Operator ":" ->
                            D.stack
                                [ D.reflow <|
                                    "I was expecting to see an equals sign next, followed by an expression"
                                        ++ " telling me what to compute."
                                , D.toSimpleNote <|
                                    "It looks like you may be trying to write a type annotation? It is not"
                                        ++ " possible to add type annotations on destructuring definitions like this."
                                        ++ " You can assign a name to the overall structure, put a type annotation on"
                                        ++ " that, and then destructure separately though."
                                ]

                        _ ->
                            D.reflow <|
                                "I was expecting to see an equals sign next, followed by an expression"
                                    ++ " telling me what to compute."
                    )

        DestructBody expr row col ->
            toExprReport source (InDestruct startRow startCol) expr row col

        DestructIndentEquals row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck trying to parse this definition:"
                    , D.reflow <|
                        "I was expecting to see an equals sign next, followed by an expression"
                            ++ " telling me what to compute."
                    )

        DestructIndentBody row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED DEFINITION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck while parsing this definition:"
                    , D.reflow <|
                        "I was expecting to see an expression next. What is it equal to?"
                    )



-- CASE


toCaseReport : Code.Source -> Context -> Case -> P.Row -> P.Col -> Report.Report
toCaseReport source context case_ startRow startCol =
    case case_ of
        CaseSpace space row col ->
            toSpaceReport source space row col

        CaseOf row col ->
            toUnfinishCaseReport source row col startRow startCol <|
                D.fillSep [ D.reflow "I was expecting to see the", D.dullyellowS "of", D.reflow "keyword next." ]

        CasePattern pattern row col ->
            toPatternReport source PCase pattern row col

        CaseArrow row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I am partway through parsing a `case` expression, but I got stuck here:"
                            , D.reflow <|
                                "It looks like you are trying to use `"
                                    ++ keyword
                                    ++ "` in one of your"
                                    ++ " patterns, but it is a reserved word. Try using a different name?"
                            )

                Code.Operator ":" ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "UNEXPECTED OPERATOR" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I am partway through parsing a `case` expression, but I got stuck here:"
                            , D.fillSep <|
                                [ D.reflow "I am seeing"
                                , D.dullyellowS ":"
                                , D.reflow "but maybe you want"
                                , D.greenS "::"
                                , D.fromChars "instead?"
                                , D.reflow "For pattern matching on lists?"
                                ]
                            )

                Code.Operator "=" ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "UNEXPECTED OPERATOR" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I am partway through parsing a `case` expression, but I got stuck here:"
                            , D.fillSep <|
                                [ D.reflow "I am seeing"
                                , D.dullyellowS "="
                                , D.reflow "but maybe you want"
                                , D.greenS "->"
                                , D.fromChars "instead?"
                                ]
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "MISSING ARROW" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I am partway through parsing a `case` expression, but I got stuck here:"
                            , D.stack
                                [ D.reflow "I was expecting to see an arrow next."
                                , noteForCaseIndentError
                                ]
                            )

        CaseExpr expr row col ->
            toExprReport source (InNode NCase startRow startCol context) expr row col

        CaseBranch expr row col ->
            toExprReport source (InNode NBranch startRow startCol context) expr row col

        CaseIndentOf row col ->
            toUnfinishCaseReport source row col startRow startCol <|
                D.fillSep [ D.reflow "I was expecting to see the", D.dullyellowS "of", D.reflow "keyword next." ]

        CaseIndentExpr row col ->
            toUnfinishCaseReport source row col startRow startCol <|
                D.reflow "I was expecting to see a expression next."

        CaseIndentPattern row col ->
            toUnfinishCaseReport source row col startRow startCol <|
                D.reflow "I was expecting to see a pattern next."

        CaseIndentArrow row col ->
            toUnfinishCaseReport source row col startRow startCol <|
                D.fillSep
                    [ D.reflow "I just saw a pattern, so I was expecting"
                    , D.reflow "to see a"
                    , D.dullyellowS "->"
                    , D.fromChars "next."
                    ]

        CaseIndentBranch row col ->
            toUnfinishCaseReport source row col startRow startCol <|
                D.reflow <|
                    "I was expecting to see an expression next. What should I do when"
                        ++ " I run into this particular pattern?"

        CasePatternAlignment indent row col ->
            toUnfinishCaseReport source row col startRow startCol <|
                D.reflow <|
                    "I suspect this is a pattern that is not indented far enough? ("
                        ++ String.fromInt indent
                        ++ " spaces)"


toUnfinishCaseReport : Code.Source -> P.Row -> P.Col -> P.Row -> P.Col -> D.Doc -> Report.Report
toUnfinishCaseReport source row col startRow startCol message =
    let
        surroundings =
            A.Region (A.Position startRow startCol) (A.Position row col)

        region =
            toRegion row col
    in
    Report.Report "UNFINISHED CASE" region <|
        Code.toSnippet source surroundings (Just region) <|
            ( D.reflow <|
                "I was partway through parsing a `case` expression, but I got stuck here:"
            , D.stack
                [ message
                , noteForCaseError
                ]
            )


noteForCaseError : D.Doc
noteForCaseError =
    D.stack
        [ D.toSimpleNote <|
            "Here is an example of a valid `case` expression for reference."
        , D.vcat <|
            [ D.indent 4 <| D.fillSep [ D.cyanS "case", D.fromChars "maybeWidth", D.cyanS "of" ]
            , D.indent 6 <| D.fillSep [ D.blue "Just", D.reflow "width ->" ]
            , D.indent 8 <| D.fillSep [ D.reflow "width +", D.dullyellowS "200" ]
            , D.fromChars ""
            , D.indent 6 <| D.fillSep [ D.blue "Nothing", D.fromChars "->" ]
            , D.indent 8 <| D.fillSep [ D.dullyellowS "400" ]
            ]
        , D.reflow <|
            "Notice the indentation. Each pattern is aligned, and each branch is indented"
                ++ " a bit more than the corresponding pattern. That is important!"
        ]


noteForCaseIndentError : D.Doc
noteForCaseIndentError =
    D.stack
        [ D.toSimpleNote <|
            "Sometimes I get confused by indentation, so try to make your `case` look"
                ++ " something like this:"
        , D.vcat <|
            [ D.indent 4 <| D.fillSep [ D.cyanS "case", D.fromChars "maybeWidth", D.cyanS "of" ]
            , D.indent 6 <| D.fillSep [ D.blue "Just", D.reflow "width ->" ]
            , D.indent 8 <| D.fillSep [ D.reflow "width +", D.dullyellowS "200" ]
            , D.fromChars ""
            , D.indent 6 <| D.fillSep [ D.blue "Nothing", D.fromChars "->" ]
            , D.indent 8 <| D.fillSep [ D.dullyellowS "400" ]
            ]
        , D.reflow <|
            "Notice the indentation! Patterns are aligned with each other. Same indentation."
                ++ " The expressions after each arrow are all indented a bit more than the patterns."
                ++ " That is important!"
        ]



-- IF


toIfReport : Code.Source -> Context -> If -> P.Row -> P.Col -> Report.Report
toIfReport source context if_ startRow startCol =
    case if_ of
        IfSpace space row col ->
            toSpaceReport source space row col

        IfThen row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting to see more of this `if` expression, but I got stuck here:"
                    , D.fillSep <|
                        [ D.reflow "I was expecting to see the"
                        , D.cyanS "then"
                        , D.reflow "keyword next."
                        ]
                    )

        IfElse row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting to see more of this `if` expression, but I got stuck here:"
                    , D.fillSep <|
                        [ D.reflow "I was expecting to see the"
                        , D.cyanS "else"
                        , D.reflow "keyword next."
                        ]
                    )

        IfElseBranchStart row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw the start of an `else` branch, but then I got stuck here:"
                    , D.reflow <|
                        "I was expecting to see an expression next. Maybe it is not filled in yet?"
                    )

        IfCondition expr row col ->
            toExprReport source (InNode NCond startRow startCol context) expr row col

        IfThenBranch expr row col ->
            toExprReport source (InNode NThen startRow startCol context) expr row col

        IfElseBranch expr row col ->
            toExprReport source (InNode NElse startRow startCol context) expr row col

        IfIndentCondition row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting to see more of this `if` expression, but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I was expecting to see an expression like"
                            , D.dullyellowS "x < 0"
                            , D.reflow "that evaluates to True or False."
                            ]
                        , D.toSimpleNote <|
                            "I can be confused by indentation. Maybe something is not indented enough?"
                        ]
                    )

        IfIndentThen row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting to see more of this `if` expression, but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I was expecting to see the"
                            , D.cyanS "then"
                            , D.reflow "keyword next."
                            ]
                        , D.toSimpleNote <|
                            "I can be confused by indentation. Maybe something is not indented enough?"
                        ]
                    )

        IfIndentThenBranch row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck after the start of this `then` branch:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see an expression next. Maybe it is not filled in yet?"
                        , D.toSimpleNote <|
                            "I can be confused by indentation, so if the `then` branch is already"
                                ++ " present, it may not be indented enough for me to recognize it."
                        ]
                    )

        IfIndentElseBranch row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED IF" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I got stuck after the start of this `else` branch:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see an expression next. Maybe it is not filled in yet?"
                        , D.toSimpleNote <|
                            "I can be confused by indentation, so if the `else` branch is already"
                                ++ " present, it may not be indented enough for me to recognize it."
                        ]
                    )

        IfIndentElse row col ->
            case Code.nextLineStartsWithKeyword "else" source row of
                Just ( elseRow, elseCol ) ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position elseRow elseCol)

                        region =
                            toWiderRegion elseRow elseCol 4
                    in
                    Report.Report "WEIRD ELSE BRANCH" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was partway through an `if` expression when I got stuck here:"
                            , D.fillSep <|
                                [ D.reflow "I think this"
                                , D.cyanS "else"
                                , D.reflow "keyword needs to be indented more."
                                , D.reflow "Try adding some spaces before it."
                                ]
                            )

                Nothing ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED IF" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was expecting to see an `else` branch after this:"
                            , D.stack
                                [ D.fillSep
                                    [ D.reflow "I know what to do when the condition is True,"
                                    , D.reflow "but what happens when it is False?"
                                    , D.reflow "Add an"
                                    , D.cyanS "else"
                                    , D.reflow "branch to handle that scenario!"
                                    ]
                                ]
                            )



-- RECORD


toRecordReport : Code.Source -> Context -> Record -> P.Row -> P.Col -> Report.Report
toRecordReport source context record startRow startCol =
    case record of
        RecordOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I just started parsing a record, but I got stuck on this field name:"
                            , D.reflow <|
                                "It looks like you are trying to use `"
                                    ++ keyword
                                    ++ "` as a field name, but"
                                    ++ " that is a reserved word. Try using a different name!"
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN RECORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I just started parsing a record, but I got stuck here:"
                            , D.stack
                                [ D.fillSep
                                    [ D.reflow "I was expecting to see a record field defined next,"
                                    , D.reflow "so I am looking for a name like"
                                    , D.dullyellowS "userName"
                                    , D.fromChars "or"
                                    , D.hcat [ D.dullyellowS "plantHeight", D.fromChars "." ]
                                    ]
                                , D.toSimpleNote <|
                                    "Field names must start with a lower-case letter. After that, you can use"
                                        ++ " any sequence of letters, numbers, and underscores."
                                , noteForRecordError
                                ]
                            )

        RecordEnd row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN RECORD" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a record, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.reflow "I was expecting to see a closing curly brace before this,"
                            , D.reflow "so try adding a"
                            , D.dullyellowS "}"
                            , D.reflow "and see if that helps?"
                            ]
                        , D.toSimpleNote <|
                            "When I get stuck like this, it usually means that there is a missing parenthesis"
                                ++ " or bracket somewhere earlier. It could also be a stray keyword or operator."
                        ]
                    )

        RecordField row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I am partway through parsing a record, but I got stuck on this field name:"
                            , D.reflow <|
                                "It looks like you are trying to use `"
                                    ++ keyword
                                    ++ "` as a field name, but"
                                    ++ " that is a reserved word. Try using a different name!"
                            )

                Code.Other (Just ',') ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "EXTRA COMMA" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I am partway through parsing a record, but I got stuck here:"
                            , D.stack
                                [ D.reflow <|
                                    "I am seeing two commas in a row. This is the second one!"
                                , D.reflow <|
                                    "Just delete one of the commas and you should be all set!"
                                , noteForRecordError
                                ]
                            )

                Code.Close _ '}' ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "EXTRA COMMA" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I am partway through parsing a record, but I got stuck here:"
                            , D.stack
                                [ D.reflow <|
                                    "Trailing commas are not allowed in records. Try deleting the comma that appears"
                                        ++ " before this closing curly brace."
                                , noteForRecordError
                                ]
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN RECORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I am partway through parsing a record, but I got stuck here:"
                            , D.stack
                                [ D.fillSep
                                    [ D.reflow "I was expecting to see another record field defined next,"
                                    , D.reflow "so I am looking for a name like"
                                    , D.dullyellowS "userName"
                                    , D.fromChars "or"
                                    , D.hcat [ D.dullyellowS "plantHeight", D.fromChars "." ]
                                    ]
                                , D.toSimpleNote <|
                                    "Field names must start with a lower-case letter. After that, you can use"
                                        ++ " any sequence of letters, numbers, and underscores."
                                , noteForRecordError
                                ]
                            )

        RecordEquals row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "PROBLEM IN RECORD" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a record, but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I just saw a field name, so I was expecting to see"
                            , D.reflow "an equals sign next. So try putting an"
                            , D.greenS "="
                            , D.reflow "sign here?"
                            ]
                        , noteForRecordError
                        ]
                    )

        RecordExpr expr row col ->
            toExprReport source (InNode NRecord startRow startCol context) expr row col

        RecordSpace space row col ->
            toSpaceReport source space row col

        RecordIndentOpen row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw the opening curly brace of a record, but then I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I am expecting a record like"
                            , D.dullyellowS "{ x = 3, y = 4 }"
                            , D.fromChars "here."
                            , D.reflow "Try defining some fields of your own?"
                            ]
                        , noteForRecordIndentError
                        ]
                    )

        RecordIndentEnd row col ->
            case Code.nextLineStartsWithCloseCurly source row of
                Just ( curlyRow, curlyCol ) ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position curlyRow curlyCol)

                        region =
                            toRegion curlyRow curlyCol
                    in
                    Report.Report "NEED MORE INDENTATION" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was partway through parsing a record, but I got stuck here:"
                            , D.stack
                                [ D.reflow <|
                                    "I need this curly brace to be indented more. Try adding some spaces before it!"
                                , noteForRecordError
                                ]
                            )

                Nothing ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED RECORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was partway through parsing a record, but I got stuck here:"
                            , D.stack
                                [ D.fillSep <|
                                    [ D.reflow "I was expecting to see a closing curly brace next."
                                    , D.reflow "Try putting a"
                                    , D.greenS "}"
                                    , D.reflow "next and see if that helps?"
                                    ]
                                , noteForRecordIndentError
                                ]
                            )

        RecordIndentField row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a record, but I got stuck after that last comma:"
                    , D.stack
                        [ D.reflow <|
                            "Trailing commas are not allowed in records, so the fix mxyay be to"
                                ++ " delete that last comma? Or maybe you were in the middle of defining"
                                ++ " an additional field?"
                        , noteForRecordError
                        ]
                    )

        RecordIndentEquals row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a record. I just saw a record"
                            ++ " field, so I was expecting to see an equals sign next:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "Try putting an"
                            , D.greenS "="
                            , D.reflow "followed by an expression?"
                            ]
                        , noteForRecordIndentError
                        ]
                    )

        RecordIndentExpr row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a record, and I was expecting to run into an expression next:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "Try putting something like"
                            , D.dullyellowS "42"
                            , D.fromChars "or"
                            , D.dullyellowS "\"hello\""
                            , D.reflow "for now?"
                            ]
                        , noteForRecordIndentError
                        ]
                    )


noteForRecordError : D.Doc
noteForRecordError =
    D.stack <|
        [ D.toSimpleNote
            "If you are trying to define a record across multiple lines, I recommend using this format:"
        , D.indent 4 <|
            D.vcat <|
                [ D.hcat [ D.fromChars "{ name = ", D.dullyellowS "\"Alice\"" ]
                , D.hcat [ D.fromChars ", age = ", D.dullyellowS "42" ]
                , D.hcat [ D.fromChars ", height = ", D.dullyellowS "1.75" ]
                , D.fromChars "}"
                ]
        , D.reflow <|
            "Notice that each line starts with some indentation. Usually two or four spaces."
                ++ " This is the stylistic convention in the Elm ecosystem."
        ]


noteForRecordIndentError : D.Doc
noteForRecordIndentError =
    D.stack
        [ D.toSimpleNote <|
            "I may be confused by indentation. For example, if you are trying to define"
                ++ " a record across multiple lines, I recommend using this format:"
        , D.indent 4 <|
            D.vcat <|
                [ D.hcat [ D.fromChars "{ name = ", D.dullyellowS "\"Alice\"" ]
                , D.hcat [ D.fromChars ", age = ", D.dullyellowS "42" ]
                , D.hcat [ D.fromChars ", height = ", D.dullyellowS "1.75" ]
                , D.fromChars "}"
                ]
        , D.reflow <|
            "Notice that each line starts with some indentation. Usually two or four spaces."
                ++ " This is the stylistic convention in the Elm ecosystem!"
        ]



-- TUPLE


toTupleReport : Code.Source -> Context -> Tuple -> P.Row -> P.Col -> Report.Report
toTupleReport source context tuple startRow startCol =
    case tuple of
        TupleExpr expr row col ->
            toExprReport source (InNode NParens startRow startCol context) expr row col

        TupleSpace space row col ->
            toSpaceReport source space row col

        TupleEnd row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting to see a closing parentheses next, but I got stuck here:"
                    , D.stack
                        [ D.fillSep [ D.reflow "Try adding a", D.dullyellowS ")", D.reflow "to see if that helps?" ]
                        , D.toSimpleNote <|
                            "I can get stuck when I run into keywords, operators, parentheses, or brackets"
                                ++ " unexpectedly. So there may be some earlier syntax trouble (like extra parenthesis"
                                ++ " or missing brackets) that is confusing me."
                        ]
                    )

        TupleOperatorClose row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED OPERATOR FUNCTION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow "I was expecting a closing parenthesis here:"
                    , D.stack <|
                        [ D.fillSep [ D.reflow "Try adding a", D.dullyellowS ")", D.reflow "to see if that helps!" ]
                        , D.toSimpleNote <|
                            "I think I am parsing an operator function right now, so I am expecting to see"
                                ++ " something like (+) or (&&) where an operator is surrounded by parentheses with"
                                ++ " no extra spaces."
                        ]
                    )

        TupleOperatorReserved operator row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNEXPECTED SYMBOL" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I ran into an unexpected symbol here:"
                    , D.fillSep <|
                        case operator of
                            Symbol.BadDot ->
                                [ D.reflow "Maybe you wanted a record accessor like", D.dullyellowS ".x", D.fromChars "or", D.dullyellowS ".name", D.fromChars "instead?" ]

                            Symbol.BadPipe ->
                                [ D.fromChars "Try", D.dullyellowS "(||)", D.reflow "instead? To turn boolean OR into a function?" ]

                            Symbol.BadArrow ->
                                [ D.reflow "Maybe you wanted", D.dullyellowS "(>)", D.fromChars "or", D.dullyellowS "(>=)", D.fromChars "instead?" ]

                            Symbol.BadEquals ->
                                [ D.fromChars "Try", D.dullyellowS "(==)", D.reflow "instead? To make a function that checks equality?" ]

                            Symbol.BadHasType ->
                                [ D.fromChars "Try", D.dullyellowS "(::)", D.reflow "instead? To add values to the front of lists?" ]
                    )

        TupleIndentExpr1 row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw an open parenthesis, so I was expecting to see an expression next."
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "Something like"
                            , D.dullyellowS "(4 + 5)"
                            , D.fromChars "or"
                            , D.hcat [ D.dullyellowS "(String.reverse \"desserts\")", D.fromChars "." ]
                            , D.reflow "Anything where you are putting parentheses around normal expressions."
                            ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation in cases like this, so"
                                ++ " maybe you have an expression but it is not indented enough?"
                        ]
                    )

        TupleIndentExprN row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED TUPLE" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I think I am in the middle of parsing a tuple. I just saw a comma, so I was expecting to see an expression next."
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "A tuple looks like"
                            , D.dullyellowS "(3,4)"
                            , D.fromChars "or"
                            , D.hcat [ D.dullyellowS "(\"Tom\",42)", D.fromChars "," ]
                            , D.reflow "so I think there is an expression missing here?"
                            ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation in cases like this, so"
                                ++ " maybe you have an expression but it is not indented enough?"
                        ]
                    )

        TupleIndentEnd row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting to see a closing parenthesis next:"
                    , D.stack
                        [ D.fillSep [ D.reflow "Try adding a", D.dullyellowS ")", D.reflow "to see if that helps!" ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation in cases like this, so"
                                ++ " maybe you have a closing parenthesis but it is not indented enough?"
                        ]
                    )


toListReport : Code.Source -> Context -> TList -> P.Row -> P.Col -> Report.Report
toListReport source context list startRow startCol =
    case list of
        ListSpace space row col ->
            toSpaceReport source space row col

        ListOpen row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a list, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.reflow "I was expecting to see a closing square bracket before this,"
                            , D.reflow "so try adding a"
                            , D.dullyellowS "]"
                            , D.reflow "and see if that helps?"
                            ]
                        , D.toSimpleNote <|
                            "When I get stuck like this, it usually means that there is a missing parenthesis"
                                ++ " or bracket somewhere earlier. It could also be a stray keyword or operator."
                        ]
                    )

        ListExpr expr row col ->
            case expr of
                Start r c ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position r c)

                        region =
                            toRegion r c
                    in
                    Report.Report "UNFINISHED LIST" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was expecting to see another list entry after that last comma:"
                            , D.stack
                                [ D.reflow <|
                                    "Trailing commas are not allowed in lists, so the fix may be to delete the comma?"
                                , D.toSimpleNote
                                    "I recommend using the following format for lists that span multiple lines:"
                                , D.indent 4 <|
                                    D.vcat <|
                                        [ D.hcat [ D.fromChars "[ ", D.dullyellowS "\"Alice\"" ]
                                        , D.hcat [ D.fromChars ", ", D.dullyellowS "\"Bob\"" ]
                                        , D.hcat [ D.fromChars ", ", D.dullyellowS "\"Chuck\"" ]
                                        , D.fromChars "]"
                                        ]
                                , D.reflow <|
                                    "Notice that each line starts with some indentation. Usually two or four spaces."
                                        ++ " This is the stylistic convention in the Elm ecosystem."
                                ]
                            )

                _ ->
                    toExprReport source (InNode NList startRow startCol context) expr row col

        ListEnd row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a list, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.reflow "I was expecting to see a closing square bracket before this,"
                            , D.reflow "so try adding a"
                            , D.dullyellowS "]"
                            , D.reflow "and see if that helps?"
                            ]
                        , D.toSimpleNote <|
                            "When I get stuck like this, it usually means that there is a missing parenthesis"
                                ++ " or bracket somewhere earlier. It could also be a stray keyword or operator."
                        ]
                    )

        ListIndentOpen row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I cannot find the end of this list:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "You could change it to something like"
                            , D.dullyellowS "[3,4,5]"
                            , D.reflow "or even just"
                            , D.hcat [ D.dullyellowS "[]", D.fromChars "." ]
                            , D.reflow "Anything where there is an open and close square brace,"
                            , D.reflow "and where the elements of the list are separated by commas."
                            ]
                        , D.toSimpleNote <|
                            "I may be confused by indentation. For example, if you are trying to define"
                                ++ " a list across multiple lines, I recommend using this format:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.hcat [ D.fromChars "[ ", D.dullyellowS "\"Alice\"" ]
                                , D.hcat [ D.fromChars ", ", D.dullyellowS "\"Bob\"" ]
                                , D.hcat [ D.fromChars ", ", D.dullyellowS "\"Chuck\"" ]
                                , D.fromChars "]"
                                ]
                        , D.reflow <|
                            "Notice that each line starts with some indentation. Usually two or four spaces."
                                ++ " This is the stylistic convention in the Elm ecosystem."
                        ]
                    )

        ListIndentEnd row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I cannot find the end of this list:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "You can just add a closing"
                            , D.dullyellowS "]"
                            , D.reflow "right here, and I will be all set!"
                            ]
                        , D.toSimpleNote <|
                            "I may be confused by indentation. For example, if you are trying to define"
                                ++ " a list across multiple lines, I recommend using this format:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.hcat [ D.fromChars "[ ", D.dullyellowS "\"Alice\"" ]
                                , D.hcat [ D.fromChars ", ", D.dullyellowS "\"Bob\"" ]
                                , D.hcat [ D.fromChars ", ", D.dullyellowS "\"Chuck\"" ]
                                , D.fromChars "]"
                                ]
                        , D.reflow <|
                            "Notice that each line starts with some indentation. Usually two or four spaces."
                                ++ " This is the stylistic convention in the Elm ecosystem."
                        ]
                    )

        ListIndentExpr row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting to see another list entry after this comma:"
                    , D.stack
                        [ D.reflow <|
                            "Trailing commas are not allowed in lists, so the fix may be to delete the comma?"
                        , D.toSimpleNote
                            "I recommend using the following format for lists that span multiple lines:"
                        , D.indent 4 <|
                            D.vcat <|
                                [ D.hcat [ D.fromChars "[ ", D.dullyellowS "\"Alice\"" ]
                                , D.hcat [ D.fromChars ", ", D.dullyellowS "\"Bob\"" ]
                                , D.hcat [ D.fromChars ", ", D.dullyellowS "\"Chuck\"" ]
                                , D.fromChars "]"
                                ]
                        , D.reflow <|
                            "Notice that each line starts with some indentation. Usually two or four spaces."
                                ++ " This is the stylistic convention in the Elm ecosystem."
                        ]
                    )


toFuncReport : Code.Source -> Context -> Func -> P.Row -> P.Col -> Report.Report
toFuncReport source context func startRow startCol =
    case func of
        FuncSpace space row col ->
            toSpaceReport source space row col

        FuncArg pattern row col ->
            toPatternReport source PArg pattern row col

        FuncBody expr row col ->
            toExprReport source (InNode NFunc startRow startCol context) expr row col

        FuncArrow row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was parsing an anonymous function, but I got stuck here:"
                            , D.reflow <|
                                "It looks like you are trying to use `"
                                    ++ keyword
                                    ++ "` as an argument, but"
                                    ++ " it is a reserved word in this language. Try using a different argument name!"
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED ANONYMOUS FUNCTION" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I just saw the beginning of an anonymous function, so I was expecting to see an arrow next:"
                            , D.fillSep <|
                                [ D.reflow "The syntax for anonymous functions is"
                                , D.dullyellowS "(\\x -> x + 1)"
                                , D.reflow "so I am missing the arrow and the body of the function."
                                ]
                            )

        FuncIndentArg row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "MISSING ARGUMENT" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw the beginning of an anonymous function, so I was expecting to see an argument next:"
                    , D.stack
                        [ D.fillSep
                            [ D.reflow "Something like"
                            , D.dullyellowS "x"
                            , D.fromChars "or"
                            , D.hcat [ D.dullyellowS "name", D.fromChars "." ]
                            , D.reflow "Anything that starts with a lower case letter!"
                            ]
                        , D.toSimpleNote <|
                            "The syntax for anonymous functions is (\\x -> x + 1) where the backslash"
                                ++ " is meant to look a bit like a lambda if you squint. This visual pun seemed"
                                ++ " like a better idea at the time!"
                        ]
                    )

        FuncIndentArrow row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED ANONYMOUS FUNCTION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw the beginning of an anonymous function, so I was expecting to see an arrow next:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "The syntax for anonymous functions is"
                            , D.dullyellowS "(\\x -> x + 1)"
                            , D.reflow "so I am missing the arrow and the body of the function."
                            ]
                        , D.toSimpleNote <|
                            "It is possible that I am confused about indetation! I generally recommend"
                                ++ " switching to named functions if the definition cannot fit inline nicely, so"
                                ++ " either (1) try to fit the whole anonymous function on one line or (2) break"
                                ++ " the whole thing out into a named function. Things tend to be clearer that way!"
                        ]
                    )

        FuncIndentBody row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED ANONYMOUS FUNCTION" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting to see the body of your anonymous function next:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "The syntax for anonymous functions is"
                            , D.dullyellowS "(\\x -> x + 1)"
                            , D.reflow "so I am missing all the stuff after the arrow!"
                            ]
                        , D.toSimpleNote <|
                            "It is possible that I am confused about indetation! I generally recommend"
                                ++ " switching to named functions if the definition cannot fit inline nicely, so"
                                ++ " either (1) try to fit the whole anonymous function on one line or (2) break"
                                ++ " the whole thing out into a named function. Things tend to be clearer that way!"
                        ]
                    )



-- PATTERN


type PContext
    = PCase
    | PArg
    | PLet


toPatternReport : Code.Source -> PContext -> Pattern -> P.Row -> P.Col -> Report.Report
toPatternReport source context pattern startRow startCol =
    case pattern of
        PRecord record row col ->
            toPRecordReport source record row col

        PTuple tuple row col ->
            toPTupleReport source context tuple row col

        PList list row col ->
            toPListReport source context list row col

        PStart row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword

                        inThisThing =
                            case context of
                                PArg ->
                                    "as an argument"

                                PCase ->
                                    "in this pattern"

                                PLet ->
                                    "in this pattern"
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "It looks like you are trying to use `"
                                    ++ keyword
                                    ++ "` "
                                    ++ inThisThing
                                    ++ ":"
                            , D.reflow <|
                                "This is a reserved word! Try using some other name?"
                            )

                Code.Operator "-" ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "UNEXPECTED SYMBOL" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I ran into a minus sign unexpectedly in this pattern:"
                            , D.reflow <|
                                "It is not possible to pattern match on negative numbers at this"
                                    ++ " time. Try using an `if` expression for that sort of thing for now."
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN PATTERN" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I wanted to parse a pattern next, but I got stuck here:"
                            , D.fillSep <|
                                [ D.reflow "I am not sure why I am getting stuck exactly."
                                , D.reflow "I just know that I want a pattern next."
                                , D.reflow "Something as simple as"
                                , D.dullyellowS "maybeHeight"
                                , D.fromChars "or"
                                , D.dullyellowS "result"
                                , D.reflow "would work!"
                                ]
                            )

        PChar char row col ->
            toCharReport source char row col

        PString string row col ->
            toStringReport source string row col

        PNumber number row col ->
            toNumberReport source number row col

        PFloat width row col ->
            let
                region =
                    toWiderRegion row col width
            in
            Report.Report "UNEXPECTED PATTERN" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I cannot pattern match with floating point numbers:"
                    , D.fillSep <|
                        [ D.reflow "Equality on floats can be unreliable, so you usually want"
                        , D.reflow "to check that they are nearby with some sort of"
                        , D.dullyellowS "(abs (actual - expected) < 0.001)"
                        , D.fromChars "check."
                        ]
                    )

        PAlias row col ->
            let
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PATTERN" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was expecting to see a variable name after the `as` keyword:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "The `as` keyword lets you write patterns like"
                            , D.hcat [ D.fromChars "((", D.dullyellowS "x", D.fromChars ",", D.dullyellowS "y", D.fromChars ") ", D.cyanS "as", D.dullyellowS " point", D.fromChars ")" ]
                            , D.reflow "so you can refer to individual parts of the tuple with"
                            , D.dullyellowS "x"
                            , D.fromChars "and"
                            , D.dullyellowS "y"
                            , D.reflow "or you refer to the whole"
                            , D.reflow "thing with"
                            , D.hcat [ D.dullyellowS "point", D.fromChars "." ]
                            ]
                        , D.reflow <|
                            "So I was expecting to see a variable name after the `as` keyword here. Sometimes"
                                ++ " people just want to use `as` as a variable name though. Try using a different name"
                                ++ " in that case!"
                        ]
                    )

        PWildcardNotVar name width row col ->
            let
                region =
                    toWiderRegion row col width

                examples =
                    case String.uncons (SE.dropWhile ((==) '_') name) of
                        Nothing ->
                            [ D.dullyellowS "x", D.fromChars "or", D.dullyellowS "age" ]

                        Just ( c, cs ) ->
                            [ D.dullyellowS (String.cons (Char.toLower c) cs) ]
            in
            Report.Report "UNEXPECTED NAME" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "Variable names cannot start with underscores like this:"
                    , D.fillSep <|
                        [ D.reflow "You can either have an underscore like"
                        , D.dullyellowS "_"
                        , D.fromChars "to"
                        , D.reflow "ignore the value, or you can have a name like"
                        ]
                            ++ examples
                            ++ [ D.reflow "to use the matched value." ]
                    )

        PSpace space row col ->
            toSpaceReport source space row col

        PIndentStart row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PATTERN" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I wanted to parse a pattern next, but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I am not sure why I am getting stuck exactly."
                            , D.reflow "I just know that I want a pattern next."
                            , D.reflow "Something as simple as"
                            , D.dullyellowS "maybeHeight"
                            , D.fromChars "or"
                            , D.dullyellowS "result"
                            , D.reflow "would work!"
                            ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation. If you think there is a pattern next, maybe"
                                ++ " it needs to be indented a bit more?"
                        ]
                    )

        PIndentAlias row col ->
            let
                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PATTERN" region <|
                Code.toSnippet source region Nothing <|
                    ( D.reflow <|
                        "I was expecting to see a variable name after the `as` keyword:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "The `as` keyword lets you write patterns like"
                            , D.hcat [ D.fromChars "((", D.dullyellowS "x", D.fromChars ",", D.dullyellowS "y", D.fromChars ") ", D.cyanS "as", D.dullyellowS " point", D.fromChars ")" ]
                            , D.reflow "so you can refer to individual parts of the tuple with"
                            , D.dullyellowS "x"
                            , D.fromChars "and"
                            , D.dullyellowS "y"
                            , D.reflow "or you refer to the whole"
                            , D.reflow "thing with"
                            , D.dullyellowS "point."
                            ]
                        , D.reflow <|
                            "So I was expecting to see a variable name after the `as` keyword here. Sometimes"
                                ++ " people just want to use `as` as a variable name though. Try using a different name"
                                ++ " in that case!"
                        ]
                    )


toPRecordReport : Code.Source -> PRecord -> P.Row -> P.Col -> Report.Report
toPRecordReport source record startRow startCol =
    case record of
        PRecordOpen row col ->
            toUnfinishRecordPatternReport source row col startRow startCol <|
                D.reflow "I was expecting to see a field name next."

        PRecordEnd row col ->
            toUnfinishRecordPatternReport source row col startRow startCol <|
                D.fillSep
                    [ D.reflow "I was expecting to see a closing curly brace next."
                    , D.reflow "Try adding a"
                    , D.dullyellowS "}"
                    , D.fromChars "here?"
                    ]

        PRecordField row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was not expecting to see `"
                                    ++ keyword
                                    ++ "` as a record field name:"
                            , D.reflow <|
                                "This is a reserved word, not available for variable names. Try another name!"
                            )

                _ ->
                    toUnfinishRecordPatternReport source row col startRow startCol <|
                        D.reflow "I was expecting to see a field name next."

        PRecordSpace space row col ->
            toSpaceReport source space row col

        PRecordIndentOpen row col ->
            toUnfinishRecordPatternReport source row col startRow startCol <|
                D.reflow "I was expecting to see a field name next."

        PRecordIndentEnd row col ->
            toUnfinishRecordPatternReport source row col startRow startCol <|
                D.fillSep
                    [ D.reflow "I was expecting to see a closing curly brace next."
                    , D.reflow "Try adding a"
                    , D.dullyellowS "}"
                    , D.fromChars "here?"
                    ]

        PRecordIndentField row col ->
            toUnfinishRecordPatternReport source row col startRow startCol <|
                D.reflow "I was expecting to see a field name next."


toUnfinishRecordPatternReport : Code.Source -> P.Row -> P.Col -> P.Row -> P.Col -> D.Doc -> Report.Report
toUnfinishRecordPatternReport source row col startRow startCol message =
    let
        surroundings =
            A.Region (A.Position startRow startCol) (A.Position row col)

        region =
            toRegion row col
    in
    Report.Report "UNFINISHED RECORD PATTERN" region <|
        Code.toSnippet source surroundings (Just region) <|
            ( D.reflow <|
                "I was partway through parsing a record pattern, but I got stuck here:"
            , D.stack
                [ message
                , D.toFancyHint <|
                    [ D.reflow "A record pattern looks like"
                    , D.dullyellowS "{x,y}"
                    , D.fromChars "or"
                    , D.dullyellowS "{name,age}"
                    , D.reflow "where you list the field names you want to access."
                    ]
                ]
            )


toPTupleReport : Code.Source -> PContext -> PTuple -> P.Row -> P.Col -> Report.Report
toPTupleReport source context tuple startRow startCol =
    case tuple of
        PTupleOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "It looks like you are trying to use `"
                                    ++ keyword
                                    ++ "` as a variable name:"
                            , D.reflow <|
                                "This is a reserved word! Try using some other name?"
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED PARENTHESES" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I just saw an open parenthesis, but I got stuck here:"
                            , D.fillSep
                                [ D.reflow "I was expecting to see a pattern next."
                                , D.reflow "Maybe it will end up being something"
                                , D.fromChars "like"
                                , D.dullyellowS "(x,y)"
                                , D.fromChars "or"
                                , D.hcat [ D.dullyellowS "(name, _)", D.fromChars "?" ]
                                ]
                            )

        PTupleEnd row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I ran into a reserved word in this pattern:"
                            , D.reflow <|
                                "The `"
                                    ++ keyword
                                    ++ "` keyword is reserved. Try using a different name instead!"
                            )

                Code.Operator op ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col op
                    in
                    Report.Report "UNEXPECTED SYMBOL" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I ran into the "
                                    ++ op
                                    ++ " symbol unexpectedly in this pattern:"
                            , D.reflow <|
                                "Only the :: symbol that works in patterns. It is useful if you"
                                    ++ " are pattern matching on lists, trying to get the first element"
                                    ++ " off the front. Did you want that instead?"
                            )

                Code.Close term bracket ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report ("STRAY " ++ String.map Char.toUpper term) region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I ran into a an unexpected "
                                    ++ term
                                    ++ " in this pattern:"
                            , D.reflow <|
                                "This "
                                    ++ String.cons bracket " does not match up with an earlier open "
                                    ++ term
                                    ++ ". Try deleting it?"
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED PARENTHESES" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was partway through parsing a pattern, but I got stuck here:"
                            , D.fillSep
                                [ D.reflow "I was expecting a closing parenthesis next, so"
                                , D.reflow "try adding a"
                                , D.dullyellowS ")"
                                , D.reflow "to see if that helps?"
                                ]
                            )

        PTupleExpr pattern row col ->
            toPatternReport source context pattern row col

        PTupleSpace space row col ->
            toSpaceReport source space row col

        PTupleIndentEnd row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting a closing parenthesis next:"
                    , D.stack
                        [ D.fillSep [ D.reflow "Try adding a", D.dullyellowS ")", D.reflow "to see if that helps?" ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation in cases like this, so"
                                ++ " maybe you have a closing parenthesis but it is not indented enough?"
                        ]
                    )

        PTupleIndentExpr1 row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw an open parenthesis, but then I got stuck here:"
                    , D.fillSep
                        [ D.reflow "I was expecting to see a pattern next."
                        , D.reflow "Maybe it will end up being something"
                        , D.fromChars "like"
                        , D.dullyellowS "(x,y)"
                        , D.fromChars "or"
                        , D.hcat [ D.dullyellowS "(name, _)", D.fromChars "?" ]
                        ]
                    )

        PTupleIndentExprN row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED TUPLE PATTERN" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a tuple pattern, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.reflow "I was expecting to see a pattern next."
                            , D.reflow "I am expecting the final result to be something"
                            , D.fromChars "like"
                            , D.dullyellowS "(x,y)"
                            , D.fromChars "or"
                            , D.hcat [ D.dullyellowS "(name, _)", D.fromChars "." ]
                            ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation in cases like this, so the problem"
                                ++ " may be that the next part is not indented enough?"
                        ]
                    )


toPListReport : Code.Source -> PContext -> PList -> P.Row -> P.Col -> Report.Report
toPListReport source context list startRow startCol =
    case list of
        PListOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "It looks like you are trying to use `"
                                    ++ keyword
                                    ++ "` to name an element of a list:"
                            , D.reflow <|
                                "This is a reserved word though! Try using some other name?"
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED LIST PATTERN" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I just saw an open square bracket, but then I got stuck here:"
                            , D.fillSep [ D.reflow "Try adding a", D.dullyellowS "]", D.reflow "to see if that helps?" ]
                            )

        PListEnd row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST PATTERN" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting a closing square bracket to end this list pattern:"
                    , D.fillSep [ D.reflow "Try adding a", D.dullyellowS "]", D.reflow "to see if that helps?" ]
                    )

        PListExpr pattern row col ->
            toPatternReport source context pattern row col

        PListSpace space row col ->
            toSpaceReport source space row col

        PListIndentOpen row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST PATTERN" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw an open square bracket, but then I got stuck here:"
                    , D.stack
                        [ D.fillSep [ D.reflow "Try adding a", D.dullyellowS "]", D.reflow "to see if that helps?" ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation in cases like this, so"
                                ++ " maybe there is something next, but it is not indented enough?"
                        ]
                    )

        PListIndentEnd row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST PATTERN" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting a closing square bracket to end this list pattern:"
                    , D.stack
                        [ D.fillSep [ D.reflow "Try adding a", D.dullyellowS "]", D.reflow "to see if that helps?" ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation in cases like this, so"
                                ++ " maybe you have a closing square bracket but it is not indented enough?"
                        ]
                    )

        PListIndentExpr row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED LIST PATTERN" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a list pattern, but I got stuck here:"
                    , D.stack
                        [ D.reflow <|
                            "I was expecting to see another pattern next. Maybe a variable name."
                        , D.toSimpleNote <|
                            "I can get confused by indentation in cases like this, so"
                                ++ " maybe there is more to this pattern but it is not indented enough?"
                        ]
                    )



-- TYPES


type TContext
    = TC_Annotation Name.Name
    | TC_CustomType
    | TC_TypeAlias
    | TC_Port


toTypeReport : Code.Source -> TContext -> Type -> P.Row -> P.Col -> Report.Report
toTypeReport source context tipe startRow startCol =
    case tipe of
        TRecord record row col ->
            toTRecordReport source context record row col

        TTuple tuple row col ->
            toTTupleReport source context tuple row col

        TStart row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was expecting to see a type next, but I got stuck on this reserved word:"
                            , D.reflow <|
                                "It looks like you are trying to use `"
                                    ++ keyword
                                    ++ "` as a type variable, but "
                                    ++ " it is a reserved word. Try using a different name!"
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col

                        thing =
                            case context of
                                TC_Annotation _ ->
                                    "type annotation"

                                TC_CustomType ->
                                    "custom type"

                                TC_TypeAlias ->
                                    "type alias"

                                TC_Port ->
                                    "port"

                        something =
                            case context of
                                TC_Annotation name ->
                                    "the `" ++ name ++ "` type annotation"

                                TC_CustomType ->
                                    "a custom type"

                                TC_TypeAlias ->
                                    "a type alias"

                                TC_Port ->
                                    "a port"
                    in
                    Report.Report ("PROBLEM IN " ++ String.map Char.toUpper thing) region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was partway through parsing "
                                    ++ something
                                    ++ ", but I got stuck here:"
                            , D.fillSep <|
                                [ D.reflow "I was expecting to see a type next."
                                , D.reflow "Try putting"
                                , D.dullyellowS "Int"
                                , D.fromChars "or"
                                , D.dullyellowS "String"
                                , D.fromChars "for"
                                , D.fromChars "now?"
                                ]
                            )

        TSpace space row col ->
            toSpaceReport source space row col

        TIndentStart row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col

                thing =
                    case context of
                        TC_Annotation _ ->
                            "type annotation"

                        TC_CustomType ->
                            "custom type"

                        TC_TypeAlias ->
                            "type alias"

                        TC_Port ->
                            "port"
            in
            Report.Report ("UNFINISHED " ++ String.map Char.toUpper thing) region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was partway through parsing a "
                            ++ thing
                            ++ ", but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I was expecting to see a type next."
                            , D.reflow "Try putting"
                            , D.dullyellowS "Int"
                            , D.fromChars "or"
                            , D.dullyellowS "String"
                            , D.fromChars "for"
                            , D.fromChars "now?"
                            ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation. If you think there is already a type"
                                ++ " next, maybe it is not indented enough?"
                        ]
                    )


toTRecordReport : Code.Source -> TContext -> TRecord -> P.Row -> P.Col -> Report.Report
toTRecordReport source context record startRow startCol =
    case record of
        TRecordOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I just started parsing a record type, but I got stuck on this field name:"
                            , D.reflow <|
                                "It looks like you are trying to use `"
                                    ++ keyword
                                    ++ "` as a field name, but"
                                    ++ " that is a reserved word. Try using a different name!"
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED RECORD TYPE" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I just started parsing a record type, but I got stuck here:"
                            , D.fillSep
                                [ D.reflow "Record types look like"
                                , D.dullyellowS "{ name : String, age : Int },"
                                , D.reflow "so I was expecting to see a field name next."
                                ]
                            )

        TRecordEnd row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD TYPE" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a record type, but I got stuck here:"
                    , D.stack
                        [ D.fillSep
                            [ D.reflow "I was expecting to see a closing curly brace before this,"
                            , D.reflow "so try adding a"
                            , D.dullyellowS "}"
                            , D.reflow "and see if that helps?"
                            ]
                        , D.toSimpleNote <|
                            "When I get stuck like this, it usually means that there is a missing parenthesis"
                                ++ " or bracket somewhere earlier. It could also be a stray keyword or operator."
                        ]
                    )

        TRecordField row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I am partway through parsing a record type, but I got stuck on this field name:"
                            , D.reflow <|
                                "It looks like you are trying to use `"
                                    ++ keyword
                                    ++ "` as a field name, but"
                                    ++ " that is a reserved word. Try using a different name!"
                            )

                Code.Other (Just ',') ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "EXTRA COMMA" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I am partway through parsing a record type, but I got stuck here:"
                            , D.stack
                                [ D.reflow <|
                                    "I am seeing two commas in a row. This is the second one!"
                                , D.reflow <|
                                    "Just delete one of the commas and you should be all set!"
                                , noteForRecordTypeError
                                ]
                            )

                Code.Close _ '}' ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "EXTRA COMMA" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I am partway through parsing a record type, but I got stuck here:"
                            , D.stack
                                [ D.reflow <|
                                    "Trailing commas are not allowed in record types. Try deleting the comma that"
                                        ++ " appears before this closing curly brace."
                                , noteForRecordTypeError
                                ]
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "PROBLEM IN RECORD TYPE" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I am partway through parsing a record type, but I got stuck here:"
                            , D.stack
                                [ D.fillSep <|
                                    [ D.reflow "I was expecting to see another record field defined next,"
                                    , D.reflow "so I am looking for a name like"
                                    , D.dullyellowS "userName"
                                    , D.fromChars "or"
                                    , D.hcat [ D.dullyellowS "plantHeight", D.fromChars "." ]
                                    ]
                                , noteForRecordTypeError
                                ]
                            )

        TRecordColon row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD TYPE" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a record type, but I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I just saw a field name, so I was expecting to see"
                            , D.reflow "a colon next. So try putting an"
                            , D.greenS ":"
                            , D.reflow "sign here?"
                            ]
                        , noteForRecordTypeError
                        ]
                    )

        TRecordType tipe row col ->
            toTypeReport source context tipe row col

        TRecordSpace space row col ->
            toSpaceReport source space row col

        TRecordIndentOpen row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD TYPE" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw the opening curly brace of a record type, but then I got stuck here:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "I am expecting a record like"
                            , D.dullyellowS "{ name : String, age : Int }"
                            , D.fromChars "here."
                            , D.reflow "Try defining some fields of your own?"
                            ]
                        , noteForRecordTypeIndentError
                        ]
                    )

        TRecordIndentEnd row col ->
            case Code.nextLineStartsWithCloseCurly source row of
                Just ( curlyRow, curlyCol ) ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position curlyRow curlyCol)

                        region =
                            toRegion curlyRow curlyCol
                    in
                    Report.Report "NEED MORE INDENTATION" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was partway through parsing a record type, but I got stuck here:"
                            , D.stack
                                [ D.reflow <|
                                    "I need this curly brace to be indented more. Try adding some spaces before it!"
                                , noteForRecordTypeError
                                ]
                            )

                Nothing ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED RECORD TYPE" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I was partway through parsing a record type, but I got stuck here:"
                            , D.stack
                                [ D.fillSep <|
                                    [ D.reflow "I was expecting to see a closing curly brace next."
                                    , D.reflow "Try putting a"
                                    , D.greenS "}"
                                    , D.reflow "next and see if that helps?"
                                    ]
                                , noteForRecordTypeIndentError
                                ]
                            )

        TRecordIndentField row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD TYPE" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a record type, but I got stuck after that last comma:"
                    , D.stack
                        [ D.reflow <|
                            "Trailing commas are not allowed in record types, so the fix may be to"
                                ++ " delete that last comma? Or maybe you were in the middle of defining"
                                ++ " an additional field?"
                        , noteForRecordTypeIndentError
                        ]
                    )

        TRecordIndentColon row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD TYPE" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a record type. I just saw a record"
                            ++ " field, so I was expecting to see a colon next:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "Try putting an"
                            , D.greenS ":"
                            , D.reflow "followed by a type?"
                            ]
                        , noteForRecordTypeIndentError
                        ]
                    )

        TRecordIndentType row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED RECORD TYPE" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I am partway through parsing a record type, and I was expecting to run into a type next:"
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "Try putting something like"
                            , D.dullyellowS "Int"
                            , D.fromChars "or"
                            , D.dullyellowS "String"
                            , D.reflow "for now?"
                            ]
                        , noteForRecordTypeIndentError
                        ]
                    )


noteForRecordTypeError : D.Doc
noteForRecordTypeError =
    D.stack <|
        [ D.toSimpleNote
            "If you are trying to define a record type across multiple lines, I recommend using this format:"
        , D.indent 4 <|
            D.vcat <|
                [ D.fromChars "{ name : String"
                , D.fromChars ", age : Int"
                , D.fromChars ", height : Float"
                , D.fromChars "}"
                ]
        , D.reflow <|
            "Notice that each line starts with some indentation. Usually two or four spaces."
                ++ " This is the stylistic convention in the Elm ecosystem."
        ]


noteForRecordTypeIndentError : D.Doc
noteForRecordTypeIndentError =
    D.stack <|
        [ D.toSimpleNote <|
            "I may be confused by indentation. For example, if you are trying to define"
                ++ " a record type across multiple lines, I recommend using this format:"
        , D.indent 4 <|
            D.vcat <|
                [ D.fromChars "{ name : String"
                , D.fromChars ", age : Int"
                , D.fromChars ", height : Float"
                , D.fromChars "}"
                ]
        , D.reflow <|
            "Notice that each line starts with some indentation. Usually two or four spaces."
                ++ " This is the stylistic convention in the Elm ecosystem."
        ]


toTTupleReport : Code.Source -> TContext -> TTuple -> P.Row -> P.Col -> Report.Report
toTTupleReport source context tuple startRow startCol =
    case tuple of
        TTupleOpen row col ->
            case Code.whatIsNext source row col of
                Code.Keyword keyword ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toKeywordRegion row col keyword
                    in
                    Report.Report "RESERVED WORD" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I ran into a reserved word unexpectedly:"
                            , D.reflow <|
                                "It looks like you are trying to use `"
                                    ++ keyword
                                    ++ "` as a variable name, but "
                                    ++ " it is a reserved word. Try using a different name!"
                            )

                _ ->
                    let
                        surroundings =
                            A.Region (A.Position startRow startCol) (A.Position row col)

                        region =
                            toRegion row col
                    in
                    Report.Report "UNFINISHED PARENTHESES" region <|
                        Code.toSnippet source surroundings (Just region) <|
                            ( D.reflow <|
                                "I just saw an open parenthesis, so I was expecting to see a type next."
                            , D.fillSep <|
                                [ D.reflow "Something like"
                                , D.dullyellowS "(Maybe Int)"
                                , D.fromChars "or"
                                , D.hcat [ D.dullyellowS "(List Person)", D.fromChars "." ]
                                , D.reflow "Anything where you are putting parentheses around normal types."
                                ]
                            )

        TTupleEnd row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting to see a closing parenthesis next, but I got stuck here:"
                    , D.stack
                        [ D.fillSep [ D.reflow "Try adding a", D.dullyellowS ")", D.reflow "to see if that helps?" ]
                        , D.toSimpleNote <|
                            "I can get stuck when I run into keywords, operators, parentheses, or brackets"
                                ++ " unexpectedly. So there may be some earlier syntax trouble (like extra parenthesis"
                                ++ " or missing brackets) that is confusing me."
                        ]
                    )

        TTupleType tipe row col ->
            toTypeReport source context tipe row col

        TTupleSpace space row col ->
            toSpaceReport source space row col

        TTupleIndentType1 row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I just saw an open parenthesis, so I was expecting to see a type next."
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "Something like"
                            , D.dullyellowS "(Maybe Int)"
                            , D.fromChars "or"
                            , D.hcat [ D.dullyellowS "(List Person)", D.fromChars "." ]
                            , D.reflow "Anything where you are putting parentheses around normal types."
                            ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation in cases like this, so"
                                ++ " maybe you have a type but it is not indented enough?"
                        ]
                    )

        TTupleIndentTypeN row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED TUPLE TYPE" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I think I am in the middle of parsing a tuple type. I just saw a comma, so I was expecting to see a type next."
                    , D.stack
                        [ D.fillSep <|
                            [ D.reflow "A tuple type looks like"
                            , D.dullyellowS "(Float,Float)"
                            , D.fromChars "or"
                            , D.hcat [ D.dullyellowS "(String,Int)", D.fromChars "," ]
                            , D.reflow "so I think there is a type missing here?"
                            ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation in cases like this, so"
                                ++ " maybe you have an expression but it is not indented enough?"
                        ]
                    )

        TTupleIndentEnd row col ->
            let
                surroundings =
                    A.Region (A.Position startRow startCol) (A.Position row col)

                region =
                    toRegion row col
            in
            Report.Report "UNFINISHED PARENTHESES" region <|
                Code.toSnippet source surroundings (Just region) <|
                    ( D.reflow <|
                        "I was expecting to see a closing parenthesis next:"
                    , D.stack <|
                        [ D.fillSep [ D.reflow "Try adding a", D.dullyellowS ")", D.reflow "to see if that helps!" ]
                        , D.toSimpleNote <|
                            "I can get confused by indentation in cases like this, so"
                                ++ " maybe you have a closing parenthesis but it is not indented enough?"
                        ]
                    )
