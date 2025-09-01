{- MANUALLY FORMATTED -}
module Builder.Reporting.Exit exposing
  ( Init(..), initToReport
  --, Diff(..), diffToReport
  , Make(..), makeToReport
  --, Bump(..), bumpToReport
  , Repl(..), replToReport
  --, Publish(..), publishToReport
  , Install(..), installToReport
  , Reactor(..), reactorToReport
  --, newPackageOverview
  --
  , Solver(..)
  , Outline(..)
  , OutlineProblem(..)
  , Details(..), toDetailsReport
  , DetailsBadDep(..)
  , PackageProblem(..)
  , RegistryProblem(..), toRegistryProblemReport
  , BuildProblem(..), toBuildProblemReport
  , BuildProjectProblem(..)
  --, DocsProblem(..)
  , Generate(..)
  ----
  --, toString
  --, toStderr
  , toClient -- toJson
  )


import Builder.File as File
import Builder.Http as Http
import Builder.Reporting.Exit.Help as Help
import Compiler.Data.Name as N
import Compiler.Data.NonEmptyList as NE
import Compiler.Elm.Constraint as C
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Json.Decode as Decode
import Compiler.Json.String as JS
import Compiler.Parse.Primitives exposing (Row, Col)
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D exposing (d, da)
import Compiler.Reporting.Error as Error
import Compiler.Reporting.Error.Import as Import
import Compiler.Reporting.Error.Json as Json
import Compiler.Reporting.Render.Code as Code
import Elm.Error as Client
import Extra.System.Dir as Dir exposing (FilePath)
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Http as SysHttp



-- RENDERERS


toClient : Help.Report -> Client.Error
toClient report =
  Help.reportToClient report



-- INIT (line 85)


type Init
  = InitNoSolution (TList Pkg.Name)
  | InitNoOfflineSolution (TList Pkg.Name)
  | InitSolverProblem Solver
  | InitAlreadyExists
  | InitRegistryProblem RegistryProblem


initToReport : Init -> Help.Report
initToReport exit =
  case exit of
    InitNoSolution pkgs ->
      Help.report "NO SOLUTION" Nothing
        "I tried to create an elm.json with the following direct dependencies:"
        [ D.indent 4 <| D.vcat <|
            MList.map (D.dullyellow << D.fromChars << Pkg.toChars) pkgs
        , D.reflow <|
            "I could not find compatible versions though! This should not happen, so please"
            ++ " ask around one of the community forums at https://elm-lang.org/community to learn"
            ++ " what is going on!"
        ]

    InitNoOfflineSolution pkgs ->
      Help.report "NO OFFLINE SOLUTION" Nothing
        "I tried to create an elm.json with the following direct dependencies:"
        [ D.indent 4 <| D.vcat <|
            MList.map (D.dullyellow << D.fromChars << Pkg.toChars) pkgs
        , D.reflow <|
            "I could not find compatible versions though, but that may be because I could not"
            ++ " connect to https://package.elm-lang.org to get the latest list of packages. Are"
            ++ " you able to connect to the internet? Please ask around one of the community"
            ++ " forums at https://elm-lang.org/community for help!"
        ]

    InitSolverProblem solver ->
      toSolverReport solver

    InitAlreadyExists ->
      Help.report "EXISTING PROJECT" Nothing
        "You already have an elm.json file, so there is nothing for me to initialize!"
        [ D.fillSep
            [d"Maybe",D.green (D.fromChars (D.makeLink "init")),d"can",d"help"
            ,d"you",d"figure",d"out",d"what",d"to",d"do",d"next?"
            ]
        ]

    InitRegistryProblem problem ->
      toRegistryProblemReport "PROBLEM LOADING PACKAGE LIST" problem <|
        "I need the list of published packages before I can start initializing projects"



-- SOLVER (line 899)


type Solver
  = SolverBadCacheData Pkg.Name V.Version
  | SolverBadHttpData Pkg.Name V.Version String
  | SolverBadHttp Pkg.Name V.Version Http.Error


toSolverReport : Solver -> Help.Report
toSolverReport problem =
  case problem of
    SolverBadCacheData pkg vsn ->
      Help.report "PROBLEM SOLVING PACKAGE CONSTRAINTS" Nothing
        (
          "I need the elm.json of " ++ Pkg.toChars pkg ++ " " ++ V.toChars vsn ++ " to"
          ++ " help me search for a set of compatible packages. I had it cached locally, but"
          ++ " it looks like the file was corrupted!"
        )
        [ D.reflow <|
            "I deleted the cached version, so the next run should download a fresh copy."
            ++ " Hopefully that will get you unstuck, but it will not resolve the root"
            ++ " problem if a 3rd party tool is modifing cached files for some reason."
        ]

    SolverBadHttpData pkg vsn url ->
      Help.report "PROBLEM SOLVING PACKAGE CONSTRAINTS" Nothing
        (
          "I need the elm.json of " ++ Pkg.toChars pkg ++ " " ++ V.toChars vsn ++ " to"
          ++ " help me search for a set of compatible packages, but I ran into corrupted"
          ++ " information from:"
        )
        [ D.indent 4 <| D.dullyellow <| D.fromChars url
        , D.reflow <|
            "Is something weird with your internet connection. We have gotten reports that"
            ++ " schools, businesses, airports, etc. sometimes intercept requests and add things"
            ++ " to the body or change its contents entirely. Could that be the problem?"
        ]

    SolverBadHttp pkg vsn httpError ->
      toHttpErrorReport "PROBLEM SOLVING PACKAGE CONSTRAINTS" httpError <|
        "I need the elm.json of " ++ Pkg.toChars pkg ++ " " ++ V.toChars vsn
        ++ " to help me search for a set of compatible packages"



-- INSTALL (line 735)


type Install
  = InstallBadOutline Outline
  | InstallBadRegistry RegistryProblem
  | InstallNoOnlineAppSolution Pkg.Name
  | InstallNoOfflineAppSolution Pkg.Name
  | InstallNoOnlinePkgSolution Pkg.Name
  | InstallNoOfflinePkgSolution Pkg.Name
  | InstallHadSolverTrouble Solver
  | InstallUnknownPackageOnline Pkg.Name (TList Pkg.Name)
  | InstallUnknownPackageOffline Pkg.Name (TList Pkg.Name)
  | InstallBadDetails Details


installToReport : Install -> Help.Report
installToReport exit =
  case exit of
    InstallBadOutline outline ->
      toOutlineReport outline

    InstallBadRegistry problem ->
      toRegistryProblemReport "PROBLEM LOADING PACKAGE LIST" problem <|
        "I need the list of published packages to figure out how to install things"

    InstallNoOnlineAppSolution pkg ->
      Help.report "CANNOT FIND COMPATIBLE VERSION" (Just (Dir.fromString "elm.json"))
        (
          "I cannot find a version of " ++ Pkg.toChars pkg ++ " that is compatible"
          ++ " with your existing dependencies."
        )
        [ D.reflow <|
            "I checked all the published versions. When that failed, I tried to find any"
            ++ " compatible combination of these packages, even if it meant changing all your"
            ++ " existing dependencies! That did not work either!"
        , D.reflow <|
            "This is most likely to happen when a package is not upgraded yet. Maybe a new"
            ++ " version of Elm came out recently? Maybe a common package was changed recently?"
            ++ " Maybe a better package came along, so there was no need to upgrade this one?"
            ++ " Try asking around https://elm-lang.org/community to learn what might be going on"
            ++ " with this package."
        , D.toSimpleNote <|
            "Whatever the case, please be kind to the relevant package authors! Having"
            ++ " friendly interactions with users is great motivation, and conversely, getting"
            ++ " berated by strangers on the internet sucks your soul dry. Furthermore, package"
            ++ " authors are humans with families, friends, jobs, vacations, responsibilities,"
            ++ " goals, etc. They face obstacles outside of their technical work you will never"
            ++ " know about, so please assume the best and try to be patient and supportive!"
        ]

    InstallNoOfflineAppSolution pkg ->
      Help.report "CANNOT FIND COMPATIBLE VERSION LOCALLY" (Just (Dir.fromString "elm.json"))
        (
          "I cannot find a version of " ++ Pkg.toChars pkg ++ " that is compatible"
          ++ " with your existing dependencies."
        )
        [ D.reflow <|
            "I was not able to connect to https://package.elm-lang.org/ though, so I was only"
            ++ " able to look through packages that you have downloaded in the past."
        , D.reflow <|
            "Try again later when you have internet!"
        ]

    InstallNoOnlinePkgSolution pkg ->
      Help.report "CANNOT FIND COMPATIBLE VERSION" (Just (Dir.fromString "elm.json"))
        (
          "I cannot find a version of " ++ Pkg.toChars pkg ++ " that is compatible"
          ++ " with your existing constraints."
        )
        [ D.reflow <|
            "With applications, I try to broaden the constraints to see if anything works,"
            ++ " but messing with package constraints is much more delicate business. E.g. making"
            ++ " your constraints stricter may make it harder for applications to find compatible"
            ++ " dependencies. So fixing something here may break it for a lot of other people!"
        , D.reflow <|
            "So I recommend making an application with the same dependencies as your package."
            ++ " See if there is a solution at all. From there it may be easier to figure out"
            ++ " how to proceed in a way that will disrupt your users as little as possible. And"
            ++ " the solution may be to help other package authors to get their packages updated,"
            ++ " or to drop a dependency entirely."
        ]

    InstallNoOfflinePkgSolution pkg ->
      Help.report "CANNOT FIND COMPATIBLE VERSION LOCALLY" (Just (Dir.fromString "elm.json"))
        (
          "I cannot find a version of " ++ Pkg.toChars pkg ++ " that is compatible"
          ++ " with your existing constraints."
        )
        [ D.reflow <|
            "I was not able to connect to https://package.elm-lang.org/ though, so I was only"
            ++ " able to look through packages that you have downloaded in the past."
        , D.reflow <|
            "Try again later when you have internet!"
        ]

    InstallHadSolverTrouble solver ->
      toSolverReport solver

    InstallUnknownPackageOnline pkg suggestions ->
      Help.docReport "UNKNOWN PACKAGE" Nothing
        (
          D.fillSep
            [d"I",d"cannot",d"find",d"a",d"package",d"named",da[D.red (D.fromPackage pkg), d"."]]
        )
        [ D.reflow <|
            "I looked through https://package.elm-lang.org for packages with similar names"
            ++ " and found these:"
        , D.indent 4 <| D.dullyellow <| D.vcat <| MList.map D.fromPackage suggestions
        , D.reflow <| "Maybe you want one of these instead?"
        ]

    InstallUnknownPackageOffline pkg suggestions ->
      Help.docReport "UNKNOWN PACKAGE" Nothing
        (
          D.fillSep
            [d"I",d"cannot",d"find",d"a",d"package",d"named",da[D.red (D.fromPackage pkg), d"."]]
        )
        [ D.reflow <|
            "I could not connect to https://package.elm-lang.org though, so new packages may"
            ++ " have been published since I last updated my local cache of package names."
        , D.reflow <|
            "Looking through the locally cached names, the closest ones are:"
        , D.indent 4 <| D.dullyellow <| D.vcat <| MList.map D.fromPackage suggestions
        , D.reflow <| "Maybe you want one of these instead?"
        ]

    InstallBadDetails details ->
      toDetailsReport details



-- OUTLINE (line 945)


type Outline
  = OutlineHasBadStructure (Decode.Error OutlineProblem)
  | OutlineHasMissingSrcDirs FilePath (TList FilePath)
  | OutlineHasDuplicateSrcDirs String FilePath FilePath
  | OutlineNoPkgCore
  | OutlineNoAppCore
  | OutlineNoAppJson


type OutlineProblem
  = OP_BadType
  | OP_BadPkgName Row Col
  | OP_BadVersion Row Col
  | OP_BadConstraint C.Error
  | OP_BadModuleName Row Col
  | OP_BadModuleHeaderTooLong
  | OP_BadDependencyName Row Col
  | OP_BadLicense (TList JS.TString)
  | OP_BadSummaryTooLong
  | OP_NoSrcDirs


toOutlineReport : Outline -> Help.Report
toOutlineReport problem =
  case problem of
    OutlineHasBadStructure decodeError ->
      Json.toReport (Dir.fromString "elm.json") (Json.FailureToReport toOutlineProblemReport) decodeError <|
        Json.ExplicitReason "I ran into a problem with your elm.json file."

    OutlineHasMissingSrcDirs dir dirs ->
      case dirs of
        [] ->
          Help.report "MISSING SOURCE DIRECTORY" (Just (Dir.fromString "elm.json"))
            "I need a valid elm.json file, but the \"source-directories\" field lists the following directory:"
            [ D.indent 4 <| D.red <| D.fromPath dir
            , D.reflow <|
                "I cannot find it though. Is it missing? Is there a typo?"
            ]

        _::_ ->
          Help.report "MISSING SOURCE DIRECTORIES" (Just (Dir.fromString "elm.json"))
            "I need a valid elm.json file, but the \"source-directories\" field lists the following directories:"
            [ D.indent 4 <| D.vcat <|
                MList.map (D.red << D.fromPath) (dir::dirs)
            , D.reflow <|
                "I cannot find them though. Are they missing? Are there typos?"
            ]

    OutlineHasDuplicateSrcDirs canonicalDir dir1 dir2 ->
      if dir1 == dir2 then
        Help.report "REDUNDANT SOURCE DIRECTORIES" (Just (Dir.fromString "elm.json"))
          "I need a valid elm.json file, but the \"source-directories\" field lists the same directory twice:"
          [ D.indent 4 <| D.vcat <|
              MList.map (D.red << D.fromPath) [dir1,dir2]
          , D.reflow <|
              "Remove one of the entries!"
          ]
      else
        Help.report "REDUNDANT SOURCE DIRECTORIES" (Just (Dir.fromString "elm.json"))
          "I need a valid elm.json file, but the \"source-directories\" field has some redundant directories:"
          [ D.indent 4 <| D.vcat <|
              MList.map (D.red << D.fromPath) [dir1,dir2]
          , D.reflow <|
              "These are two different ways of refering to the same directory:"
          , D.indent 4 <| D.dullyellow <| D.fromChars canonicalDir
          , D.reflow <|
              "Remove one of the redundant entries from your \"source-directories\" field."
          ]

    OutlineNoPkgCore ->
      Help.report "MISSING DEPENDENCY" (Just (Dir.fromString "elm.json"))
        ("I need to see an \"elm/core\" dependency your elm.json file. The default imports"
        ++ " of `List` and `Maybe` do not work without it.")
        [ D.reflow <|
            "If you modified your elm.json by hand, try to change it back! And if you are"
            ++ " having trouble getting back to a working elm.json, it may be easier to find a"
            ++ " working package and start fresh with their elm.json file."
        ]

    OutlineNoAppCore ->
      Help.report "MISSING DEPENDENCY" (Just (Dir.fromString "elm.json"))
        ("I need to see an \"elm/core\" dependency your elm.json file. The default imports"
        ++ " of `List` and `Maybe` do not work without it.")
        [ D.reflow <|
            "If you modified your elm.json by hand, try to change it back! And if you are"
            ++ " having trouble getting back to a working elm.json, it may be easier to delete it"
            ++ " and use `elm init` to start fresh."
        ]

    OutlineNoAppJson ->
      Help.report "MISSING DEPENDENCY" (Just (Dir.fromString "elm.json"))
        ("I need to see an \"elm/json\" dependency your elm.json file. It helps me handle"
        ++ " flags and ports.")
        [ D.reflow <|
            "If you modified your elm.json by hand, try to change it back! And if you are"
            ++ " having trouble getting back to a working elm.json, it may be easier to delete it"
            ++ " and use `elm init` to start fresh."
        ]


toOutlineProblemReport : FilePath -> Code.Source -> Json.Context -> A.Region -> OutlineProblem -> Help.Report
toOutlineProblemReport path source _ region problem =
  let
    toHighlight row col =
      Just <| A.Region (A.Position row col) (A.Position row col)

    toSnippet title highlight pair =
      Help.jsonReport title (Just path) <|
        Code.toSnippet source region highlight pair
  in
  case problem of
    OP_BadType ->
      toSnippet "UNEXPECTED TYPE" Nothing
        ( D.reflow <|
            "I got stuck while reading your elm.json file. I cannot handle a \"type\" like this:"
        , D.fillSep
            [d"Try",d"changing",d"the",d"\"type\"",d"to"
            ,D.greenS "\"application\"",d"or",D.greenS "\"package\"",d"instead."
            ]
        )

    OP_BadPkgName row col ->
      toSnippet "INVALID PACKAGE NAME" (toHighlight row col)
        ( D.reflow <|
            "I got stuck while reading your elm.json file. I ran into trouble with the package name:"
        , D.stack
            [ D.fillSep
                [d"Package",d"names",d"are",d"always",d"written",d"as"
                ,D.greenS "\"author/project\""
                ,d"so",d"I",d"am",d"expecting",d"to",d"see",d"something",d"like:"
                ]
            , D.dullyellow <| D.indent 4 <| D.vcat <|
                [ d"\"mdgriffith/elm-ui\""
                , d"\"w0rm/elm-physics\""
                , d"\"Microsoft/elm-json-tree-view\""
                , d"\"FordLabs/elm-star-rating\""
                , d"\"1602/json-schema\""
                ]
            , D.reflow <|
                "The author name should match your GitHub name exactly, and the project name"
                ++ " needs to follow these rules:"
            , D.indent 4 <| D.vcat <|
                [ d"+--------------------------------------+-----------+-----------+"
                , d"| RULE                                 | BAD       | GOOD      |"
                , d"+--------------------------------------+-----------+-----------+"
                , d"| only lower case, digits, and hyphens | elm-HTTP  | elm-http  |"
                , d"| no leading digits                    | 3D        | elm-3d    |"
                , d"| no non-ASCII characters              | elm-bjørn | elm-bear  |"
                , d"| no underscores                       | elm_ui    | elm-ui    |"
                , d"| no double hyphens                    | elm--hash | elm-hash  |"
                , d"| no starting or ending hyphen         | -elm-tar- | elm-tar   |"
                , d"+--------------------------------------+-----------+-----------+"
                ]
            , D.toSimpleNote <|
                "These rules only apply to the project name, so you should never need"
                ++ " to change your GitHub name!"
            ]
        )

    OP_BadVersion row col ->
      toSnippet "PROBLEM WITH VERSION" (toHighlight row col)
        ( D.reflow <|
            "I got stuck while reading your elm.json file. I was expecting a version number here:"
        , D.fillSep
            [d"I",d"need",d"something",d"like",D.greenS "\"1.0.0\"",d"or",D.greenS "\"2.0.4\""
            ,d"that",d"explicitly",d"states",d"all",d"three",d"numbers!"
            ]
        )

    OP_BadConstraint constraintError ->
      case constraintError of
        C.BadFormat row col ->
          toSnippet "PROBLEM WITH CONSTRAINT" (toHighlight row col)
            ( D.reflow <|
                "I got stuck while reading your elm.json file. I do not understand this version constraint:"
            , D.stack
                [ D.fillSep
                    [d"I",d"need",d"something",d"like",D.greenS "\"1.0.0 <= v < 2.0.0\""
                    ,d"that",d"explicitly",d"lists",d"the",d"lower",d"and",d"upper",d"bounds."
                    ]
                , D.toSimpleNote <|
                    "The spaces in there are required! Taking them out will confuse me. Adding"
                    ++ " extra spaces confuses me too. I recommend starting with a valid example"
                    ++ " and just changing the version numbers."
                ]
            )

        C.InvalidRange before after ->
          if before == after then
            toSnippet "PROBLEM WITH CONSTRAINT" Nothing
              ( D.reflow <|
                  "I got stuck while reading your elm.json file. I ran into an invalid version constraint:"
              , D.fillSep
                  [d"Elm",d"checks",d"that",d"all",d"package",d"APIs",d"follow",d"semantic",d"versioning,"
                  ,d"so",d"it",d"is",d"best",d"to",d"use",d"wide",d"constraints.",d"I",d"recommend"
                  ,D.green <| da[d"\"", D.fromVersion before, d" <= v < ", D.fromVersion (V.bumpMajor after), d"\""]
                  ,d"since",d"it",d"is",d"guaranteed",d"that",d"breaking",d"API",d"changes",d"cannot"
                  ,d"happen",d"in",d"any",d"of",d"the",d"versions",d"in",d"that",d"range."
                  ]
              )

          else
            toSnippet "PROBLEM WITH CONSTRAINT" Nothing
              ( D.reflow <|
                  "I got stuck while reading your elm.json file. I ran into an invalid version constraint:"
              , D.fillSep
                  [d"Maybe",d"you",d"want",d"something",d"like"
                  ,D.green <| da[d"\"", D.fromVersion before, d" <= v < ", D.fromVersion (V.bumpMajor before), d"\""]
                  ,d"instead?",d"Elm",d"checks",d"that",d"all",d"package",d"APIs",d"follow",d"semantic"
                  ,d"versioning,",d"so",d"it",d"is",d"guaranteed",d"that",d"breaking",d"API",d"changes"
                  ,d"cannot",d"happen",d"in",d"any",d"of",d"the",d"versions",d"in",d"that",d"range."
                  ]
              )

    OP_BadModuleName row col ->
      toSnippet "PROBLEM WITH MODULE NAME" (toHighlight row col)
        ( D.reflow <|
            "I got stuck while reading your elm.json file. I was expecting a module name here:"
        , D.fillSep
            [d"I",d"need",d"something",d"like",D.greenS "\"Html.Events\""
            ,d"or",D.greenS "\"Browser.Navigation\""
            ,d"where",d"each",d"segment",d"starts",d"with",d"a",d"capital"
            ,d"letter",d"and",d"the",d"segments",d"are",d"separated",d"by",d"dots."
            ]
        )

    OP_BadModuleHeaderTooLong ->
      toSnippet "HEADER TOO LONG" Nothing
        ( D.reflow <|
            "I got stuck while reading your elm.json file. This section header is too long:"
        , D.stack
            [ D.fillSep
                [d"I",d"need",d"it",d"to",d"be"
                ,D.greenS "under",D.greenS "20",D.greenS "bytes"
                ,d"so",d"it",d"renders",d"nicely",d"on",d"the",d"package",d"website!"
                ]
            , D.toSimpleNote <|
                "I count the length in bytes, so using non-ASCII characters costs extra."
                ++ " Please report your case at https://github.com/elm/compiler/issues if this seems"
                ++ " overly restrictive for your needs."
            ]
        )

    OP_BadDependencyName row col ->
      toSnippet "PROBLEM WITH DEPENDENCY NAME" (toHighlight row col)
        ( D.reflow <|
            "I got stuck while reading your elm.json file. There is something wrong with this dependency name:"
        , D.stack
            [ D.fillSep
                [d"Package",d"names",d"always",d"include",d"the",d"name",d"of",d"the",d"author,"
                ,d"so",d"I",d"am",d"expecting",d"to",d"see",d"dependencies",d"like"
                ,D.dullyellowS "\"mdgriffith/elm-ui\"",d"and"
                ,da[D.dullyellowS "\"Microsoft/elm-json-tree-view\"", d"."]
                ]
            , D.fillSep <|
                [d"I",d"generally",d"recommend",d"finding",d"the",d"package",d"you",d"want",d"on"
                ,d"the",d"package",d"website,",d"and",d"installing",d"it",d"with",d"the"
                ,D.greenS "elm install",d"command!"
                ]
            ]
        )

    OP_BadLicense suggestions ->
      toSnippet "UNKNOWN LICENSE" Nothing
        ( D.reflow <|
            "I got stuck while reading your elm.json file. I do not know about this type of license:"
        ,
          D.stack
            [ D.fillSep
                [d"Elm",d"packages",d"generally",d"use"
                ,D.greenS "\"BSD-3-Clause\"",d"or",da[D.greenS "\"MIT\"", d",d"]
                ,d"but",d"I",d"accept",d"any",d"OSI",d"approved",d"SPDX",d"license."
                ,d"Here",d"some",d"that",d"seem",d"close",d"to",d"what",d"you",d"wrote:"
                ]
            , D.indent 4 <| D.dullyellow <| D.vcat <| MList.map (D.fromChars << JS.toChars) suggestions
            , D.reflow <|
                "Check out https://spdx.org/licenses/ for the full list of options."
            ]
        )

    OP_BadSummaryTooLong ->
      toSnippet "SUMMARY TOO LONG" Nothing
        ( D.reflow <|
            "I got stuck while reading your elm.json file. Your \"summary\" is too long:"
        , D.stack
            [ D.fillSep
                [d"I",d"need",d"it",d"to",d"be"
                ,D.greenS "under",D.greenS "80",D.greenS "bytes"
                ,d"so",d"it",d"renders",d"nicely",d"on",d"the",d"package",d"website!"
                ]
            , D.toSimpleNote <|
                "I count the length in bytes, so using non-ASCII characters costs extra."
                ++ " Please report your case at https://github.com/elm/compiler/issues if this seems"
                ++ " overly restrictive for your needs."
            ]
        )

    OP_NoSrcDirs ->
      toSnippet "NO SOURCE DIRECTORIES" Nothing
        ( D.reflow <|
            "I got stuck while reading your elm.json file. You do not have any \"source-directories\" listed here:"
        , D.fillSep
            [d"I",d"need",d"something",d"like",D.greenS "[\"src\"]"
            ,d"so",d"I",d"know",d"where",d"to",d"look",d"for",d"your",d"modules!"
            ]
        )



-- DETAILS (line 1257)


type Details
  = DetailsNoSolution
  | DetailsNoOfflineSolution
  | DetailsSolverProblem Solver
  | DetailsBadElmInPkg C.Constraint
  | DetailsBadElmInAppOutline V.Version
  | DetailsHandEditedDependencies
  | DetailsBadOutline Outline
  | DetailsCannotGetRegistry RegistryProblem
  | DetailsBadDeps FilePath (TList DetailsBadDep)


type DetailsBadDep
  = BD_BadDownload Pkg.Name V.Version PackageProblem
  | BD_BadBuild Pkg.Name V.Version (Map.Map Pkg.Comparable V.Version)


toDetailsReport : Details -> Help.Report
toDetailsReport details =
  case details of
    DetailsNoSolution ->
      Help.report "INCOMPATIBLE DEPENDENCIES" (Just (Dir.fromString "elm.json"))
        "The dependencies in your elm.json are not compatible."
        [ D.fillSep
            [d"Did",d"you",d"change",d"them",d"by",d"hand?",d"Try",d"to",d"change",d"it",d"back!"
            ,d"It",d"is",d"much",d"more",d"reliable",d"to",d"add",d"dependencies",d"with",da[D.greenS "elm install", d"."]
            ]
        , D.reflow <|
            "Please ask for help on the community forums if you try those paths and are still"
            ++ " having problems!"
        ]

    DetailsNoOfflineSolution ->
      Help.report "TROUBLE VERIFYING DEPENDENCIES" (Just (Dir.fromString "elm.json"))
        ("I could not connect to https://package.elm-lang.org to get the latest list of"
        ++ " packages, and I was unable to verify your dependencies with the information I"
        ++ " have cached locally.")
        [ D.reflow <|
            "Are you able to connect to the internet? These dependencies may work once you"
            ++ " get access to the registry!"
        , D.toFancyNote
            [d"If",d"you",d"changed",d"your",d"dependencies",d"by",d"hand,",d"try",d"to",d"change",d"them",d"back!"
            ,d"It",d"is",d"much",d"more",d"reliable",d"to",d"add",d"dependencies",d"with",da[D.greenS "elm install", d"."]
            ]
        ]

    DetailsSolverProblem solver ->
      toSolverReport solver

    DetailsBadElmInPkg constraint ->
      Help.report "ELM VERSION MISMATCH" (Just (Dir.fromString "elm.json"))
        "Your elm.json says this package needs a version of Elm in this range:"
        [ D.indent 4 <| D.dullyellow <| D.fromChars <| C.toChars constraint
        , D.fillSep
            [ d"But", d"you", d"are", d"using", d"Elm"
            , D.red (D.fromVersion V.compiler)
            , d"right", d"now."
            ]
        ]

    DetailsBadElmInAppOutline version ->
      Help.report "ELM VERSION MISMATCH" (Just (Dir.fromString "elm.json"))
        "Your elm.json says this application needs a different version of Elm."
        [ D.fillSep
            [ d"It", d"requires"
            , da[D.green (D.fromVersion version), d","]
            , d"but", d"you", d"are", d"using"
            , D.red (D.fromVersion V.compiler)
            , d"right", d"now."
            ]
        ]

    DetailsHandEditedDependencies ->
      Help.report "ERROR IN DEPENDENCIES" (Just (Dir.fromString "elm.json"))
        ("It looks like the dependencies elm.json in were edited by hand (or by a 3rd"
        ++ " party tool) leaving them in an invalid state.")
        [ D.fillSep
            [d"Try",d"to",d"change",d"them",d"back",d"to",d"what",d"they",d"were",d"before!"
            ,d"It",d"is",d"much",d"more",d"reliable",d"to",d"add",d"dependencies",d"with",da[D.greenS "elm install", d"."]
            ]
        , D.reflow <|
            "Please ask for help on the community forums if you try those paths and are still"
            ++ " having problems!"
        ]

    DetailsBadOutline outline ->
      toOutlineReport outline

    DetailsCannotGetRegistry problem ->
      toRegistryProblemReport "PROBLEM LOADING PACKAGE LIST" problem <|
        "I need the list of published packages to verify your dependencies"

    DetailsBadDeps cacheDir deps ->
      case MList.sortOn toBadDepRank deps of
        [] ->
          Help.report "PROBLEM BUILDING DEPENDENCIES" Nothing
            "I am not sure what is going wrong though."
            [ D.reflow <|
                "I would try deleting the " ++ Dir.toString cacheDir ++ " and elm-stuff/ directories, then"
                ++ " trying to build again. That will work if some cached files got corrupted"
                ++ " somehow."
            , D.reflow <|
                "If that does not work, go to https://elm-lang.org/community and ask for"
                ++ " help. This is a weird case!"
            ]

        d::_ ->
          case d of
            BD_BadDownload pkg vsn packageProblem ->
              toPackageProblemReport pkg vsn packageProblem

            BD_BadBuild pkg vsn fingerprint ->
              Help.report "PROBLEM BUILDING DEPENDENCIES" Nothing
                "I ran into a compilation error when trying to build the following package:"
                [ D.indent 4 <| D.red <| D.fromChars <| Pkg.toChars pkg ++ " " ++ V.toChars vsn
                , D.reflow <|
                    "This probably means it has package constraints that are too wide. It may be"
                    ++ " possible to tweak your elm.json to avoid the root problem as a stopgap. Head"
                    ++ " over to https://elm-lang.org/community to get help figuring out how to take"
                    ++ " this path!"
                , D.toSimpleNote <|
                    "To help with the root problem, please report this to the package author along"
                    ++ " with the following information:"
                , D.indent 4 <| D.vcat <|
                    MList.map (\(p,v) -> D.fromChars <| Pkg.toChars (Pkg.fromComparable p) ++ " " ++ V.toChars v) <|
                      Map.toList fingerprint
                , D.reflow <|
                    "If you want to help out even more, try building the package locally. That should"
                    ++ " give you much more specific information about why this package is failing to"
                    ++ " build, which will in turn make it easier for the package author to fix it!"
                ]


toBadDepRank : DetailsBadDep -> Int -- lower is better
toBadDepRank badDep =
  case badDep of
    BD_BadDownload _ _ _ -> 0
    BD_BadBuild _ _ _ -> 1



-- PACKAGE PROBLEM (line 1401)


type PackageProblem
  = PP_BadEndpointRequest Http.Error
  | PP_BadEndpointContent String
  | PP_BadArchiveRequest Http.Error
  | PP_BadArchiveContent String


toPackageProblemReport : Pkg.Name -> V.Version -> PackageProblem -> Help.Report
toPackageProblemReport pkg vsn problem =
  let
    thePackage =
      Pkg.toChars pkg ++ " " ++ V.toChars vsn
  in
  case problem of
    PP_BadEndpointRequest httpError ->
      toHttpErrorReport "PROBLEM DOWNLOADING PACKAGE" httpError <|
        "I need to find the latest download link for " ++ thePackage

    PP_BadEndpointContent url ->
      Help.report "PROBLEM DOWNLOADING PACKAGE" Nothing
        (
          "I need to find the latest download link for " ++ thePackage ++ ", but I ran into corrupted information from:"
        )
        [ D.indent 4 <| D.dullyellow <| D.fromChars url
        , D.reflow <|
            "Is something weird with your internet connection. We have gotten reports that"
            ++ " schools, businesses, airports, etc. sometimes intercept requests and add things"
            ++ " to the body or change its contents entirely. Could that be the problem?"
        ]

    PP_BadArchiveRequest httpError ->
      toHttpErrorReport "PROBLEM DOWNLOADING PACKAGE" httpError <|
        "I was trying to download the source code for " ++ thePackage

    PP_BadArchiveContent url ->
      Help.report "PROBLEM DOWNLOADING PACKAGE" Nothing
        (
          "I downloaded the source code for " ++ thePackage ++ " from:"
        )
        [ D.indent 4 <| D.dullyellow <| D.fromChars url
        , D.reflow <|
            "But I was unable to unzip the data. Maybe there is something weird with"
            ++ " your internet connection. We have gotten reports that schools, businesses,"
            ++ " airports, etc. sometimes intercept requests and add things to the body or"
            ++ " change its contents entirely. Could that be the problem?"
        ]



-- REGISTRY PROBLEM (line 1471)


type RegistryProblem
  = RP_Http Http.Error
  | RP_Data String String


toRegistryProblemReport : String -> RegistryProblem -> String -> Help.Report
toRegistryProblemReport title problem context =
  case problem of
    RP_Http err ->
      toHttpErrorReport title err context

    RP_Data url body ->
      Help.report title Nothing (context ++ ", so I fetched:")
        [ D.indent 4 <| D.dullyellow <| D.fromChars url
        , D.reflow <|
            "I got the data back, but it was not what I was expecting. The response"
            ++ " body contains " ++ String.fromInt (String.length body) ++ " bytes. Here is the "
            ++ if String.length body <= 76 then "whole thing:" else "beginning:"
        , D.indent 4 <| D.dullyellow <| D.fromChars <|
            if String.length body <= 76
            then body
            else String.left 73 body ++ "..."
        , D.reflow <|
            "Does this error keep showing up? Maybe there is something weird with your"
            ++ " internet connection. We have gotten reports that schools, businesses,"
            ++ " airports, etc. sometimes intercept requests and add things to the body"
            ++ " or change its contents entirely. Could that be the problem?"
        ]


toHttpErrorReport : String -> Http.Error -> String -> Help.Report
toHttpErrorReport title err context =
  let
    toHttpReport intro url details =
      Help.report title Nothing intro <|
        D.indent 4 (D.dullyellow (D.fromChars url)) :: details
  in
  case err of
    Http.BadHttp url httpExceptionContent ->
      case httpExceptionContent of
        SysHttp.BadStatus code ->
          toHttpReport (context ++ ", so I tried to fetch:") url
            [ D.fillSep <|
                [d"But",d"it",d"came",d"back",d"as",D.red (D.fromInt code)]
                ++ MList.map D.fromChars (String.words "(message)")
            , D.indent 4 <| D.reflow <| "(body)"
            , D.reflow <|
                "This may mean some online endpoint changed in an unexpected way, so if does not"
                ++ " seem like something on your side is causing this (e.g. firewall) please report"
                ++ " this to https://github.com/elm/compiler/issues with your operating system, Elm"
                ++ " version, the command you ran, the terminal output, and any additional information"
                ++ " that can help others reproduce the error!"
            ]

        otherException ->
          toHttpReport (context ++ ", so I tried to fetch:") url
            [ D.reflow <| "But my HTTP library is giving me the following error message:"
            , D.indent 4 <| D.fromChars (Debug.toString otherException)
            , D.reflow <|
                "Are you somewhere with a slow internet connection? Or no internet?"
                ++ " Does the link I am trying to fetch work in your browser? Maybe the"
                ++ " site is down? Does your internet connection have a firewall that"
                ++ " blocks certain domains? It is usually something like that!"
            ]



-- MAKE (line 1585)


type Make
  = MakeNoOutline
  | MakeCannotOptimizeAndDebug
  | MakeBadDetails Details
  | MakeAppNeedsFileNames
  | MakePkgNeedsExposing
  | MakeMultipleFilesIntoHtml
  | MakeNoMain
  | MakeNonMainFilesIntoJavaScript ModuleName.Raw (TList ModuleName.Raw)
  | MakeCannotBuild BuildProblem
  | MakeBadGenerate Generate


makeToReport : Make -> Help.Report
makeToReport make =
  case make of
    MakeNoOutline ->
      Help.report "NO elm.json FILE" Nothing
        "It looks like you are starting a new Elm project. Very exciting! Try running:"
        [ D.indent 4 <| D.green <| d"elm init"
        , D.reflow <|
            "It will help you get set up. It is really simple!"
        ]

    MakeCannotOptimizeAndDebug ->
      Help.docReport "CLASHING FLAGS" Nothing
        ( D.fillSep
            [d"I",d"cannot",d"compile",d"with",D.redS "--optimize",d"and"
            ,D.redS "--debug",d"at",d"the",d"same",d"time."
            ]
        )
        [ D.reflow <|
            "I need to take away information to optimize things, and I need to"
            ++ " add information to add the debugger. It is impossible to do both"
            ++ " at once though! Pick just one of those flags and it should work!"
        ]

    MakeBadDetails detailsProblem ->
      toDetailsReport detailsProblem

    MakeAppNeedsFileNames ->
      Help.report "NO INPUT" Nothing
        "What should I make though? I need specific files like:"
        [ D.vcat
            [ D.indent 4 <| D.greenS "elm make src/Main.elm"
            , D.indent 4 <| D.greenS "elm make src/This.elm src/That.elm"
            ]
        , D.reflow <|
            "I recommend reading through https://guide.elm-lang.org for guidance on what to"
            ++ " actually put in those files!"
        ]

    MakePkgNeedsExposing ->
      Help.report "NO INPUT" Nothing
        "What should I make though? I need specific files like:"
        [ D.vcat
            [ D.indent 4 <| D.greenS "elm make src/Main.elm"
            , D.indent 4 <| D.greenS "elm make src/This.elm src/That.elm"
            ]
        , D.reflow <|
            "You can also entries to the \"exposed-modules\" list in your elm.json file, and"
            ++ " I will try to compile the relevant files."
        ]

    MakeMultipleFilesIntoHtml ->
      Help.report "TOO MANY FILES" Nothing
        (
          "When producing an HTML file, I can only handle one file."
        )
        [ D.fillSep
            [d"Switch",d"to",D.dullyellowS "--output=/dev/null",d"if",d"you",d"just",d"want"
            ,d"to",d"get",d"compile",d"errors.",d"This",d"skips",d"the",d"code",d"gen",d"phase,"
            ,d"so",d"it",d"can",d"be",d"a",d"bit",d"faster",d"than",d"other",d"options",d"sometimes."
            ]
        , D.fillSep
            [d"Switch",d"to",D.dullyellowS "--output=elm.js",d"if",d"you",d"want",d"multiple"
            ,d"`main`",d"values",d"available",d"in",d"a",d"single",d"JavaScript",d"file.",d"Then"
            ,d"you",d"can",d"make",d"your",d"own",d"customized",d"HTML",d"file",d"that",d"embeds"
            ,d"multiple",d"Elm",d"nodes.",d"The",d"generated",d"JavaScript",d"also",d"shares"
            ,d"dependencies",d"between",d"modules,",d"so",d"it",d"should",d"be",d"smaller",d"than"
            ,d"compiling",d"each",d"module",d"separately."
            ]
        ]

    MakeNoMain ->
      Help.report "NO MAIN" Nothing
        (
          "When producing an HTML file, I require that the given file has a `main` value."
          ++ " That way I have something to show on screen!"
        )
        [ D.reflow <|
            "Try adding a `main` value to your file? Or if you just want to verify that this"
            ++ " module compiles, switch to --output=/dev/null to skip the code gen phase"
            ++ " altogether."
        , D.toSimpleNote <|
            "Adding a `main` value can be as brief as adding something like this:"
        , D.vcat
            [ D.fillSep [D.cyanS "import",d"Html"]
            , d""
            , D.fillSep [D.greenS "main",d"="]
            , D.indent 2 <| D.fillSep [da[D.cyanS "Html", d".text"],D.dullyellowS "\"Hello!\""]
            ]
        , D.reflow <|
            "From there I can create an HTML file that says \"Hello!\" on screen. I recommend"
            ++ " looking through https://guide.elm-lang.org for more guidance on how to fill in"
            ++ " the `main` value."
        ]

    MakeNonMainFilesIntoJavaScript m ms ->
      case ms of
        [] ->
          Help.report "NO MAIN" Nothing
            (
              "When producing a JS file, I require that the given file has a `main` value. That"
              ++ " way Elm." ++ ModuleName.toChars m ++ ".init() is definitely defined in the"
              ++ " resulting file!"
            )
            [ D.reflow <|
                "Try adding a `main` value to your file? Or if you just want to verify that this"
                ++ " module compiles, switch to --output=/dev/null to skip the code gen phase"
                ++ " altogether."
            , D.toSimpleNote <|
                "Adding a `main` value can be as brief as adding something like this:"
            , D.vcat
                [ D.fillSep [D.cyanS "import",d"Html"]
                , d""
                , D.fillSep [D.greenS "main",d"="]
                , D.indent 2 <| D.fillSep [da[D.cyanS "Html", d".text"],D.dullyellowS "\"Hello!\""]
                ]
            , D.reflow <|
                "Or use https://package.elm-lang.org/packages/elm/core/latest/Platform#worker to"
                ++ " make a `main` with no user interface."
            ]

        _::_ ->
          Help.report "NO MAIN" Nothing
            (
              "When producing a JS file, I require that given files all have `main` values."
              ++ " That way functions like Elm." ++ ModuleName.toChars m ++ ".init() are"
              ++ " definitely defined in the resulting file. I am missing `main` values in:"
            )
            [ D.indent 4 <| D.red <| D.vcat <| MList.map D.fromName (m::ms)
            , D.reflow <|
                "Try adding a `main` value to them? Or if you just want to verify that these"
                ++ " modules compile, switch to --output=/dev/null to skip the code gen phase"
                ++ " altogether."
            , D.toSimpleNote <|
                "Adding a `main` value can be as brief as adding something like this:"
            , D.vcat
                [ D.fillSep [D.cyanS "import",d"Html"]
                , d""
                , D.fillSep [D.greenS "main",d"="]
                , D.indent 2 <| D.fillSep [da[D.cyanS "Html", d".text"],D.dullyellowS "\"Hello!\""]
                ]
            , D.reflow <|
                "Or use https://package.elm-lang.org/packages/elm/core/latest/Platform#worker to"
                ++ " make a `main` with no user interface."
            ]

    MakeCannotBuild buildProblem ->
      toBuildProblemReport buildProblem

    MakeBadGenerate generateProblem ->
      toGenerateReport generateProblem


-- BUILD PROBLEM (line 1755)


type BuildProblem
  = BuildBadModules FilePath Error.Module (TList Error.Module)
  | BuildProjectProblem BuildProjectProblem


type BuildProjectProblem
  = BP_PathUnknown FilePath
  | BP_WithBadExtension FilePath
  | BP_WithAmbiguousSrcDir FilePath FilePath FilePath
  | BP_MainPathDuplicate FilePath FilePath
  | BP_RootNameDuplicate ModuleName.Raw FilePath FilePath
  | BP_RootNameInvalid FilePath FilePath
  | BP_CannotLoadDependencies
  | BP_Cycle ModuleName.Raw (TList ModuleName.Raw)
  | BP_MissingExposed (NE.TList (ModuleName.Raw, Import.Problem))


toBuildProblemReport : BuildProblem -> Help.Report
toBuildProblemReport problem =
  case problem of
    BuildBadModules root e es ->
      Help.compilerReport root e es

    BuildProjectProblem projectProblem ->
      toProjectProblemReport projectProblem


toProjectProblemReport : BuildProjectProblem -> Help.Report
toProjectProblemReport projectProblem =
  case projectProblem of
    BP_PathUnknown path ->
      Help.report "FILE NOT FOUND" Nothing
        "I cannot find this file:"
        [ D.indent 4 <| D.red <| D.fromPath path
        , D.reflow <| "Is there a typo?"
        , D.toSimpleNote <|
            "If you are just getting started, try working through the examples in the"
            ++ " official guide https://guide.elm-lang.org to get an idea of the kinds of things"
            ++ " that typically go in a src/Main.elm file."
        ]

    BP_WithBadExtension path ->
      Help.report "UNEXPECTED FILE EXTENSION" Nothing
        "I can only compile Elm files (with a .elm extension) but you want me to compile:"
        [ D.indent 4 <| D.red <| D.fromPath path
        , D.reflow <| "Is there a typo? Can the file extension be changed?"
        ]

    BP_WithAmbiguousSrcDir path srcDir1 srcDir2 ->
      Help.report "CONFUSING FILE" Nothing
        "I am getting confused when I try to compile this file:"
        [ D.indent 4 <| D.red <| D.fromPath path
        , D.reflow <|
            "I always check if files appear in any of the \"source-directories\" listed in"
            ++ " your elm.json to see if there might be some cached information about them. That"
            ++ " can help me compile faster! But in this case, it looks like this file may be in"
            ++ " either of these directories:"
        , D.indent 4 <| D.red <| D.vcat <| MList.map D.fromPath [srcDir1,srcDir2]
        , D.reflow <|
            "Try to make it so no source directory contains another source directory!"
        ]

    BP_MainPathDuplicate path1 path2 ->
      Help.report "CONFUSING FILES" Nothing
        "You are telling me to compile these two files:"
        [ D.indent 4 <| D.red <| D.vcat <| MList.map D.fromPath [ path1, path2 ]
        , D.reflow <|
            if path1 == path2 then
              "Why are you telling me twice? Is something weird going on with a script?"
              ++ " I figured I would let you know about it just in case something is wrong."
              ++ " Only list it once and you should be all set!"
            else
              "But seem to be the same file though... It makes me think something tricky is"
              ++ " going on with symlinks in your project, so I figured I would let you know"
              ++ " about it just in case. Remove one of these files from your command to get"
              ++ " unstuck!"
        ]

    BP_RootNameDuplicate name outsidePath otherPath ->
      Help.report "MODULE NAME CLASH" Nothing
        "These two files are causing a module name clash:"
        [ D.indent 4 <| D.red <| D.vcat <| MList.map D.fromPath [ outsidePath, otherPath ]
        , D.reflow <|
            "They both say `module " ++ ModuleName.toChars name ++ " exposing (..)` up"
            ++ " at the top, but they cannot have the same name!"
        , D.reflow <|
            "Try changing to a different module name in one of them!"
        ]

    BP_RootNameInvalid givenPath srcDir ->
      Help.report "UNEXPECTED FILE NAME" Nothing
        "I am having trouble with this file name:"
        [ D.indent 4 <| D.red <| D.fromPath givenPath
        , D.reflow <|
            "I found it in your " ++ Dir.toString srcDir ++ "/ directory"
            ++ " which is good, but I expect all of the files in there to use the following"
            ++ " module naming convention:"
        , toModuleNameConventionTable srcDir [ "Main", "HomePage", "Http.Helpers" ]
        , D.reflow <|
            "Notice that the names always start with capital letters! Can you make your file"
            ++ " use this naming convention?"
        , D.toSimpleNote <|
            "Having a strict naming convention like this makes it a lot easier to find"
            ++ " things in large projects. If you see a module imported, you know where to look"
            ++ " for the corresponding file every time!"
        ]

    BP_CannotLoadDependencies ->
      corruptCacheReport

    BP_Cycle name names ->
      Help.report "IMPORT CYCLE" Nothing
        "Your module imports form a cycle:"
        [ D.cycle 4 name names
        , D.reflow <|
            "Learn more about why this is disallowed and how to break cycles here:"
            ++ D.makeLink "import-cycles"
        ]

    BP_MissingExposed (NE.CList (name, problem) _) ->
      case problem of
        Import.NotFound ->
          Help.report "MISSING MODULE" (Just (Dir.fromString "elm.json"))
            "The  \"exposed-modules\" of your elm.json lists the following module:"
            [ D.indent 4 <| D.red <| D.fromName name
            , D.reflow <|
                "But I cannot find it in your src/ directory. Is there a typo? Was it renamed?"
            ]

        Import.Ambiguous _ pkg ->
          Help.report "AMBIGUOUS MODULE NAME" (Just (Dir.fromString "elm.json"))
            "The  \"exposed-modules\" of your elm.json lists the following module:"
            [ D.indent 4 <| D.red <| D.fromName name
            , D.reflow <|
                "But a module from " ++ Pkg.toChars pkg ++ " already uses that name. Try"
                ++ " choosing a different name for your local file."
            ]

        Import.AmbiguousLocal path1 path2 paths ->
          Help.report "AMBIGUOUS MODULE NAME" (Just (Dir.fromString "elm.json"))
            "The  \"exposed-modules\" of your elm.json lists the following module:"
            [ D.indent 4 <| D.red <| D.fromName name
            , D.reflow <|
                "But I found multiple files with that name:"
            , D.dullyellow <| D.indent 4 <| D.vcat <|
                MList.map D.fromPath (path1::path2::paths)
            , D.reflow <|
                "Change the module names to be distinct!"
            ]

        Import.AmbiguousForeign _ _ _ ->
          Help.report "MISSING MODULE" (Just (Dir.fromString "elm.json"))
            "The  \"exposed-modules\" of your elm.json lists the following module:"
            [ D.indent 4 <| D.red <| D.fromName name
            , D.reflow <|
                "But I cannot find it in your src/ directory. Is there a typo? Was it renamed?"
            , D.toSimpleNote <|
                "It is not possible to \"re-export\" modules from other packages. You can only"
                ++ " expose modules that you define in your own code."
            ]


toModuleNameConventionTable : FilePath -> TList String -> D.Doc
toModuleNameConventionTable srcDir names =
  let
    toPair name =
      ( name
      , Dir.toString srcDir ++ String.map (\c -> if c == '.' then '/' else c) name ++ ".elm"
      )

    namePairs = MList.map toPair names
    nameWidth = MList.maximum (11 :: MList.map (String.length << Tuple.first) namePairs)
    pathWidth = MList.maximum ( 9 :: MList.map (String.length << Tuple.second) namePairs)

    padded width str =
      str ++ String.repeat (width - String.length str) " "

    toRow (name, path) =
      D.fromChars <|
        "| " ++ padded nameWidth name ++ " | " ++ padded pathWidth path ++ " |"

    bar =
      D.fromChars <|
        "+-" ++ String.repeat nameWidth "-" ++ "-+-" ++ String.repeat pathWidth "-" ++ "-+"
  in
  D.indent 4 <| D.vcat <|
    [ bar, toRow ("Module Name", "File Path"), bar ] ++ MList.map toRow namePairs ++ [ bar ]



-- GENERATE (line 1948)


type Generate
  = GenerateCannotLoadArtifacts
  | GenerateCannotOptimizeDebugValues ModuleName.Raw (TList ModuleName.Raw)


toGenerateReport : Generate -> Help.Report
toGenerateReport problem =
  case problem of
    GenerateCannotLoadArtifacts ->
      corruptCacheReport

    GenerateCannotOptimizeDebugValues m ms ->
      Help.report "DEBUG REMNANTS" Nothing
        "There are uses of the `Debug` module in the following modules:"
        [ D.indent 4 <| D.red <| D.vcat <| MList.map (D.fromChars << ModuleName.toChars) (m::ms)
        , D.reflow "But the --optimize flag only works if all `Debug` functions are removed!"
        , D.toSimpleNote <|
            "The issue is that --optimize strips out info needed by `Debug` functions."
            ++ " Here are two examples:"
        , D.indent 4 <| D.reflow <|
            "(1) It shortens record field names. This makes the generated JavaScript"
            ++ " smaller, but `Debug.toString` cannot know the real field names anymore."
        , D.indent 4 <| D.reflow <|
            "(2) Values like `type Height = Height Float` are unboxed. This reduces"
            ++ " allocation, but it also means that `Debug.toString` cannot tell if it is"
            ++ " looking at a `Height` or `Float` value."
        , D.reflow <|
            "There are a few other cases like that, and it will be much worse once we start"
            ++ " inlining code. That optimization could move `Debug.log` and `Debug.todo` calls,"
            ++ " resulting in unpredictable behavior. I hope that clarifies why this restriction"
            ++ " exists!"
        ]



-- CORRUPT CACHE (line 1986)


corruptCacheReport : Help.Report
corruptCacheReport =
  Help.report "CORRUPT CACHE" Nothing
    "It looks like some of the information cached in elm-stuff/ has been corrupted."
    [ D.reflow <|
        "Try deleting your elm-stuff/ directory to get unstuck."
    , D.toSimpleNote <|
        "This almost certainly means that a 3rd party tool (or editor plugin) is"
        ++ " causing problems your the elm-stuff/ directory. Try disabling 3rd party tools"
        ++ " one by one until you figure out which it is!"
    ]



-- REACTOR (line 2003)


type Reactor
  = ReactorNoOutline
  | ReactorBadDetails Details
  | ReactorBadBuild BuildProblem
  | ReactorBadGenerate Generate


reactorToReport : Reactor -> Help.Report
reactorToReport problem =
  case problem of
    ReactorNoOutline ->
      Help.report "NEW PROJECT?" Nothing
        "Are you trying to start a new project? Try this command in the terminal:"
        [ D.indent 4 <| D.greenS "elm init"
        , D.reflow "It will help you get started!"
        ]

    ReactorBadDetails details ->
      toDetailsReport details

    ReactorBadBuild buildProblem ->
      toBuildProblemReport buildProblem

    ReactorBadGenerate generate ->
      toGenerateReport generate



-- REPL (line 2034)


type Repl
  = ReplBadDetails Details
  | ReplBadInput String Error.Error
  | ReplBadLocalDeps FilePath Error.Module (TList Error.Module)
  | ReplProjectProblem BuildProjectProblem
  | ReplBadGenerate Generate
  | ReplBadCache
  | ReplBlocked


replToReport : Repl -> Help.Report
replToReport problem =
  case problem of
    ReplBadDetails details ->
      toDetailsReport details

    ReplBadInput source err ->
      Help.compilerReport (Dir.fromString "/") (Error.Module N.replModule (Dir.fromString "REPL") File.zeroTime source err) []

    ReplBadLocalDeps root e es ->
      Help.compilerReport root e es

    ReplProjectProblem projectProblem ->
      toProjectProblemReport projectProblem

    ReplBadGenerate generate ->
      toGenerateReport generate

    ReplBadCache ->
      corruptCacheReport

    ReplBlocked ->
      corruptCacheReport
