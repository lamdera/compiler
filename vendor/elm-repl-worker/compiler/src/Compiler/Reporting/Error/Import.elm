{- MANUALLY FORMATTED -}
module Compiler.Reporting.Error.Import exposing
  ( Error(..)
  , Problem(..)
  , toReport
  )


import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D exposing (d, da)
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Report as Report
import Compiler.Reporting.Suggest as Suggest
import Extra.System.Dir exposing (FilePath)
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Set as Set


-- ERROR


type Error =
  Error
    {- region -} A.Region
    {- import -} ModuleName.Raw
    {- unimported -} (Set.Set ModuleName.Raw)
    {- problem -} Problem


type Problem
  = NotFound
  | Ambiguous FilePath Pkg.Name
  | AmbiguousLocal FilePath FilePath (TList FilePath)
  | AmbiguousForeign Pkg.Name Pkg.Name (TList Pkg.Name)



-- TO REPORT


toReport : Code.Source -> Error -> Report.Report
toReport source (Error region name unimportedModules problem) =
  case problem of
    NotFound ->
      Report.Report "MODULE NOT FOUND" region <|
        Code.toSnippet source region Nothing
          (
            D.reflow <|
              "You are trying to import a `" ++ ModuleName.toChars name ++ "` module:"
          ,
            D.stack
              [
                D.reflow <|
                  "I checked the \"dependencies\" and \"source-directories\" listed in your elm.json,"
                  ++ " but I cannot find it! Maybe it is a typo for one of these names?"
              ,
                D.dullyellow <| D.indent 4 <| D.vcat <|
                  MList.map D.fromName (toSuggestions name unimportedModules)
              ,
                case Map.lookup name Pkg.suggestions of
                  Nothing ->
                    D.toSimpleHint <|
                      "If it is not a typo, check the \"dependencies\" and \"source-directories\""
                      ++ " of your elm.json to make sure all the packages you need are listed there!"

                  Just dependency ->
                    D.toFancyHint
                      [d"Maybe",d"you",d"want",d"the"
                      ,da[d"`", D.fromName name, d"`"]
                      ,d"module",d"defined",d"in",d"the"
                      ,D.fromChars (Pkg.toChars dependency)
                      ,d"package?",d"Running"
                      ,D.green (D.fromChars ("elm install " ++ Pkg.toChars dependency))
                      ,d"should",d"make",d"it",d"available!"
                      ]
              ]
          )

    Ambiguous path pkg ->
      Report.Report "AMBIGUOUS IMPORT" region <|
        Code.toSnippet source region Nothing
          (
            D.reflow <|
              "You are trying to import a `" ++ ModuleName.toChars name ++ "` module:"
          ,
            D.stack
              [
                D.fillSep <|
                  [d"But",d"I",d"found",d"multiple",d"modules",d"with",d"that",d"name.",d"One",d"in",d"the"
                  ,D.dullyellow (D.fromChars (Pkg.toChars pkg))
                  ,d"package,",d"and",d"another",d"defined",d"locally",d"in",d"the"
                  ,D.dullyellow (D.fromPath path)
                  ,d"file.",d"I",d"do",d"not",d"have",d"a",d"way",d"to",d"choose",d"between",d"them."
                  ]
              ,
                D.reflow <|
                  "Try changing the name of the locally defined module to clear up the ambiguity?"
              ]
          )

    AmbiguousLocal path1 path2 paths ->
      Report.Report "AMBIGUOUS IMPORT" region <|
        Code.toSnippet source region Nothing
          (
            D.reflow <|
              "You are trying to import a `" ++ ModuleName.toChars name ++ "` module:"
          ,
            D.stack
              [
                D.reflow <|
                  "But I found multiple files in your \"source-directories\" with that name:"
              ,
                D.dullyellow <| D.indent 4 <| D.vcat <|
                  MList.map D.fromPath (path1::path2::paths)
              ,
                D.reflow <|
                  "Change the module names to be distinct!"
              ]
          )

    AmbiguousForeign pkg1 pkg2 pkgs ->
      Report.Report "AMBIGUOUS IMPORT" region <|
        Code.toSnippet source region Nothing
          (
            D.reflow <|
              "You are trying to import a `" ++ ModuleName.toChars name ++ "` module:"
          ,
            D.stack
              [
                D.reflow <|
                  "But multiple packages in your \"dependencies\" that expose a module that name:"
              ,
                D.dullyellow <| D.indent 4 <| D.vcat <|
                  MList.map (D.fromChars << Pkg.toChars) (pkg1::pkg2::pkgs)
              ,
                D.reflow <|
                  "There is no way to disambiguate in cases like this right now. Of the known name"
                  ++ " clashes, they are usually for packages with similar purposes, so the current"
                  ++ " recommendation is to pick just one of them."
              , D.toSimpleNote <|
                  "It seems possible to resolve this with new syntax in imports, but that is"
                  ++ " more complicated than it sounds. Right now, our module names are tied to GitHub"
                  ++ " repos, but we may want to get rid of that dependency for a variety of reasons."
                  ++ " That would in turn have implications for our package infrastructure, hosting"
                  ++ " costs, and possibly on how package names are specified. The particular syntax"
                  ++ " chosen seems like it would interact with all these factors in ways that are"
                  ++ " difficult to predict, potentially leading to harder problems later on. So more"
                  ++ " design work and planning is needed on these topics."
              ]
          )



toSuggestions : ModuleName.Raw -> Set.Set ModuleName.Raw -> TList ModuleName.Raw
toSuggestions name unimportedModules =
  MList.take 4 <|
    Suggest.sort (ModuleName.toChars name) ModuleName.toChars (Set.toList unimportedModules)
