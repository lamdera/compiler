{- MANUALLY FORMATTED -}
module Compiler.Reporting.Error exposing
  ( Module(..)
  , Error(..)
  , toDoc
  --, toJson
  , toClient
  )


import Builder.File as File
import Compiler.Data.NonEmptyList as NE
import Compiler.Data.OneOrMore as OneOrMore
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Nitpick.PatternMatches as PatternMatches
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D exposing (d)
import Compiler.Reporting.Error.Canonicalize as Canonicalize
import Compiler.Reporting.Error.Import as Import
import Compiler.Reporting.Error.Main as Main
import Compiler.Reporting.Error.Pattern as Pattern
import Compiler.Reporting.Error.Syntax as Syntax
import Compiler.Reporting.Error.Type as Type
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Render.Type.Localizer as L
import Compiler.Reporting.Report as Report
import Elm.Error as Client
import Extra.System.Dir as Dir exposing (FilePath)
import Extra.Type.List as MList exposing (TList)



-- MODULE


type Module =
  Module
    {- name -} ModuleName.Raw
    {- absolutePath -} FilePath
    {- modificationTime -} File.Time
    {- source -} String
    {- error -} Error


getName (Module name _ _ _ _) = name
getModificationTime (Module _ _ time _ _) = File.toMillis time



-- ERRORS


type Error
  = BadSyntax Syntax.Error
  | BadImports (NE.TList Import.Error)
  | BadNames (OneOrMore.OneOrMore Canonicalize.Error)
  | BadTypes L.Localizer (NE.TList Type.Error)
  | BadMains L.Localizer (OneOrMore.OneOrMore Main.Error)
  | BadPatterns (NE.TList PatternMatches.Error)



-- TO REPORT


toReports : Code.Source -> Error -> NE.TList Report.Report
toReports source err =
  case err of
    BadSyntax syntaxError ->
      NE.CList (Syntax.toReport source syntaxError) []

    BadImports errs ->
      NE.fmap (Import.toReport source) errs

    BadNames errs ->
      NE.fmap (Canonicalize.toReport source) (OneOrMore.destruct NE.CList errs)

    BadTypes localizer errs ->
      NE.fmap (Type.toReport source localizer) errs

    BadMains localizer errs ->
      NE.fmap (Main.toReport localizer source) (OneOrMore.destruct NE.CList errs)

    BadPatterns errs ->
      NE.fmap (Pattern.toReport source) errs



-- TO DOC


toDoc : FilePath -> Module -> TList Module -> D.Doc
toDoc root err errs =
  let
    (NE.CList m ms) = NE.sortBy getModificationTime (NE.CList err errs)
  in
  D.vcat (toDocHelp root m ms)


toDocHelp : FilePath -> Module -> TList Module -> TList D.Doc
toDocHelp root module1 modules =
  case modules of
    [] ->
      [moduleToDoc root module1
      ,d""
      ]

    module2 :: otherModules ->
      moduleToDoc root module1
      :: toSeparator module1 module2
      :: toDocHelp root module2 otherModules


toSeparator : Module -> Module -> D.Doc
toSeparator beforeModule afterModule =
  let
    before = ModuleName.toChars (getName beforeModule) ++ "  ↑    "
    after  = "    ↓  " ++  ModuleName.toChars (getName afterModule)
  in
    D.dullred <| D.vcat <|
      [ D.indent (80 - String.length before) (D.fromChars before)
      , d"====o======================================================================o===="
      , D.fromChars after
      , d""
      , d""
      ]



-- MODULE TO DOC


moduleToDoc : FilePath -> Module -> D.Doc
moduleToDoc root (Module _ absolutePath _ source err) =
  let
    reports =
      toReports (Code.toSource source) err

    relativePath =
      Dir.makeRelative root absolutePath
  in
  D.vcat <| MList.map (reportToDoc relativePath) (NE.toList reports)


reportToDoc : FilePath -> Report.Report -> D.Doc
reportToDoc relativePath (Report.Report title _ message) =
  D.vcat
    [ toMessageBar title relativePath
    , d""
    , message
    , d""
    ]


toMessageBar : String -> FilePath -> D.Doc
toMessageBar title filePath =
  let
    usedSpace =
      4 + String.length title + 1 + String.length (Dir.toString filePath)
  in
    D.dullcyan <| D.fromChars <|
      "-- " ++ title
      ++ " " ++ String.repeat (max 1 (80 - usedSpace)) "-"
      ++ " " ++ (Dir.toString filePath)



-- TO CLIENT (original: TO JSON)


toClient : Module -> Client.BadModule
toClient (Module name path _ source err) =
  let
    reports =
      toReports (Code.toSource source) err
  in
  { path = Dir.toString path
  , name = name
  , problems = MList.map reportToClient (NE.toList reports)
  }


reportToClient : Report.Report -> Client.Problem
reportToClient (Report.Report title region message) =
  { title = title
  , region = toClientRegion region
  , message = D.toClient message
  }


toClientRegion : A.Region -> Client.Region
toClientRegion (A.Region (A.Position sr sc) (A.Position er ec)) =
  { start =
      { line = sr
      , column = sc
      }
  , end =
      { line = er
      , column = ec
      }
  }
