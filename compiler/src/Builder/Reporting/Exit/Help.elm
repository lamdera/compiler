{- MANUALLY FORMATTED -}
module Builder.Reporting.Exit.Help exposing
  ( Report
  , report
  , docReport
  , jsonReport
  , compilerReport
  , reportToDoc
  , reportToClient -- reportToJson
  --, toString
  --, toStdout
  --, toStderr
  )


import Compiler.Reporting.Doc as D exposing (d)
import Compiler.Reporting.Error as Error
import Elm.Error as Client
import Extra.System.Dir as Dir exposing (FilePath)
import Extra.Type.List as MList exposing (TList)



-- REPORT


type Report
  = CompilerReport FilePath Error.Module (TList Error.Module)
  | Report
      {- title -} String
      {- path -} (Maybe FilePath)
      {- message -} D.Doc


report : String -> Maybe FilePath -> String -> TList D.Doc -> Report
report title path startString others =
  Report title path <| D.stack (D.reflow startString::others)


docReport : String -> Maybe FilePath -> D.Doc -> TList D.Doc -> Report
docReport title path startDoc others =
  Report title path <| D.stack (startDoc::others)


jsonReport : String -> Maybe FilePath -> D.Doc -> Report
jsonReport =
  Report


compilerReport : FilePath -> Error.Module -> TList Error.Module -> Report
compilerReport =
  CompilerReport



-- TO DOC


reportToDoc : Report -> D.Doc
reportToDoc report_ =
  case report_ of
    CompilerReport root e es ->
      Error.toDoc root e es

    Report title maybePath message ->
      let
        makeDashes n =
          String.repeat (max 1 (80 - n)) "-"

        errorBarEnd =
          case maybePath of
            Nothing ->
              makeDashes (4 + String.length title)

            Just path ->
              makeDashes (5 + String.length title + String.length (Dir.toString path)) ++ " " ++ (Dir.toString path)

        errorBar =
          D.dullcyan <|
            D.hsep [d"--", D.fromChars title, D.fromChars errorBarEnd ]
      in
        D.stack [errorBar, message, d""]



-- TO CLIENT (original: TO JSON)


reportToClient : Report -> Client.Error
reportToClient report_ =
  case report_ of
    CompilerReport _ e es ->
      Client.ModuleProblems <| MList.map Error.toClient (e::es)

    Report title maybePath message ->
      Client.GeneralProblem
        { path = Maybe.map Dir.toString maybePath
        , title = title
        , message = D.toClient message
        }
