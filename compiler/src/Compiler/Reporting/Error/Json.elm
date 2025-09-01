{- MANUALLY FORMATTED -}
module Compiler.Reporting.Error.Json exposing
  ( toReport
  , FailureToReport(..)
  , Context(..)
  , Reason(..)
  )


import Builder.Reporting.Exit.Help as Help
import Compiler.Data.NonEmptyList as NE
import Compiler.Json.Decode as JD
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Doc as D exposing (d, da)
import Compiler.Reporting.Render.Code as Code
import Extra.System.Dir exposing (FilePath)
import Extra.Type.List as MList



-- TO REPORT


toReport : FilePath -> FailureToReport x -> JD.Error x -> Reason -> Help.Report
toReport path ftr err reason =
  case err of
    JD.DecodeProblem bytes problem ->
      problemToReport path ftr (Code.toSource bytes) CRoot problem reason

    JD.ParseProblem bytes parseError ->
      parseErrorToReport path (Code.toSource bytes) parseError reason


type Reason =
  ExplicitReason String


because : Reason -> String -> String
because (ExplicitReason iNeedThings) problem =
  iNeedThings ++ " " ++ problem



-- PARSE ERROR TO REPORT


parseErrorToReport : FilePath -> Code.Source -> JD.ParseError -> Reason -> Help.Report
parseErrorToReport path source parseError reason =
  let
    toSnippet title row col (problem, details) =
      let
        pos = A.Position row col
        surroundings = A.Region (A.Position (max 1 (row - 2)) 1) pos
        region = A.Region pos pos
      in
      Help.jsonReport title (Just path) <|
        Code.toSnippet source surroundings (Just region)
          ( D.reflow (because reason problem)
          , details
          )
  in
  case parseError of
    JD.Start row col ->
      toSnippet "EXPECTING A VALUE" row col
        (
          "I was expecting to see a JSON value next:"
        ,
          D.stack
            [ D.fillSep
                [d"Try",d"something",d"like",D.dullyellowS "\"this\"",d"or"
                ,D.dullyellowS "42",d"to",d"move",d"on",d"to",d"better",d"hints!"
                ]
            , D.toSimpleNote <|
                "The JSON specification does not allow trailing commas, so you can sometimes"
                ++ " get this error in arrays that have an extra comma at the end. In that case,"
                ++ " remove that last comma or add another array entry after it!"
            ]
        )

    JD.ObjectField row col ->
      toSnippet "EXTRA COMMA" row col
        (
          "I was partway through parsing a JSON object when I got stuck here:"
        ,
          D.stack
            [ D.fillSep
                [d"I",d"saw",d"a",d"comma",d"right",d"before",d"I",d"got",d"stuck",d"here,"
                ,d"so",d"I",d"was",d"expecting",d"to",d"see",d"a",d"field",d"name",d"like"
                ,D.dullyellowS "\"type\"",d"or",D.dullyellowS "\"dependencies\"",d"next."
                ]
            , D.reflow <|
                "This error is commonly caused by trailing commas in JSON objects. Those are"
                ++ " actually disallowed by <https://json.org> so check the previous line for a"
                ++ " trailing comma that may need to be deleted."
            , objectNote
            ]
        )

    JD.ObjectColon row col ->
      toSnippet "EXPECTING COLON" row col
        (
          "I was partway through parsing a JSON object when I got stuck here:"
        ,
          D.stack
            [ D.reflow <| "I was expecting to see a colon next."
            , objectNote
            ]
        )

    JD.ObjectEnd row col ->
      toSnippet "UNFINISHED OBJECT" row col
        (
          "I was partway through parsing a JSON object when I got stuck here:"
        ,
          D.stack
            [ D.reflow <|
                "I was expecting to see a comma or a closing curly brace next."
            , D.reflow <|
                "Is a comma missing on the previous line? Is an array missing a closing square"
                ++ " bracket? It is often something tricky like that!"
            , objectNote
            ]
        )

    JD.ArrayEnd row col ->
      toSnippet "UNFINISHED ARRAY" row col
        (
          "I was partway through parsing a JSON array when I got stuck here:"
        ,
          D.stack
            [ D.reflow <| "I was expecting to see a comma or a closing square bracket next."
            , D.reflow <|
                "Is a comma missing on the previous line? It is often something like that!"
            ]
        )

    JD.StringProblem stringProblem row col ->
      case stringProblem of
        JD.BadStringEnd ->
          toSnippet "ENDLESS STRING" row col
            (
              "I got to the end of the line without seeing the closing double quote:"
            ,
              D.fillSep <|
                [d"Strings",d"look",d"like",D.greenS "\"this\"",d"with",d"double"
                ,d"quotes",d"on",d"each",d"end.",d"Is",d"the",d"closing",d"double"
                ,d"quote",d"missing",d"in",d"your",d"code?"
                ]
            )

        JD.BadStringControlChar ->
          toSnippet "UNEXPECTED CONTROL CHARACTER" row col
            (
              "I ran into a control character unexpectedly:"
            ,
              D.reflow <|
                "These are characters that represent tabs, backspaces, newlines, and"
                ++ " a bunch of other invisible characters. They all come before 20 in the"
                ++ " ASCII range, and they are disallowed by the JSON specificaiton. Maybe"
                ++ " a copy/paste added one of these invisible characters to your JSON?"
            )

        JD.BadStringEscapeChar ->
          toSnippet "UNKNOWN ESCAPE" row col
            (
              "Backslashes always start escaped characters, but I do not recognize this one:"
            ,
              D.stack
                [ D.reflow <|
                    "Valid escape characters include:"
                , D.dullyellow <| D.indent 4 <| D.vcat <|
                    [d"\\\"",d"\\\\",d"\\/",d"\\b",d"\\f",d"\\n",d"\\r",d"\\t",d"\\u003D"]
                , D.reflow <|
                    "Do you want one of those instead? Maybe you need \\\\ to escape a backslash?"
                ]
            )

        JD.BadStringEscapeHex ->
          toSnippet "BAD HEX ESCAPE" row col
            (
              "This is not a valid hex escape:"
            ,
              D.fillSep <|
                [d"Valid",d"hex",d"escapes",d"in",d"JSON",d"are",d"between"
                ,D.greenS "\\u0000",d"and",D.greenS "\\uFFFF"
                ,d"and",d"always",d"have",d"exactly",d"four",d"digits."
                ]
            )

    JD.NoLeadingZeros row col ->
      toSnippet "BAD NUMBER" row col
        (
          "Numbers cannot start with zeros like this:"
        ,
          D.reflow <| "Try deleting the leading zeros?"
        )

    JD.NoFloats row col ->
      toSnippet "UNEXPECTED NUMBER" row col
        (
          "I got stuck while trying to parse this number:"
        ,
          D.reflow <|
            "I do not accept floating point numbers like 3.1415 right now. That kind"
            ++ " of JSON value is not needed for any of the uses that Elm has for now."
        )

    JD.BadEnd row col ->
      toSnippet "JSON PROBLEM" row col
        (
          "I was partway through parsing some JSON when I got stuck here:"
        ,
          D.reflow <|
            "I am not really sure what is wrong. This sometimes means there is extra"
            ++ " stuff after a valid JSON value?"
        )


objectNote : D.Doc
objectNote =
  D.stack
    [ D.toSimpleNote <| "Here is an example of a valid JSON object for reference:"
    , D.vcat
        [ D.indent 4 <| d"{"
        , D.indent 6 <| da[D.dullyellowS "\"name\"", d": ", D.dullyellowS "\"Tom\"", d","]
        , D.indent 6 <| da[D.dullyellowS "\"age\"", d": ", D.dullyellowS "42"]
        , D.indent 4 <| d"}"
        ]
    , D.reflow <|
        "Notice that (1) the field names are in double quotes and (2) there is no"
        ++ " trailing comma after the last entry. Both are strict requirements in JSON!"
    ]



-- PROBLEM TO REPORT


type Context
  = CRoot
  | CField String
  | CIndex Int Context


problemToReport : FilePath -> FailureToReport x -> Code.Source -> Context -> JD.Problem x -> Reason -> Help.Report
problemToReport path ftr source context problem reason =
  case problem of
    JD.Field field prob ->
      problemToReport path ftr source (CField field) prob reason

    JD.Index index prob ->
      problemToReport path ftr source (CIndex index context) prob reason

    JD.OneOf p ps ->
      -- NOTE: only displays the deepest problem. This works well for the kind
      -- of JSON used by Elm, but probably would not work well in general.
      let
        (NE.CList prob _) = NE.sortBy (negate << getMaxDepth) (NE.CList p ps)
      in
      problemToReport path ftr source context prob reason

    JD.Failure region x ->
      getFailureToReport ftr path source context region x

    JD.Expecting region expectation ->
      expectationToReport path source context region expectation reason


getMaxDepth : JD.Problem x -> Int
getMaxDepth problem =
  case problem of
    JD.Field _ prob  -> 1 + getMaxDepth prob
    JD.Index _ prob  -> 1 + getMaxDepth prob
    JD.OneOf p ps    -> MList.maximum (getMaxDepth p :: MList.map getMaxDepth ps)
    JD.Failure _ _   -> 0
    JD.Expecting _ _ -> 0


type FailureToReport x =
  FailureToReport {- failureToReport -} (FilePath -> Code.Source -> Context -> A.Region -> x -> Help.Report)

getFailureToReport (FailureToReport a) = a


expectationToReport : FilePath -> Code.Source -> Context -> A.Region -> JD.DecodeExpectation -> Reason -> Help.Report
expectationToReport path source context (A.Region start _) expectation reason =
  let
    region =
      A.Region start start

    introduction =
      case context of
        CRoot ->
          "I ran into some trouble here:"

        CField field ->
          "I ran into trouble with the value of the \"" ++ field ++ "\" field:"

        CIndex index (CField field) ->
          "When looking at the \"" ++ field ++ "\" field, I ran into trouble with the "
          ++ D.intToOrdinal index ++ " entry:"

        CIndex index _ ->
          "I ran into trouble with the " ++ D.intToOrdinal index ++ " index of this array:"

    toSnippet title aThing =
      Help.jsonReport title (Just path) <|
        Code.toSnippet source region Nothing
          ( D.reflow (because reason introduction)
          , D.fillSep <| [d"I",d"was",d"expecting",d"to",d"run",d"into"] ++ aThing
          )
  in
  case expectation of
    JD.TObject ->
      toSnippet "EXPECTING OBJECT" [d"an", da[D.greenS "OBJECT",d "."]]

    JD.TArray ->
      toSnippet "EXPECTING ARRAY" [d"an", da[D.greenS "ARRAY", d"."]]

    JD.TString ->
      toSnippet "EXPECTING STRING" [d"a", da[D.greenS "STRING", d"."]]

    JD.TObjectWith field ->
      toSnippet "MISSING FIELD"
        [d"an",D.greenS "OBJECT",d"with",d"a"
        ,D.green (da[d"\"", D.fromChars field, d"\""])
        ,d"field."
        ]
