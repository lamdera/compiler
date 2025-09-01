{- MANUALLY FORMATTED -}
module Compiler.Reporting.Error.Pattern exposing
  ( {-P.Error(..)
  ,-} toReport
  )


import Compiler.Elm.String as ES
import Compiler.Nitpick.PatternMatches as P
import Compiler.Reporting.Doc as D exposing (d, da)
import Compiler.Reporting.Render.Code as Code
import Compiler.Reporting.Report as Report
import Extra.Type.List as MList exposing (TList)



-- TO REPORT


toReport : Code.Source -> P.Error -> Report.Report
toReport source err =
  case err of
    P.Redundant caseRegion patternRegion index ->
      Report.Report "REDUNDANT PATTERN" patternRegion <|
        Code.toSnippet source caseRegion (Just patternRegion)
          (
            D.reflow <|
              "The " ++ D.intToOrdinal index ++ " pattern is redundant:"
          ,
            D.reflow <|
              "Any value with this shape will be handled by a previous"
              ++ " pattern, so it should be removed."
          )

    P.Incomplete region context unhandled ->
      case context of
        P.BadArg ->
          Report.Report "UNSAFE PATTERN" region <|
            Code.toSnippet source region Nothing
              (
                d"This pattern does not cover all possibilities:"
              ,
                D.stack
                  [ d"Other possibilities include:"
                  , unhandledPatternsToDocBlock unhandled
                  , D.reflow <|
                      "I would have to crash if I saw one of those! So rather than"
                      ++ " pattern matching in function arguments, put a `case` in"
                      ++ " the function body to account for all possibilities."
                  ]
              )

        P.BadDestruct ->
          Report.Report "UNSAFE PATTERN" region <|
            Code.toSnippet source region Nothing
              (
                d"This pattern does not cover all possible values:"
              ,
                D.stack
                  [ d"Other possibilities include:"
                  , unhandledPatternsToDocBlock unhandled
                  , D.reflow <|
                      "I would have to crash if I saw one of those! You can use"
                      ++ " `let` to deconstruct values only if there is ONE possibility."
                      ++ " Switch to a `case` expression to account for all possibilities."
                  , D.toSimpleHint <|
                      "Are you calling a function that definitely returns values"
                      ++ " with a very specific shape? Try making the return type of"
                      ++ " that function more specific!"
                  ]
              )

        P.BadCase ->
          Report.Report "MISSING PATTERNS" region <|
            Code.toSnippet source region Nothing
              (
                d"This `case` does not have branches for all possibilities:"
              ,
                D.stack
                  [ d"Missing possibilities include:"
                  , unhandledPatternsToDocBlock unhandled
                  , D.reflow <|
                      "I would have to crash if I saw one of those. Add branches for them!"
                  , D.link "Hint"
                      "If you want to write the code for each branch later, use `Debug.todo` as a placeholder. Read"
                      "missing-patterns"
                      "for more guidance on this workflow."
                  ]
              )



-- PATTERN TO DOC


unhandledPatternsToDocBlock : TList P.Pattern -> D.Doc
unhandledPatternsToDocBlock unhandledPatterns =
  D.indent 4 <| D.dullyellow <| D.vcat <|
    MList.map (patternToDoc Unambiguous) unhandledPatterns


type Context
  = Arg
  | Head
  | Unambiguous


patternToDoc : Context -> P.Pattern -> D.Doc
patternToDoc context pattern =
  case delist pattern [] of
    NonList P.Anything ->
      d"_"

    NonList (P.Literal literal) ->
      case literal of
        P.Chr chr ->
          da[d"'", D.fromChars (ES.toChars chr), d"'"]

        P.Str str ->
          da[d"\"", D.fromChars (ES.toChars str), d"\""]

        P.CInt int ->
          D.fromInt int

    NonList (P.Ctor _ "#0" []) ->
      d"()"

    NonList (P.Ctor _ "#2" [a,b]) ->
      da[d"( ", patternToDoc Unambiguous a,
      d", ", patternToDoc Unambiguous b,
      d" )"]

    NonList (P.Ctor _ "#3" [a,b,c]) ->
      da[d"( ", patternToDoc Unambiguous a,
      d", ", patternToDoc Unambiguous b,
      d", ", patternToDoc Unambiguous c,
      d" )"]

    NonList (P.Ctor _ name args) ->
      let
        ctorDoc =
          D.hsep (D.fromName name :: MList.map (patternToDoc Arg) args)
      in
      if context == Arg && MList.length args > 0 then
        da[d"(", ctorDoc, d")"]
      else
        ctorDoc

    FiniteList [] ->
      d"[]"

    FiniteList entries ->
      let entryDocs = MList.map (patternToDoc Unambiguous) entries in
      da[d"[", D.hcat (MList.intersperse (d",") entryDocs), d"]"]

    Conses conses finalPattern ->
      let
        consDoc =
          MList.foldr
            (\hd tl -> da[patternToDoc Head hd, d" :: ", tl])
            (patternToDoc Unambiguous finalPattern)
            conses
      in
      if context == Unambiguous then
        consDoc
      else
        da[d"(", consDoc, d")"]


type Structure
  = FiniteList (TList P.Pattern)
  | Conses (TList P.Pattern) P.Pattern
  | NonList P.Pattern


delist : P.Pattern -> TList P.Pattern -> Structure
delist pattern revEntries =
  case pattern of
    P.Ctor _ "[]" [] ->
      FiniteList revEntries

    P.Ctor _ "::" [hd,tl] ->
      delist tl (hd::revEntries)

    _ ->
      case revEntries of
        [] ->
          NonList pattern

        _ ->
          Conses (MList.reverse revEntries) pattern
