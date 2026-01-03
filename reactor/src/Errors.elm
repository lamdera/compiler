-- @LAMDERA: Added `jumpTo` from `worker/src/Errors.elm` and adapted it for Lamdera.
port module Errors exposing (main)


import Browser
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String
import Json.Decode as D
import Elm.Error as Error



-- PORTS


port jumpTo : String -> Cmd msg


jumpUrl : String -> Error.Region -> String
jumpUrl filePath region =
  "/_x/editor/" ++ filePath ++ "?row=" ++ String.fromInt region.start.line ++ "&column=" ++ String.fromInt region.start.column



-- MAIN


main =
  Browser.document
    { init = \flags -> (D.decodeValue Error.decoder flags, Cmd.none)
    , update = \(filePath, region) result -> (result, jumpTo (jumpUrl filePath region))
    , view = view
    , subscriptions = \_ -> Sub.none
    }


type alias Msg = (String, Error.Region)



-- VIEW


view : Result D.Error Error.Error -> Browser.Document Msg
view result =
  { title = "Problem!"
  , body =
      case result of
        Err err ->
          [ text (D.errorToString err) ]

        Ok error ->
          [ viewError error ]
  }


viewError : Error.Error -> Html Msg
viewError error =
  div
    [ style "width" "100%"
    , style "min-height" "100%"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "center"
    , style "background-color" "rgb(39, 40, 34)"
    , style "color" "rgb(233, 235, 235)"
    , style "font-family" "monospace"
    ]
    [ div
        [ style "display" "block"
        , style "white-space" "pre-wrap"
        , style "background-color" "black"
        , style "padding" "2em"
        , style "box-sizing" "border-box"
        , style "width" "100%"
        , style "overflow" "scroll"
        ]
        (viewErrorHelp error)
    ]


viewErrorHelp : Error.Error -> List (Html Msg)
viewErrorHelp error =
  case error of
    Error.GeneralProblem { path, title, message } ->
      viewHeader title path Nothing :: viewMessage message

    Error.ModuleProblems badModules ->
      viewBadModules badModules



-- VIEW HEADER


viewHeader : String -> Maybe String -> Maybe Error.Region -> Html Msg
viewHeader title maybeFilePath maybeRegion =
  let
    left = "-- " ++ title ++ " "

    (rightLength, rightElements) =
      case (maybeFilePath, maybeRegion) of
        (Just filePath, Nothing) ->
          let
            fullText =
              " " ++ filePath
          in
          ( String.length fullText
          , [text fullText]
          )

        (Just filePath, Just region) ->
          let
            fullText =
              filePath ++ ":" ++ String.fromInt region.start.line ++ ":" ++ String.fromInt region.start.column
          in
          ( 1 + String.length fullText
          , [ text " "
            , span
                [ style "cursor" "pointer"
                , style "text-decoration" "underline"
                , onClick (filePath, region)
                ]
                [ text fullText ]

            ]
          )

        _ ->
          (0, [])
  in
  span [ style "color" "rgb(51,187,200)" ]
    ( text (left ++ String.repeat (80 - String.length left - rightLength) "-")
      :: rightElements
      ++ [ text "\n\n" ]
    )



-- VIEW BAD MODULES


viewBadModules : List Error.BadModule -> List (Html Msg)
viewBadModules badModules =
  case badModules of
    [] ->
      []

    [badModule] ->
      [viewBadModule badModule]

    a :: b :: cs ->
      viewBadModule a :: viewSeparator a.name b.name :: viewBadModules (b :: cs)


viewBadModule : Error.BadModule -> Html Msg
viewBadModule { path, problems } =
  span [] (List.map (viewProblem path) problems)


viewProblem : String -> Error.Problem -> Html Msg
viewProblem filePath problem =
  span [] (viewHeader problem.title (Just filePath) (Just problem.region) :: viewMessage problem.message)


viewSeparator : String -> String -> Html msg
viewSeparator before after =
  span [ style "color" "rgb(211,56,211)" ]
    [ text <|
        String.padLeft 80 ' ' (before ++ "  ↑    ") ++ "\n" ++
        "====o======================================================================o====\n" ++
        "    ↓  " ++ after ++ "\n\n\n"
    ]



-- VIEW MESSAGE


viewMessage : List Error.Chunk -> List (Html msg)
viewMessage chunks =
  case chunks of
    [] ->
      [ text "\n\n\n" ]

    chunk :: others ->
      let
        htmlChunk =
          case chunk of
            Error.Unstyled string ->
              text string

            Error.Styled style string ->
              span (styleToAttrs style) [ text string ]
      in
      htmlChunk :: viewMessage others


styleToAttrs : Error.Style -> List (Attribute msg)
styleToAttrs { bold, underline, color } =
  addBold bold <| addUnderline underline <| addColor color []


addBold : Bool -> List (Attribute msg) -> List (Attribute msg)
addBold bool attrs =
  if bool then
    style "font-weight" "bold" :: attrs
  else
    attrs


addUnderline : Bool -> List (Attribute msg) -> List (Attribute msg)
addUnderline bool attrs =
  if bool then
    style "text-decoration" "underline" :: attrs
  else
    attrs


addColor : Maybe Error.Color -> List (Attribute msg) -> List (Attribute msg)
addColor maybeColor attrs =
  case maybeColor of
    Nothing ->
      attrs

    Just color ->
      style "color" (colorToCss color) :: attrs


colorToCss : Error.Color -> String
colorToCss color =
  case color of
    Error.Red -> "rgb(194,54,33)"
    Error.RED -> "rgb(252,57,31)"
    Error.Magenta -> "rgb(211,56,211)"
    Error.MAGENTA -> "rgb(249,53,248)"
    Error.Yellow -> "rgb(173,173,39)"
    Error.YELLOW -> "rgb(234,236,35)"
    Error.Green -> "rgb(37,188,36)"
    Error.GREEN -> "rgb(49,231,34)"
    Error.Cyan -> "rgb(51,187,200)"
    Error.CYAN -> "rgb(20,240,240)"
    Error.Blue -> "rgb(73,46,225)"
    Error.BLUE -> "rgb(88,51,255)"
    Error.White -> "rgb(203,204,205)"
    Error.WHITE -> "rgb(233,235,235)"
    Error.Black -> "rgb(0,0,0)"
    Error.BLACK -> "rgb(129,131,131)"
