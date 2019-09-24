module AllTypes_Check exposing (..)

import AllTypes exposing (..)
import Array
import Browser exposing (Document)
import Bytes.Decode as D
import Bytes.Encode as E
import Debug
import Dict
import Element exposing (..)
import Element.Background as Background
import Html
import Lamdera.Evergreen
import Msg exposing (..)
import Result exposing (Result(..))
import Set
import Time


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Noop


type alias Model =
    { x : Int }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { x = 1 }, Cmd.none )


allTypesMocks =
    [ { int = 1
      , float = 1.3
      , bool = True
      , char = 'c'
      , string = "test"
      , listInt = [ 3, 2, 1 ]
      , setFloat = Set.fromList [ 1.2, 2.3, 3.4 ]
      , arrayString = Array.fromList [ "herp", "derp" ]
      , dict = Dict.fromList [ ( "Nice", [ 1, 2, 3 ] ), ( "Twice", [ 3, 2, 1 ] ) ]
      , time = Time.millisToPosix 1000000000
      , order = LT
      , union = Recursive Leaf
      , unit = ()
      }
    ]


aliasedInt : AliasInt
aliasedInt =
    444


unionMocks =
    [ Leaf
    , ValueInt 123
    , ValueFloat 8.9
    , ValueBool True
    , ValueChar 'C'
    , ValueString "Stringy!"
    , ValueListBool [ False, False, False ]
    , ValueSetFloat (Set.fromList [ 6.1, 7.4, 8.9 ])
    , ValueArrayString (Array.fromList [ "Hello", "Yellow" ])
    , ValueDict (Dict.fromList [ ( "first", [ 23, 24, 25 ] ), ( "second", [ 58, 12, 98 ] ) ])
    , ValueOrder GT
    , ValueUnit ()
    , Aliased aliasedInt
    , Recursive (Recursive (Recursive (ValueListBool [ True, False ])))
    , ValueTwoParams False 'c'
    , ValueTuple ( 312, "Testing" )
    , ValueResult (Ok 42)
    , ValueResult (Err "Oops!")
    ]


view : Model -> { body : List (Html.Html msg), title : String }
view model =
    { title = "Hello"
    , body =
        [ layout [] <|
            column [ spacing 10, padding 10 ]
                [ encodeDecodeCheck "AllTypes" allTypesMocks AllTypes.evg_encode_AllTypes AllTypes.evg_decode_AllTypes
                , encodeDecodeCheck "Unions" unionMocks AllTypes.evg_encode_Union AllTypes.evg_decode_Union
                , encodeDecodeCheck "Herp" [ Derp "arg1" "arg2" ] Msg.evg_encode_Herp Msg.evg_decode_Herp
                , encodeDecodeCheck "Referenced" [ Root, Wrapped (Derp "test" "best") ] AllTypes.evg_encode_Referenced AllTypes.evg_decode_Referenced
                , encodeDecodeCheck "ReferencedRecord" [ { wrapped = Derp "test" "nest" } ] AllTypes.evg_encode_ReferencedRecord AllTypes.evg_decode_ReferencedRecord
                ]
        ]
    }


encodeDecodeCheck : String -> List a -> (a -> Lamdera.Evergreen.Encoder) -> D.Decoder a -> Element msg
encodeDecodeCheck label mock encoder decoder =
    let
        roundtripMatches =
            Ok mock == decoded

        encoded =
            E.encode 0 (E.list encoder mock)

        decoded =
            D.decodeString (D.list decoder) encoded
    in
    column []
        [ row [ padding 10 ] [ paragraph [] [ text <| "Encoded " ++ label ++ ": " ++ encoded ] ]
        , row [ padding 10 ] [ paragraph [] [ text <| "Decoded " ++ label ++ ": " ++ Debug.toString decoded ] ]
        , if roundtripMatches then
            row [ padding 10, Background.color (rgb255 186 255 188) ] [ paragraph [] [ text <| label ++ " equal to original? " ++ Debug.toString roundtripMatches ] ]

          else
            row [ padding 10, Background.color (rgb255 255 179 186) ] [ paragraph [] [ text <| label ++ " equal to original? " ++ Debug.toString roundtripMatches ] ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
