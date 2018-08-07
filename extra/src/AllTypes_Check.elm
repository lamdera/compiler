module AllTypes_Check exposing (..)

import AllTypes exposing (..)
import AllTypes_Gen
import Array
import Browser exposing (Document)
import Debug
import Dict
import Html
import Json.Decode as D
import Json.Encode as E
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


allTypesMock =
    { int = 1
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


unionMocks =
    [ Leaf
    , Recursive (Recursive (Recursive (DeeplyValued [ True, False ])))
    , Valued 123
    , DeeplyValued [ False, False, False ]
    , Leaf
    ]


view model =
    let
        encoded =
            E.encode 0 (AllTypes_Gen.evg_e_AllTypes allTypesMock)

        decoded =
            D.decodeString AllTypes_Gen.evg_d_AllTypes encoded

        e2 =
            E.encode 0 (E.list AllTypes_Gen.evg_e_Union unionMocks)

        d2 =
            D.decodeString (D.list AllTypes_Gen.evg_d_Union) e2
    in
    { title = "Hello"
    , body =
        [ Html.div [] [ Html.text <| "Encoded AllTypes: " ++ encoded ]
        , Html.div [] [ Html.text <| "Decoded AllTypes: " ++ Debug.toString decoded ]
        , Html.div [] [ Html.text <| "Equality to original? " ++ Debug.toString (Ok allTypesMock == decoded) ]
        , Html.div [] [ Html.text <| "Encoded Unions: " ++ e2 ]
        , Html.div [] [ Html.text <| "Decoded Unions: " ++ Debug.toString d2 ]
        ]
    }


update msg model =
    ( model, Cmd.none )


subscriptions model =
    Sub.none
