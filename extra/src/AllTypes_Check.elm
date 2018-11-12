module AllTypes_Check exposing (Model, Msg(..), allTypesMock, init, main, subscriptions, unionMocks, update, view)

import AllTypes exposing (..)
import Array
import Browser exposing (Document)
import Debug
import Dict
import Element exposing (..)
import Element.Background as Background
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



-- anotherMocks =
--     [ Test, Best ]


view : Model -> { body : List (Html.Html msg), title : String }
view model =
    let
        encoded =
            E.encode 0 (AllTypes.evg_e_AllTypes allTypesMock)

        decoded =
            D.decodeString AllTypes.evg_d_AllTypes encoded

        recordsMatching =
            Ok allTypesMock == decoded

        e2 =
            E.encode 0 (E.list AllTypes.evg_e_Union unionMocks)

        d2 =
            D.decodeString (D.list AllTypes.evg_d_Union) e2

        customTypesMatching =
            Ok unionMocks == d2

        -- e3 =
        --     E.encode 0 (E.list AllTypes.evg_e_Another anotherMocks)
        -- d3 =
        --     D.decodeString (D.list AllTypes.evg_d_Another) e2
    in
    { title = "Hello"
    , body =
        [ layout [] <|
            column [ spacing 10, padding 10 ]
                [ row [ padding 10 ] [ paragraph [] [ text <| "Encoded AllTypes: " ++ encoded ] ]
                , row [ padding 10 ] [ paragraph [] [ text <| "Decoded AllTypes: " ++ Debug.toString decoded ] ]
                , if recordsMatching then
                    row [ padding 10, Background.color (rgb255 186 255 188) ] [ paragraph [] [ text <| "Record equal to original? " ++ Debug.toString recordsMatching ] ]

                  else
                    row [ padding 10, Background.color (rgb255 255 179 186) ] [ paragraph [] [ text <| "Record equal to original? " ++ Debug.toString recordsMatching ] ]
                , row [ padding 10 ] [ paragraph [] [ text <| "Encoded Unions: " ++ e2 ] ]
                , row [ padding 10 ] [ paragraph [] [ text <| "Decoded Unions: " ++ Debug.toString d2 ] ]
                , if customTypesMatching then
                    row [ padding 10, Background.color (rgb255 186 255 188) ] [ paragraph [] [ text <| "Custom type equal to original? " ++ Debug.toString customTypesMatching ] ]

                  else
                    row [ padding 10, Background.color (rgb255 255 179 186) ] [ paragraph [] [ text <| "Custom type equal to original? " ++ Debug.toString customTypesMatching ] ]

                -- , row [] [ Html.text <| "Encoded Another: " ++ e3 ]
                -- , row [] [ Html.text <| "Shadow value not existent in code: " ++ Debug.toString AllTypes.evg ]
                ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
