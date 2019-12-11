module Frontend exposing (Model, app)

import Html exposing (Html, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Lamdera.Frontend
import Lamdera.Types exposing (Milliseconds)
import Types exposing (..)


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.Frontend.application is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Lamdera.Frontend.application
        { init = \_ _ -> init
        , update = update
        , updateFromBackend = updateFromBackend
        , view =
            \model ->
                { title = "Lamdera counter app"
                , body = [ view model ]
                }
        , subscriptions = \_ -> Sub.none
        , onUrlChange = \_ -> FNoop
        , onUrlRequest = \_ -> FNoop
        }


type alias Model =
    { counter : Int }


init : ( Model, Cmd FrontendMsg )
init =
    ( { counter = 0 }, sendToBackend ClientJoin )


view : Model -> Html FrontendMsg
view model =
    Html.div [ Html.Attributes.style "padding" "30px" ]
        [ Html.button [ onClick Increment ] [ text "+" ]
        , Html.text (String.fromInt model.counter)
        , Html.button [ onClick Decrement ] [ text "-" ]
        ]


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        Increment ->
            ( { model | counter = model.counter + 1 }, sendToBackend CounterIncremented )

        Decrement ->
            ( { model | counter = model.counter - 1 }, sendToBackend CounterDecremented )

        FNoop ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        CounterNewValue newValue ->
            ( { model | counter = newValue }, Cmd.none )


sendToBackend : Msg.ToBackend -> Cmd Msg.FrontendMsg
sendToBackend msg =
    Lamdera.Frontend.sendToBackend 1000 (\_ -> FNoop) msg
