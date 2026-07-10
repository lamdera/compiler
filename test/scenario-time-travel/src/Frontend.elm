module Frontend exposing (app)

import Browser
import Browser.Navigation
import Html exposing (Html, button, div, h3, text)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onClick)
import Lamdera
import Types exposing (..)
import Url


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : Url.Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ key =
    ( { key = key, counter = 0, localCounter = 0 }, Cmd.none )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        Increment ->
            ( model, Lamdera.sendToBackend CounterIncremented )

        Decrement ->
            ( model, Lamdera.sendToBackend CounterDecremented )

        LocalIncrement ->
            ( { model | localCounter = model.localCounter + 1 }, Cmd.none )

        LocalDecrement ->
            ( { model | localCounter = model.localCounter - 1 }, Cmd.none )

        UrlClicked _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        CounterNewValue value ->
            ( { model | counter = value }, Cmd.none )


counterRow : String -> String -> FrontendMsg -> FrontendMsg -> Int -> Html FrontendMsg
counterRow prefix label decMsg incMsg value =
    div []
        [ h3 [] [ text label ]
        , div
            [ style "display" "flex"
            , style "align-items" "center"
            , style "gap" "16px"
            ]
            [ button [ id (prefix ++ "dec"), onClick decMsg, style "font-size" "24px", style "width" "48px" ] [ text "-" ]
            , div [ id (prefix ++ "counter"), style "font-size" "32px", style "min-width" "60px", style "text-align" "center" ]
                [ text (String.fromInt value) ]
            , button [ id (prefix ++ "inc"), onClick incMsg, style "font-size" "24px", style "width" "48px" ] [ text "+" ]
            ]
        ]


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Time Travel Demo"
    , body =
        [ div [ style "padding" "40px", style "font-family" "sans-serif" ]
            [ counterRow "" "Backend counter (shared between all tabs)" Decrement Increment model.counter
            , counterRow "local-" "Local counter (this tab only, never touches the backend)" LocalDecrement LocalIncrement model.localCounter
            ]
        ]
    }
