module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Env
import External
import Html
import Html.Attributes as Attr
import Lamdera
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        selfRef =
            -- This causes the `init` expression to change from a AST.Optimized.Define to a AST.Optimized.Cycle
            -- which in the past wasn't properly handled by AppConfig.findSecretUses. This is a regression test.
            init

        both =
            Env.both

        test =
            -- This gives us a mututally recursive function AST.Optimized.Cycle with two names. This is a regression test.
            \_ -> odd 1
    in
    ( { key = key
      , message =
            "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding!"
                ++ " frontendOnly:"
                ++ Env.frontendOnly
                ++ " both:"
                ++ both
      }
    , Cmd.none
    )


odd n =
    if n == 0 then
        False

    else
        even (n - 1)


even n =
    if n == 0 then
        let
            external =
                -- This calls Env.external
                External.something
        in
        True

    else
        odd (n - 1)


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view model =
    { title = ""
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text model.message ]
            ]
        ]
    }
