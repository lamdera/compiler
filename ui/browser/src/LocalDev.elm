module LocalDev exposing (main)

import Backend
import Browser
import Browser.Navigation as Navigation
import Frontend
import Html
import Html.Attributes as A
import Html.Events
import Lamdera.Debug as Lamdera
import Lamdera.Types exposing (ClientId, Milliseconds, MsgId, WsError)
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg, ToBackend, ToFrontend)
import Url exposing (Url)


type Msg
    = FEMsg Types.FrontendMsg
    | BEMsg Types.BackendMsg
    | BEtoFE ClientId Types.ToFrontend
    | FEtoBE Types.ToBackend
    | ResetDebugStore


type alias Model =
    { fem : FrontendModel
    , bem : BackendModel
    , originalUrl : Url.Url
    , originalKey : Navigation.Key
    }


userFrontendApp =
    Frontend.app


userBackendApp =
    Backend.app


init : flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( ifem, iFeCmds ) =
            userFrontendApp.init url key

        ( ibem, iBeCmds ) =
            userBackendApp.init

        ( fem, newFeCmds ) =
            case Lamdera.debugR "fe" ifem of
                Nothing ->
                    ( ifem, iFeCmds )

                Just rfem ->
                    ( rfem, Cmd.none )

        ( bem, newBeCmds ) =
            case Lamdera.debugR "be" ibem of
                Nothing ->
                    ( ibem, iBeCmds )

                Just rbem ->
                    ( rbem, Cmd.none )
    in
    ( { fem = fem
      , bem = bem
      , originalKey = key
      , originalUrl = url
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        FEMsg frontendMsg ->
            let
                x =
                    Debug.log "FEMsg" frontendMsg

                ( newFem, newFeCmds ) =
                    userFrontendApp.update frontendMsg m.fem
            in
            ( { m | fem = Lamdera.debugS "fe" newFem, bem = m.bem }, Cmd.map FEMsg newFeCmds )

        BEMsg backendMsg ->
            let
                x =
                    Debug.log "BEMsg" backendMsg

                ( newBem, newBeCmds ) =
                    userBackendApp.update backendMsg m.bem
            in
            ( { m | fem = m.fem, bem = Lamdera.debugS "be" newBem }, Cmd.map BEMsg newBeCmds )

        BEtoFE clientId toFrontend ->
            let
                x =
                    Debug.log "BEtoFE" toFrontend

                ( newFem, newFeCmds ) =
                    userFrontendApp.updateFromBackend toFrontend m.fem
            in
            ( { m | fem = Lamdera.debugS "fe" newFem, bem = m.bem }, Cmd.map FEMsg newFeCmds )

        FEtoBE toBackend ->
            let
                _ =
                    Debug.log "FEtoBE" toBackend

                ( newBem, newBeCmds ) =
                    userBackendApp.updateFromFrontend "clientIdLocalDev" toBackend m.bem
            in
            ( { m | fem = m.fem, bem = Lamdera.debugS "be" newBem }, Cmd.map BEMsg newBeCmds )

        ResetDebugStore ->
            let
                defaultUrl =
                    -- @TODO improve this in future, maybe keep track of all URLs?
                    { protocol = Url.Http
                    , host = "localhost"
                    , port_ = Just 8000
                    , path = "/lamdera"
                    , query = Nothing
                    , fragment = Nothing
                    }

                ( newFem, newFeCmds ) =
                    userFrontendApp.init defaultUrl m.originalKey

                ( newBem, newBeCmds ) =
                    userBackendApp.init
            in
            ( { m
                | fem = Lamdera.debugS "fe" newFem
                , bem = Lamdera.debugS "be" newBem
              }
            , Cmd.batch
                [ Cmd.map FEMsg newFeCmds
                , Cmd.map BEMsg newBeCmds
                ]
            )


subscriptions { fem, bem } =
    Sub.batch
        [ Sub.map FEMsg (userFrontendApp.subscriptions fem)
        , Sub.map BEMsg (userBackendApp.subscriptions bem)
        ]


lamderaPane =
    Html.div []
        [ Html.span
            [ Html.Events.onClick ResetDebugStore
            , A.style "font-family" "sans-serif"
            , A.style "font-size" "12px"
            , A.style "padding" "5px"
            , A.style "color" "#fff"
            , A.style "background-color" "#61b6cd"
            , A.style "display" "inline-block"
            , A.style "position" "absolute"
            , A.style "top" "0"
            , A.style "left" "0"
            , A.style "z-index" "100"
            ]
            [ Html.text "Reset" ]
        ]


mapDocument msg { title, body } =
    { title = title, body = [ lamderaPane ] ++ List.map (Html.map msg) body }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = \{ fem } -> mapDocument FEMsg (userFrontendApp.view fem)
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \url -> FEMsg (userFrontendApp.onUrlRequest url)
        , onUrlChange = \ureq -> FEMsg (userFrontendApp.onUrlChange ureq)
        }
