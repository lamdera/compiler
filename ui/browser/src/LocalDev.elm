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
    , originalKey : Navigation.Key
    }


userFrontendApp =
    Frontend.app


userBackendApp =
    Backend.app


init : flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( fem, userFrontendCmds ) =
            userFrontendApp.init url key

        ( bem, userBackendCmds ) =
            userBackendApp.init
    in
    ( { fem = Lamdera.debugR "fe" fem
      , bem = Lamdera.debugR "be" bem
      , originalKey = key
      }
    , Cmd.batch
        [ Cmd.map FEMsg userFrontendCmds
        , Cmd.map BEMsg userBackendCmds
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        FEMsg frontendMsg ->
            let
                x =
                    Debug.log "FEMsg" frontendMsg

                ( newUserFrontendModel, newUserFrontendCmds ) =
                    userFrontendApp.update frontendMsg m.fem
            in
            ( { m | fem = Lamdera.debugS "fe" newUserFrontendModel, bem = m.bem }, Cmd.map FEMsg newUserFrontendCmds )

        BEMsg backendMsg ->
            let
                x =
                    Debug.log "BEMsg" backendMsg

                ( newUserBackendModel, newUserBackendCmds ) =
                    userBackendApp.update backendMsg m.bem
            in
            ( { m | fem = m.fem, bem = Lamdera.debugS "be" newUserBackendModel }, Cmd.map BEMsg newUserBackendCmds )

        BEtoFE clientId toFrontend ->
            let
                x =
                    Debug.log "BEtoFE" toFrontend

                ( newUserFrontendModel, newUserFrontendCmds ) =
                    userFrontendApp.updateFromBackend toFrontend m.fem
            in
            ( { m | fem = Lamdera.debugS "fe" newUserFrontendModel, bem = m.bem }, Cmd.map FEMsg newUserFrontendCmds )

        FEtoBE toBackend ->
            let
                _ =
                    Debug.log "FEtoBE" toBackend

                ( newUserBackendModel, newUserBackendCmds ) =
                    userBackendApp.updateFromFrontend "clientIdLocalDev" toBackend m.bem
            in
            ( { m | fem = m.fem, bem = Lamdera.debugS "be" newUserBackendModel }, Cmd.map BEMsg newUserBackendCmds )

        ResetDebugStore ->
            -- ( { m | fem = fem, bem = bem }, Cmd.none )
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

                ( femi, userFrontendCmds ) =
                    userFrontendApp.init defaultUrl m.originalKey

                ( bemi, userBackendCmds ) =
                    userBackendApp.init
            in
            ( { m
                | fem = Lamdera.debugS "fe" femi
                , bem = Lamdera.debugS "be" bemi
              }
            , Navigation.reloadAndSkipCache
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
            , A.style "padding" "4px"
            , A.style "color" "#fff"
            , A.style "background-color" "#61b6cd"
            , A.style "display" "inline-block"
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
