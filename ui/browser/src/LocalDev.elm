module LocalDev exposing (main)

import Backend
import Browser
import Browser.Navigation as Navigation
import Frontend
import Html
import Lamdera.Types exposing (ClientId, Milliseconds, MsgId, WsError)
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg, ToBackend, ToFrontend)
import Url exposing (Url)


type Msg
    = FEMsg Types.FrontendMsg
    | BEMsg Types.BackendMsg
    | BEtoFE ClientId Types.ToFrontend
    | FEtoBE Milliseconds MsgId Types.ToBackend (Result WsError () -> Msg)


userFrontendApp =
    Frontend.app


userBackendApp =
    Backend.app


init : flags -> Url.Url -> Navigation.Key -> ( ( FrontendModel, BackendModel ), Cmd Msg )
init flags url nav =
    let
        ( userFrontendModel, userFrontendCmds ) =
            userFrontendApp.init url nav

        ( userBackendModel, userBackendCmds ) =
            userBackendApp.init
    in
    ( ( userFrontendModel, userBackendModel )
    , Cmd.batch
        [ Cmd.map FEMsg userFrontendCmds
        , Cmd.map BEMsg userBackendCmds
        ]
    )


update : Msg -> ( FrontendModel, BackendModel ) -> ( ( FrontendModel, BackendModel ), Cmd Msg )
update msg ( userFrontendModel, userBackendModel ) =
    case msg of
        FEMsg frontendMsg ->
            let
                x =
                    Debug.log "FEMsg" frontendMsg

                ( newUserFrontendModel, newUserFrontendCmds ) =
                    userFrontendApp.update frontendMsg userFrontendModel
            in
            ( ( newUserFrontendModel, userBackendModel ), Cmd.map FEMsg newUserFrontendCmds )

        BEMsg backendMsg ->
            let
                x =
                    Debug.log "BEMsg" backendMsg

                ( newUserBackendModel, newUserBackendCmds ) =
                    userBackendApp.update backendMsg userBackendModel
            in
            ( ( userFrontendModel, newUserBackendModel ), Cmd.map BEMsg newUserBackendCmds )

        BEtoFE clientId toFrontend ->
            let
                x =
                    Debug.log "BEtoFE" toFrontend

                ( newUserFrontendModel, newUserFrontendCmds ) =
                    userFrontendApp.updateFromBackend toFrontend userFrontendModel
            in
            ( ( newUserFrontendModel, userBackendModel ), Cmd.map FEMsg newUserFrontendCmds )

        FEtoBE ms msgId toBackend tagger ->
            let
                _ =
                    Debug.log "FEtoBE" toBackend

                ( newUserBackendModel, newUserBackendCmds ) =
                    userBackendApp.updateFromFrontend "clientIdLocalDev" toBackend userBackendModel
            in
            ( ( userFrontendModel, newUserBackendModel ), Cmd.map BEMsg newUserBackendCmds )


subscriptions ( userFrontendModel, userBackendModel ) =
    Sub.batch
        [ Sub.map FEMsg (userFrontendApp.subscriptions userFrontendModel)
        , Sub.map BEMsg (userBackendApp.subscriptions userBackendModel)
        ]


mapDocument fn { title, body } =
    { title = title, body = List.map (Html.map fn) body }


main : Program () ( FrontendModel, BackendModel ) Msg
main =
    Browser.application
        { init = init
        , view = \( userFrontendModel, _ ) -> mapDocument FEMsg (userFrontendApp.view userFrontendModel)
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \url -> FEMsg (userFrontendApp.onUrlRequest url)
        , onUrlChange = \ureq -> FEMsg (userFrontendApp.onUrlChange ureq)
        }
