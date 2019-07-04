module LocalDev exposing (main)

import Backend
import Browser
import Browser.Navigation as Navigation
import Frontend
import Html
import Lamdera.Types exposing (Milliseconds, MsgId, WsError)
import Msg exposing (BackendMsg, FrontendMsg, ToBackend, ToFrontend)
import Url exposing (Url)


{-| WARNING: do not change the types in this module, they must be as is for `lamdera reactor` to work
This local development harness is temporary and will be integrated into the reactor in future
-}
type Msg
    = FEMsg Msg.FrontendMsg
    | BEMsg Msg.BackendMsg
    | BEtoFE Milliseconds MsgId Msg.ToFrontend (Result WsError () -> Msg)
    | FEtoBE Milliseconds MsgId Msg.ToBackend (Result WsError () -> Msg)


init : flags -> Url.Url -> Navigation.Key -> ( ( Frontend.Model, Backend.Model ), Cmd Msg )
init flags url nav =
    let
        ( feim, fecmd ) =
            Frontend.app.init url nav

        ( beim, becmd ) =
            Backend.app.init
    in
    ( ( feim, beim ), Cmd.batch [ Cmd.map FEMsg fecmd, Cmd.map BEMsg becmd ] )


update : Msg -> ( Frontend.Model, Backend.Model ) -> ( ( Frontend.Model, Backend.Model ), Cmd Msg )
update msg ( fem, bem ) =
    case msg of
        FEMsg frontendMsg ->
            let
                ( nfem, fcmd ) =
                    Frontend.app.update frontendMsg fem

                _ =
                    Debug.log "FEMsg" frontendMsg
            in
            ( ( nfem, bem ), Cmd.map FEMsg fcmd )

        BEMsg backendMsg ->
            let
                ( nbem, bcmd ) =
                    Backend.app.update backendMsg bem

                _ =
                    Debug.log "BEMsg" backendMsg
            in
            ( ( fem, nbem ), Cmd.map BEMsg bcmd )

        BEtoFE ms msgId toFrontend tagger ->
            let
                ( nfem, fcmd ) =
                    Frontend.app.updateFromBackend toFrontend fem

                ( newModel, ncmd ) =
                    -- TODO: delay response by `ms` milliseconds
                    update (tagger (Ok ())) ( nfem, bem )

                _ =
                    Debug.log "BEtoFE" ( toFrontend, tagger (Ok ()) )
            in
            ( newModel, Cmd.batch [ Cmd.map FEMsg fcmd, ncmd ] )

        FEtoBE ms msgId toBackend tagger ->
            let
                ( nbem, bcmd ) =
                    Backend.app.updateFromFrontend "ClientID-local-dev" toBackend bem

                ( newModel, ncmd ) =
                    -- TODO: delay response by `ms` milliseconds
                    update (tagger (Ok ())) ( fem, nbem )

                _ =
                    Debug.log "FEtoBE" ( toBackend, tagger (Ok ()) )
            in
            ( newModel, Cmd.batch [ Cmd.map BEMsg bcmd, ncmd ] )


subscriptions ( fem, _ ) =
    Sub.map FEMsg (Frontend.app.subscriptions fem)


mapDocument fn { title, body } =
    { title = title, body = List.map (Html.map fn) body }


main : Program () ( Frontend.Model, Backend.Model ) Msg
main =
    Browser.application
        { init = init
        , view = \( fem, _ ) -> mapDocument FEMsg (Frontend.app.view fem)
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \url -> FEMsg (Frontend.app.onUrlRequest url)
        , onUrlChange = \ureq -> FEMsg (Frontend.app.onUrlChange ureq)
        }
