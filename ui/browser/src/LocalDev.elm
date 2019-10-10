module LocalDev exposing (main)

import Backend
import Browser
import Browser.Navigation as Navigation
import Frontend
import Html
import Lamdera.Types exposing (Milliseconds, MsgId, WsError)
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg, ToBackend, ToFrontend)
import Url exposing (Url)


{-| WARNING: do not change the types in this module, they must be as is for `lamdera reactor` to work
-}
type Msg
    = FEMsg Types.FrontendMsg
    | BEMsg Types.BackendMsg
    | BEtoFE Milliseconds MsgId Types.ToFrontend (Result WsError () -> Msg)
    | FEtoBE Milliseconds MsgId Types.ToBackend (Result WsError () -> Msg)


fApp =
    Frontend.app
        { inPort = Debug.todo "F.inPort"
        , outPort = Debug.todo "F.outPort"
        , encodeToBackend = Debug.todo "F.encodeToBackend"
        , decodeToBackend = Debug.todo "F.decodeToBackend"
        , encodeToFrontend = Debug.todo "F.encodeToFrontend"
        , decodeToFrontend = Debug.todo "F.decodeToFrontend"
        }


bApp =
    Backend.app
        { inPort = Debug.todo "B.inPort"
        , outPort = Debug.todo "B.outPort"
        , requestSnapshot = Debug.todo "B.requestSnapshot"
        , createSnapshot = Debug.todo "B.createSnapshot"
        , restoreSnapshot = Debug.todo "B.restoreSnapshot"
        , enterUpgradeMode = Debug.todo "B.enterUpgradeMode"
        , createFinalSnapshot = Debug.todo "B.createFinalSnapshot"
        , redirectMsg = Debug.todo "B.redirectMsg"
        , encodeBackendModel = Debug.todo "B.encodeBackendModel"
        , decodeBackendModel = Debug.todo "B.decodeBackendModel"
        , encodeToBackend = Debug.todo "B.encodeToBackend"
        , decodeToBackend = Debug.todo "B.decodeToBackend"
        , encodeToFrontend = Debug.todo "B.encodeToFrontend"
        , decodeToFrontend = Debug.todo "B.decodeToFrontend"
        }


init : flags -> Url.Url -> Navigation.Key -> ( ( FrontendModel, BackendModel ), Cmd Msg )
init flags url nav =
    let
        ( feim, fecmd ) =
            fApp.init url nav

        ( beim, becmd ) =
            bApp.init
    in
    ( ( feim, beim ), Cmd.batch [ Cmd.map FEMsg fecmd, Cmd.map BEMsg becmd ] )


update : Msg -> ( FrontendModel, BackendModel ) -> ( ( FrontendModel, BackendModel ), Cmd Msg )
update msg ( fem, bem ) =
    case msg of
        FEMsg frontendMsg ->
            let
                ( nfem, fcmd ) =
                    fApp.update frontendMsg fem

                _ =
                    Debug.log "FEMsg" frontendMsg
            in
            ( ( nfem, bem ), Cmd.map FEMsg fcmd )

        BEMsg backendMsg ->
            let
                ( nbem, bcmd ) =
                    bApp.update backendMsg bem

                _ =
                    Debug.log "BEMsg" backendMsg
            in
            ( ( fem, nbem ), Cmd.map BEMsg bcmd )

        BEtoFE ms msgId toFrontend tagger ->
            let
                ( nfem, fcmd ) =
                    fApp.updateFromBackend toFrontend fem

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
                    bApp.updateFromFrontend "ClientID-local-dev" toBackend bem

                ( newModel, ncmd ) =
                    -- TODO: delay response by `ms` milliseconds
                    update (tagger (Ok ())) ( fem, nbem )

                _ =
                    Debug.log "FEtoBE" ( toBackend, tagger (Ok ()) )
            in
            ( newModel, Cmd.batch [ Cmd.map BEMsg bcmd, ncmd ] )


subscriptions ( fem, bem ) =
    Sub.batch
        [ Sub.map FEMsg (fApp.subscriptions fem)
        , Sub.map BEMsg (bApp.subscriptions bem)
        ]


mapDocument fn { title, body } =
    { title = title, body = List.map (Html.map fn) body }


main : Program () ( FrontendModel, BackendModel ) Msg
main =
    Browser.application
        { init = init
        , view = \( fem, _ ) -> mapDocument FEMsg (fApp.view fem)
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \url -> FEMsg (fApp.onUrlRequest url)
        , onUrlChange = \ureq -> FEMsg (fApp.onUrlChange ureq)
        }
