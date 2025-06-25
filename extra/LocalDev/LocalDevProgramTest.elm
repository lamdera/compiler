port module LocalDev exposing (main)

{-

   Hello you curious thing!

   This is the development harness used for local development of Lamdera apps.

   This file should not be used as a reference for building Lamdera apps, see
   https://dashboard.lamdera.app/docs/building instead.

   The features used by this file are subject to change/removal and should not
   be relied on in any way.

-}

import Backend
import Effect.LocalDev exposing (ConnectionMsg, Msg(..), WireMsg)
import Env
import Frontend
import Json.Encode
import Lamdera exposing (ClientId, Key, SessionId, Url)
import Lamdera.Json as Json
import Lamdera.Wire3 exposing (Bytes)
import LamderaRPC
import RPC
import Types


{-| Injected when parsed by Lamdera/CLI/Live.hs
-}
currentVersion =
    ( 0, 0, 0 )


port send_ToBackend : Bytes -> Cmd msg


port receive_ToBackend : (( SessionId, ClientId, Bytes ) -> msg) -> Sub msg


port save_BackendModel : { t : String, f : Bool, b : Bytes } -> Cmd msg


port send_EnvMode : { t : String, v : String } -> Cmd msg


port send_ToFrontend : WireMsg -> Cmd msg


port receive_ToFrontend : (WireMsg -> msg) -> Sub msg



-- @TODO this isn't used currently but needs to adapt for state restore functions?


port receive_BackendModel : (Bytes -> msg) -> Sub msg



-- @LEGCACY END


port setNodeTypeLeader : (Bool -> msg) -> Sub msg


port setLiveStatus : (Bool -> msg) -> Sub msg


port setClientId : (String -> msg) -> Sub msg


port rpcIn : (Json.Value -> msg) -> Sub msg


port rpcOut : Json.Value -> Cmd msg


port onConnection : (ConnectionMsg -> msg) -> Sub msg


port onDisconnection : (ConnectionMsg -> msg) -> Sub msg


port localDevGotEvent : (Json.Encode.Value -> msg) -> Sub msg


port localDevStartRecording : () -> Cmd msg


port localDevStopRecording : () -> Cmd msg


port localDevCopyToClipboard : String -> Cmd msg


main =
    let
        _ =
            shouldProxy
    in
    Effect.LocalDev.localDev
        { send_ToBackend = send_ToBackend
        , receive_ToBackend = receive_ToBackend
        , save_BackendModel = save_BackendModel
        , send_EnvMode = send_EnvMode
        , send_ToFrontend = send_ToFrontend
        , receive_ToFrontend = receive_ToFrontend
        , receive_BackendModel = receive_BackendModel
        , setNodeTypeLeader = setNodeTypeLeader
        , setLiveStatus = setLiveStatus
        , setClientId = setClientId
        , rpcIn = rpcIn
        , mkrrc = mkrrc
        , onConnection = onConnection
        , onDisconnection = onDisconnection
        , localDevGotEvent = localDevGotEvent
        , localDevStartRecording = localDevStartRecording
        , localDevStopRecording = localDevStopRecording
        , localDevCopyToClipboard = localDevCopyToClipboard
        , currentVersion = currentVersion
        , w3_encode_BackendModel = Types.w3_encode_BackendModel
        , w3_decode_BackendModel = Types.w3_decode_BackendModel
        , w3_encode_ToFrontend = Types.w3_encode_ToFrontend
        , w3_decode_ToFrontend = Types.w3_decode_ToFrontend
        , w3_encode_ToBackend = Types.w3_encode_ToBackend
        , w3_decode_ToBackend = Types.w3_decode_ToBackend
        , envMeta =
            case Env.mode of
                Env.Production ->
                    ( "Prod", "#E06C75" )

                Env.Development ->
                    ( "Dev", "#85BC7A" )
        , userFrontendApp = Frontend.app
        , userBackendApp = Backend.app
        }


mkrrc m rpcArgsJson =
    let
        log t v =
            if m.devbar.logging then
                Debug.log t v

            else
                v
    in
    -- The case expression is just so we have the right amount of tabbing for the code insertion
    case () of
        () ->
            -- MKRRC
            ( m, Cmd.none )



-- -}


{-| Used directly by the core CORS modification to decide which Msg types need

the CORS flag set for subsequent Cmd's they'll initiate

-}
shouldProxy : Msg frontendMsg backendMsg toFrontend toBackend -> Bool
shouldProxy msg =
    case msg of
        BEMsg _ ->
            True

        FEtoBE _ ->
            True

        FEtoBEDelayed _ ->
            True

        ReceivedToBackend _ ->
            True

        _ ->
            False
