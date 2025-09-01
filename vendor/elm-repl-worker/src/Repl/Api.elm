module Repl.Api exposing
    ( ClientRequest
    , ClientRequestWire
    , JavaScriptRequest
    , JavaScriptRequestWire
    , JavaScriptResponse(..)
    , JavaScriptResponseWire
    , WorkerResponse
    , WorkerResponseWire
    , WorkerState(..)
    , clientRequestCodec
    , clientToWorkerApi
    , clientToWorkerLowLevelSendFun
    , workerResponseCodec
    , workerToJavaScriptRequester
    )

import Extra.System.IO.Port as Port
import Extra.Type.List exposing (TList)



-- CLIENT TO WORKER API


clientToWorkerApi : Port.SyncApi s ClientRequest ClientRequest ClientRequestWire WorkerResponse WorkerResponse WorkerResponseWire
clientToWorkerApi =
    Port.syncApi clientRequestCodec workerResponseCodec


type alias ClientRequest =
    { userInput : String }


type alias ClientRequestWire =
    String


clientRequestCodec : Port.SafeCodec ClientRequest ClientRequestWire
clientRequestCodec =
    { encode = \{ userInput } -> userInput
    , decode = \wire -> { userInput = wire }
    }


type alias WorkerResponse =
    { workerState : WorkerState
    , messages : TList String
    }


type WorkerState
    = WorkerStateRunning (Maybe String)
    | WorkerStateStopped (Maybe String)


type alias WorkerResponseWire =
    ( Bool, Maybe String, TList String )


workerResponseCodec : Port.SafeCodec WorkerResponse WorkerResponseWire
workerResponseCodec =
    { encode =
        \{ workerState, messages } ->
            case workerState of
                WorkerStateRunning prefill ->
                    ( True, prefill, messages )

                WorkerStateStopped error ->
                    ( False, error, messages )
    , decode =
        \wire ->
            case wire of
                ( True, prefill, messages ) ->
                    { workerState = WorkerStateRunning prefill, messages = messages }

                ( False, error, messages ) ->
                    { workerState = WorkerStateStopped error, messages = messages }
    }


clientToWorkerLowLevelSendFun : Port.LowLevelSendFun s WorkerResponse WorkerResponseWire
clientToWorkerLowLevelSendFun =
    Port.lowLevelSend workerResponseCodec



-- WORKER TO JAVASCRIPT API


workerToJavaScriptRequester : Port.SyncRequesterFun s JavaScriptRequest JavaScriptRequestWire JavaScriptResponse JavaScriptResponseWire
workerToJavaScriptRequester =
    (Port.syncApi javaScriptRequestCodec javaScriptResponseCodec).requesterFun


type alias JavaScriptRequest =
    String


type alias JavaScriptRequestWire =
    String


javaScriptRequestCodec : Port.SafeCodec JavaScriptRequest JavaScriptRequestWire
javaScriptRequestCodec =
    { encode = identity
    , decode = identity
    }


type JavaScriptResponse
    = JavaScriptOutput String
    | JavaScriptError String


type alias JavaScriptResponseWire =
    ( Bool, String )


javaScriptResponseCodec : Port.SafeCodec JavaScriptResponse JavaScriptResponseWire
javaScriptResponseCodec =
    { encode =
        \response ->
            case response of
                JavaScriptOutput output ->
                    ( True, output )

                JavaScriptError error ->
                    ( False, error )
    , decode =
        \wire ->
            case wire of
                ( True, output ) ->
                    JavaScriptOutput output

                ( False, error ) ->
                    JavaScriptError error
    }
