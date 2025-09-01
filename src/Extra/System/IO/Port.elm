module Extra.System.IO.Port exposing
    ( Codec
    , LowLevelSend
    , LowLevelSendFun
    , ReceivePort
    , SafeCodec
    , SendPort
    , SyncApi
    , SyncCont
    , SyncLens
    , SyncRequest
    , SyncRequester
    , SyncRequesterFun
    , SyncRespond
    , SyncResponder
    , SyncResponderFun
    , lowLevelSend
    , lowLevelSendFun
    , syncApi
    )

import Extra.System.IO as IO
import Extra.Type.Lens as Lens



-- CODEC


type alias Codec valueOut valueIn wire =
    { encode : valueOut -> wire
    , decode : wire -> valueIn
    }


type alias SafeCodec value wire =
    Codec value value wire



-- PORTS


type alias SendPort msg wire =
    wire -> Cmd msg


type alias ReceivePort msg wire =
    (wire -> msg) -> Sub msg



-- LOW LEVEL PRIMITIVES


type alias LowLevelSend s valueOut =
    valueOut -> IO.IO s ()


lowLevelSend :
    Codec valueOut valueIn wire
    -> SendPort (IO.IO s ()) wire
    -> LowLevelSend s valueOut
lowLevelSend codec sendPort value =
    IO.liftCmdIO (sendPort (codec.encode value))


type alias LowLevelSendFun s valueOut wire =
    SendPort (IO.IO s ()) wire
    -> LowLevelSend s valueOut


lowLevelSendFun :
    Codec valueOut valueIn wire
    -> LowLevelSendFun s valueOut wire
lowLevelSendFun codec sendPort =
    lowLevelSend codec sendPort



-- SYNC REQUESTER


type alias SyncCont s responseIn =
    responseIn -> IO.IO s ()


type alias SyncLens s responseIn =
    Lens.Lens s (Maybe (SyncCont s responseIn))


type alias SyncRequest s requestOut responseIn =
    requestOut -> IO.IO s responseIn


syncRequest :
    Codec requestOut requestIn requestWire
    -> SyncLens s responseIn
    -> SendPort (IO.IO s ()) requestWire
    -> SyncRequest s requestOut responseIn
syncRequest requestCodec lens requestPort requestOut =
    IO.liftCont <|
        \cont ->
            IO.sequence
                [ IO.putLens lens (Just cont)
                , lowLevelSend requestCodec requestPort requestOut
                ]


type alias SyncRespond s =
    s -> Sub (IO.IO s ())


syncResponse :
    Codec responseOut responseIn responseWire
    -> SyncLens s responseIn
    -> ReceivePort (IO.IO s ()) responseWire
    -> SyncRespond s
syncResponse responseCodec lens responsePort state =
    responsePort <|
        \responseWire ->
            case lens.getter state of
                Just cont ->
                    IO.sequence
                        [ IO.putLens lens Nothing
                        , cont (responseCodec.decode responseWire)
                        ]

                Nothing ->
                    IO.noOp


type alias SyncRequester s requestOut responseIn =
    { request : SyncRequest s requestOut responseIn
    , respond : SyncRespond s
    }


type alias SyncRequesterFun s requestOut requestWire responseIn responseWire =
    SyncLens s responseIn
    -> SendPort (IO.IO s ()) requestWire
    -> ReceivePort (IO.IO s ()) responseWire
    -> SyncRequester s requestOut responseIn


syncRequesterFun :
    Codec requestOut requestIn requestWire
    -> Codec responseOut responseIn responseWire
    -> SyncRequesterFun s requestOut requestWire responseIn responseWire
syncRequesterFun requestCodec responseCodec lens sendPort receivePort =
    { request = syncRequest requestCodec lens sendPort
    , respond = syncResponse responseCodec lens receivePort
    }



-- SYNC RESPONDER


type alias SyncResponder s requestIn responseOut =
    (requestIn -> IO.IO s responseOut) -> Sub (IO.IO s ())


type alias SyncResponderFun s requestIn requestWire responseOut responseWire =
    ReceivePort (IO.IO s ()) requestWire
    -> SendPort (IO.IO s ()) responseWire
    -> SyncResponder s requestIn responseOut


syncResponderFun :
    Codec requestOut requestIn requestWire
    -> Codec responseOut responseIn responseWire
    -> SyncResponderFun s requestIn requestWire responseOut responseWire
syncResponderFun requestCodec responseCodec receivePort sendPort callback =
    receivePort <|
        \requestWire ->
            IO.bind (callback (requestCodec.decode requestWire)) <|
                \response ->
                    lowLevelSend responseCodec sendPort response



-- SYNC API


type alias SyncApi s requestOut requestIn requestWire responseOut responseIn responseWire =
    { requesterFun : SyncRequesterFun s requestOut requestWire responseIn responseWire
    , responderFun : SyncResponderFun s requestIn requestWire responseOut responseWire
    }


syncApi :
    Codec requestOut requestIn requestWire
    -> Codec responseOut responseIn responseWire
    -> SyncApi s requestOut requestIn requestWire responseOut responseIn responseWire
syncApi requestCodec responseCodec =
    { requesterFun = syncRequesterFun requestCodec responseCodec
    , responderFun = syncResponderFun requestCodec responseCodec
    }
