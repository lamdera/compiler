module Lamdera.Repl.Interface exposing
    ( bem
    , broadcast
    , capture
    , clearCaptures
    , fem
    , finishCapture
    , replay
    , sendToBE
    , sendToFE
    , setBem
    , setFem
    , updateBE
    , updateFE
    )

import Lamdera
import Types


bem : Types.BackendModel
bem =
    onlyInLeader checkedBem


checkedBem : Types.BackendModel
checkedBem =
    jsImpl "app.fns.getModel().bem"


fem : Types.FrontendModel
fem =
    jsImpl "app.fns.getModel().fem"


setBem : Types.BackendModel -> Types.BackendModel
setBem =
    onlyInLeader checkedSetBem


checkedSetBem : Types.BackendModel -> Types.BackendModel
checkedSetBem =
    jsImpl "function(m) { app.fns.setBem(m); return m }"


setFem : Types.FrontendModel -> Types.FrontendModel
setFem =
    jsImpl "function(m) { app.fns.setFem(m); return m }"


updateBE : Types.BackendMsg -> Types.BackendMsg
updateBE =
    onlyInLeader checkedUpdateBE


checkedUpdateBE : Types.BackendMsg -> Types.BackendMsg
checkedUpdateBE =
    jsImpl "function(m) { app.fns.sendToApp({$:'BEMsg', a:m}); return m }"


updateFE : Types.FrontendMsg -> Types.FrontendMsg
updateFE =
    jsImpl "function(m) { app.fns.sendToApp({$:'FEMsg', a:m}); return m }"


sendToBE : Types.ToBackend -> Types.ToBackend
sendToBE =
    jsImpl "function(m) { app.fns.sendToApp({$:'FEtoBE', a:m}); return m }"


sendToFE : Lamdera.ClientId -> Types.ToFrontend -> Types.ToFrontend
sendToFE =
    onlyInLeader checkedSendToFE


checkedSendToFE : Lamdera.ClientId -> Types.ToFrontend -> Types.ToFrontend
checkedSendToFE c m =
    sendToFET ( c, m )


broadcast : Types.ToFrontend -> Types.ToFrontend
broadcast =
    onlyInLeader checkedBroadcast


checkedBroadcast : Types.ToFrontend -> Types.ToFrontend
checkedBroadcast m =
    sendToFET ( "b", m )


sendToFET : ( Lamdera.ClientId, Types.ToFrontend ) -> Types.ToFrontend
sendToFET =
    jsImpl "function(t) { app.fns.sendToApp({$:'BEtoFE', a:t.a, b:t.b}); return t.b }"


onlyInLeader : a -> a
onlyInLeader a =
    case nt of
        Leader ->
            a

        Follower ->
            Debug.todo """
This backend function can only by used in the leader tab (green dot)

For more info say :lamdera
or look at <docs>
"""


nt : NodeType
nt =
    jsImpl "app.fns.getModel().nodeType"


type NodeType
    = Follower
    | Leader


jsImpl : String -> a
jsImpl _ =
    Debug.todo "The functions in module Repl.Interface can only by used in the Lamdera Live REPL."



-- CAPTURES


type CaptureState a
    = NotCaptured
    | Captured a
    | MultipleCaptures


captureState : CaptureState a
captureState =
    NotCaptured


setCaptureState : CaptureState a -> ()
setCaptureState =
    jsImpl "function(c) { $author$project$Lamdera$Repl$Interface$captureState = c; return _Utils_Tuple0; }"


type ReplayState
    = NoReplay
    | Replaying String


replayState : ReplayState
replayState =
    NoReplay


setReplayState : ReplayState -> ()
setReplayState =
    jsImpl "function(r) { $author$project$Lamdera$Repl$Interface$replayState = r; return _Utils_Tuple0; }"


replay : ( String, () -> a ) -> a
replay ( name, fun ) =
    case replayState of
        oldReplyState ->
            case setReplayState (Replaying name) of
                () ->
                    case fun () of
                        result ->
                            case setReplayState oldReplyState of
                                () ->
                                    result


getCapturedValue : String -> a
getCapturedValue =
    jsImpl "function(n) { return captures[n]; }"


setCapturedValue : ( String, a ) -> ()
setCapturedValue =
    jsImpl "function(p) { captures[p.a] = p.b; return _Utils_Tuple0; }"


clearCaptures : () -> ()
clearCaptures =
    jsImpl "function() { captures = Object.create(null); return _Utils_Tuple0; }"


capture : a -> a
capture value =
    case replayState of
        Replaying name ->
            getCapturedValue name

        _ ->
            case captureState of
                NotCaptured ->
                    case setCaptureState (Captured value) of
                        () ->
                            value

                _ ->
                    case setCaptureState MultipleCaptures of
                        () ->
                            Debug.todo """
Can't call 'capture' multiple times in the same expression

More info at <docs>
"""


finishCapture : String -> String
finishCapture name =
    case captureState of
        Captured value ->
            case setCapturedValue ( name, value ) of
                () ->
                    "t"

        _ ->
            "f"
