module Repl.Interface exposing
    ( bem
    , broadcast
    , fem
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
    jsImpl "getApp().fns.getModel().bem"


fem : Types.FrontendModel
fem =
    jsImpl "getApp().fns.getModel().fem"


setBem : Types.BackendModel -> Types.BackendModel
setBem =
    onlyInLeader checkedSetBem


checkedSetBem : Types.BackendModel -> Types.BackendModel
checkedSetBem =
    jsImpl "function(m) { getApp().fns.setBem(m); return m }"


setFem : Types.FrontendModel -> Types.FrontendModel
setFem =
    jsImpl "function(m) { getApp().fns.setFem(m); return m }"


updateBE : Types.BackendMsg -> Types.BackendMsg
updateBE =
    onlyInLeader checkedUpdateBE


checkedUpdateBE : Types.BackendMsg -> Types.BackendMsg
checkedUpdateBE =
    jsImpl "function(m) { getApp().fns.sendToApp({$:'BEMsg', a:m}); return m }"


updateFE : Types.FrontendMsg -> Types.FrontendMsg
updateFE =
    jsImpl "function(m) { getApp().fns.sendToApp({$:'FEMsg', a:m}); return m }"


sendToBE : Types.ToBackend -> Types.ToBackend
sendToBE =
    jsImpl "function(m) { getApp().fns.sendToApp({$:'FEtoBE', a:m}); return m }"


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
    jsImpl "function(t) { getApp().fns.sendToApp({$:'BEtoFE', a:t.a, b:t.b}); return t.b }"


onlyInLeader : a -> a
onlyInLeader a =
    case nt of
        Leader ->
            a

        Follower ->
            Debug.todo "Use leader tab (green dot) for backend functions.\n"


nt : NodeType
nt =
    jsImpl "getApp().fns.getModel().nodeType"


type NodeType
    = Follower
    | Leader


jsImpl : String -> a
jsImpl _ =
    Debug.todo "The functions in module Repl.Interface can only by used in the Lamdera Live REPL."
