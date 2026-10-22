module LamderaHelpers exposing (decodeWireAnalytics, decodeWirePayloadHeader)

{-| Stand-ins for the runtime's LamderaHelpers, which the backend injection
(extra/Lamdera/Injection.hs) references by name.
-}


decodeWirePayloadHeader : String -> ( Int, Int )
decodeWirePayloadHeader _ =
    ( -1, -1 )


decodeWireAnalytics : String -> String
decodeWireAnalytics s =
    s
