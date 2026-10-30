port module Main exposing (main)

{-| Encodes sample values of the scenario's ToBackend, BackendMsg, ToFrontend and
BackendModel with the generated Wire3 codecs and sends each one's bytes as hex through a
port, as ( type, label, hex ). check.js decodes them with the wire schema and compares with
what it expects for each label.
-}

import Array
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Dict
import Lamdera.Wire3 as W
import Math.Matrix4
import Math.Vector2
import Math.Vector3
import Platform
import SeqDict
import SeqSet
import Set
import Time
import Types exposing (..)
import Url


port go : (() -> msg) -> Sub msg


port out : List ( String, String, String ) -> Cmd msg


hex : Bytes -> String
hex bytes =
    let
        digit d =
            String.slice d (d + 1) "0123456789abcdef"

        step ( n, acc ) =
            if n <= 0 then
                Bytes.Decode.succeed (Bytes.Decode.Done (String.concat (List.reverse acc)))

            else
                Bytes.Decode.unsignedInt8 |> Bytes.Decode.map (\b -> Bytes.Decode.Loop ( n - 1, (digit (b // 16) ++ digit (modBy 16 b)) :: acc ))
    in
    Bytes.Decode.decode (Bytes.Decode.loop ( Bytes.width bytes, [] ) step) bytes
        |> Maybe.withDefault "<hex failed>"


toBackend : String -> ToBackend -> ( String, String, String )
toBackend label v =
    ( "ToBackend", label, hex (W.bytesEncode (w3_encode_ToBackend v)) )


backendMsg : String -> BackendMsg -> ( String, String, String )
backendMsg label v =
    ( "BackendMsg", label, hex (W.bytesEncode (w3_encode_BackendMsg v)) )


toFrontend : String -> ToFrontend -> ( String, String, String )
toFrontend label v =
    ( "ToFrontend", label, hex (W.bytesEncode (w3_encode_ToFrontend v)) )


backendModel : String -> BackendModel -> ( String, String, String )
backendModel label v =
    ( "BackendModel", label, hex (W.bytesEncode (w3_encode_BackendModel v)) )


url : Url.Url
url =
    { protocol = Url.Https, host = "example.com", port_ = Just 8443, path = "/a/b", query = Just "q=1", fragment = Nothing }


samples : List ( String, String, String )
samples =
    [ toBackend "noop" NoOpToBackend
    , toBackend "addTodo" (AddTodo "milk ünï ✓ 😀")
    , toBackend "numbersSmall" (Numbers 0 0.5)
    , toBackend "numbersNeg" (Numbers -1 -0.0)
    , toBackend "numbers215" (Numbers 107 1.5e300)
    , toBackend "numbers216" (Numbers 108 (1 / 0))
    , toBackend "numbers9431" (Numbers -4716 (-1 / 0))
    , toBackend "numbers65535" (Numbers 32767 (0 / 0))
    , toBackend "numbersInt32" (Numbers 2147483647 5.0e-324)
    , toBackend "numbersIntMin32" (Numbers -2147483648 3)
    , toBackend "numbersBig" (Numbers 1099511627776 0)
    , toBackend "numbersMaxSafe" (Numbers 9007199254740991 0)
    , toBackend "numbersPastSafe" (Numbers 36028797018963968 0)
    , toBackend "edge" (Edge GT '😀' ())
    , toBackend "saved" (Saved { title = "t", done = True, tags = [ "a", "b" ] })
    , toBackend "grow" (Grow (Node (Node Leaf 1 Leaf) 2 Leaf))
    , toBackend "wrappedNothing" (Wrapped { value = Nothing })
    , toBackend "wrappedOk" (Wrapped { value = Just (Ok 42) })
    , toBackend "wrappedErr" (Wrapped { value = Just (Err "bad") })
    , toBackend "containers"
        (Containers
            (Dict.fromList [ ( "b", 2 ), ( "a", 1 ) ])
            (Set.fromList [ 3, -1, 2 ])
            (Array.fromList [ 1.5, -2 ])
            [ ( 1, "one" ), ( 2, "two" ) ]
        )
    , toBackend "seqs" (Seqs (SeqDict.fromList [ ( ( 2, 1 ), "x" ), ( ( 1, 2 ), "y" ) ]) (SeqSet.fromList [ "z", "a" ]))
    , toBackend "triple" (Triple ( False, "", -7.25 ))
    , toBackend "blob" (Blob (Bytes.Encode.encode (Bytes.Encode.sequence (List.map Bytes.Encode.unsignedInt8 [ 0, 1, 2, 255 ]))))
    , toBackend "kernel"
        (Kernel (Time.millisToPosix 1700000000123)
            "session-1"
            "client-1"
            (Math.Vector2.vec2 1 2)
            (Math.Vector3.vec3 0.5 -1 3)
            Math.Matrix4.identity
        )
    , toBackend "visit" (Visit url)
    , toBackend "deep" (Deep (Add (Lit "x") (Block (Seq [ Return (Lit "y"), Seq [] ]))))
    , backendMsg "many000" Many000
    , backendMsg "many259" Many259
    , backendMsg "many128" Many128
    , toFrontend "welcome" (Welcome { reordered = { zeta = 1, alpha = "a", b = True, aa = 2, a_b = 3, aB = 4 }, nested = { value = { value = 5 } } })
    , toFrontend "pong" Pong
    , backendModel "model"
        { sessions = SeqDict.fromList [ ( "s1", [ "c1", "c2" ] ) ]
        , todos = Dict.fromList [ ( 7, { title = "x", done = False, tags = [] } ) ]
        }
    ]


main : Program () () ()
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \_ m -> ( m, out samples )
        , subscriptions = \_ -> go (\_ -> ())
        }
