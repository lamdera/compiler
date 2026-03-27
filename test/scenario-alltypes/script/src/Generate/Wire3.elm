module Generate.Wire3 exposing
    ( bytesDecodeCall
    , bytesEncodeCall
    , seqDictFromList
    , seqSetFromList
    , w3Decode
    , w3Encode
    )

{-| Elm.value references for lamdera-specific functions not in elm-codegen registry.
-}

import Elm
import Elm.Annotation as Type


{-| Reference to a compiler-generated w3\_encode\_ function in a given module.
-}
w3Encode : List String -> String -> Elm.Expression
w3Encode modulePath typeName =
    Elm.value
        { importFrom = modulePath
        , name = "w3_encode_" ++ typeName
        , annotation = Nothing
        }


{-| Reference to a compiler-generated w3\_decode\_ function in a given module.
-}
w3Decode : List String -> String -> Elm.Expression
w3Decode modulePath typeName =
    Elm.value
        { importFrom = modulePath
        , name = "w3_decode_" ++ typeName
        , annotation = Nothing
        }


{-| Bytes.Encode.encode : Encoder -> Bytes
-}
bytesEncodeCall : Elm.Expression -> Elm.Expression
bytesEncodeCall encoder =
    Elm.apply
        (Elm.value
            { importFrom = [ "Bytes", "Encode" ]
            , name = "encode"
            , annotation = Nothing
            }
        )
        [ encoder ]


{-| Bytes.Decode.decode : Decoder a -> Bytes -> Maybe a
-}
bytesDecodeCall : Elm.Expression -> Elm.Expression -> Elm.Expression
bytesDecodeCall decoder bytes =
    Elm.apply
        (Elm.value
            { importFrom = [ "Bytes", "Decode" ]
            , name = "decode"
            , annotation = Nothing
            }
        )
        [ decoder, bytes ]


{-| SeqDict.fromList
-}
seqDictFromList : Elm.Expression -> Elm.Expression
seqDictFromList listExpr =
    Elm.apply
        (Elm.value
            { importFrom = [ "SeqDict" ]
            , name = "fromList"
            , annotation = Nothing
            }
        )
        [ listExpr ]


{-| SeqSet.fromList
-}
seqSetFromList : Elm.Expression -> Elm.Expression
seqSetFromList listExpr =
    Elm.apply
        (Elm.value
            { importFrom = [ "SeqSet" ]
            , name = "fromList"
            , annotation = Nothing
            }
        )
        [ listExpr ]
