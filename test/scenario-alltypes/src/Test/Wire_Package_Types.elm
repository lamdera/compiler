module Test.Wire_Package_Types exposing (..)

import Bytes
import Bytes.Decode
import Bytes.Encode
import Lamdera.Wire3
import SeqDict exposing (..)
import SeqSet exposing (..)
import Time
import Url


type PackageTypes
    = SDict (SeqDict Tag String)
    | SSet (SeqSet Tag)


type Tag
    = Foo
    | Bar


type alias PackageTypesRecord =
    { sDict : SeqDict Tag String
    , sSet : SeqSet Tag
    }


expected_w3_encode_PackageTypes : PackageTypes -> Lamdera.Wire3.Encoder
expected_w3_encode_PackageTypes w3v =
    case w3v of
        SDict v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Bytes.Encode.unsignedInt8
                    0
                , SeqDict.encodeDict
                    w3_encode_Tag
                    Lamdera.Wire3.encodeString
                    v0
                ]

        SSet v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Bytes.Encode.unsignedInt8
                    1
                , SeqSet.encodeSet
                    w3_encode_Tag
                    v0
                ]


expected_w3_decode_PackageTypes =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode
                            SDict
                            |> Lamdera.Wire3.andMapDecode
                                (SeqDict.decodeDict
                                    w3_decode_Tag
                                    Lamdera.Wire3.decodeString
                                )

                    1 ->
                        Lamdera.Wire3.succeedDecode
                            SSet
                            |> Lamdera.Wire3.andMapDecode
                                (SeqSet.decodeSet
                                    w3_decode_Tag
                                )

                    _ ->
                        Lamdera.Wire3.failDecode
            )


expected_w3_encode_PackageTypesRecord : PackageTypesRecord -> Lamdera.Wire3.Encoder
expected_w3_encode_PackageTypesRecord =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ SeqDict.encodeDict
                w3_encode_Tag
                Lamdera.Wire3.encodeString
                w3_rec_var0.sDict
            , SeqSet.encodeSet
                w3_encode_Tag
                w3_rec_var0.sSet
            ]


expected_w3_decode_PackageTypesRecord =
    Lamdera.Wire3.succeedDecode
        (\sDict0 sSet0 -> { sDict = sDict0, sSet = sSet0 })
        |> Lamdera.Wire3.andMapDecode
            (SeqDict.decodeDict
                w3_decode_Tag
                Lamdera.Wire3.decodeString
            )
        |> Lamdera.Wire3.andMapDecode
            (SeqSet.decodeSet
                w3_decode_Tag
            )
