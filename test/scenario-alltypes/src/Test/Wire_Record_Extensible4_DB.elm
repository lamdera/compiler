module Test.Wire_Record_Extensible4_DB exposing (..)

import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
import Lamdera.Wire3
import Random exposing (Seed)


type alias UUID =
    String


type alias DB a =
    { db : Dict UUID { a | uuid : UUID }
    , seed : Seed
    }


expected_w3_encode_DB : ({ a | uuid : UUID } -> Lamdera.Wire3.Encoder) -> (DB a -> Lamdera.Wire3.Encoder)
expected_w3_encode_DB w3_x_c_a =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ Lamdera.Wire3.encodeDict
                w3_encode_UUID
                w3_x_c_a
                w3_rec_var0.db
            , Random.w3_encode_Seed
                w3_rec_var0.seed
            ]


expected_w3_decode_DB w3_x_c_a =
    Lamdera.Wire3.succeedDecode
        (\db0 seed0 -> { db = db0, seed = seed0 })
        |> Lamdera.Wire3.andMapDecode
            (Lamdera.Wire3.decodeDict
                w3_decode_UUID
                w3_x_c_a
            )
        |> Lamdera.Wire3.andMapDecode
            Random.w3_decode_Seed
