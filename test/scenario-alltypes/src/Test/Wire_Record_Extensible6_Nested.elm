module Test.Wire_Record_Extensible6_Nested exposing (..)

import Bytes.Decode
import Bytes.Encode
import Lamdera.Wire3


type alias Derp =
    A { dog : () }



-- So working backwards from a concrete example, what would this encoder/decoder look like?


expected_w3_encode_Derp : Derp -> Lamdera.Wire3.Encoder
expected_w3_encode_Derp =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ Lamdera.Wire3.encodeUnit w3_rec_var0.a
            , Lamdera.Wire3.encodeUnit w3_rec_var0.dog
            ]


expected_w3_decode_Derp =
    Lamdera.Wire3.succeedDecode
        (\a0 dog0 -> { a = a0, dog = dog0 })
        |> Lamdera.Wire3.andMapDecode
            Lamdera.Wire3.decodeUnit
        |> Lamdera.Wire3.andMapDecode
            Lamdera.Wire3.decodeUnit



{-

   Notice how the attributes `.a` and `.dog` are both statically called – that's because
   Wire's record encoding is lexicographically ordered by field name.

   We're specifically _not_ calling w3_encode_A, because if we did, we'd have to chose
   between one of two implementations:

    Lamdera.Wire3.encodeSequenceWithoutLength
        (w3_encode_A w3_rec_var0
            ++ [ Lamdera.Wire3.encodeUnit w3_rec_var0.dog
               ]
        )

    Lamdera.Wire3.encodeSequenceWithoutLength
        ([ Lamdera.Wire3.encodeUnit w3_rec_var0.dog
         ]
            ++ w3_encode_A w3_rec_var0
        )

    In this specific case, the first one is correct, because `a` comes before `dog`,
    but if the attribute was `x`, it would be the second one.

    So to fix _that_, we'd need to return some extra info such that a caller could
    sort the attributes... aaand now we're really getting into the weeds.


    So we're effectively saying "if you're going to use an extensible record, then use a
    type alias that concretely fills all type vars, and then use that type instead".

    This leaves us with an outstanding problem for anonymous extensible records, i.e.:

    type alias SomeRecord =
        { herp : String
        , derp : Int
        , extensible : A { dog : String }
        }

    Here we've got the `A { dog : String }` in the middle of the record, and we can't
    just use `A { dog : String }` as a type, because it's not a concrete type, and as
    we just discussed, we can't make wire encoding/decoding functions for it on the fly,
    because we can't sort the attributes. It needs to be concrete.


-}
-- None of these can actually be used, because type variables must be fully applied,
-- meaning the holey extensible record itself is never directly as a value.


type alias A x =
    { x | a : () }


expected_w3_encode_A : ({ x | a : () } -> Lamdera.Wire3.Encoder) -> (A x -> Lamdera.Wire3.Encoder)
expected_w3_encode_A w3_x_c_x =
    w3_x_c_x


expected_w3_decode_A : Bytes.Decode.Decoder (A { a : () })
expected_w3_decode_A =
    -- "Cannot decode an extensible record with unfilled type vars"
    Lamdera.Wire3.failDecode


type alias B x =
    A { x | b : () }


expected_w3_encode_B : (B x -> Lamdera.Wire3.Encoder) -> (B x -> Lamdera.Wire3.Encoder)
expected_w3_encode_B w3_x_c_x =
    w3_x_c_x


expected_w3_decode_B w3_x_c_x =
    -- "Cannot decode an extensible record with unfilled type vars"
    Lamdera.Wire3.failDecode


type alias C x =
    B { x | c : () }


expected_w3_encode_C : (C x -> Lamdera.Wire3.Encoder) -> (C x -> Lamdera.Wire3.Encoder)
expected_w3_encode_C w3_x_c_x =
    w3_x_c_x


expected_w3_decode_C w3_x_c_x =
    -- "Cannot decode an extensible record with unfilled type vars"
    Lamdera.Wire3.failDecode
