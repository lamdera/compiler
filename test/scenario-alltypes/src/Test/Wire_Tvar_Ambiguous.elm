module Test.Wire_Tvar_Ambiguous exposing (..)

{-| From morphir-elm:Morphir/IR/Type.elm

Here we have an `a` tvar that ambiguously gets transferred through a few module contexts.

The tricky part here is we need to know when to leave certain TVars unresolved. I.e:

CustomTypeDefinition (Test.Wire\_Tvar\_Ambiguous2.AccessControlled (Maybe a))
..................... ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Is paramaterised by `a` but filling it in with `(Maybe a)` confuses the type system,
given it remains generic at the top level (i.e. `Definition a` itself is not reified!)

The test itself has a minor difference in gen diff for record treatment hence the commenting out.

-}

import Bytes.Decode
import Bytes.Encode
import Lamdera.Wire3
import Test.Wire_Tvar_Ambiguous2


type Definition a
    = CustomTypeDefinition (Test.Wire_Tvar_Ambiguous2.AccessControlled (Maybe a))
    | TypeAliasDefinition (Maybe a)


expected_w3_encode_Definition : (a -> Lamdera.Wire3.Encoder) -> Definition a -> Lamdera.Wire3.Encoder
expected_w3_encode_Definition w3_x_c_a w3v =
    case w3v of
        CustomTypeDefinition v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Bytes.Encode.unsignedInt8 0, Test.Wire_Tvar_Ambiguous2.w3_encode_AccessControlled (Lamdera.Wire3.encodeMaybe w3_x_c_a) v0 ]

        TypeAliasDefinition v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Bytes.Encode.unsignedInt8 1, Lamdera.Wire3.encodeMaybe w3_x_c_a v0 ]


expected_w3_decode_Definition w3_x_c_a =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode CustomTypeDefinition |> Lamdera.Wire3.andMapDecode (Test.Wire_Tvar_Ambiguous2.w3_decode_AccessControlled (Lamdera.Wire3.decodeMaybe w3_x_c_a))

                    1 ->
                        Lamdera.Wire3.succeedDecode TypeAliasDefinition |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeMaybe w3_x_c_a)

                    _ ->
                        Lamdera.Wire3.failDecode
            )
