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

import Lamdera.Wire3
import Test.Wire_Tvar_Ambiguous2


type Definition a
    = CustomTypeDefinition (Test.Wire_Tvar_Ambiguous2.AccessControlled (Maybe a))
    | TypeAliasDefinition (Maybe a)


expected_w2_encode_Definition w2_x_c_a w2v =
    case w2v of
        CustomTypeDefinition v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0, Test.Wire_Tvar_Ambiguous2.w2_encode_AccessControlled (Lamdera.Wire3.encodeMaybe w2_x_c_a) v0 ]

        TypeAliasDefinition v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 1, Lamdera.Wire3.encodeMaybe w2_x_c_a v0 ]


expected_w2_decode_Definition w2_x_c_a =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire3.succeedDecode CustomTypeDefinition |> Lamdera.Wire3.andMapDecode (Test.Wire_Tvar_Ambiguous2.w2_decode_AccessControlled (Lamdera.Wire3.decodeMaybe w2_x_c_a))

                    1 ->
                        Lamdera.Wire3.succeedDecode TypeAliasDefinition |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeMaybe w2_x_c_a)

                    _ ->
                        Lamdera.Wire3.failDecode
            )
