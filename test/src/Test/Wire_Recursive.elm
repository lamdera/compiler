module Test.Wire_Recursive exposing (..)

import Lamdera.Wire2



-- elm-explorations/test's mutually recursive types


type Lazy a
    = Lazy (() -> a)
    | Evaluated a


type LazyListView a
    = Nil
    | Cons a (LazyList a)


{-| Lazy List type.
-}
type alias LazyList a =
    Lazy (LazyListView a)


expected_w2_encode_Lazy w2_x_c_a w2v =
    case w2v of
        Evaluated v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0, w2_x_c_a v0 ]

        Lazy v0 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 1, Lamdera.Wire2.failEncode v0 ]


expected_w2_decode_Lazy w2_x_c_a =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode Evaluated |> Lamdera.Wire2.andMapDecode w2_x_c_a

                    1 ->
                        Lamdera.Wire2.succeedDecode Lazy |> Lamdera.Wire2.andMapDecode Lamdera.Wire2.failDecode

                    _ ->
                        Lamdera.Wire2.failDecode
            )


expected_w2_encode_LazyListView w2_x_c_a w2v =
    case w2v of
        Cons v0 v1 ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 0, w2_x_c_a v0, w2_encode_LazyList w2_x_c_a v1 ]

        Nil ->
            Lamdera.Wire2.encodeSequenceWithoutLength [ Lamdera.Wire2.encodeUnsignedInt8 1 ]


expected_w2_decode_LazyListView w2_x_c_a =
    Lamdera.Wire2.decodeUnsignedInt8
        |> Lamdera.Wire2.andThenDecode
            (\w2v ->
                case w2v of
                    0 ->
                        Lamdera.Wire2.succeedDecode Cons |> Lamdera.Wire2.andMapDecode w2_x_c_a |> Lamdera.Wire2.andMapDecode (w2_decode_LazyList w2_x_c_a)

                    1 ->
                        Lamdera.Wire2.succeedDecode Nil

                    _ ->
                        Lamdera.Wire2.failDecode
            )


expected_w2_encode_LazyList w2_x_c_a =
    w2_encode_Lazy (w2_encode_LazyListView w2_x_c_a)


expected_w2_decode_LazyList w2_x_c_a =
    w2_decode_Lazy (w2_decode_LazyListView w2_x_c_a)
