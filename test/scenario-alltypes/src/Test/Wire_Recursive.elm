module Test.Wire_Recursive exposing (..)

import Lamdera.Wire3


type Recursive a
    = Recurse (Recursive a)


expected_w3_encode_Recursive w3_x_c_a w3v =
    case w3v of
        Recurse v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0, w3_encode_Recursive w3_x_c_a v0 ]


expected_w3_decode_Recursive w3_x_c_a =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode Recurse |> Lamdera.Wire3.andMapDecode (w3_decode_Recursive w3_x_c_a)

                    _ ->
                        Lamdera.Wire3.failDecode
            )



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


expected_w3_encode_Lazy w3_x_c_a w3v =
    case w3v of
        Evaluated v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0, w3_x_c_a v0 ]

        Lazy v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 1, Lamdera.Wire3.failEncode () ]


expected_w3_decode_Lazy w3_x_c_a =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode Evaluated |> Lamdera.Wire3.andMapDecode w3_x_c_a

                    1 ->
                        Lamdera.Wire3.succeedDecode Lazy |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.failDecode

                    _ ->
                        Lamdera.Wire3.failDecode
            )


expected_w3_encode_LazyListView w3_x_c_a w3v =
    case w3v of
        Cons v0 v1 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0, w3_x_c_a v0, w3_encode_LazyList w3_x_c_a v1 ]

        Nil ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 1 ]


expected_w3_decode_LazyListView w3_x_c_a =
    Lamdera.Wire3.decodeUnsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode Cons |> Lamdera.Wire3.andMapDecode w3_x_c_a |> Lamdera.Wire3.andMapDecode (w3_decode_LazyList w3_x_c_a)

                    1 ->
                        Lamdera.Wire3.succeedDecode Nil

                    _ ->
                        Lamdera.Wire3.failDecode
            )


expected_w3_encode_LazyList w3_x_c_a =
    w3_encode_Lazy (w3_encode_LazyListView w3_x_c_a)


expected_w3_decode_LazyList w3_x_c_a =
    w3_decode_Lazy (w3_decode_LazyListView w3_x_c_a)



{-

   @TODO This type doesn't work for gen, neither in the old Source-based gen, nor in the new injection.

   From folkertdev:
   and just to elaborate: elm does not support polymorphic recursion (any more). That means that withing
   the body of foo, foo has a fixed type (specifically the type variables in its signature). That means
   we cannot use it in its own body at another type this restriction makes inference simpler/faster, and
   enables monomorphization (which may be relevant when compiling to C/WASM)


-}
--
--
-- type Deque a
--     = Deque (Deque (Maybe a))
--
--
--
-- expected_w3_encode_Deque : (a -> Lamdera.Wire3.Encoder) -> Deque a -> Lamdera.Wire3.Encoder
-- expected_w3_encode_Deque w3_x_c_a w3v =
--     case w3v of
--         Deque v0 ->
--             Lamdera.Wire3.encodeSequenceWithoutLength [ Lamdera.Wire3.encodeUnsignedInt8 0, expected_w3_encode_Deque w3_x_c_a v0 ]
--
--
-- expected_w3_decode_Deque : Lamdera.Wire3.Decoder a -> Lamdera.Wire3.Decoder (Deque a)
-- expected_w3_decode_Deque w3_x_c_a =
--     Lamdera.Wire3.decodeUnsignedInt8
--         |> Lamdera.Wire3.andThenDecode
--             (\w3v ->
--                 case w3v of
--                     0 ->
--                         Lamdera.Wire3.succeedDecode Deque |> Lamdera.Wire3.andMapDecode (expected_w3_decode_Deque (Lamdera.Wire3.decodeMaybe w3_x_c_a))
--
--                     _ ->
--                         Lamdera.Wire3.failDecode
--             )
