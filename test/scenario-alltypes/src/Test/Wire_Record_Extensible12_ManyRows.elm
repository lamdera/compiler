module Test.Wire_Record_Extensible12_ManyRows exposing (..)

{-| Regression: a type variable that is the row tail of more than one record in one type.

    type alias Player a = { a | status : Int, moves : List Int }
    type T a = A { a | x : Int } | B (Player a)

The codec of a row tail's type variable is the codec of the whole record it ends, and
no one codec is the codec of both a `{ a | x : Int }` and a `{ a | moves, status }`. The
generated codecs asked for one anyway, the union of the two, which nothing has: inferring
the decoder never finished, `lamdera make` grew past 46GB (Elm's unifier loops on
`{ a | x : Int }` against `{ a | moves : List Int, status : Int }`).

Such a parameter's codecs now take a codec for each of its records, below `T`'s take the
codecs of a `{ a | moves, status }` and of a `{ a | x }`.

-}

import Bytes.Decode
import Bytes.Encode
import Lamdera.Wire3
import Test.Wire_Record_Extensible12_ManyRows_Other as Other exposing (Player)


type T a
    = A { a | x : Int }
    | B (Player a)


expected_w3_encode_T : ({ a | moves : List Int, status : Int } -> Lamdera.Wire3.Encoder) -> ({ a | x : Int } -> Lamdera.Wire3.Encoder) -> T a -> Lamdera.Wire3.Encoder
expected_w3_encode_T w3_x_c_a__1 w3_x_c_a__2 w3v =
    case w3v of
        A v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Bytes.Encode.unsignedInt8 0, w3_x_c_a__2 v0 ]

        B v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength [ Bytes.Encode.unsignedInt8 1, Other.w3_encode_Player w3_x_c_a__1 v0 ]


expected_w3_decode_T w3_x_c_a__1 w3_x_c_a__2 =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode A |> Lamdera.Wire3.andMapDecode w3_x_c_a__2

                    1 ->
                        Lamdera.Wire3.succeedDecode B |> Lamdera.Wire3.andMapDecode (Other.w3_decode_Player w3_x_c_a__1)

                    _ ->
                        Lamdera.Wire3.failDecode
            )


{-| The type variable on its own as well, and three records -}
type Bare a
    = BareRow { a | x : Int }
    | BareVar a
    | BareList (List a)


type Three a
    = ThreeX { a | x : Int }
    | ThreeXY { a | x : Int, y : Int }
    | ThreePlayer (Player a)


{-| Passed on through another custom type, an alias and a record field -}
type Wrap b
    = Wrap (T b)
    | WrapMaybe (Maybe (T b))


type alias Holder b =
    { t : T b, three : Three b, n : Int }


{-| An alias whose own parameter has two records, called rather than inlined -}
type alias Both a =
    { first : { a | x : Int }, second : Player a }


{-| A foreign type with two records, generic and at a record -}
type UsesOther b
    = UsesOther (Other.Other b)
    | UsesOtherAt (Other.Other { name : String })


{-| A parameter with a single record next to it keeps its single codec -}
type Mixed a b
    = MixedA (T a)
    | MixedB { b | z : Int }


{-| An alias in the row of the argument -}
type alias Tagged r =
    { r | tag : Int }


type Tags b
    = Tags (T (Tagged b))
    | TagsAt (T (Tagged { name : String }))


{-| An alias extending a record, its other parameter with two records in it, inlined -}
type alias Ext r p =
    { r | f : { p | x : Int }, g : { p | y : Int } }


{-| Recursive -}
type Rec a
    = RecA { a | x : Int }
    | RecB { a | y : Int } (List (Rec a))


type alias Named =
    { name : String }


{-| All of the above at records -}
type alias Concrete =
    { t : T { name : String }
    , tNamed : T Named
    , bare : Bare Named
    , three : Three { name : String }
    , wrap : Wrap Named
    , holder : Holder Named
    , both : Both { name : String }
    , usesOther : UsesOther Named
    , other : Other.Other Named
    , mixed : Mixed Named { name : String }
    , tags : Tags Named
    , ext : Ext { z : Int } { name : String }
    , rec : Rec Named
    }


{-| The decoders are inferred: these check they decode the type they are for -}
decodeConcrete : Lamdera.Wire3.Decoder Concrete
decodeConcrete =
    w3_decode_Concrete


decodeT : Lamdera.Wire3.Decoder (T Named)
decodeT =
    w3_decode_T w3_decode_PlayerNamed w3_decode_RowNamed


decodeBare : Lamdera.Wire3.Decoder (Bare Named)
decodeBare =
    w3_decode_Bare w3_decode_Named w3_decode_RowNamed


type alias PlayerNamed =
    Player Named


type alias RowNamed =
    { name : String, x : Int }
