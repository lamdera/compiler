module Test.Wire_Record_Extensible7_ParamConstraint exposing (..)

{-| Regression: the record constraint an encoder puts on its `w3_x_c_<tvar>` parameter,
for the two shapes where lifting that constraint out of an alias body got it wrong.

The constraint only ever appears in the generated TYPE SIGNATURE - the codec bodies here
are untouched by either fix, so nothing about the wire format changes.

1.  `ParamConstraintString` below: an extensible-record alias with a SECOND parameter used
    inside its constraint fields, applied with a concrete argument for that parameter.

        ParamConstraintValue b a = { a | value : Maybe b }
        ParamConstraintString  a = ParamConstraintValue String a

    The constraint came out in the inner alias's own names - `{ a | value : Maybe b }` -
    and `b` is not free in w3_encode_ParamConstraintString's signature. Type inference
    then dies on a Map.! in Type.Instantiate, which reaches users as "thread blocked
    indefinitely in an MVar operation". Fixed by substituting the alias's arguments into
    the lifted constraint.

2.  `ParamConstraintNested` below: an extensible-record alias applied to ANOTHER
    extensible-record alias applied to our tvar.

        ParamConstraintWindow a = { a | windowSize : Int }
        ParamConstraintStream a = { a | locale : String }
        ParamConstraintNested a = ParamConstraintStream (ParamConstraintWindow a)

    Our `a` is not an argument of ParamConstraintStream, it is the row tail of the one
    argument it does get. Constraint extraction read Stream's body under our own name
    anyway, which happens to line up only because Stream named its parameter `a` too, and
    it missed Window's field entirely. The signature then contradicted the body, as
    "The 1st argument to `w3_encode_ParamConstraintWindow` is not what I expect".
    Fixed by descending into the arguments, and by only reading the body for a parameter
    we are actually the row tail of.

Found in the wild on four real apps. All four reach 1 through elm-athlete/athlete, which is full of the
shape: `StringValue a = ValueAttribute String a` over `ValueAttribute b a = { a | value :
Maybe b }`, and `OnStringInputEvent msg a = OnInputEvent String msg a` over
`OnInputEvent b msg a = { a | onInputEvent : Maybe (b -> msg), fromStringInput : String ->
b }`. Three of them then reach 2 through their own
`BS a = BrowserStream (BSWithWindowSize a)`.

-}

import Lamdera.Wire3


type alias ParamConstraintValue b a =
    { a | value : Maybe b }


expected_w3_encode_ParamConstraintValue : (b -> Lamdera.Wire3.Encoder) -> ({ a | value : Maybe b } -> Lamdera.Wire3.Encoder) -> ParamConstraintValue b a -> Lamdera.Wire3.Encoder
expected_w3_encode_ParamConstraintValue w3_x_c_b w3_x_c_a =
    w3_x_c_a


expected_w3_decode_ParamConstraintValue w3_x_c_b w3_x_c_a =
    w3_x_c_a


{-| The `String` here is the whole point: `b` is bound to a concrete type, so the
constraint lifted out of ParamConstraintValue's body has to say `Maybe String`, not
`Maybe b`.
-}
type alias ParamConstraintString a =
    ParamConstraintValue String a


expected_w3_encode_ParamConstraintString : ({ a | value : Maybe String } -> Lamdera.Wire3.Encoder) -> ParamConstraintString a -> Lamdera.Wire3.Encoder
expected_w3_encode_ParamConstraintString w3_x_c_a =
    w3_encode_ParamConstraintValue Lamdera.Wire3.encodeString w3_x_c_a


expected_w3_decode_ParamConstraintString w3_x_c_a =
    w3_decode_ParamConstraintValue Lamdera.Wire3.decodeString w3_x_c_a


type alias ParamConstraintWindow a =
    { a | windowSize : Int }


expected_w3_encode_ParamConstraintWindow : ({ a | windowSize : Int } -> Lamdera.Wire3.Encoder) -> ParamConstraintWindow a -> Lamdera.Wire3.Encoder
expected_w3_encode_ParamConstraintWindow w3_x_c_a =
    w3_x_c_a


expected_w3_decode_ParamConstraintWindow w3_x_c_a =
    w3_x_c_a


type alias ParamConstraintStream a =
    { a | locale : String }


expected_w3_encode_ParamConstraintStream : ({ a | locale : String } -> Lamdera.Wire3.Encoder) -> ParamConstraintStream a -> Lamdera.Wire3.Encoder
expected_w3_encode_ParamConstraintStream w3_x_c_a =
    w3_x_c_a


expected_w3_decode_ParamConstraintStream w3_x_c_a =
    w3_x_c_a


{-| `a` is the row tail of the argument, not the argument itself, so BOTH aliases'
fields land on it.
-}
type alias ParamConstraintNested a =
    ParamConstraintStream (ParamConstraintWindow a)


expected_w3_encode_ParamConstraintNested : ({ a | locale : String, windowSize : Int } -> Lamdera.Wire3.Encoder) -> ParamConstraintNested a -> Lamdera.Wire3.Encoder
expected_w3_encode_ParamConstraintNested w3_x_c_a =
    w3_encode_ParamConstraintStream (w3_encode_ParamConstraintWindow w3_x_c_a)


expected_w3_decode_ParamConstraintNested w3_x_c_a =
    w3_decode_ParamConstraintStream (w3_decode_ParamConstraintWindow w3_x_c_a)
