module Test.Wire_Record_Extensible11_UnionOfAliasRow exposing (..)

{-| Regression: a type parameter that is the row tail of an extensible record somewhere
inside a custom type, or inside an alias that is called rather than inlined.

    type alias Player a = { a | status : Int, moves : List Int }
    type alias ProcessLine race player = { race : race, player : Player player, step : Int }
    type ProcessCheckpointResult race player
        = PlayerActive (ProcessLine race player)
        | PlayerFinished (ProcessLine race player)

ProcessLine's codecs take the codec of the whole `{ player | moves : List Int, status : Int }`
for `player`, but ProcessCheckpointResult's encoder was annotated with a plain
`player -> Encoder`: a TYPE MISMATCH. And at `ProcessCheckpointResult String { name : String }`
the codecs were called with the codec of `{ name : String }` alone, where they need the one
of `{ moves : List Int, name : String, status : Int }` (the decoder's type was then missing
`moves` and `status`). Found in an app.

An alias called with a record, `WithPlayer { name : String }` below, had the same problem,
only without any error from the encoder: it dropped `moves` and `status`.

-}

import Lamdera.Wire3


type alias Player a =
    { a | status : Int, moves : List Int }


type alias ProcessLine race player =
    { race : race
    , player : Player player
    , step : Int
    }


type ProcessCheckpointResult race player
    = PlayerActive (ProcessLine race player)
    | PlayerFinished (ProcessLine race player)


{-| Other parameter names than the alias's -}
type Renamed r p
    = Renamed (ProcessLine r p)
    | RenamedNone


{-| The record directly, and the alias directly -}
type DirectRow a
    = DirectRow { a | x : Int }
    | DirectNone


type DirectAlias a
    = DirectAlias (Player a)


{-| A custom type through another one -}
type Outer race player
    = Outer (ProcessCheckpointResult race player)
    | OuterRenamed (Renamed race player)


{-| The alias in a record field, generic -}
type alias Holder race player =
    { line : ProcessLine race player }


{-| The custom type in a record field, generic -}
type alias Wrapped p =
    { result : ProcessCheckpointResult Int p }


{-| An alias that is called rather than inlined, the row tail inside it -}
type alias WithPlayer a =
    { inner : Player a, n : Int }


type alias Named =
    { name : String }


{-| All of the above at a record -}
type alias Concrete =
    { result : ProcessCheckpointResult String { name : String }
    , renamed : Renamed Int Named
    , outer : Outer Int { name : String }
    , direct : DirectRow { name : String }
    , directAlias : DirectAlias { name : String }
    , holder : Holder Int { name : String }
    , wrapped : Wrapped { name : String }
    , withPlayer : WithPlayer { name : String }
    , line : ProcessLine Int { name : String }
    }


{-| The decoders are inferred: these check they decode the type they are for -}
decodeConcrete : Lamdera.Wire3.Decoder Concrete
decodeConcrete =
    w3_decode_Concrete


decodeResult : Lamdera.Wire3.Decoder (ProcessCheckpointResult String Named)
decodeResult =
    w3_decode_ProcessCheckpointResult Lamdera.Wire3.decodeString w3_decode_PlayerNamed


type alias PlayerNamed =
    Player Named
