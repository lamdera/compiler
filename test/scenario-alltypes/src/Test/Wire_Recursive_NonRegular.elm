module Test.Wire_Recursive_NonRegular exposing (..)

{-| Regression: custom types that refer to themselves with different type arguments
(non-regular, or "nested", data types).

Elm has no polymorphic recursion: inside its own definition a function is only ever
used at the type it is being defined at, annotation or not. Mutual recursion is fine,
an annotated function used by ANOTHER definition is instantiated afresh there. So a
codec for `Icon hasId` that calls itself for an `Icon WithId` field does not typecheck:
the encoders (annotated) fail outright, and the decoders (inferred) quietly get
specialised to `WithId`, failing wherever the type is used at another argument.

Found in lattyware/elm-fontawesome 6.0.0 (`FontAwesome.Internal.Icon`, the `Icon` below),
which made the open source app massivedecks fail to build with `lamdera make`.

-}

import Lamdera.Wire3


type WithId
    = WithId


type WithoutId
    = WithoutId


{-| elm-fontawesome's shape: a phantom parameter, and a field fixing it -}
type Icon hasId
    = Icon
        { id : Maybe String
        , outer : Maybe (Icon WithId)
        }


{-| A parameter that IS used, recursing at a bigger type -}
type Nested a
    = NestedNil
    | NestedCons a (Nested (List a))


{-| Parameters swapped on the way down -}
type Swap a b
    = SwapEnd a
    | SwapNext (Swap b a)


{-| Back to itself at another argument, but only through another type -}
type MutualA x
    = MutualA x (Maybe (MutualB x))


type MutualB y
    = MutualB y (Maybe (MutualA String))


{-| ...and through an alias -}
type Via a
    = Via a (Maybe (ViaAlias a))


type alias ViaAlias a =
    Via (List a)


{-| A regular recursive type next to them, whose codecs must not change -}
type Regular a
    = RegularLeaf a
    | RegularNode (List (Regular a))


{-| Each of them used at other arguments than the ones they recurse at -}
type alias NonRegularHolder =
    { plain : Icon WithoutId
    , withId : Icon WithId
    , nested : Nested Int
    , swap : Swap Int String
    , mutual : MutualA Bool
    , via : Via Float
    , regular : Regular String
    }
