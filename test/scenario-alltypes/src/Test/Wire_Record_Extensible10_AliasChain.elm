module Test.Wire_Record_Extensible10_AliasChain exposing (..)

{-| Regression: chains of extensible record aliases whose innermost argument is an alias
that is itself an alias (rather than a record), or mentions a type variable.

    type alias Outer a = { a | outer : Int }
    type alias Middle b = { b | middle : Int }
    type alias Inner d = { d | inner : Int }
    type alias Base m = { base : m }
    type alias InnerBase m = Inner (Base m)

`Outer (Middle (InnerBase Int))` crashed the compiler ("bad nesting Holey"): `InnerBase Int`
resolves to an alias of a record, not to a record. With a type variable,
`Outer (Middle (InnerBase m))`, resolution stopped at the generic `InnerBase m`, so the
chain was not inlined and `Outer`'s codec was called with `Middle`'s, which is the codec of
a closed record lacking `outer`: a TYPE MISMATCH. Found in massivedecks
(`MassiveDecks.Card.Source.Methods.ExternalMethods msg`, the `MethodsLike` shape below).

-}


type alias Outer a =
    { a | outer : Int }


type alias Middle b =
    { b | middle : Int }


type alias Inner d =
    { d | inner : Int }


type alias Base m =
    { base : m }


type alias InnerBase m =
    Inner (Base m)


type alias ChainConcrete =
    Outer (Middle (InnerBase Int))


type alias ChainGeneric m =
    Outer (Middle (InnerBase m))


{-| Still row polymorphic all the way down: not inlined, and never was -}
type alias ChainOpen m =
    Outer (Middle m)


{-| massivedecks' shape, records of functions (which never go over the wire) included -}
type alias MethodsLike msg =
    WithEditor (WithTooltip (GeneralExternal msg) msg) msg


type alias General msg =
    { name : () -> String
    , logo : () -> Maybe msg
    }


type alias GeneralExternal msg =
    WithEmpty (General msg)


type alias WithTooltip general msg =
    { general
        | tooltip : String -> Maybe msg
        , defaultDetails : Int -> String
    }


type alias WithEditor general msg =
    { general
        | editor : Int -> msg
        , equals : Int -> Bool
    }


type alias WithEmpty rest =
    { rest
        | empty : Int -> String
        , id : () -> String
    }
