module Test.Wire_Alias_5_ParamChainLeaf exposing (..)

{-| Leaf of the Wire_Alias_5_ParamChain regression: a parameterized record alias.

Its Holey body mentions its own bound `input` tvar, which is what made
extractTvarsInType report a phantom free tvar for any type containing it.

Lives in its own module on purpose: the entry fixture must NOT import this one, so that
inlining the chain there would reference a codec from a module it cannot see.

-}


type alias ParamChainLeaf input =
    { input : input
    , showErrors : Bool
    }
