module Test.Wire_Alias_5_ParamChainMid exposing (..)

{-| Middle of the Wire_Alias_5_ParamChain regression: a parameterized alias whose body is
a bare application of ANOTHER parameterized alias.

That bare application is what makes this a chain rather than a plain alias to a record,
and it is the shape that sent codegen down the TAlias chain-resolution branch.

-}

import Test.Wire_Alias_5_ParamChainLeaf exposing (ParamChainLeaf)


type alias ParamChainOuter p =
    ParamChainInner p


type alias ParamChainInner p =
    { leaf : ParamChainLeaf String
    , flag : Bool
    , param : p
    }
