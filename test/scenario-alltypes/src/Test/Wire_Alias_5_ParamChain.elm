module Test.Wire_Alias_5_ParamChain exposing (..)

{-| Regression: a parameterized alias CHAIN referenced with a CONCRETE argument, from a
module that does not import the chain's leaf.

    ParamChainLeaf input = { input : input, showErrors : Bool }   -- Leaf module
    ParamChainOuter p    = ParamChainInner p                      -- Mid module
    ParamChainInner p    = { leaf : ParamChainLeaf String, ... }  -- Mid module
    ParamChainUser       = { framed : ParamChainOuter Int }       -- here

Two separate bugs made this uncompilable, both only when the chain was applied to a
concrete type:

1.  extractTvarsInType counted a Holey alias body's own BOUND parameters as free, so
    resolveTvar judged the concrete argument "still generic" and skipped substituting it.
    The generated codec then referenced a w3_x_c_<tvar> parameter that only exists on the
    chain's own codecs. In a decoder that is an unbound name, crashing type inference with
    a Map.! that reached users as "thread blocked indefinitely in an MVar operation"; in an
    encoder it surfaced as TOO MANY ARGS. Fixed by extractFreeTvarsInType, which treats a
    Holey body's parameters as bound.

2.  The TAlias chain-resolution branch inlined ANY chain that resolved to a record, not
    only the extensible-record case it was added for. Inlining a plain chain copies the
    inner record's field types into THIS module, which then references
    Test.Wire_Alias_5_ParamChainLeaf's codecs — a module this one deliberately does not
    import. foreignTypeSig finds no interface, getForeignSig falls back to its
    non-parameterized failure signatures, and the reference dies on arity. Fixed by
    guarding that branch on reachesExtensibleRecord.

The three-module split is load-bearing: collapse them into one and bug 2 stops
reproducing, because the leaf's codecs would then be in scope.

Found in the wild on customer apps `doomshare` (TOO MANY ARGS, via
`Frame p = Model p` holding a `Form.Field String`) and `character` (MVar, via
`Substrate a = UndoList a`).

-}

import Test.Wire_Alias_5_ParamChainMid exposing (ParamChainOuter)


type alias ParamChainUser =
    { framed : ParamChainOuter Int
    , label : String
    }
