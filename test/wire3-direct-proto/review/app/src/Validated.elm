module Validated exposing (..)

{-| Types with `w3_validate_<T>` validators, for the direct-vs-Elm oracles: validators
that accept everything, reject some values, reject based on nested content, on
recursive, mutually recursive and parameterised types, used inside Lists, Dicts and
Maybes, as Enum and Unbox unions, and a type without a validator that contains
validated ones. check.js wraps every `w3_validate_*` at the JS level, so it compares
the sequence of validator calls (and their arguments) too, not just the outcomes.

This module must not reference its own generated wire functions (a module with
validators can't), so Main does that.
-}

import Dict exposing (Dict)


{-| Rejects negative numbers. Single ctor with a single arg: Unbox in prod.
-}
type Positive
    = Positive Int


w3_validate_Positive : Positive -> Result String ()
w3_validate_Positive (Positive n) =
    if n < 0 then
        Err ("Positive " ++ String.fromInt n)

    else
        Ok ()


{-| Recursive. A leaf must not be negative, a node must not be labelled 13, and a node
whose left child is a leaf must not have that leaf be larger than the node's label
(nested content). Validation runs at every level, innermost first.
-}
type VTree
    = VLeaf Int
    | VNode VTree Int VTree


w3_validate_VTree : VTree -> Result String ()
w3_validate_VTree t =
    case t of
        VLeaf n ->
            if n < 0 then
                Err ("VLeaf " ++ String.fromInt n)

            else
                Ok ()

        VNode left label _ ->
            if label == 13 then
                Err "VNode 13"

            else
                case left of
                    VLeaf n ->
                        if n > label + 1000 then
                            Err ("VNode left leaf " ++ String.fromInt n ++ " > " ++ String.fromInt label)

                        else
                            Ok ()

                    _ ->
                        Ok ()


{-| Parameterised and recursive through a List. Rejects more than 3 children; the
validator can't look inside `a`.
-}
type VRose a
    = VRose a (List (VRose a))


w3_validate_VRose : VRose a -> Result String ()
w3_validate_VRose (VRose _ kids) =
    if List.length kids > 3 then
        Err ("VRose kids " ++ String.fromInt (List.length kids))

    else
        Ok ()


{-| Two type variables, recursive through Maybe, accepts everything.
-}
type VPair a b
    = VPair a b (Maybe (VPair a b))
    | VPairEnd


w3_validate_VPair : VPair a b -> Result String ()
w3_validate_VPair _ =
    Ok ()


{-| All-nullary (Enum in prod). Rejects VC.
-}
type VEnum
    = VA
    | VB
    | VC


w3_validate_VEnum : VEnum -> Result String ()
w3_validate_VEnum e =
    case e of
        VC ->
            Err "VC"

        _ ->
            Ok ()


{-| Single ctor, single arg holding a List of validated values. Rejects on the nested
content: more than 4 elements, or a sum over 1000 (the elements were validated first).
-}
type VBox
    = VBox (List Positive)


w3_validate_VBox : VBox -> Result String ()
w3_validate_VBox (VBox ps) =
    let
        total =
            List.foldl (\(Positive n) acc -> acc + n) 0 ps
    in
    if List.length ps > 4 then
        Err "VBox too long"

    else if total > 1000 then
        Err ("VBox total " ++ String.fromInt total)

    else
        Ok ()


{-| Mutually recursive, each with its own validator.
-}
type VPing
    = VPing Int (Maybe VPong)


type VPong
    = VPong String (List VPing)


w3_validate_VPing : VPing -> Result String ()
w3_validate_VPing (VPing n _) =
    if n == 7 then
        Err "VPing 7"

    else
        Ok ()


w3_validate_VPong : VPong -> Result String ()
w3_validate_VPong (VPong s pings) =
    if s == "bad" then
        Err "VPong bad"

    else if List.length pings > 2 then
        Err "VPong too many pings"

    else
        Ok ()


{-| Phantom type variable.
-}
type VPhantom a
    = VPhantom Int


w3_validate_VPhantom : VPhantom a -> Result String ()
w3_validate_VPhantom (VPhantom n) =
    if n == 99 then
        Err "VPhantom 99"

    else
        Ok ()


{-| A record argument with a Dict of validated values.
-}
type VRec
    = VRec { name : String, scores : Dict String Positive, child : Maybe VRec }
    | VRecNone


w3_validate_VRec : VRec -> Result String ()
w3_validate_VRec r =
    case r of
        VRec { name } ->
            if String.length name > 8 then
                Err ("VRec name " ++ name)

            else
                Ok ()

        VRecNone ->
            Ok ()


{-| No validator, but contains validated types: its own decode never validates, the
values inside it are validated as they are decoded.
-}
type Unvalidated
    = Unvalidated (List Positive) (Maybe VEnum)


type alias VModel =
    { positives : List Positive
    , tree : VTree
    , roses : Dict String (VRose Int)
    , rosePair : VRose ( Positive, String )
    , pair : Maybe (VPair String Positive)
    , enums : List VEnum
    , box : VBox
    , ping : VPing
    , phantom : VPhantom String
    , rec : VRec
    , unvalidated : Unvalidated
    , keyed : Dict Int (List (Maybe VTree))
    }
