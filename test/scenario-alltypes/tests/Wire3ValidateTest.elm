module Wire3ValidateTest exposing (suite)

{-| Runtime tests for the w3_validate decoder hook: decoding must SUCCEED when
`w3_validate_*` returns `Ok ()` and must be REJECTED (decode to `Nothing`) when
it returns `Err`. Encoding never validates -- only decoding does -- so we can
encode a value that fails validation and confirm that decoding it back fails.

Wire functions are imported from the fixture modules (cross-module): a module
that both defines a validator and uses that type's generated wire functions in
its own top-level code would hit the codec ordering rules, so the round-trip
helpers live here instead.

Run with the Lamdera compiler (the wire functions are compiler-generated):

    cd test/scenario-alltypes && npx elm-test --compiler=lamdera tests/Wire3ValidateTest.elm

-}

import Bytes.Decode
import Bytes.Encode
import Expect
import Test exposing (Test, describe, test)
import Test.Wire_Validate
    exposing
        ( Container
        , Validated(..)
        , w3_decode_Container
        , w3_decode_Validated
        , w3_encode_Container
        , w3_encode_Validated
        , w3_unsafe_decode_Container
        , w3_unsafe_decode_Validated
        )
import Test.Wire_Validate_Recursive
    exposing
        ( Tree(..)
        , w3_decode_Tree
        , w3_encode_Tree
        , w3_unsafe_decode_Tree
        )


roundtripValidated : Validated -> Maybe Validated
roundtripValidated value =
    Bytes.Decode.decode
        w3_decode_Validated
        (Bytes.Encode.encode (w3_encode_Validated value))


roundtripTree : Tree -> Maybe Tree
roundtripTree value =
    Bytes.Decode.decode
        w3_decode_Tree
        (Bytes.Encode.encode (w3_encode_Tree value))


roundtripContainer : Container -> Maybe Container
roundtripContainer value =
    Bytes.Decode.decode
        w3_decode_Container
        (Bytes.Encode.encode (w3_encode_Container value))


{-| Same round-trips, but decoding via the w3_unsafe_decode_* chain, which never
runs the validators. Encoding is shared (there is only one encoder).
-}
roundtripValidatedUnsafe : Validated -> Maybe Validated
roundtripValidatedUnsafe value =
    Bytes.Decode.decode
        w3_unsafe_decode_Validated
        (Bytes.Encode.encode (w3_encode_Validated value))


roundtripTreeUnsafe : Tree -> Maybe Tree
roundtripTreeUnsafe value =
    Bytes.Decode.decode
        w3_unsafe_decode_Tree
        (Bytes.Encode.encode (w3_encode_Tree value))


roundtripContainerUnsafe : Container -> Maybe Container
roundtripContainerUnsafe value =
    Bytes.Decode.decode
        w3_unsafe_decode_Container
        (Bytes.Encode.encode (w3_encode_Container value))


suite : Test
suite =
    describe "w3_validate gates decoding"
        [ describe "shallow custom type (Validated)"
            [ describe "passes validation -> Just"
                [ test "ValidatedInt with a non-negative value" <|
                    \_ ->
                        roundtripValidated (ValidatedInt 5)
                            |> Expect.equal (Just (ValidatedInt 5))
                , test "ValidatedInt 0 (boundary)" <|
                    \_ ->
                        roundtripValidated (ValidatedInt 0)
                            |> Expect.equal (Just (ValidatedInt 0))
                , test "ValidatedString with a non-empty value" <|
                    \_ ->
                        roundtripValidated (ValidatedString "hello")
                            |> Expect.equal (Just (ValidatedString "hello"))
                ]
            , describe "fails validation -> Nothing"
                [ test "ValidatedInt with a negative value is rejected" <|
                    \_ ->
                        roundtripValidated (ValidatedInt (-1))
                            |> Expect.equal Nothing
                , test "ValidatedString with an empty value is rejected" <|
                    \_ ->
                        roundtripValidated (ValidatedString "")
                            |> Expect.equal Nothing
                ]
            ]
        , describe "recursive type (Tree) -- validation runs at every node"
            [ test "an all-valid tree roundtrips successfully" <|
                \_ ->
                    let
                        tree =
                            Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
                    in
                    roundtripTree tree
                        |> Expect.equal (Just tree)
            , test "a tree with a deeply-nested negative leaf is rejected" <|
                \_ ->
                    -- The outer Branch passes validation, but decoding the
                    -- inner Trees recurses through w3_decode_Tree (which runs
                    -- the validator), so the (Leaf (-3)) at the bottom fails and
                    -- the whole decode returns Nothing.
                    roundtripTree (Branch (Leaf 1) (Branch (Leaf 2) (Leaf (-3))))
                        |> Expect.equal Nothing
            , test "a shallow tree with a bad immediate leaf is rejected" <|
                \_ ->
                    roundtripTree (Branch (Leaf (-1)) (Leaf 3))
                        |> Expect.equal Nothing
            ]
        , describe "validation runs through aggregate fields (Container)"
            [ test "a valid container roundtrips successfully" <|
                \_ ->
                    let
                        c : Container
                        c =
                            { item = ValidatedInt 5
                            , items = [ ValidatedInt 1, ValidatedString "ok" ]
                            }
                    in
                    roundtripContainer c
                        |> Expect.equal (Just c)
            , test "a container with an invalid `item` field is rejected" <|
                \_ ->
                    roundtripContainer
                        { item = ValidatedInt (-1)
                        , items = []
                        }
                        |> Expect.equal Nothing
            , test "a container with an invalid entry in `items` is rejected" <|
                \_ ->
                    roundtripContainer
                        { item = ValidatedInt 5
                        , items = [ ValidatedInt 1, ValidatedString "" ]
                        }
                        |> Expect.equal Nothing
            ]
        , describe "w3_unsafe_decode_* never validates (trusted-data path)"
            [ test "an invalid ValidatedInt round-trips via unsafe decode" <|
                \_ ->
                    -- The validating decoder rejects this (see below); the unsafe
                    -- decoder must accept it unchanged.
                    roundtripValidatedUnsafe (ValidatedInt (-1))
                        |> Expect.equal (Just (ValidatedInt (-1)))
            , test "an invalid ValidatedString round-trips via unsafe decode" <|
                \_ ->
                    roundtripValidatedUnsafe (ValidatedString "")
                        |> Expect.equal (Just (ValidatedString ""))
            , test "the validating decoder still rejects the same value" <|
                \_ ->
                    roundtripValidated (ValidatedInt (-1))
                        |> Expect.equal Nothing
            , test "unsafe decode does not validate nested nodes (deep bad leaf)" <|
                \_ ->
                    let
                        tree =
                            Branch (Leaf 1) (Branch (Leaf 2) (Leaf (-3)))
                    in
                    roundtripTreeUnsafe tree
                        |> Expect.equal (Just tree)
            , test "unsafe decode does not validate aggregate fields" <|
                \_ ->
                    let
                        c : Container
                        c =
                            { item = ValidatedInt (-1)
                            , items = [ ValidatedString "" ]
                            }
                    in
                    roundtripContainerUnsafe c
                        |> Expect.equal (Just c)
            ]
        ]
