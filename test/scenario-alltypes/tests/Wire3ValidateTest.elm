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
        , w3_decodeWithoutValidate_Container
        , w3_decodeWithoutValidate_Validated
        , w3_decode_Container
        , w3_decode_Validated
        , w3_encode_Container
        , w3_encode_Validated
        )
import Test.Wire_Validate_Recursive
    exposing
        ( Tree(..)
        , w3_decodeWithoutValidate_Tree
        , w3_decode_Tree
        , w3_encode_Tree
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


{-| Sibling round-trips using the non-validating decoder variant. These should
accept values the validating variant rejects, because no `w3_validate_*` is
called at any level.
-}
roundtripValidatedWithoutValidate : Validated -> Maybe Validated
roundtripValidatedWithoutValidate value =
    Bytes.Decode.decode
        w3_decodeWithoutValidate_Validated
        (Bytes.Encode.encode (w3_encode_Validated value))


roundtripTreeWithoutValidate : Tree -> Maybe Tree
roundtripTreeWithoutValidate value =
    Bytes.Decode.decode
        w3_decodeWithoutValidate_Tree
        (Bytes.Encode.encode (w3_encode_Tree value))


roundtripContainerWithoutValidate : Container -> Maybe Container
roundtripContainerWithoutValidate value =
    Bytes.Decode.decode
        w3_decodeWithoutValidate_Container
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
        , describe "w3_decodeWithoutValidate_ skips validation entirely"
            [ test "Validated: a value that fails validation still decodes" <|
                \_ ->
                    -- w3_decode_Validated would reject this (ValidatedInt < 0),
                    -- but w3_decodeWithoutValidate_Validated never calls
                    -- w3_validate_Validated, so the round-trip succeeds.
                    roundtripValidatedWithoutValidate (ValidatedInt (-1))
                        |> Expect.equal (Just (ValidatedInt (-1)))
            , test "Validated: an empty string still decodes" <|
                \_ ->
                    roundtripValidatedWithoutValidate (ValidatedString "")
                        |> Expect.equal (Just (ValidatedString ""))
            , test "Tree: validation does not cascade -- a deeply-nested negative leaf still decodes" <|
                \_ ->
                    let
                        tree =
                            Branch (Leaf 1) (Branch (Leaf 2) (Leaf (-3)))
                    in
                    roundtripTreeWithoutValidate tree
                        |> Expect.equal (Just tree)
            , test "Container: an invalid Validated field still decodes (the alias variant uses non-validating field decoders)" <|
                \_ ->
                    let
                        c : Container
                        c =
                            { item = ValidatedInt (-1)
                            , items = [ ValidatedString "" ]
                            }
                    in
                    roundtripContainerWithoutValidate c
                        |> Expect.equal (Just c)
            ]
        ]
