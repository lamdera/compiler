module Wire3ValidateTest exposing (suite)

{-| Runtime tests for the w3_validate decoder hook: decoding must SUCCEED when
`w3_validate_*` returns `Ok ()` and must be REJECTED (decode to `Nothing`) when
it returns `Err`.

Note that encoding never validates — only decoding does — so we can encode a
value that fails validation and confirm that decoding it back fails.

This imports the type and its compiler-generated wire functions from
Test.Wire_Validate. The reference is intentionally cross-module: a module that
both defines a validator and uses that type's generated wire functions in its
own top-level code would hit the codec ordering rules, so the round-trip helper
lives here instead.

Run with the Lamdera compiler (the wire functions are compiler-generated):

    cd test/scenario-alltypes && npx elm-test --compiler=lamdera tests/Wire3ValidateTest.elm

-}

import Bytes.Decode
import Bytes.Encode
import Expect
import Test exposing (Test, describe, test)
import Test.Wire_Validate exposing (Validated(..), w3_decode_Validated, w3_encode_Validated)


{-| Encode then decode. A successful decode is `Just value`; a decode that fails
(including a validation failure) is `Nothing`.
-}
roundtrip : Validated -> Maybe Validated
roundtrip value =
    Bytes.Decode.decode
        w3_decode_Validated
        (Bytes.Encode.encode (w3_encode_Validated value))


suite : Test
suite =
    describe "w3_validate gates decoding"
        [ describe "values that pass validation decode successfully (Ok ())"
            [ test "ValidatedInt with a non-negative value" <|
                \_ ->
                    roundtrip (ValidatedInt 5)
                        |> Expect.equal (Just (ValidatedInt 5))
            , test "ValidatedInt 0 (boundary)" <|
                \_ ->
                    roundtrip (ValidatedInt 0)
                        |> Expect.equal (Just (ValidatedInt 0))
            , test "ValidatedString with a non-empty value" <|
                \_ ->
                    roundtrip (ValidatedString "hello")
                        |> Expect.equal (Just (ValidatedString "hello"))
            ]
        , describe "values that fail validation are rejected (Err -> Nothing)"
            [ test "ValidatedInt with a negative value is rejected" <|
                \_ ->
                    roundtrip (ValidatedInt (-1))
                        |> Expect.equal Nothing
            , test "ValidatedString with an empty value is rejected" <|
                \_ ->
                    roundtrip (ValidatedString "")
                        |> Expect.equal Nothing
            ]
        ]
