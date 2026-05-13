module Test.Wire_Unsupported exposing (..)

import Lamdera.Wire3


type alias RecordWithFunctions =
    { something : Int -> String }


expected_w3_encode_RecordWithFunctions : RecordWithFunctions -> Lamdera.Wire3.Encoder
expected_w3_encode_RecordWithFunctions =
    Lamdera.Wire3.failEncode


expected_w3_decode_RecordWithFunctions =
    Lamdera.Wire3.failDecode


-- Function-type aliases should not generate codecs at all (no w3_encode_/w3_decode_ generated).
-- These caused TOO MANY ARGS errors in billstclair/elm-s3 when parameterized function aliases
-- like `type alias ResponseDecoder a = Metadata -> String -> Result String a` got codecs with
-- wrong arity.


type alias FuncAlias =
    Int -> String


type alias ParamFuncAlias a =
    String -> Result String a


-- A type wrapping a function-type alias exercises the TAlias branch in Encoder/Decoder,
-- which must short-circuit to failEncode/failDecode for function-type aliases.


type alias WrapFuncAlias a =
    List (ParamFuncAlias a)


expected_w3_encode_WrapFuncAlias : (a -> Lamdera.Wire3.Encoder) -> WrapFuncAlias a -> Lamdera.Wire3.Encoder
expected_w3_encode_WrapFuncAlias w3_x_c_a =
    Lamdera.Wire3.encodeList Lamdera.Wire3.failEncode


expected_w3_decode_WrapFuncAlias w3_x_c_a =
    Lamdera.Wire3.decodeList Lamdera.Wire3.failDecode
