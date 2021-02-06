module Test.Wire_Alias_1_Basic exposing (..)

import Lamdera.Wire3
import Test.External exposing (..)


type alias AliasInt =
    Int


type alias AliasResult =
    Result String Int


type alias AliasExternal =
    Maybe ExternalCustomBasic


expected_w2_encode_AliasExternal =
    Lamdera.Wire3.encodeMaybe Test.External.w2_encode_ExternalCustomBasic


expected_w2_decode_AliasExternal =
    Lamdera.Wire3.decodeMaybe Test.External.w2_decode_ExternalCustomBasic


expected_w2_encode_AliasInt =
    Lamdera.Wire3.encodeInt


expected_w2_decode_AliasInt =
    Lamdera.Wire3.decodeInt


expected_w2_encode_AliasResult =
    Lamdera.Wire3.encodeResult Lamdera.Wire3.encodeString Lamdera.Wire3.encodeInt


expected_w2_decode_AliasResult =
    Lamdera.Wire3.decodeResult
        Lamdera.Wire3.decodeString
        Lamdera.Wire3.decodeInt
