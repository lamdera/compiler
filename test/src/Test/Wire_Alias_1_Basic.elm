module Test.Wire_Alias_1_Basic exposing (..)

import Lamdera.Wire2
import Test.External exposing (..)


type alias AliasInt =
    Int


type alias AliasResult =
    Result String Int


type alias AliasExternal =
    Maybe ExternalCustomBasic


w2_encode_AliasExternal =
    Lamdera.Wire2.encodeMaybe Test.External.w2_encode_ExternalCustomBasic


w2_decode_AliasExternal =
    Lamdera.Wire2.decodeMaybe Test.External.w2_decode_ExternalCustomBasic


w2_encode_AliasInt =
    Lamdera.Wire2.encodeInt


w2_decode_AliasInt =
    Lamdera.Wire2.decodeInt


w2_encode_AliasResult =
    Lamdera.Wire2.encodeResult Lamdera.Wire2.encodeString Lamdera.Wire2.encodeInt


w2_decode_AliasResult =
    Lamdera.Wire2.decodeResult Lamdera.Wire2.decodeString Lamdera.Wire2.decodeInt
