module Evergreen exposing (..)

{-

   data types available in elm:
   - int
   - float
   - string
   - char
   - unit
   - records
   - tuples
   - custom types
     - bool, user-defined types, possibly with holes
   - type aliases

   all codecs take a record argument with its type arguments as keys, possibly empty I guess, and codecs as values

-}
-- encode ints

import Bytes as B
import Bytes.Decode as D exposing (Decoder)
import Bytes.Encode as E exposing (Encoder)
import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


decodeAndMap : Decoder a -> Decoder (a -> b) -> Decoder b
decodeAndMap =
    D.map2 (|>)

decodeAndThen = D.andThen

--

endianness =
    B.LE

succeedDecode : a -> Decoder a
succeedDecode = D.succeed

failDecode : Decoder a
failDecode = D.fail

failEncode : a -> Encoder
failEncode a = encodeSequence []



-- CODECS


encodeFloat64 : Float -> Encoder
encodeFloat64 f =
    E.float64 endianness f


decodeFloat64 : Decoder Float
decodeFloat64 =
    D.float64 endianness



--


intDivBy : Int -> Int -> Int
intDivBy b a =
    let
        v =
            toFloat a / toFloat b
    in
    if v < 0 then
        -(floor -v)

    else
        floor v



--
{-
   varint format:
   0-216   1 byte    value = B0
   217-248 2 bytes   value = 216 + 256 * (B0 - 216) + B1
   249-255 3-9 bytes value = (B0 - 249 + 2) little-endian bytes following B0.

   and then:
   Integers are mapped to positive integers, so that positive integers become positive even numbers (2n) and negative integers become positive odd numbers. (-2n-1)
   This is the same as moving the sign bit from the most significant position to the least significant. Otherwise, varint will encode negative numbers as large integers.

   inspiration:
   https://github.com/dominictarr/signed-varint
   IeSQLite4:
   0-184   1 byte    value = B0
   185-248 2 bytes   value = 185 + 256 * (B0 - 185) + B1
   249-255 3-9 bytes value = (B0 - 249 + 2) little-endian bytes following B0.

   NOTE: Elm (js) uses F64 to represent ints, so not all values are available, and since we use the least significant bit for sign bit, large numbers become positive, even larger numbers become even, even larger become multiples of 4, etc.
-}


encodeInt64 : Int -> Encoder
encodeInt64 i =
    let
        n =
            signedToUnsigned i

        n0 =
            modBy 256 n

        n1 =
            modBy 256 (n |> intDivBy 256)

        n2 =
            modBy 256 (n |> intDivBy 256 |> intDivBy 256)

        n3 =
            modBy 256 (n |> intDivBy 256 |> intDivBy 256 |> intDivBy 256)

        ei e =
            E.sequence <| List.map E.unsignedInt8 e
    in
    if n <= 215 then
        ei <| [ n ]

    else if n <= 9431 then
        ei <| [ 216 + ((n - 216) |> intDivBy 256), modBy 256 (n - 216) ]

    else if n < 256 * 256 then
        ei <| [ 252, n0, n1 ]

    else if n < 256 * 256 * 256 then
        ei <| [ 253, n0, n1, n2 ]

    else if n < 256 * 256 * 256 * 256 then
        ei <| [ 254, n0, n1, n2, n3 ]

    else
        E.sequence [ E.unsignedInt8 255, encodeFloat64 (toFloat i) ]


signedToUnsigned i =
    if i < 0 then
        -2 * i - 1

    else
        2 * i


unsignedToSigned i =
    if modBy 2 i == 1 then
        (i + 1) |> intDivBy -2

    else
        i |> intDivBy 2


decodeInt64 : Decoder Int
decodeInt64 =
    let
        d =
            decodeAndMap D.unsignedInt8
    in
    D.unsignedInt8
        |> D.andThen
            (\n0 ->
                if n0 <= 215 then
                    D.succeed n0 |> D.map unsignedToSigned

                else if n0 < 252 then
                    D.succeed (\b0 -> (n0 - 216) * 256 + b0 + 216) |> d |> D.map unsignedToSigned

                else if n0 == 252 then
                    D.succeed (\b0 b1 -> b0 * 256 + b1) |> d |> d |> D.map unsignedToSigned

                else if n0 == 253 then
                    D.succeed (\b0 b1 b2 -> (b0 * 256 + b1) * 256 + b2) |> d |> d |> d |> D.map unsignedToSigned

                else if n0 == 254 then
                    D.succeed (\b0 b1 b2 b3 -> ((b0 * 256 + b1) * 256 + b2) * 256 + b3) |> d |> d |> d |> d |> D.map unsignedToSigned

                else
                    decodeFloat64 |> D.map identityFloatToInt
            )


{-| `floor` is one of few functions that turn integer floats in js into typed integers in Elm, e.g. the Float `3` into the Int `3`.
-}
identityFloatToInt : Float -> Int
identityFloatToInt =
    floor


encodeString : String -> Encoder
encodeString s =
    E.sequence
        [ E.unsignedInt32 endianness (E.getStringWidth s)
        , E.string s
        ]


decodeString : Decoder String
decodeString =
    D.unsignedInt32 endianness
        |> D.andThen D.string



-- lists


encodeList : (a -> Encoder) -> List a -> Encoder
encodeList enc s =
    E.sequence
        (encodeInt64 (List.length s)
            :: List.map enc s
        )


decodeList : Decoder a -> Decoder (List a)
decodeList decoder =
    let
        listStep : ( Int, List a ) -> Decoder (D.Step ( Int, List a ) (List a))
        listStep ( n, xs ) =
            if n <= 0 then
                D.succeed (D.Done xs)

            else
                D.map (\x -> D.Loop ( n - 1, x :: xs )) decoder
    in
    decodeInt64
        |> D.andThen (\len -> D.loop ( len, [] ) listStep |> D.map List.reverse)




encodeSequence : List Encoder -> Encoder
encodeSequence s =
    E.sequence (encodeInt64 (List.length s) :: s)


--


encodeChar : Char -> Encoder
encodeChar c =
    Char.toCode c
        |> encodeInt64


decodeChar : Decoder Char
decodeChar =
    decodeInt64 |> D.map Char.fromCode



--


encodeUnit : () -> Encoder
encodeUnit () =
    E.sequence []


decodeUnit : Decoder ()
decodeUnit =
    D.succeed ()



--


encodePair : (a -> Encoder) -> (b -> Encoder) -> ( a, b ) -> Encoder
encodePair c_a c_b ( a, b ) =
    E.sequence [ c_a a, c_b b ]


decodePair : Decoder a -> Decoder b -> Decoder ( a, b )
decodePair c_a c_b =
    D.succeed (\a b -> ( a, b ))
        |> decodeAndMap c_a
        |> decodeAndMap c_b


encodeTriple : (a -> Encoder) -> (b -> Encoder) -> (c -> Encoder) -> ( a, b, c ) -> Encoder
encodeTriple c_a c_b c_c ( a, b, c ) =
    E.sequence [ c_a a, c_b b, c_c c ]


decodeTriple : Decoder a -> Decoder b -> Decoder c -> Decoder ( a, b, c )
decodeTriple c_a c_b c_c =
    D.succeed (\a b c -> ( a, b, c ))
        |> decodeAndMap c_a
        |> decodeAndMap c_b
        |> decodeAndMap c_c


-- Array

encodeArray : (a -> Encoder) -> Array a -> Encoder
encodeArray enc a =
    encodeList enc (Array.toList a)

decodeArray : Decoder a -> Decoder (Array a)
decodeArray a = decodeList a |> D.map Array.fromList

-- Basics


encodeBool : Bool -> Encoder
encodeBool b =
    encodeString <|
    case b of
        True -> "True"
        False -> "False"

decodeBool : Decoder Bool
decodeBool =
    decodeString
    |> D.andThen
      (\s ->
        case s of
          "True" -> D.succeed True
          "False" -> D.succeed False
          _ -> D.fail
      )


encodeInt : Int -> Encoder
encodeInt = encodeInt64

decodeInt : Decoder Int
decodeInt = decodeInt64


encodeFloat : Float -> Encoder
encodeFloat = encodeFloat64

decodeFloat : Decoder Float
decodeFloat = decodeFloat64


encodeOrder order =
  case order of
    LT -> encodeSequence [encodeString "LT"]
    EQ -> encodeSequence [encodeString "EQ"]
    GT -> encodeSequence [encodeString "GT"]

decodeOrder =
  decodeString |> decodeAndThen (\order ->
    case order of
      "LT" -> succeedDecode LT
      "EQ" -> succeedDecode EQ
      "GT" -> succeedDecode GT
      _ -> failDecode
  )

-- Dict

encodeDict : (comparable -> Encoder) -> (value -> Encoder) -> Dict comparable value-> Encoder
encodeDict encKey encValue d =
    encodeList (encodePair encKey encValue) (Dict.toList d)

decodeDict : (Decoder comparable) -> (Decoder value) -> Decoder (Dict comparable value)
decodeDict decKey decValue =
    decodeList (decodePair decKey decValue) |> D.map Dict.fromList

-- Set

encodeSet : (comparable -> Encoder) -> Set comparable -> Encoder
encodeSet encVal s =
    encodeList encVal (Set.toList s)


decodeSet : (Decoder comparable) -> Decoder (Set comparable)
decodeSet decVal =
    decodeList decVal |> D.map Set.fromList
