module Compiler.Elm.Version exposing
    ( Comparable
    , Version(..)
    , bComparable
    , bVersion
    , bumpMajor
    , bumpMinor
    , compiler
    , decoder
    , encode
    , getMajor
    , max
    , one
    , parser
    , toChars
    , toComparable
    )

import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Parse.Primitives as P exposing (Col, Row)
import Extra.Data.Binary as B
import Extra.Data.Binary.Get as BG
import Extra.Data.Binary.Put as BP



-- VERSION


type Version
    = Version
        --{ major : Int
        --, minor : Int
        --, patch : Int
        --}
        Int
        Int
        Int


getMajor : Version -> Int
getMajor (Version major _ _) =
    major


type alias Comparable =
    ( Int, Int, Int )


fromComparable : Comparable -> Version
fromComparable ( major, minor, patch ) =
    Version major minor patch


toComparable : Version -> Comparable
toComparable (Version major minor patch) =
    ( major, minor, patch )


one : Version
one =
    Version 1 0 0


max : Version
max =
    Version (2 ^ 16 - 1) 0 0


compiler : Version
compiler =
    Version 0 19 1



-- BUMP


bumpMinor : Version -> Version
bumpMinor (Version major minor _) =
    Version major (minor + 1) 0


bumpMajor : Version -> Version
bumpMajor (Version major _ _) =
    Version (major + 1) 0 0



-- TO CHARS


toChars : Version -> String
toChars (Version major minor patch) =
    String.fromInt major ++ "." ++ String.fromInt minor ++ "." ++ String.fromInt patch



-- JSON


decoder : D.Decoder ( Row, Col ) Version
decoder =
    D.customString parser Tuple.pair


encode : Version -> E.Value
encode version =
    E.chars (toChars version)



-- BINARY


bVersion : B.Binary Version
bVersion =
    { put =
        \(Version major minor patch) ->
            if major < 255 && minor < 256 && patch < 256 then
                BP.put3 B.bWord8.put B.bWord8.put B.bWord8.put major minor patch

            else
                BP.put4 B.bWord8.put B.bWord16.put B.bWord16.put B.bWord16.put 255 major minor patch
    , get =
        BG.bind B.bWord8.get <|
            \word ->
                if word == 255 then
                    BG.liftM3 Version B.bWord16.get B.bWord16.get B.bWord16.get

                else
                    BG.liftM2 (Version word) B.bWord8.get B.bWord8.get
    }


bComparable : B.Binary Comparable
bComparable =
    B.iso toComparable fromComparable bVersion



-- PARSER


parser : P.Parser ( Row, Col ) Version
parser =
    P.bind numberParser <|
        \major ->
            P.bind (P.word1 0x2E {- . -} Tuple.pair) <|
                \_ ->
                    P.bind numberParser <|
                        \minor ->
                            P.bind (P.word1 0x2E {- . -} Tuple.pair) <|
                                \_ ->
                                    P.bind numberParser <|
                                        \patch ->
                                            P.return (Version major minor patch)


numberParser : P.Parser ( Row, Col ) Int
numberParser =
    P.Parser <|
        \(P.State src pos end indent row col) ->
            if pos >= end then
                P.Eerr row col Tuple.pair

            else
                let
                    word =
                        P.unsafeIndex src pos
                in
                if word == 0x30 {- 0 -} then
                    let
                        newState =
                            P.State src (pos + 1) end indent row (col + 1)
                    in
                    P.Cok 0 newState

                else if isDigit word then
                    let
                        ( total, newPos ) =
                            chompWord16 src (pos + 1) end (word - 0x30)

                        newState =
                            P.State src newPos end indent row (col + (newPos - pos))
                    in
                    P.Cok total newState

                else
                    P.Eerr row col Tuple.pair


chompWord16 : String -> Int -> Int -> Int -> ( Int, Int )
chompWord16 src pos end total =
    if pos >= end then
        ( total, pos )

    else
        let
            word =
                P.unsafeIndex src pos
        in
        if isDigit word then
            chompWord16 src (pos + 1) end (10 * total + word - 0x30)

        else
            ( total, pos )


isDigit : Int -> Bool
isDigit word =
    0x30 {- 0 -} <= word && word <= {- 9 -} 0x39
