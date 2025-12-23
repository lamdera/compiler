module Compiler.Elm.String exposing
    ( Chunk(..)
    , TString
    , bTString
    , fromChunks
    , toChars
    )

import Compiler.Data.Utf8 as Utf8
import Extra.Data.Binary as B
import Extra.Type.List as MList exposing (TList)
import Hex



-- STRINGS


type alias TString =
    Utf8.Utf8



-- HELPERS


toChars : TString -> String
toChars =
    identity



-- FROM CHUNKS


type Chunk
    = Slice Int Int
    | Escape Int
    | CodePoint Int


fromChunks : String -> TList Chunk -> TString
fromChunks src chunks =
    String.concat (MList.map (chunkToString src) chunks)


chunkToString : String -> Chunk -> String
chunkToString src chunk =
    case chunk of
        Slice start len ->
            String.slice start (start + len) src

        Escape code ->
            String.cons '\\' (String.fromChar (Char.fromCode code))

        CodePoint code ->
            if code < 0xFFFF then
                codeToString code

            else
                let
                    rest =
                        code - 0x00010000

                    hi =
                        rest // 0x0400

                    lo =
                        modBy 0x0400 rest
                in
                codeToString (hi + 0xD800)
                    ++ codeToString (lo + 0xDC00)


codeToString : Int -> String
codeToString code =
    code |> Hex.toString |> String.toUpper |> String.padLeft 4 '0' |> String.append "\\u"



-- BINARY


bTString : B.Binary TString
bTString =
    Utf8.bVeryLong
