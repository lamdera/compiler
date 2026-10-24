module Types exposing (..)

{-| Types from packages that get no generated Wire codecs (elm/bytes here), which
the compiler has no special case for either. Before lamdera/compiler#70 a type with
parameters failed to compile with TOO MANY ARGS, and one without crashed code
generation. They now encode as failures, and `lamdera check` rejects them.
-}

import Bytes exposing (Bytes)
import Bytes.Decode


type alias BackendModel =
    { endianness : Bytes.Endianness
    , step : Bytes.Decode.Step Int String
    , bytes : Bytes
    }


type alias Supported =
    { bytes : Bytes
    }
