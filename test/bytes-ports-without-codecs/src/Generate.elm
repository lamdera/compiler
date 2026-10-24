port module Generate exposing (main)

import Bytes exposing (Bytes)


port out : Bytes -> Cmd msg


port inc : (Bytes -> msg) -> Sub msg


main : Program Bytes Bytes Bytes
main =
    Platform.worker
        { init = \flags -> ( flags, out flags )
        , update = \b _ -> ( b, out b )
        , subscriptions = \_ -> inc identity
        }
