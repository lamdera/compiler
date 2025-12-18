port module Backend_Eval_ exposing (main)

import Lamdera.Wire3 as Wire
import Types


port log : String -> Cmd msg


main : Program Wire.Bytes () ()
main =
    Platform.worker
        { init = \bytes -> ( (), log (eval bytes) )
        , update = \() () -> ( (), Cmd.none )
        , subscriptions = \() -> Sub.none
        }


eval : Wire.Bytes -> String
eval bytes =
    case Wire.bytesDecode Types.w3_decode_BackendModel bytes of
        Just model ->
            Debug.toString (expression model)

        Nothing ->
            "Failed to decode BackendModel"


expression model =
    model
