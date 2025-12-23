module Extra.System.Exception exposing (handle)

import Extra.System.IO as IO exposing (IO)
import Extra.Type.Either exposing (Either(..))


handle : (e1 -> IO s (Either e2 a)) -> IO s (Either e1 (Either e2 a)) -> IO s (Either e2 a)
handle onError ia =
    IO.bind ia <|
        \result ->
            case result of
                Right a ->
                    IO.return a

                Left e1 ->
                    onError e1



-- TODO: implement a replacement for Debug.todo
--
--error : String -> a
--error msg =
--    if JE.object [ ( "keyatouterlevel", JE.object [ ( msg, JE.null ) ] ) ] == JE.object [] then
--        error "yes"
--
--    else
--        error "no"
