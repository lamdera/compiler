module Extra.System.Dir.Local exposing (getTree)

import Bytes exposing (Bytes)
import Extra.System.Dir.Util as Util
import Extra.System.IO as IO
import Extra.Type.Either exposing (Either(..))


getTree : Maybe String -> String -> Util.IO b c d e f g h (Util.Tree b c d e f g h)
getTree prefix remotePath =
    Util.requestString prefix remotePath <|
        \response ->
            case response of
                Right body ->
                    processBody prefix remotePath body

                Left _ ->
                    Util.getTreeError


processBody : Maybe String -> String -> String -> Util.IO b c d e f g h (Util.Tree b c d e f g h)
processBody prefix remotePath body =
    Util.getTree (fileStep prefix) beforeDirStep afterDirStep remotePath body


type alias Acc =
    String


fileStep : Maybe String -> Util.FileStep b c d e f g h Acc
fileStep prefix name _ dirPath =
    ( dirPath, getFile prefix (dirPath ++ "/" ++ name) )


beforeDirStep : Util.BeforeDirStep Acc
beforeDirStep name dirPath =
    dirPath ++ "/" ++ name


afterDirStep : Util.AfterDirStep Acc
afterDirStep _ beforeFilePath _ =
    beforeFilePath


getFile : Maybe String -> String -> Util.IO b c d e f g h (Maybe Bytes)
getFile prefix filePath =
    Util.requestBytes prefix filePath <|
        \response ->
            case response of
                Right body ->
                    IO.return (Just body)

                Left _ ->
                    IO.return Nothing
