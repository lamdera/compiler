module Extra.System.Dir.Zip exposing (getTree)

import Bytes exposing (Bytes)
import Extra.System.Dir.Util as Util
import Extra.System.IO as IO
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Time
import Zip
import Zip.Entry


getTree : Maybe String -> String -> Util.IO b c d e f g h (Util.Tree b c d e f g h)
getTree prefix url =
    Util.requestBytes prefix url <|
        \result ->
            case result of
                Right bytes ->
                    processBytes bytes

                Left _ ->
                    Util.getTreeError


processBytes : Bytes -> Util.IO b c d e f g h (Util.Tree b c d e f g h)
processBytes bytes =
    case Zip.fromBytes bytes of
        Just zip ->
            processZip zip

        Nothing ->
            Util.getTreeError


processZip : Zip.Zip -> Util.IO b c d e f g h (Util.Tree b c d e f g h)
processZip zip =
    IO.rmap IO.now <|
        \now ->
            Zip.entries zip
                |> MList.sortOn Zip.Entry.path
                |> MList.foldl (addEntry now) ( [], Map.empty )
                |> finishTree now


type alias Acc b c d e f g h =
    ( {- directories -} TList ( String, Util.Directory b c d e f g h )
    , {- base -} Util.Directory b c d e f g h
    )


addEntry : Time.Posix -> Acc b c d e f g h -> Zip.Entry.Entry -> Acc b c d e f g h
addEntry now (( directories, _ ) as acc) entry =
    case splitEntryPath entry of
        ( entryPath, maybeFileName ) ->
            case dropCommonBase (MList.reverse directories) entryPath of
                ( upCount, downPath ) ->
                    acc
                        |> goUp now upCount
                        |> goDown downPath
                        |> addFile now maybeFileName entry


splitEntryPath : Zip.Entry.Entry -> ( TList String, Maybe String )
splitEntryPath entry =
    let
        path =
            Zip.Entry.path entry
                |> String.split "/"
                |> MList.filter (not << String.isEmpty)
    in
    if Zip.Entry.isDirectory entry then
        ( path, Nothing )

    else
        case MList.reverse path of
            name :: parents ->
                ( MList.reverse parents, Just name )

            [] ->
                ( [], Nothing )


dropCommonBase : TList ( String, Util.Directory b c d e f g h ) -> TList String -> ( Int, TList String )
dropCommonBase directories entryPath =
    case ( directories, entryPath ) of
        ( ( dirName, _ ) :: dirTail, entryName :: entryTail ) ->
            if dirName == entryName then
                dropCommonBase dirTail entryTail

            else
                ( MList.length directories, entryPath )

        _ ->
            ( MList.length directories, entryPath )


goUp : Time.Posix -> Int -> Acc b c d e f g h -> Acc b c d e f g h
goUp now upCount (( directories, root ) as acc) =
    if upCount > 0 then
        case directories of
            ( name, dir ) :: ( parentName, parentDir ) :: rest ->
                goUp now
                    (upCount - 1)
                    ( ( parentName, Map.insert name ( now, Util.DirectoryEntry dir ) parentDir ) :: rest, root )

            [ ( name, dir ) ] ->
                ( [], Map.insert name ( now, Util.DirectoryEntry dir ) root )

            [] ->
                acc

    else
        acc


goDown : TList String -> Acc b c d e f g h -> Acc b c d e f g h
goDown downPath (( directories, root ) as acc) =
    case downPath of
        name :: rest ->
            goDown rest ( ( name, Map.empty ) :: directories, root )

        [] ->
            acc


addFile : Time.Posix -> Maybe String -> Zip.Entry.Entry -> Acc b c d e f g h -> Acc b c d e f g h
addFile now maybeName entry (( directories, root ) as acc) =
    case maybeName of
        Just name ->
            case directories of
                ( dirName, dir ) :: rest ->
                    ( ( dirName, addFileHelper now name entry dir ) :: rest, root )

                [] ->
                    ( [], addFileHelper now name entry root )

        Nothing ->
            acc


addFileHelper : Time.Posix -> String -> Zip.Entry.Entry -> Util.Directory b c d e f g h -> Util.Directory b c d e f g h
addFileHelper now name entry dir =
    case Zip.Entry.toBytes entry of
        Ok bytes ->
            Map.insert name ( now, Util.UnreadFileEntry (Bytes.width bytes) (IO.return (Just bytes)) ) dir

        Err _ ->
            dir


finishTree : Time.Posix -> Acc b c d e f g h -> Util.Tree b c d e f g h
finishTree now (( directories, _ ) as acc) =
    case goUp now (MList.length directories) acc of
        ( _, root ) ->
            ( now, root )
