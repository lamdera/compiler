module Extra.System.Dir exposing
    ( Directory
    , Entry(..)
    , FileName
    , FilePath
    , GlobalState
    , LocalState
    , addExtension
    , addName
    , addNames
    , combine
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , dropLastName
    , fromString
    , getAppUserDataDirectory
    , getCurrentDirectory
    , getCurrentDirectoryEntriesPure
    , getCurrentDirectoryNamesPure
    , getModificationTime
    , getNames
    , initialState
    , isRelative
    , makeAbsolute
    , makeRelative
    , mountLocal
    , mountStatic
    , mountZip
    , readFile
    , removeDirectory
    , removeFile
    , resetFileSystem
    , setCurrentDirectory
    , splitExtension
    , splitLastName
    , toString
    , writeFile
    )

import Bytes exposing (Bytes)
import Extra.System.Config as Config
import Extra.System.Dir.Local as Local
import Extra.System.Dir.Static as Static
import Extra.System.Dir.Util as Util
import Extra.System.Dir.Zip as Zip
import Extra.System.IO as IO
import Extra.Type.Lens exposing (Lens)
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Global
import Task
import Time



-- PUBLIC STATE


type alias GlobalState c d e f g h =
    Config.GlobalState (LocalState c d e f g h) c d e f g h


type LocalState c d e f g h
    = LocalState
        -- root
        (Directory c d e f g h)
        -- cwd
        (TList FileName)


initialState : LocalState c d e f g h
initialState =
    LocalState
        -- root
        Map.empty
        -- cwd
        []


lensFileSystem : Lens (GlobalState c d e f g h) (LocalState c d e f g h)
lensFileSystem =
    { getter = \(Global.State _ x _ _ _ _ _ _) -> x
    , setter = \x (Global.State a _ c d e f g h) -> Global.State a x c d e f g h
    }


lensRoot : Lens (GlobalState c d e f g h) (Directory c d e f g h)
lensRoot =
    { getter = \(Global.State _ (LocalState x _) _ _ _ _ _ _) -> x
    , setter = \x (Global.State a (LocalState _ bi) c d e f g h) -> Global.State a (LocalState x bi) c d e f g h
    }


lensCwd : Lens (GlobalState c d e f g h) (TList FileName)
lensCwd =
    { getter = \(Global.State _ (LocalState _ x) _ _ _ _ _ _) -> x
    , setter = \x (Global.State a (LocalState ai _) c d e f g h) -> Global.State a (LocalState ai x) c d e f g h
    }



-- PRIVATE IO


type alias IO c d e f g h v =
    IO.IO (GlobalState c d e f g h) v


resetFileSystem : IO c d e f g h ()
resetFileSystem =
    IO.putLens lensFileSystem initialState



-- FILE PATHS


type alias FileName =
    String


type FilePath
    = Absolute (TList FileName)
    | Relative (TList FileName)


getNames : FilePath -> TList FileName
getNames path =
    case path of
        Absolute names ->
            names

        Relative names ->
            names


modifyNames : FilePath -> (TList FileName -> TList FileName) -> FilePath
modifyNames path f =
    case path of
        Absolute names ->
            Absolute (f names)

        Relative names ->
            Relative (f names)



-- FROM AND TO STRING


fromString : String -> FilePath
fromString string =
    if String.startsWith "/" string then
        fromStringHelper Absolute (String.dropLeft 1 string)

    else
        fromStringHelper Relative string


fromStringHelper : (TList FileName -> FilePath) -> String -> FilePath
fromStringHelper constructor string =
    string
        |> String.split "/"
        |> MList.filter (\name -> name /= "" && name /= ".")
        |> MList.reverse
        |> constructor


toString : FilePath -> String
toString path =
    case path of
        Absolute names ->
            "/" ++ String.join "/" (MList.reverse names)

        Relative [] ->
            "."

        Relative names ->
            String.join "/" (MList.reverse names)



-- STANDARD FUNCTIONS


addExtension : FilePath -> String -> FilePath
addExtension path extension =
    case splitLastName path of
        ( parent, name ) ->
            addName parent (name ++ "." ++ extension)


addName : FilePath -> FileName -> FilePath
addName path name =
    modifyNames path (\names -> name :: names)


addNames : FilePath -> TList FileName -> FilePath
addNames path names =
    MList.foldl addName path names


combine : FilePath -> FilePath -> FilePath
combine bPath aPath =
    case aPath of
        Absolute _ ->
            aPath

        Relative aNames ->
            modifyNames bPath (\bNames -> aNames ++ bNames)


dropLastName : FilePath -> FilePath
dropLastName path =
    Tuple.first (splitLastName path)


isRelative : FilePath -> Bool
isRelative path =
    case path of
        Absolute _ ->
            False

        Relative _ ->
            True


makeRelative : FilePath -> FilePath -> FilePath
makeRelative base path =
    case ( base, path ) of
        ( Absolute baseNames, Absolute pathNames ) ->
            Relative <| MList.reverse <| makeRelativeHelper (MList.reverse baseNames) (MList.reverse pathNames)

        _ ->
            path


makeRelativeHelper : TList FileName -> TList FileName -> TList FileName
makeRelativeHelper baseNames pathNames =
    case ( baseNames, pathNames ) of
        ( baseName :: baseRest, pathName :: pathRest ) ->
            if baseName == pathName then
                makeRelativeHelper baseRest pathRest

            else
                pathNames

        _ ->
            pathNames


splitExtension : FileName -> ( FileName, String )
splitExtension name =
    case MList.reverse (String.split "." name) of
        extension :: rest ->
            ( String.join "." (MList.reverse rest), extension )

        _ ->
            ( name, "" )


splitLastName : FilePath -> ( FilePath, FileName )
splitLastName path =
    case path of
        Absolute (name :: rest) ->
            ( Absolute rest, name )

        Relative (name :: rest) ->
            ( Relative rest, name )

        _ ->
            ( path, "" )



-- FILE SYSTEM


type alias Directory c d e f g h =
    Map.Map FileName ( Time.Posix, Entry c d e f g h )


type Entry c d e f g h
    = FileEntry Bytes
    | DirectoryEntry (Directory c d e f g h)
    | MountedFileEntry Int (IO c d e f g h (Maybe Bytes))


createDirectoryIfMissing : Bool -> FilePath -> IO c d e f g h ()
createDirectoryIfMissing createParents filePath =
    walkFileSystem createParents filePath <|
        \maybeNode now ->
            ( Maybe.andThen
                (\( directory, fileName, maybeEntry ) ->
                    case maybeEntry of
                        Nothing ->
                            Just (Map.insert fileName ( now, DirectoryEntry Map.empty ) directory)

                        _ ->
                            Nothing
                )
                maybeNode
            , ()
            )


doesDirectoryExist : FilePath -> IO c d e f g h Bool
doesDirectoryExist filePath =
    walkFileSystem False filePath <|
        \maybeNode _ ->
            ( Nothing
            , case maybeNode of
                Just ( _, _, Just ( _, DirectoryEntry _ ) ) ->
                    True

                _ ->
                    False
            )


doesFileExist : FilePath -> IO c d e f g h Bool
doesFileExist filePath =
    walkFileSystem False filePath <|
        \maybeNode _ ->
            ( Nothing
            , case maybeNode of
                Just ( _, _, Just ( _, FileEntry _ ) ) ->
                    True

                Just ( _, _, Just ( _, MountedFileEntry _ _ ) ) ->
                    True

                _ ->
                    False
            )


getAppUserDataDirectory : FileName -> IO c d e f g h FilePath
getAppUserDataDirectory app =
    IO.return <| Absolute [ "." ++ app ]


getCurrentDirectory : IO c d e f g h FilePath
getCurrentDirectory =
    IO.rmap IO.get (\s -> Absolute (lensCwd.getter s))


getModificationTime : FilePath -> IO c d e f g h Time.Posix
getModificationTime path =
    walkFileSystem False path <|
        \maybeNode _ ->
            ( Nothing
            , case maybeNode of
                Just ( _, _, Just ( time, FileEntry _ ) ) ->
                    time

                Just ( _, _, Just ( time, MountedFileEntry _ _ ) ) ->
                    time

                _ ->
                    Time.millisToPosix 0
            )


makeAbsolute : FilePath -> IO c d e f g h FilePath
makeAbsolute path =
    case path of
        Absolute _ ->
            IO.return path

        Relative _ ->
            IO.rmap getCurrentDirectory (\cwd -> combine cwd path)


mountLocal : String -> FilePath -> IO c d e f g h ()
mountLocal mountPoint filePath =
    IO.bind Config.mountPrefix <|
        \mountPrefix ->
            IO.bind (Local.getTree mountPrefix mountPoint) (mountHelper filePath)


mountStatic : String -> FilePath -> IO c d e f g h ()
mountStatic mountPoint filePath =
    IO.bind (Static.getTree (Just "") mountPoint) (mountHelper filePath)


mountZip : String -> FilePath -> IO c d e f g h ()
mountZip mountPoint filePath =
    IO.bind Config.httpPrefix <|
        \httpPrefix ->
            IO.bind (Zip.getTree httpPrefix mountPoint) (mountHelper filePath)


mountHelper : FilePath -> Util.Tree (LocalState c d e f g h) c d e f g h -> IO c d e f g h ()
mountHelper filePath mountedTree =
    walkFileSystem True filePath <|
        \maybeNode _ ->
            ( Maybe.andThen (mountEntry (mapMountedTree mountedTree)) maybeNode, () )


mountEntry :
    ( Time.Posix, Directory c d e f g h )
    -> ( Directory c d e f g h, String, Maybe ( Time.Posix, Entry c d e f g h ) )
    -> Maybe (Directory c d e f g h)
mountEntry ( time, mountedDirectory ) ( directory, fileName, maybeEntry ) =
    case maybeEntry of
        Nothing ->
            if fileName == "" then
                -- root
                if Map.null directory then
                    -- empty root => replace with mounted tree
                    Just mountedDirectory

                else
                    -- non-empty root => reject mount
                    Nothing

            else
                -- non-existing entry => add mounted tree
                Just (Map.insert fileName ( time, DirectoryEntry mountedDirectory ) directory)

        Just ( _, DirectoryEntry subDirectory ) ->
            if Map.null subDirectory then
                -- empty directory => replace with mounted tree
                Just (Map.insert fileName ( time, DirectoryEntry mountedDirectory ) directory)

            else
                -- non-empty directory => reject mount
                Nothing

        _ ->
            -- other cases => reject mount
            Nothing


readFile : FilePath -> IO c d e f g h (Maybe Bytes)
readFile filePath =
    IO.andThen getFileContent <|
        walkFileSystem False filePath <|
            \maybeNode _ ->
                ( Nothing
                , case maybeNode of
                    Just ( _, _, Just ( _, FileEntry contents ) ) ->
                        Just (BytesContent contents)

                    Just ( _, _, Just ( _, MountedFileEntry _ io ) ) ->
                        Just (MountedContent io)

                    _ ->
                        Nothing
                )


type FileContent c d e f g h
    = BytesContent Bytes
    | MountedContent (IO c d e f g h (Maybe Bytes))


getFileContent : Maybe (FileContent c d e f g h) -> IO c d e f g h (Maybe Bytes)
getFileContent maybeContent =
    case maybeContent of
        Just (BytesContent bytes) ->
            IO.return (Just bytes)

        Just (MountedContent io) ->
            io

        Nothing ->
            IO.return Nothing


removeDirectory : FilePath -> IO c d e f g h ()
removeDirectory filePath =
    walkFileSystem False filePath <|
        \maybeNode _ ->
            case maybeNode of
                Just ( directory, fileName, Just ( _, DirectoryEntry _ ) ) ->
                    ( Just (Map.delete fileName directory), () )

                _ ->
                    ( Nothing, () )


removeFile : FilePath -> IO c d e f g h ()
removeFile filePath =
    walkFileSystem False filePath <|
        \maybeNode _ ->
            case maybeNode of
                Just ( directory, fileName, Just ( _, FileEntry _ ) ) ->
                    ( Just (Map.delete fileName directory), () )

                _ ->
                    ( Nothing, () )


setCurrentDirectory : FilePath -> IO c d e f g h ()
setCurrentDirectory cwd =
    IO.bind (makeAbsolute cwd) <|
        \absolutePath ->
            IO.putLens lensCwd <| getNames absolutePath


writeFile : FilePath -> Bytes -> IO c d e f g h ()
writeFile filePath contents =
    walkFileSystem False filePath <|
        \maybeNode now ->
            case maybeNode of
                Just ( _, _, Just ( _, DirectoryEntry _ ) ) ->
                    -- can't overwrite a directory
                    ( Nothing, () )

                Just ( directory, fileName, _ ) ->
                    ( Just (Map.insert fileName ( now, FileEntry contents ) directory), () )

                _ ->
                    ( Nothing, () )



-- PURE IO


getCurrentDirectoryNamesPure : GlobalState c d e f g h -> TList FileName
getCurrentDirectoryNamesPure state =
    MList.reverse (lensCwd.getter state)


getCurrentDirectoryEntryPure : GlobalState c d e f g h -> Directory c d e f g h
getCurrentDirectoryEntryPure state =
    lensRoot.getter state
        |> walkFileSystemPure
            False
            (Absolute (lensCwd.getter state))
            (Time.millisToPosix 0)
            (\maybeNode _ ->
                ( Nothing
                , case maybeNode of
                    Just ( root, "", Nothing ) ->
                        root

                    Just ( _, _, Just ( _, DirectoryEntry directory ) ) ->
                        directory

                    _ ->
                        Map.empty
                )
            )
        |> Tuple.first


getCurrentDirectoryEntriesPure :
    GlobalState c d e f g h
    -> (FileName -> Int -> Time.Posix -> z)
    -> ( TList z, TList z )
getCurrentDirectoryEntriesPure state f =
    Map.foldlWithKey
        (\( dirs, files ) name ( time, entry ) ->
            case entry of
                DirectoryEntry directory ->
                    ( f name (Map.size directory) time :: dirs, files )

                FileEntry bytes ->
                    ( dirs, f name (Bytes.width bytes) time :: files )

                MountedFileEntry size _ ->
                    ( dirs, f name size time :: files )
        )
        ( [], [] )
        (getCurrentDirectoryEntryPure state)



-- LOW LEVEL


walkFileSystem :
    Bool
    -> FilePath
    -> (Maybe ( Directory c d e f g h, FileName, Maybe ( Time.Posix, Entry c d e f g h ) ) -> Time.Posix -> ( Maybe (Directory c d e f g h), v ))
    -> IO c d e f g h v
walkFileSystem createDirectories filePath callback =
    IO.bind (IO.liftA2 Tuple.pair (makeAbsolute filePath) getTime) <|
        \( absolutePath, now ) ->
            IO.bind IO.get <|
                \s ->
                    let
                        root =
                            lensRoot.getter s

                        ( result, newRoot ) =
                            walkFileSystemPure createDirectories absolutePath now callback root
                    in
                    IO.rmap (IO.putLens lensRoot newRoot) (\_ -> result)


walkFileSystemPure :
    Bool
    -> FilePath
    -> Time.Posix
    -> (Maybe ( Directory c d e f g h, FileName, Maybe ( Time.Posix, Entry c d e f g h ) ) -> Time.Posix -> ( Maybe (Directory c d e f g h), v ))
    -> Directory c d e f g h
    -> ( v, Directory c d e f g h )
walkFileSystemPure createDirectories filePath now callback root =
    let
        down :
            TList ( Directory c d e f g h, FileName )
            -> TList FileName
            -> Directory c d e f g h
            -> ( v, Directory c d e f g h )
        down visited fileNames directory =
            case fileNames of
                [] ->
                    -- only possible for root directory
                    finishWalk visited <| Just ( directory, "", Nothing )

                [ fileName ] ->
                    case Map.lookup fileName directory of
                        Just timeAndEntry ->
                            finishWalk visited <| Just ( directory, fileName, Just timeAndEntry )

                        _ ->
                            finishWalk visited <| Just ( directory, fileName, Nothing )

                fileName :: subFileNames ->
                    case Map.lookup fileName directory of
                        Just ( _, DirectoryEntry subDirectory ) ->
                            down (( directory, fileName ) :: visited) subFileNames subDirectory

                        _ ->
                            if createDirectories then
                                down (( directory, fileName ) :: visited) subFileNames Map.empty

                            else
                                finishWalk visited Nothing

        finishWalk :
            TList ( Directory c d e f g h, FileName )
            -> Maybe ( Directory c d e f g h, FileName, Maybe ( Time.Posix, Entry c d e f g h ) )
            -> ( v, Directory c d e f g h )
        finishWalk visited input =
            case callback input now of
                ( Just changedDirectory, result ) ->
                    ( result, up changedDirectory visited )

                ( Nothing, result ) ->
                    ( result, root )

        up : Directory c d e f g h -> TList ( Directory c d e f g h, FileName ) -> Directory c d e f g h
        up directory visited =
            MList.foldl
                (\changedDirectory ( parentDirectory, fileName ) ->
                    Map.insert fileName ( now, DirectoryEntry changedDirectory ) parentDirectory
                )
                directory
                visited
    in
    down [] (MList.reverse (getNames filePath)) root


getTime : IO c d e f g h Time.Posix
getTime =
    IO.liftCmd <| Task.perform identity Time.now



-- MOUNTED TREES


mapMountedTree : Util.Tree (LocalState c d e f g h) c d e f g h -> ( Time.Posix, Directory c d e f g h )
mapMountedTree mountedTree =
    Tuple.mapSecond mapMountedDirectory mountedTree


mapMountedDirectory : Util.Directory (LocalState c d e f g h) c d e f g h -> Directory c d e f g h
mapMountedDirectory mountedDirectory =
    Map.map (Tuple.mapSecond mapMountedEntry) mountedDirectory


mapMountedEntry : Util.Entry (LocalState c d e f g h) c d e f g h -> Entry c d e f g h
mapMountedEntry mountedEntry =
    case mountedEntry of
        Util.UnreadFileEntry size io ->
            MountedFileEntry size io

        Util.DirectoryEntry directory ->
            DirectoryEntry (mapMountedDirectory directory)
