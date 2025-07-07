module Main exposing (run)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import Json.Encode
import Pages.Script as Script exposing (Script)
import Versions


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    logExec "👷 Creating temporary work directory" "mkdir" [ "-p", "work" ] <| \_ ->
    logExec "👷 Creating output directory" "mkdir" [ "-p", "out" ] <| \_ ->
    Do.each Versions.versions
        (\{ lamderaVersion, elmVersion } ->
            Do.each Versions.architectures
                (\arch ->
                    let
                        fullName : String
                        fullName =
                            "lamdera-" ++ lamderaVersion ++ "-linux-" ++ arch.lamderaName

                        url : String
                        url =
                            "https://static.lamdera.com/bin/" ++ fullName

                        binaryPath : String
                        binaryPath =
                            "work/" ++ fullName
                    in
                    Do.log (Ansi.Color.fontColor Ansi.Color.brightYellow ("🏃 " ++ fullName)) <| \_ ->
                    logExecs "  🔽 Downloading"
                        [ ( "curl", [ url, "-sSL", "-o", binaryPath ] )
                        , ( "chmod", [ "+x", binaryPath ] )
                        ]
                    <| \_ ->
                    let
                        revision =
                            "1"

                        debName : String
                        debName =
                            "lamdera_" ++ lamderaVersion ++ "-" ++ elmVersion ++ "-" ++ revision

                        debPath : String
                        debPath =
                            "work/" ++ debName
                    in
                    logExecs "  🌀 📁 Preparing folders for .deb "
                        [ ( "mkdir", [ "-p", debPath ++ "/usr/local/bin" ] )
                        , ( "cp", [ binaryPath, debPath ++ "/usr/local/bin/lamdera" ] )
                        , ( "mkdir", [ debPath ++ "/DEBIAN" ] )
                        ]
                    <| \_ ->
                    Do.log (Ansi.Color.fontColor Ansi.Color.cyan "  🌀 📰 Writing DEBIAN/control file") <| \_ ->
                    Do.allowFatal
                        (Script.writeFile
                            { path = debPath ++ "/DEBIAN/control"
                            , body =
                                controlFile
                                    { arch = arch
                                    , lamderaVersion = lamderaVersion
                                    , elmVersion = elmVersion
                                    , revision = revision
                                    }
                            }
                        )
                    <| \_ ->
                    logExec "  🌀 📦 Creating the package" "dpkg-deb" [ "--root-owner-group", "--build", debPath, "out/" ++ debName ++ "_" ++ arch.debianName ++ ".deb" ] <| \_ ->
                    logExec "  🌀 🧹 Cleaning up .deb packaging folder" "rm" [ "-r", debPath ] <| \_ ->
                    Do.noop
                )
            <| \_ ->
            Do.noop
        )
    <| \_ ->
    logExec "🧹 Cleaning up temporary work directory" "rm" [ "-r", "work" ] <| \_ ->
    Do.noop


controlFile : { arch : { debianName : String, lamderaName : String }, lamderaVersion : String, elmVersion : String, revision : String } -> String
controlFile { arch, lamderaVersion, elmVersion, revision } =
    [ "Package: lamdera"
    , "Version: " ++ lamderaVersion ++ "-" ++ elmVersion ++ "-" ++ revision
    , "Section: base"
    , "Priority: optional"
    , "Architecture: " ++ arch.debianName

    -- , "Depends:"
    , "Maintainer: Mario Rogic <hello@mario.net.au>"
    , "Description: A delightful platform for full-stack web apps"
    , ""
    ]
        |> String.join "\n"


logExec : String -> String -> List String -> (() -> BackendTask FatalError b) -> BackendTask FatalError b
logExec msg cmd args k =
    Do.log (Ansi.Color.fontColor Ansi.Color.cyan msg) <| \_ ->
    Do.log (formatCmd cmd args) <| \_ ->
    Do.exec cmd args k


logExecs : String -> List ( String, List String ) -> (() -> BackendTask FatalError b) -> BackendTask FatalError b
logExecs msg cmds k =
    Do.log (Ansi.Color.fontColor Ansi.Color.cyan msg) <| \_ ->
    Do.each cmds
        (\( cmd, args ) ->
            Do.log (formatCmd cmd args) <| \_ ->
            Do.exec cmd args <| \_ ->
            Do.noop
        )
    <| \_ ->
    k ()


formatCmd : String -> List String -> String
formatCmd cmd args =
    String.join " " <|
        "   "
            :: Ansi.Color.fontColor Ansi.Color.green (escapeMaybe cmd)
            :: List.map
                (\arg ->
                    let
                        escaped : String
                        escaped =
                            escapeMaybe arg
                    in
                    if String.startsWith "\"" escaped then
                        Ansi.Color.fontColor Ansi.Color.yellow escaped

                    else if String.startsWith "-" escaped then
                        Ansi.Color.fontColor
                            (Ansi.Color.rgb
                                { blue = 0xB0
                                , green = 0xB0
                                , red = 0xB0
                                }
                            )
                            escaped

                    else
                        Ansi.Color.fontColor Ansi.Color.brightWhite escaped
                )
                args


escapeMaybe : String -> String
escapeMaybe input =
    let
        escaped =
            input |> Json.Encode.string |> Json.Encode.encode 0
    in
    if not (String.contains " " input) && escaped == "\"" ++ input ++ "\"" then
        input

    else
        escaped
