module Main exposing (run)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import Json.Encode
import Packages exposing (Package)
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    logExec "👷 Creating temporary work directory" "mkdir" [ "-p", "work" ] <| \_ ->
    logExec "👷 Creating output directory" "mkdir" [ "-p", "out" ] <| \_ ->
    Do.each Packages.packages
        (\({ name, version, url, debianArch } as package) ->
            let
                fullName : String
                fullName =
                    name ++ "-" ++ version ++ "-" ++ debianArch

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
            Do.do (prepareDeb { package = package, binaryPath = binaryPath }) <| \_ ->
            Do.noop
        )
    <| \_ ->
    logExec "🧹 Cleaning up temporary work directory" "rm" [ "-r", "work" ] <| \_ ->
    Do.noop


prepareDeb : { package : Package, binaryPath : String } -> BackendTask FatalError ()
prepareDeb { package, binaryPath } =
    let
        revision : String
        revision =
            "1"

        debName : String
        debName =
            package.name ++ "_" ++ package.version ++ "-" ++ revision

        debPath : String
        debPath =
            "work/" ++ debName
    in
    logExecs "  🌀 📁 Preparing folders for .deb "
        [ ( "rm", [ "-rf", debPath ] )
        , ( "mkdir", [ "-p", debPath ++ "/usr/local/bin" ] )
        , ( "cp", [ binaryPath, debPath ++ "/usr/local/bin/" ++ package.name ] )
        , ( "mkdir", [ debPath ++ "/DEBIAN" ] )
        ]
    <| \_ ->
    logCyan "  🌀 📰 Writing DEBIAN/control file" <| \_ ->
    Do.allowFatal
        (Script.writeFile
            { path = debPath ++ "/DEBIAN/control"
            , body = controlFile { package = package, revision = revision }
            }
        )
    <| \_ ->
    logExecs "  🌀 📦 Creating the package"
        [ ( "dpkg-deb", [ "--root-owner-group", "--build", debPath, "out/" ++ debName ++ "_" ++ package.debianArch ++ ".deb" ] ) ]
    <| \_ ->
    Do.noop


controlFile : { package : Package, revision : String } -> String
controlFile { package, revision } =
    [ "Package: " ++ package.name
    , "Version: " ++ package.version ++ "-" ++ revision
    , "Section: base"
    , "Priority: optional"
    , "Architecture: " ++ package.debianArch

    -- , "Depends:"
    , "Maintainer: " ++ package.maintainer
    , "Description: " ++ package.description
    , ""
    ]
        |> String.join "\n"


logExec : String -> String -> List String -> (() -> BackendTask FatalError b) -> BackendTask FatalError b
logExec msg cmd args k =
    logCyan msg <| \_ ->
    Do.log (formatCmd cmd args) <| \_ ->
    Do.exec cmd args k


logExecs : String -> List ( String, List String ) -> (() -> BackendTask FatalError b) -> BackendTask FatalError b
logExecs msg cmds k =
    logCyan msg <| \_ ->
    Do.each cmds
        (\( cmd, args ) ->
            Do.log (formatCmd cmd args) <| \_ ->
            Do.exec cmd args <| \_ ->
            Do.noop
        )
    <| \_ ->
    k ()


logCyan : String -> ((() -> BackendTask FatalError b) -> BackendTask FatalError b)
logCyan msg =
    Do.log (Ansi.Color.fontColor Ansi.Color.cyan msg)


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
