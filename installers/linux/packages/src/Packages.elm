module Packages exposing (Arch(..), Package, packages)


type alias Package =
    { name : String
    , version : String
    , url : String
    , arch : Arch
    , maintainer : String
    , description : String
    , hash : String
    }


type Arch
    = X86_64
    | Arm64


arches : List Arch
arches =
    [ X86_64
    , Arm64
    ]


type alias Hashes =
    { x86_64 : String
    , arm64 : String
    }


packages : List Package
packages =
    [ lamdera
        { lamderaVersion = "1.3.2"
        , elmVersion = "0.19.1"
        , hashes =
            { x86_64 = "15a69bfa98155651749e31c68d05a04fcf48bdccb86bce77b7c8872f545cecfa"
            , arm64 = "68a16bbbd2ed0ee19c36112a4c2d0abca66cf17465747e55adf2596b0921f8d7"
            }
        }
    , elmFormat
        { version = "0.8.7"
        , hashes =
            { x86_64 = "44344c7b6f838dc5d9495dfe4253280a698c2251ee8cfa29b6d1a032b6efb13b"
            , arm64 = "fe99b3925201598121aeea6b31b55bd3ab6dad743bce27082d8e01e723bd160e"
            }
        }
    , elmFormat
        { version = "0.8.8"
        , hashes =
            { x86_64 = "ee749898a07871e5dcbe7adf77a6c3d95de2fcde2e15de30e4fa7457faf05a71"
            , arm64 = "0be0046a81432e6e16340b8093cafa35a454e84956522d53f6a28f815dceac23"
            }
        }
    ]
        |> List.concatMap
            (\f ->
                List.map
                    (\arch ->
                        let
                            data : PackageData
                            data =
                                f arch
                        in
                        { name = data.name
                        , version = data.version
                        , url = data.url
                        , description = data.description
                        , maintainer = data.maintainer
                        , hash = getHash arch data.hashes
                        , arch = arch
                        }
                    )
                    arches
            )


type alias PackageData =
    { name : String
    , version : String
    , url : String
    , description : String
    , maintainer : String
    , hashes : Hashes
    }


lamdera :
    { lamderaVersion : String
    , elmVersion : String
    , hashes : Hashes
    }
    -> Arch
    -> PackageData
lamdera { lamderaVersion, elmVersion, hashes } arch =
    let
        lamderaArch : String
        lamderaArch =
            case arch of
                X86_64 ->
                    "x86_64"

                Arm64 ->
                    "arm64"

        fullName : String
        fullName =
            "lamdera-" ++ lamderaVersion ++ "-linux-" ++ lamderaArch

        url : String
        url =
            "https://static.lamdera.com/bin/" ++ fullName
    in
    { name = "lamdera"
    , version = lamderaVersion ++ "-" ++ elmVersion
    , url = url
    , description = "A delightful platform for full-stack web apps"
    , maintainer = "Mario Rogic <hello@mario.net.au>"
    , hashes = hashes
    }


elmFormat : { version : String, hashes : Hashes } -> Arch -> PackageData
elmFormat { version, hashes } arch =
    let
        elmFormatArch : String
        elmFormatArch =
            case arch of
                X86_64 ->
                    "x64"

                Arm64 ->
                    "aarch64"

        url : String
        url =
            "https://github.com/avh4/elm-format/releases/download/" ++ version ++ "/elm-format-" ++ version ++ "-linux-" ++ elmFormatArch ++ ".tgz"
    in
    { name = "elm-format"
    , version = version
    , url = url
    , description = "Formats Elm source code according to a standard set of rules based on the official Elm Style Guide"
    , maintainer = "Aaron VonderHaar <gruen0aermel@gmail.com>"
    , hashes = hashes
    }


getHash : Arch -> Hashes -> String
getHash arch hashes =
    case arch of
        X86_64 ->
            hashes.x86_64

        Arm64 ->
            hashes.arm64
