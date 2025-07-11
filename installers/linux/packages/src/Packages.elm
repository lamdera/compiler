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
            { x86_64 = "f270decbbd305905a4ba31cfaa77c9708f976941816d4834df6b995f8a7531187da3414d7695351f72e09c1c0557726deed7a38ee107139ef6fd7f08ac4fc647"
            , arm64 = "ff388e08436ad4ab69a8baf5189b293971bafc7bef46ef342131d1013687c7b026705e7021c9543257ab999a2ff51db8655283a44955faa6e7c832c26916df0f"
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


getHash : Arch -> Hashes -> String
getHash arch hashes =
    case arch of
        X86_64 ->
            hashes.x86_64

        Arm64 ->
            hashes.arm64
