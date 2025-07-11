module Packages exposing (Package, packages)


type alias Package =
    { name : String
    , version : String
    , url : String
    , debianArch : String
    , maintainer : String
    , description : String
    , hash : String
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
        |> List.concat


lamdera :
    { lamderaVersion : String
    , elmVersion : String
    , hashes : { x86_64 : String, arm64 : String }
    }
    -> List Package
lamdera { lamderaVersion, elmVersion, hashes } =
    let
        go :
            { lamderaArch : String
            , debianArch : String
            , hash : String
            }
            -> Package
        go data =
            let
                fullName : String
                fullName =
                    "lamdera-" ++ lamderaVersion ++ "-linux-" ++ data.lamderaArch

                url : String
                url =
                    "https://static.lamdera.com/bin/" ++ fullName
            in
            { name = "lamdera"
            , version = lamderaVersion ++ "-" ++ elmVersion
            , url = url
            , debianArch = data.debianArch
            , description = "A delightful platform for full-stack web apps"
            , maintainer = "Mario Rogic <hello@mario.net.au>"
            , hash = data.hash
            }
    in
    [ { lamderaArch = "x86_64", debianArch = "amd64", hash = hashes.x86_64 }
    , { lamderaArch = "arm64", debianArch = "aarch64", hash = hashes.arm64 }
    ]
        |> List.map go
