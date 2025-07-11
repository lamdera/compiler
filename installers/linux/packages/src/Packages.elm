module Packages exposing (Package, packages)


type alias Package =
    { name : String
    , version : String
    , url : String
    , debianArch : String
    , maintainer : String
    , description : String
    }


packages : List Package
packages =
    [ lamdera "1.3.2" "0.19.1"
    ]
        |> List.concat


lamdera : String -> String -> List Package
lamdera lamderaVersion elmVersion =
    [ { lamderaName = "x86_64", debianName = "amd64" }
    , { lamderaName = "arm64", debianName = "aarch64" }
    ]
        |> List.map
            (\arch ->
                let
                    fullName : String
                    fullName =
                        "lamdera-" ++ lamderaVersion ++ "-linux-" ++ arch.lamderaName

                    url : String
                    url =
                        "https://static.lamdera.com/bin/" ++ fullName
                in
                { name = "lamdera"
                , version = lamderaVersion ++ "-" ++ elmVersion
                , url = url
                , debianArch = arch.debianName
                , description = "A delightful platform for full-stack web apps"
                , maintainer = "Mario Rogic <hello@mario.net.au>"
                }
            )
