module Versions exposing (architectures, versions)


architectures : List { lamderaName : String, debianName : String }
architectures =
    [ { lamderaName = "x86_64", debianName = "amd64" }
    , { lamderaName = "arm64", debianName = "aarch64" }
    ]


versions : List { lamderaVersion : String, elmVersion : String }
versions =
    [ { lamderaVersion = "1.3.2", elmVersion = "0.19.1" }
    ]
