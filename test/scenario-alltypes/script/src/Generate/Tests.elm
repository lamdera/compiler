module Generate.Tests exposing (generateTestDecl, generateTestModule)

{-| Generates roundtrip test expressions and assembles them into a test module.
-}

import Elm
import Elm.Annotation as Type
import Gen.Expect
import Gen.Fuzz
import Gen.Test
import Generate.Fuzzers as Fuzzers
import Generate.Types exposing (GeneratedModule, TypeDecl(..))
import Generate.Wire3


{-| Generate a single roundtrip test declaration for a type.
-}
generateTestDecl : List String -> String -> Elm.Declaration
generateTestDecl modulePath typeName =
    let
        fuzzerExpr =
            Elm.value
                { importFrom = modulePath
                , name = "fuzzer_" ++ typeName
                , annotation = Nothing
                }

        encodeRef =
            Generate.Wire3.w3Encode modulePath typeName

        decodeRef =
            Generate.Wire3.w3Decode modulePath typeName
    in
    Elm.declaration ("test_" ++ typeName)
        (Gen.Test.fuzz fuzzerExpr
            (typeName ++ " roundtrip")
            (\value ->
                let
                    encoded =
                        Generate.Wire3.bytesEncodeCall
                            (Elm.apply encodeRef [ value ])

                    decoded =
                        Generate.Wire3.bytesDecodeCall decodeRef encoded
                in
                Gen.Expect.equal
                    (Elm.just value)
                    decoded
            )
        )
        |> Elm.expose


{-| Generate the full test module that imports all generated type modules
and runs roundtrip tests for each concrete type.
-}
generateTestModule : List GeneratedModule -> Elm.File
generateTestModule modules =
    let
        testDecls =
            List.concatMap testDeclsForModule modules

        suiteDecl =
            Elm.declaration "suite"
                (Gen.Test.describe "Wire3 Codec Roundtrip"
                    (List.map Tuple.second testDecls)
                )
                |> Elm.expose
    in
    Elm.file [ "WireRoundtripTests" ]
        (List.map Tuple.first testDecls ++ [ suiteDecl ])


testDeclsForModule : GeneratedModule -> List ( Elm.Declaration, Elm.Expression )
testDeclsForModule mod =
    List.filterMap
        (\decl ->
            case Fuzzers.fuzzerForDecl mod.modulePath decl of
                Just ( _, typeName ) ->
                    let
                        testDecl =
                            generateTestDecl mod.modulePath typeName

                        testExpr =
                            Elm.value
                                { importFrom = []
                                , name = "test_" ++ typeName
                                , annotation = Nothing
                                }
                    in
                    Just ( testDecl, testExpr )

                Nothing ->
                    Nothing
        )
        mod.decls
