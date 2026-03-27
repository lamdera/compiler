module GenerateRoundtrip exposing (run)

{-| Entry point for the wire codec roundtrip test generator.

Run with:

    elm-pages bundle-script GenerateRoundtrip --output generate.mjs
    node generate.mjs --seed 42 --count 5

-}

import BackendTask
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Elm
import FatalError
import Generate.Fuzzers as Fuzzers
import Generate.Tests as Tests
import Generate.Types as Types
import Pages.Script as Script


type alias CliOptions =
    { seed : Int
    , count : Int
    }


run : Script.Script
run =
    Script.withCliOptions program
        (\{ seed, count } ->
            let
                modules =
                    Types.generateModules seed count
            in
            writeModules modules
                |> BackendTask.andThen (\_ -> writeTestModule modules)
        )


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.optionalKeywordArg "seed"
                        |> Option.withDefault "42"
                        |> Option.validateMap
                            (\s ->
                                case String.toInt s of
                                    Just i ->
                                        Ok i

                                    Nothing ->
                                        Err "seed must be an integer"
                            )
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "count"
                        |> Option.withDefault "5"
                        |> Option.validateMap
                            (\s ->
                                case String.toInt s of
                                    Just i ->
                                        Ok i

                                    Nothing ->
                                        Err "count must be an integer"
                            )
                    )
            )


{-| Write all generated type+fuzzer modules.
-}
writeModules : List Types.GeneratedModule -> BackendTask.BackendTask FatalError.FatalError (List ())
writeModules modules =
    modules
        |> List.map writeModule
        |> BackendTask.combine


writeModule : Types.GeneratedModule -> BackendTask.BackendTask FatalError.FatalError ()
writeModule mod =
    let
        -- Gather type declarations
        typeDecls =
            List.concatMap Types.renderDecl mod.decls

        -- Generate fuzzer declarations for each type
        fuzzerDecls =
            List.filterMap
                (\decl ->
                    Fuzzers.fuzzerForDecl mod.modulePath decl
                        |> Maybe.map Tuple.first
                )
                mod.decls

        -- Combine types and fuzzers into one file
        fullFile =
            Elm.file mod.modulePath
                (typeDecls ++ fuzzerDecls)

        path =
            "tests/" ++ String.join "/" mod.modulePath ++ ".elm"
    in
    Script.writeFile
        { path = path
        , body = fullFile.contents
        }
        |> BackendTask.allowFatal


{-| Write the main test module that imports all generated modules.
-}
writeTestModule : List Types.GeneratedModule -> BackendTask.BackendTask FatalError.FatalError ()
writeTestModule modules =
    let
        testFile =
            Tests.generateTestModule modules
    in
    Script.writeFile
        { path = "tests/WireRoundtripTests.elm"
        , body = testFile.contents
        }
        |> BackendTask.allowFatal


