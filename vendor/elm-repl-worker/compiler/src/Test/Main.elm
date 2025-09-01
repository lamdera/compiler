module Test.Main exposing (main)

{-
   This file is used to
   - Send every module through the compiler in `make test`
   - Make `elm-review --template jfmengels/elm-review-unused/example` happy
-}

import Builder.Build
import Builder.Elm.Details
import Builder.Generate
import Builder.Reporting.Exit
import Builder.Reporting.Exit.Help
import Extra.System.Config
import Extra.System.Dir
import Extra.System.IO
import Global
import Terminal.Command
import Terminal.Helpers
import Terminal.Init
import Terminal.Install
import Terminal.Main
import Terminal.Make
import Terminal.Reactor
import Terminal.Repl


main : Program () (Terminal.Repl.GlobalState ()) (Extra.System.IO.IO (Terminal.Repl.GlobalState ()) ())
main =
    let
        initialModel : () -> Terminal.Repl.GlobalState ()
        initialModel _ =
            Global.State
                Extra.System.Config.initialState
                Extra.System.Dir.initialState
                Builder.Elm.Details.initialState
                Builder.Build.initialState
                Builder.Generate.initialState
                Terminal.Command.initialState
                Terminal.Repl.initialLocalState
                ()

        initialIO : () -> Extra.System.IO.IO (Terminal.Repl.GlobalState ()) ()
        initialIO _ =
            Extra.System.IO.sequence
                [ compilerUiInterface
                , replWorkerInterface
                ]

        toUnit : a -> ()
        toUnit _ =
            ()

        toIO : a -> Extra.System.IO.IO (Terminal.Repl.GlobalState ()) ()
        toIO _ =
            Extra.System.IO.noOp

        compilerUiInterface : Extra.System.IO.IO (Terminal.Repl.GlobalState ()) ()
        compilerUiInterface =
            Extra.System.IO.sequence
                [ Builder.Reporting.Exit.MakeNoOutline |> toIO
                , Builder.Reporting.Exit.initToReport |> toIO
                , Builder.Reporting.Exit.installToReport |> toIO
                , Builder.Reporting.Exit.makeToReport |> toIO
                , Builder.Reporting.Exit.reactorToReport |> toIO
                , Builder.Reporting.Exit.replToReport |> toIO
                , Builder.Reporting.Exit.toBuildProblemReport |> toIO
                , Builder.Reporting.Exit.toClient |> toIO
                , Builder.Reporting.Exit.toDetailsReport |> toIO
                , Builder.Reporting.Exit.toRegistryProblemReport |> toIO
                , Extra.System.Config.setHttpPrefix |> toIO
                , Extra.System.Config.setMountPrefix |> toIO
                , Extra.System.Dir.getCurrentDirectoryEntriesPure |> toIO
                , Extra.System.Dir.getCurrentDirectoryNamesPure |> toIO
                , Extra.System.Dir.mountLocal |> toIO
                , Extra.System.Dir.mountStatic |> toIO
                , Extra.System.Dir.mountZip |> toIO
                , Extra.System.Dir.removeDirectory |> toIO
                , Extra.System.Dir.resetFileSystem |> toIO
                , Extra.System.Dir.setCurrentDirectory |> toIO
                , Extra.System.IO.join |> toIO
                , Extra.System.IO.sleep |> toIO
                , Extra.System.IO.when |> toIO
                , Terminal.Command.clearPutLine |> toIO
                , Terminal.Command.clearStdOut |> toIO
                , Terminal.Command.getDurationSinceLastInput |> toIO
                , Terminal.Command.getLine |> toIO
                , Terminal.Command.getText |> toIO
                , Terminal.Command.gotInput |> toIO
                , Terminal.Command.lensInput |> toIO
                , Terminal.Command.lensPrompt |> toIO
                , Terminal.Command.lensStdOut |> toIO
                , Terminal.Command.setCurrentInput |> toIO
                , Terminal.Command.setInput |> toIO
                , Terminal.Command.setNextInput |> toIO
                , Terminal.Helpers.parsePackage |> toIO
                , Terminal.Init.run |> toIO
                , Terminal.Install.install |> toIO
                , Terminal.Make.Flags |> toIO
                , Terminal.Make.JS |> toIO
                , Terminal.Make.Html |> toIO
                , Terminal.Main.runMain |> toIO
                , Terminal.Make.run |> toIO
                , Terminal.Reactor.compile |> toIO
                , Terminal.Repl.Breakpoint |> toIO
                , Terminal.Repl.InterpreterFailure |> toIO
                , Terminal.Repl.InterpreterSuccess |> toIO
                , Terminal.Repl.Module |> toIO
                , Terminal.Repl.Normal |> toIO
                , (\interpreterInput ->
                    case interpreterInput of
                        Terminal.Repl.InterpretHtml a b ->
                            ( a, b ) |> toUnit

                        Terminal.Repl.InterpretValue a ->
                            a |> toUnit

                        Terminal.Repl.ShowError a ->
                            a |> toUnit
                  )
                    |> toIO
                , Terminal.Repl.continueInterpreter |> toIO
                , Terminal.Repl.run |> toIO
                ]

        replWorkerInterface : Extra.System.IO.IO (Terminal.Repl.GlobalState ()) ()
        replWorkerInterface =
            Extra.System.IO.sequence
                [ Builder.Reporting.Exit.replToReport |> toIO
                , Builder.Reporting.Exit.Help.reportToDoc |> toIO
                , Extra.System.Config.addAdditionalSrcDir |> toIO
                , Extra.System.Config.setHttpPrefix |> toIO
                , Extra.System.Config.setMountPrefix |> toIO
                , Extra.System.Dir.mountLocal |> toIO
                , Extra.System.Dir.mountStatic |> toIO
                , Extra.System.Dir.setCurrentDirectory |> toIO
                , Terminal.Command.clearStdOut |> toIO
                , Terminal.Command.getText |> toIO
                , Terminal.Command.lensStdOut |> toIO
                , Terminal.Repl.Configured |> toIO
                , Terminal.Repl.Flags |> toIO
                , Terminal.Repl.InterpreterFailure |> toIO
                , Terminal.Repl.InterpreterSuccess |> toIO
                , Terminal.Repl.continueInterpreter |> toIO
                , Terminal.Repl.addLine |> toIO
                , Terminal.Repl.categorize |> toIO
                , Terminal.Repl.eval |> toIO
                , Terminal.Repl.initialState |> toIO
                , Terminal.Repl.initEnv |> toIO
                , Terminal.Repl.printWelcomeMessage |> toIO
                , Terminal.Repl.renderPrefill |> toIO
                , Terminal.Repl.stripLegacyBackslash |> toIO
                , (\interpreterInput ->
                    case interpreterInput of
                        Terminal.Repl.InterpretValue a ->
                            a |> toUnit

                        Terminal.Repl.ShowError a ->
                            a |> toUnit

                        x ->
                            x |> toUnit
                  )
                    |> toIO
                ]
    in
    Platform.worker
        { init = Extra.System.IO.init initialModel initialIO
        , subscriptions = \_ -> Sub.none
        , update = Extra.System.IO.update
        }
