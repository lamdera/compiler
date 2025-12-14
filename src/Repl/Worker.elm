port module Repl.Worker exposing (main)

import Builder.Build as Build
import Builder.Elm.Details as Details
import Builder.Generate as Generate
import Builder.Reporting.Exit as Exit
import Builder.Reporting.Exit.Help as ExitHelp
import Compiler.Reporting.Doc as Doc
import Extra.System.Config as Config
import Extra.System.Dir as Dir
import Extra.System.IO as IO
import Extra.System.IO.Port as Port
import Extra.Type.Either exposing (Either(..))
import Extra.Type.Lens exposing (Lens)
import Extra.Type.List as MList exposing (TList)
import Global
import Repl.Api as Api
import Terminal.Command as Terminal
import Terminal.Repl as Repl



-- APP


main : Program Flags Model Msg
main =
    Platform.worker
        { init = IO.init initialModel initialMsg
        , subscriptions = subscriptions
        , update = IO.update
        }


type alias Flags =
    TList ( String, String, String )



-- MODEL


type alias Model =
    Repl.GlobalState LocalState


initialModel : Flags -> Model
initialModel _ =
    Global.State
        Config.initialState
        Dir.initialState
        Details.initialState
        Build.initialState
        Generate.initialState
        Terminal.initialState
        Repl.initialLocalState
        initialLocalState



-- LOCAL STATE


type LocalState
    = LocalState
        -- javaScriptCont
        (Maybe (Port.SyncCont Model Api.JavaScriptResponse))
        -- replState
        ReplState


type ReplState
    = ReplRunning (Repl.Env LocalState) Repl.State (Maybe Repl.Lines)
    | ReplStopped


initialLocalState : LocalState
initialLocalState =
    LocalState
        -- javaScriptCont
        Nothing
        -- replState
        ReplStopped


lensJavaScriptCont : Port.SyncLens Model Api.JavaScriptResponse
lensJavaScriptCont =
    { getter = \(Global.State _ _ _ _ _ _ _ (LocalState x _)) -> x
    , setter = \x (Global.State a b c d e f g (LocalState _ bi)) -> Global.State a b c d e f g (LocalState x bi)
    }


lensReplState : Lens Model ReplState
lensReplState =
    { getter = \(Global.State _ _ _ _ _ _ _ (LocalState _ x)) -> x
    , setter = \x (Global.State a b c d e f g (LocalState ai _)) -> Global.State a b c d e f g (LocalState ai x)
    }



-- MSG


type alias IO a =
    IO.IO Model a


type alias Msg =
    IO ()


initialMsg : Flags -> Msg
initialMsg flags =
    IO.sequence (MList.map flagToMsg flags)


flagToMsg : ( String, String, String ) -> Msg
flagToMsg ( config, val1, val2 ) =
    case config of
        "httpPrefix" ->
            Config.setHttpPrefix (Just val1)

        "mountPrefix" ->
            Config.setMountPrefix (Just val1)

        "srcDir" ->
            Config.addAdditionalSrcDir val1

        "mountLocal" ->
            Dir.mountLocal val1 (Dir.fromString val2)

        "mountStatic" ->
            Dir.mountStatic val1 (Dir.fromString val2)

        "currentDir" ->
            Dir.setCurrentDirectory (Dir.fromString val1)

        "start" ->
            IO.bind (handleClientCall { userInput = val1 }) clientToWorkerLowLevelSend

        _ ->
            IO.noOp



-- CLIENT API


port receiveFromClientPort : Port.ReceivePort msg Api.ClientRequestWire


port sendToClientPort : Port.SendPort msg Api.WorkerResponseWire


clientToWorkerResponder : Port.SyncResponder Model Api.ClientRequest Api.WorkerResponse
clientToWorkerResponder =
    Api.clientToWorkerApi.responderFun receiveFromClientPort sendToClientPort


clientToWorkerLowLevelSend : Port.LowLevelSend Model Api.WorkerResponse
clientToWorkerLowLevelSend =
    Api.clientToWorkerLowLevelSendFun sendToClientPort



-- JAVASCRIPT API


port sendToJavaScriptPort : Port.SendPort msg Api.JavaScriptRequestWire


port receiveFromJavaScriptPort : Port.ReceivePort msg Api.JavaScriptResponseWire


workerToJavaScriptRequester : Port.SyncRequester Model Api.JavaScriptRequest Api.JavaScriptResponse
workerToJavaScriptRequester =
    Api.workerToJavaScriptRequester lensJavaScriptCont sendToJavaScriptPort receiveFromJavaScriptPort



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ clientToWorkerResponder handleClientCall
        , workerToJavaScriptRequester.respond model
        ]



-- CLIENT API


handleClientCall : Api.ClientRequest -> IO Api.WorkerResponse
handleClientCall { userInput } =
    IO.bind
        (withRunningRepl <|
            \env state maybeLines ->
                handleClientRequestHelp env state (addLine userInput maybeLines)
        )
    <|
        \workerState ->
            IO.bind collectMessages <|
                \messages ->
                    IO.return
                        { workerState = workerState
                        , messages = messages
                        }


handleClientRequestHelp : Repl.Env LocalState -> Repl.State -> Repl.Lines -> IO Api.WorkerState
handleClientRequestHelp env state lines =
    case Repl.categorize lines of
        Repl.Done input ->
            IO.bind (Repl.eval env state input) <|
                \outcome ->
                    case outcome of
                        Repl.Loop newState ->
                            IO.bindSequence
                                [ IO.putLens lensReplState (ReplRunning env newState Nothing) ]
                                (IO.return (Api.WorkerStateRunning Nothing))

                        Repl.End ->
                            IO.bindSequence
                                [ IO.putLens lensReplState ReplStopped ]
                                (IO.return (Api.WorkerStateStopped Nothing))

        Repl.Continue prefill ->
            IO.bindSequence
                [ IO.putLens lensReplState (ReplRunning env state (Just lines)) ]
                (IO.return (Api.WorkerStateRunning (Just (Repl.renderPrefill prefill))))



-- START REPL


withRunningRepl :
    (Repl.Env LocalState -> Repl.State -> Maybe Repl.Lines -> IO Api.WorkerState)
    -> IO Api.WorkerState
withRunningRepl callback =
    IO.bind (IO.getLens lensReplState) <|
        \replState ->
            case replState of
                ReplRunning env state maybeLines ->
                    callback env state maybeLines

                ReplStopped ->
                    startRepl <|
                        \env state ->
                            --IO.bindSequence
                            --    [ IO.putLens lensReplState (ReplRunning env state Nothing) ]
                            callback env state Nothing


startRepl : (Repl.Env LocalState -> Repl.State -> IO Api.WorkerState) -> IO Api.WorkerState
startRepl replCallback =
    IO.bind getEnv <|
        \env ->
            IO.bindSequence
                [ Repl.printWelcomeMessage ]
                (replCallback env Repl.initialState)


getEnv : IO (Repl.Env LocalState)
getEnv =
    Repl.initEnv
        (Repl.Flags
            -- interpreter
            workerInterpreter
        )



-- INTERPRETER


workerInterpreter : Repl.Interpreter LocalState
workerInterpreter input =
    case input of
        Repl.InterpretValue javaScript ->
            IO.bind
                (callJavaScript javaScript)
                (Repl.continueInterpreter IO.noOp)

        Repl.ShowError error ->
            IO.bindSequence
                [ Terminal.putLine (errorToString error) ]
                (Repl.continueInterpreter IO.noOp Repl.InterpreterFailure)


callJavaScript : String -> IO Repl.InterpreterResult
callJavaScript javaScript =
    IO.bind (workerToJavaScriptRequester.request javaScript) <|
        \javaScriptResponse ->
            case javaScriptResponse of
                Api.JavaScriptOutput output ->
                    IO.bindSequence
                        [ Terminal.putLine output ]
                        (IO.return Repl.InterpreterSuccess)

                Api.JavaScriptError error ->
                    IO.bindSequence
                        [ Terminal.putLine error ]
                        (IO.return Repl.InterpreterFailure)



-- HELPER


collectMessages : IO (TList String)
collectMessages =
    IO.bind (IO.getLens Terminal.lensStdOut) <|
        \messages ->
            IO.bindSequence
                [ Terminal.clearStdOut ]
                (IO.return (MList.map Terminal.getText messages))


addLine : String -> Maybe Repl.Lines -> Repl.Lines
addLine input maybeLines =
    case maybeLines of
        Nothing ->
            Repl.Lines (Repl.stripLegacyBackslash input) []

        Just lines ->
            Repl.addLine (Repl.stripLegacyBackslash input) lines


errorToString : Exit.Repl -> String
errorToString error =
    Doc.toString (ExitHelp.reportToDoc (Exit.replToReport error))
