port module Lamdera.Repl exposing
    ( Model
    , Msg(..)
    , initialModel
    , isShown
    , subscriptions
    , update
    , view
    )

import Browser.Dom
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events
import Process
import Task



-- MODEL


type alias Model =
    { version : String
    , state : State
    }


type State
    = Stopped
    | Loading
    | LoadError String
    | Running RunningModel
    | Crashed String



-- states: stopped, loading, load error, running, crashed
-- events: toggled, reply running, reply stopped, reply crashed, restart
-- actions: call
--
-- initial state: stopped
--
-- transitions:
--
--  stopped
--      toggled       -> loading, call ""
--      reply running
--      reply stopped
--      reply crashed
--      restart
--
--  loading
--      toggled       -> stopped, call ":quit"
--      reply running -> running newRunningState
--      reply stopped
--      reply crashed -> load error
--      restart
--
--  load error
--      toggled       -> stopped
--      reply running
--      reply stopped
--      reply crashed
--      restart       -> loading ok, call ""
--
--  running
--      toggled       -> running toggledRunningState
--      reply running -> running newRunningState
--      reply stopped -> stopped
--      reply crashed -> crashed, call ":quit"
--      restart
--
--  crashed
--      toggled       -> stopped
--      reply running
--      reply stopped -> crashed
--      reply crashed
--      restart       -> loading ok, call ""


type alias RunningModel =
    { shown : Bool
    , input : String
    , prefill : Maybe String
    , pendingCommands : List String
    , output : List String
    }


initialModel : String -> Model
initialModel version =
    { version = version
    , state = Stopped
    }


isShown : Model -> Bool
isShown model =
    case model.state of
        Stopped ->
            False

        Loading ->
            False

        LoadError _ ->
            True

        Running runningModel ->
            runningModel.shown

        Crashed _ ->
            True



-- VIEW


view : Bool -> Model -> Html Msg
view leftRight model =
    case model.state of
        Stopped ->
            viewHidden

        Loading ->
            viewShown leftRight "center" <|
                viewLoading

        LoadError error ->
            viewShown leftRight "center" <|
                viewError "Error when loading the REPL:" "Retry" error

        Running runningModel ->
            if runningModel.shown then
                viewShown leftRight "flex-end" <|
                    viewRunning runningModel

            else
                viewHidden

        Crashed error ->
            viewShown leftRight "center" <|
                viewError "The REPL compiler crashed!" "Restart" error


viewHidden : Html Msg
viewHidden =
    Html.div [] []


viewShown : Bool -> String -> List (Html Msg) -> Html Msg
viewShown leftRight justify content =
    Html.div
        (styles.content
            (if leftRight then
                "left"

             else
                "right"
            )
            justify
        )
        content


viewLoading : List (Html Msg)
viewLoading =
    [ Html.div styles.loading [ Html.text "Loading REPL..." ] ]


viewRunning : RunningModel -> List (Html Msg)
viewRunning runningModel =
    []
        ++ viewOutput runningModel.output
        ++ viewInput runningModel.prefill runningModel.input


viewOutput : List String -> List (Html Msg)
viewOutput output =
    [ Html.div styles.output
        (List.map viewOutputEntry output)
    ]


viewOutputEntry : String -> Html Msg
viewOutputEntry entry =
    Html.pre styles.entry
        [ Html.text (adjustTrailingNewLines entry) ]


viewInput : Maybe String -> String -> List (Html Msg)
viewInput maybePrefill input =
    [ Html.form
        (Html.Events.onSubmit FormSubmitted :: styles.controls)
        [ Html.span []
            [ Html.text (promptString maybePrefill) ]
        , Html.input
            (Html.Attributes.id ids.replInput
                :: Html.Attributes.value input
                :: Html.Events.onInput InputChanged
                :: styles.input
            )
            []
        ]
    ]


viewError : String -> String -> String -> List (Html Msg)
viewError title buttonLabel error =
    [ Html.div []
        [ Html.text title
        , Html.pre styles.error [ Html.text (String.trimRight error) ]
        , Html.div (Html.Events.onClick RetryClicked :: styles.button) [ Html.text buttonLabel ]
        ]
    ]


promptString : Maybe String -> String
promptString maybePrefill =
    case maybePrefill of
        Just _ ->
            "|\u{00A0}"

        Nothing ->
            ">\u{00A0}"


adjustTrailingNewLines : String -> String
adjustTrailingNewLines text =
    if String.endsWith "\n" text then
        String.trimRight text ++ "\n\n"

    else
        text



-- STYLES AND IDS


styles =
    { content =
        \borderPosition justify ->
            [ style "width" "580px"
            , style "contain" "size"
            , style ("border-" ++ borderPosition) "1px solid #393939"
            , style "padding" "5px 10px"
            , style "justify-content" justify
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "font-family" "monospace"
            , style "user-select" "text"
            ]
    , output =
        [ style "display" "flex"
        , style "flex-direction" "column-reverse"
        , style "overflow-y" "auto"
        ]
    , entry =
        [ style "margin" "0"
        , style "white-space" "pre-wrap"
        , style "font-family" "inherit"
        ]
    , controls =
        [ style "display" "flex"
        ]
    , input =
        [ style "flex-grow" "1"
        , style "border" "none"
        , style "border-bottom" "1px solid"
        , style "background" "#585858"
        , style "outline" "none"
        , style "padding" "0"
        , style "font-family" "inherit"
        , style "font-size" "inherit"
        , style "color" "inherit"
        ]
    , loading =
        [ style "margin" "0 auto"
        ]
    , error =
        [ -- todo: use "red" from LocalDev.elm
          style "color" "#E06C75"
        , style "font-size" "11px"
        , style "margin" "15px 0px 15px 9px"
        ]
    , button =
        [ style "cursor" "pointer"
        , style "text-decoration" "underline"
        ]
    }


ids =
    { replInput = "repl-input"
    }



-- MSG


type Msg
    = ToggleClicked
    | RetryClicked
    | InputChanged String
    | FormSubmitted
    | WorkerReplyReceived WorkerReplyWire
    | NoOp



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveFromWorkerPort WorkerReplyReceived



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, Bool )
update msg model =
    case msg of
        ToggleClicked ->
            updateState toggleClicked model

        RetryClicked ->
            updateState retryClicked model

        InputChanged newInput ->
            updateState (inputChanged newInput) model

        FormSubmitted ->
            updateState formSubmitted model

        WorkerReplyReceived workerReplyWire ->
            updateState (workerReplyReceived workerReplyWire) model

        NoOp ->
            ( model, Cmd.none, False )


updateState : (Model -> ( State, Cmd Msg, Bool )) -> Model -> ( Model, Cmd Msg, Bool )
updateState updateFn model =
    case updateFn model of
        ( state, cmd, replStopped ) ->
            ( { version = model.version
              , state = state
              }
            , cmd
            , replStopped
            )


toggleClicked : Model -> ( State, Cmd Msg, Bool )
toggleClicked model =
    case model.state of
        Stopped ->
            ( Loading
            , callWorker firstInitialCommand
            , False
            )

        Loading ->
            ( Stopped
            , callWorker ":quit"
            , False
            )

        LoadError _ ->
            ( Stopped
            , Cmd.none
            , False
            )

        Running runningModel ->
            ( Running
                { runningModel
                    | shown =
                        not runningModel.shown
                }
            , if runningModel.shown then
                Cmd.none

              else
                focusInput
            , False
            )

        Crashed _ ->
            ( Stopped
            , Cmd.none
            , False
            )


focusInput : Cmd Msg
focusInput =
    Task.attempt (\_ -> NoOp)
        (Task.andThen
            (\_ -> Browser.Dom.focus ids.replInput)
            (Process.sleep 0)
        )


retryClicked : Model -> ( State, Cmd Msg, Bool )
retryClicked model =
    case model.state of
        LoadError _ ->
            ( Loading
            , callWorker firstInitialCommand
            , False
            )

        Crashed _ ->
            ( Loading
            , callWorker firstInitialCommand
            , False
            )

        _ ->
            ( model.state
            , Cmd.none
            , False
            )


inputChanged : String -> Model -> ( State, Cmd Msg, Bool )
inputChanged newInput model =
    ( case model.state of
        Running runningModel ->
            Running
                { runningModel
                    | input =
                        newInput
                }

        _ ->
            model.state
    , Cmd.none
    , False
    )


formSubmitted : Model -> ( State, Cmd Msg, Bool )
formSubmitted model =
    case model.state of
        Running runningModel ->
            ( Running
                { runningModel
                    | output =
                        (promptString runningModel.prefill ++ runningModel.input)
                            :: runningModel.output
                }
            , callWorker runningModel.input
            , False
            )

        _ ->
            ( model.state
            , Cmd.none
            , False
            )


callWorker : String -> Cmd Msg
callWorker input =
    sendToWorkerPort (userInputCodec.encode (UserInput input))


workerReplyReceived : WorkerReplyWire -> Model -> ( State, Cmd Msg, Bool )
workerReplyReceived workerReplyWire model =
    handleWorkerReply (workerReplyCodec.decode workerReplyWire) model


handleWorkerReply : WorkerReply -> Model -> ( State, Cmd Msg, Bool )
handleWorkerReply workerReply model =
    case workerReply.workerState of
        WorkerStateRunning maybePrefill ->
            handleWorkerReplyRunning maybePrefill workerReply.messages model

        WorkerStateStopped Nothing ->
            handleWorkerReplyStopped model

        WorkerStateStopped (Just error) ->
            handleWorkerReplyCrashed error model


handleWorkerReplyRunning : Maybe String -> List String -> Model -> ( State, Cmd Msg, Bool )
handleWorkerReplyRunning maybePrefill messages model =
    case model.state of
        Loading ->
            handleWorkerReplyRunningWhileLoading maybePrefill messages model.version

        Running runningModel ->
            handleWorkerReplyRunningWhileRunning maybePrefill messages model.version runningModel

        _ ->
            ( model.state
            , Cmd.none
            , False
            )


handleWorkerReplyRunningWhileLoading : Maybe String -> List String -> String -> ( State, Cmd Msg, Bool )
handleWorkerReplyRunningWhileLoading maybePrefill messages version =
    ( Running
        { shown = True
        , input = Maybe.withDefault "" maybePrefill
        , prefill = maybePrefill
        , pendingCommands = remainingInitialCommands
        , output = handleMessages version "" messages
        }
    , callWorker secondInitialCommand
    , False
    )


handleWorkerReplyRunningWhileRunning : Maybe String -> List String -> String -> RunningModel -> ( State, Cmd Msg, Bool )
handleWorkerReplyRunningWhileRunning maybePrefill messages version runningModel =
    case runningModel.pendingCommands of
        nextCommand :: remainingCommands ->
            ( Running
                { shown = runningModel.shown
                , input = Maybe.withDefault "" maybePrefill
                , prefill = maybePrefill
                , pendingCommands = remainingCommands
                , output = handleMessages version runningModel.input messages ++ runningModel.output
                }
            , callWorker nextCommand
            , False
            )

        [] ->
            if runningModel.input == ":reset" then
                ( Running
                    { shown = runningModel.shown
                    , input = Maybe.withDefault "" maybePrefill
                    , prefill = maybePrefill
                    , pendingCommands = secondInitialCommand :: remainingInitialCommands
                    , output = handleMessages version runningModel.input messages ++ runningModel.output
                    }
                , callWorker firstInitialCommand
                , False
                )

            else
                ( Running
                    { shown = runningModel.shown
                    , input = Maybe.withDefault "" maybePrefill
                    , prefill = maybePrefill
                    , pendingCommands = []
                    , output = handleMessages version runningModel.input messages ++ runningModel.output
                    }
                , focusInput
                , False
                )


handleWorkerReplyStopped : Model -> ( State, Cmd Msg, Bool )
handleWorkerReplyStopped model =
    case model.state of
        Running _ ->
            ( Stopped
            , Cmd.none
            , True
            )

        _ ->
            ( model.state
            , Cmd.none
            , False
            )


handleWorkerReplyCrashed : String -> Model -> ( State, Cmd Msg, Bool )
handleWorkerReplyCrashed error model =
    case model.state of
        Loading ->
            ( LoadError error
            , Cmd.none
            , False
            )

        Running _ ->
            ( Crashed error
            , callWorker ":quit"
            , False
            )

        _ ->
            ( model.state
            , Cmd.none
            , False
            )



-- INITIAL COMMANDS


firstInitialCommand : String
firstInitialCommand =
    "import Lamdera"


secondInitialCommand : String
secondInitialCommand =
    "import Lamdera.Repl.Interface exposing (..)"


remainingInitialCommands : List String
remainingInitialCommands =
    [ "import Types"
    ]



-- WORKER API


port sendToWorkerPort : UserInputWire -> Cmd msg


port receiveFromWorkerPort : (WorkerReplyWire -> msg) -> Sub msg


type UserInput
    = UserInput String


type alias UserInputWire =
    String


userInputCodec : ReplCodec UserInput UserInputWire
userInputCodec =
    { encode = \(UserInput input) -> input
    , decode = \wire -> UserInput wire
    }


type alias WorkerReply =
    { workerState : WorkerState
    , messages : List String
    }


type WorkerState
    = WorkerStateRunning (Maybe String)
    | WorkerStateStopped (Maybe String)


type alias WorkerReplyWire =
    ( Bool
    , Maybe String
    , List String
    )


workerReplyCodec : ReplCodec WorkerReply WorkerReplyWire
workerReplyCodec =
    { encode =
        \{ workerState, messages } ->
            case workerState of
                WorkerStateRunning prefill ->
                    ( True, prefill, messages )

                WorkerStateStopped error ->
                    ( False, error, messages )
    , decode =
        \wire ->
            case wire of
                ( True, prefill, messages ) ->
                    { workerState = WorkerStateRunning prefill, messages = messages }

                ( False, error, messages ) ->
                    { workerState = WorkerStateStopped error, messages = messages }
    }


type alias ReplCodec value wire =
    { encode : value -> wire
    , decode : wire -> value
    }



-- SPECIAL OUTPUT


handleMessages : String -> String -> List String -> List String
handleMessages version input messages =
    List.concatMap (handleMessage version input) messages


handleMessage : String -> String -> String -> List String
handleMessage version input message =
    applyFirstMatchingHandler version input message <|
        [ initialCommandOutputHandler
        , colonLamderaHandler
        , otherColonHandler
        , interfaceTodoHandler
        ]


type MessageHandlerResult
    = NotMatched
    | MatchedNoMessage
    | MatchedMessage String


applyFirstMatchingHandler : String -> String -> String -> List (String -> String -> String -> MessageHandlerResult) -> List String
applyFirstMatchingHandler version input message handlerList =
    case handlerList of
        nextHandler :: remainingHandlers ->
            case nextHandler version input message of
                MatchedMessage newMessage ->
                    [ newMessage ]

                MatchedNoMessage ->
                    []

                NotMatched ->
                    applyFirstMatchingHandler version input message remainingHandlers

        [] ->
            [ message ]


initialCommandOutputHandler : String -> String -> String -> MessageHandlerResult
initialCommandOutputHandler lamderaVersion input message =
    case ( input, String.lines message ) of
        ( "", [ startLine, text, lastLine ] ) ->
            case String.words startLine of
                [ leadingDashes, elmName, elmVersion, _ ] ->
                    MatchedMessage
                        (String.join
                            "\n"
                            [ [ leadingDashes, elmName, elmVersion, "/", "Lamdera", lamderaVersion, "-" ]
                                |> String.join " "
                                |> String.padRight 80 '-'
                            , text
                            , "Say :lamdera for Lamdera features! See " ++ replDocUrl
                            , lastLine
                            ]
                        )

                _ ->
                    MatchedNoMessage

        ( "", _ ) ->
            MatchedNoMessage

        _ ->
            NotMatched


colonLamderaHandler : String -> String -> String -> MessageHandlerResult
colonLamderaHandler _ input _ =
    if input == ":lamdera" then
        MatchedMessage
            ("""
The Lamdera REPL defines the following functions:

  fem      : Types.FrontendModel
  setFem   : Types.FrontendModel -> Types.FrontendModel
  updateFE : Types.FrontendMsg -> Types.FrontendMsg
  sendToBE : Types.ToBackend -> Types.ToBackend

In the leader tab (green dot) you can also call:

  bem       : Types.BackendModel
  setBem    : Types.BackendModel -> Types.BackendModel
  updateBE  : Types.BackendMsg -> Types.BackendMsg
  sendToFE  : Lamdera.ClientId -> Types.ToFrontend -> Types.ToFrontend
  broadcast : Types.ToFrontend -> Types.ToFrontend

More info at """ ++ replDocUrl ++ """
""")

    else
        NotMatched


otherColonHandler : String -> String -> String -> MessageHandlerResult
otherColonHandler _ input message =
    if String.startsWith ":" input then
        case String.lines message of
            [ err, "", c1, c2, c3, "", inf, "" ] ->
                MatchedMessage
                    (String.join "\n"
                        [ err
                        , ""
                        , String.left 11 c1 ++ "  " ++ String.dropLeft 11 c1
                        , String.left 11 c2 ++ "  " ++ String.dropLeft 11 c2
                        , String.left 11 c3 ++ "  " ++ String.dropLeft 11 c3
                        , "  :lamdera   Show information about Lamdera REPL extensions"
                        , ""
                        , inf
                        , "and at " ++ replDocUrl
                        , ""
                        ]
                    )

            _ ->
                MatchedMessage message

    else
        NotMatched


interfaceTodoHandler : String -> String -> String -> MessageHandlerResult
interfaceTodoHandler _ _ message =
    if String.startsWith "TODO in module `Lamdera.Repl.Interface`" message then
        MatchedMessage
            (String.lines message
                |> List.drop 2
                |> String.join "\n"
                |> String.replace "<docs>" replDocUrl
            )

    else
        NotMatched


replDocUrl : String
replDocUrl =
    -- The length of this string shouldn't change in order to get a nice welcome message!
    -- ......................................."
    "<https://dashboard.lamdera.app/docs/repl>"
