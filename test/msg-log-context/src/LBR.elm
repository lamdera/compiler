port module LBR exposing (main)

{-| A minimal backend for the log context test (test/Test/MsgLogContext.hs).

The module is called LBR so the compiler treats it as a Lamdera backend and adds
the backend injection (extra/Lamdera/Injection.hs). test/msg-log-context/harness.js
drives it from node through the ports and checks which global.lamderaCtx every
probe, log line, port call and outbound request saw.

Probes: the flags are a JS Proxy that records global.lamderaCtx whenever a field
is read, so `probe model label` records the context in effect wherever Elm code
evaluates it (in update, or in a Task.andThen callback).

-}

import Http
import Json.Decode as D
import LamderaHelpers
import Log
import Process
import Task exposing (Task)
import Time


port trigger : (String -> msg) -> Sub msg


port out : String -> Cmd msg


type Msg
    = Trigger String
    | GotHttp String (Result Http.Error String)
    | ChainDone String (Result Http.Error Int)
    | GotNow String Time.Posix
    | StartNested String


type alias Model =
    { probe : D.Value
    , n : Int
    }


probe : Model -> String -> Int
probe model label =
    D.decodeValue (D.field label D.int) model.probe
        |> Result.withDefault 0


serialize : Msg -> ( Int, Maybe String )
serialize msg =
    case msg of
        Trigger tag ->
            ( 1, Just tag )

        GotHttp tag _ ->
            ( 2, Just tag )

        ChainDone tag _ ->
            ( 3, Just tag )

        GotNow tag _ ->
            ( 4, Just tag )

        StartNested tag ->
            ( 5, Just tag )


{-| An Http.task, then two sleeps, probing the context after each step.
-}
chain : Model -> String -> Float -> Float -> Task Http.Error Int
chain model tag d1 d2 =
    Http.task
        { method = "GET"
        , headers = []
        , url = "http://test/task-" ++ tag
        , body = Http.emptyBody
        , resolver = Http.stringResolver (\_ -> Ok ())
        , timeout = Nothing
        }
        |> Task.andThen (\_ -> Process.sleep (d1 + toFloat (probe model ("chain:" ++ tag ++ ":1"))))
        |> Task.andThen (\_ -> Process.sleep (d2 + toFloat (probe model ("chain:" ++ tag ++ ":2"))))
        |> Task.andThen (\_ -> Task.succeed (probe model ("chain:" ++ tag ++ ":3")))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Trigger tag ->
            let
                m =
                    { model | n = model.n + probe model ("upd:" ++ tag) + String.length (Log.log ("dbg:upd:" ++ tag) tag) }
            in
            case tag of
                "sync" ->
                    -- Two msgs produced synchronously while this msg's effects are being
                    -- dispatched: their effects queue up behind this msg's
                    ( m
                    , Cmd.batch
                        [ Task.perform (GotNow "s1") Time.now
                        , Task.perform (GotNow "s2") Time.now
                        , out "port:sync"
                        ]
                    )

                "nested" ->
                    ( m, Task.perform (\_ -> StartNested "n") (Process.sleep 5) )

                _ ->
                    -- "A" and "B": an Http Cmd, a port Cmd and an async Task chain
                    ( m
                    , Cmd.batch
                        [ out ("port:" ++ tag)
                        , Http.get { url = "http://test/" ++ tag, expect = Http.expectString (GotHttp tag) }
                        , Task.attempt (ChainDone tag)
                            (if tag == "A" then
                                chain m tag 30 30

                             else
                                chain m tag 15 30
                            )
                        ]
                    )

        StartNested tag ->
            -- Runs inside a scheduler step (after a sleep), so the effects of the two Time.now
            -- msgs reach the effect managers' mailboxes before the managers are stepped
            ( { model | n = model.n + probe model "upd:startnested" }
            , Cmd.batch
                [ Task.perform (GotNow (tag ++ "1")) Time.now
                , Task.perform (GotNow (tag ++ "2")) Time.now
                ]
            )

        GotNow tag _ ->
            ( { model | n = model.n + probe model ("upd:now:" ++ tag) }
            , out ("port:now:" ++ tag)
            )

        GotHttp tag _ ->
            ( { model | n = model.n + probe model ("upd:http:" ++ tag) }
            , out ("port:http:" ++ tag)
            )

        ChainDone tag _ ->
            ( { model | n = model.n + probe model ("upd:chaindone:" ++ tag) }
            , Cmd.none
            )


main : Program D.Value Model Msg
main =
    let
        ensureOutputInclusion =
            -- Functions that aren't referenced get DCE'd, but the injection uses these from JS
            ( ( serialize, LamderaHelpers.decodeWireAnalytics, LamderaHelpers.decodeWirePayloadHeader )
            , Maybe.withDefault
            )
    in
    Platform.worker
        { init = \flags -> ( { probe = flags, n = 0 }, Cmd.none )
        , update = update
        , subscriptions = \_ -> trigger Trigger
        }
