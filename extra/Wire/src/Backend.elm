module Backend exposing (Model, app)

import Lamdera.Backend
import Lamdera.Types exposing (ClientId, Milliseconds, WsError)
import Msg exposing (BackendMsg(..), ToBackend(..), ToFrontend(..))
import Set exposing (Set)


app =
    Lamdera.Backend.application
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


type alias Model =
    { counter : Int
    , clients : Set ClientId
    }


init : ( Model, Cmd BackendMsg )
init =
    ( { counter = 0, clients = Set.empty }, Cmd.none )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


updateFromFrontend : ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend clientId msg model =
    case msg of
        ClientJoin ->
            ( { model | clients = Set.insert clientId model.clients }
            , sendToFrontend clientId (CounterNewValue model.counter)
            )

        CounterIncremented ->
            let
                newCounterValue =
                    model.counter + 1
            in
            ( { model | counter = newCounterValue }, broadcast model.clients (CounterNewValue newCounterValue) )

        CounterDecremented ->
            let
                newCounterValue =
                    model.counter - 1
            in
            ( { model | counter = newCounterValue }, broadcast model.clients (CounterNewValue newCounterValue) )


broadcast clients msg =
    clients
        |> Set.toList
        |> List.map (\clientId -> sendToFrontend clientId msg)
        |> Cmd.batch


sendToFrontend : ClientId -> ToFrontend -> Cmd BackendMsg
sendToFrontend clientId msg =
    Lamdera.Backend.sendToFrontend 1000 clientId msg
