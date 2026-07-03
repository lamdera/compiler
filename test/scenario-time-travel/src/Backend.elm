module Backend exposing (app)

import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Lamdera.onConnect ClientConnected


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { counter = 0 }, Cmd.none )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected _ clientId ->
            ( model, Lamdera.sendToFrontend clientId (CounterNewValue model.counter) )

        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ _ msg model =
    case msg of
        CounterIncremented ->
            let
                newModel =
                    { model | counter = model.counter + 1 }
            in
            ( newModel, Lamdera.broadcast (CounterNewValue newModel.counter) )

        CounterDecremented ->
            let
                newModel =
                    { model | counter = model.counter - 1 }
            in
            ( newModel, Lamdera.broadcast (CounterNewValue newModel.counter) )
