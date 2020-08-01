port module LocalDev exposing (main)

{- This file is copyright © Cofoundry Ltd - All Rights Reserved.
   Unauthorized copying of this file or its contents, via any medium is strictly prohibited.

   ---

   Hello you curious thing!

   This is the development harness used for local development of Lamdera apps.

   This file should not be used as a reference for building Lamdera apps, see
   https://dashboard.lamdera.app/docs/building instead.

   The features used by this file are subject to change/removal and should not
   be relied on in any way.

-}

import Backend
import Browser
import Env
import Frontend
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Lamdera exposing (ClientId, Key, SessionId, Url)
import Lamdera.Debug as LD
import Lamdera.Json as Json
import Lamdera.Wire2 as Wire
import Process
import Task
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg, ToBackend, ToFrontend)



-- MKRRI


lamderaVersion =
    "alpha9"


port sendToBackend : Json.Value -> Cmd msg


port sendToFrontend : Json.Value -> Cmd msg


port receiveFromBackend : (Json.Value -> msg) -> Sub msg


port receiveFromFrontend : (Json.Value -> msg) -> Sub msg


port receiveBackendModel : (Json.Value -> msg) -> Sub msg


port setNodeTypeLeader : (Bool -> msg) -> Sub msg


port setLiveStatus : (Bool -> msg) -> Sub msg


port setClientId : (String -> msg) -> Sub msg


port rpcIn : (Json.Value -> msg) -> Sub msg


port rpcOut : Json.Value -> Cmd msg


port onConnection : (Json.Value -> msg) -> Sub msg


port onDisconnection : (Json.Value -> msg) -> Sub msg


type Msg
    = FEMsg Types.FrontendMsg
    | BEMsg Types.BackendMsg
    | BEtoFE ClientId Types.ToFrontend
    | BEtoFEDelayed ClientId Types.ToFrontend
    | FEtoBE Types.ToBackend
    | FEtoBEDelayed Types.ToBackend
    | FENewUrl Url
    | OnConnection Json.Value
    | OnDisconnection Json.Value
    | ReceivedFromFrontend Json.Value
    | ReceivedFromBackend Json.Value
    | ReceivedBackendModel Json.Value
    | RPCIn Json.Value
    | SetNodeTypeLeader Bool
    | SetLiveStatus Bool
    | ReceivedClientId String
    | ExpandedDevbar
    | CollapsedDevbar
    | ResetDebugStoreBoth
    | ResetDebugStoreFE
    | ResetDebugStoreBE
    | ToggledFreezeMode
    | ToggledNetworkDelay
    | ToggledLogging
    | ClickedLocation
    | PersistBackend Bool
    | Reload
    | EnvClicked
    | EnvModeSelected String
    | Noop


type alias Model =
    { fem : FrontendModel
    , bem : BackendModel
    , bemDirty : Bool
    , originalUrl : Url
    , originalKey : Key
    , nodeType : NodeType
    , liveStatus : LiveStatus
    , sessionId : String
    , clientId : String
    , devbar :
        { expanded : Bool
        , location : Location
        , freeze : Bool
        , networkDelay : Bool
        , logging : Bool
        }
    , showModeChanger : Bool
    }


type Location
    = TopLeft
    | TopRight
    | BottomRight
    | BottomLeft


next location =
    case location of
        TopLeft ->
            TopRight

        TopRight ->
            BottomRight

        BottomRight ->
            BottomLeft

        BottomLeft ->
            TopLeft


userFrontendApp =
    Frontend.app


userBackendApp =
    Backend.app


init : Json.Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        log t v =
            if devbar.logging then
                Debug.log t v

            else
                v

        flagsDecoder =
            Json.succeed NodeInitArgs
                |> required "c" Json.decoderString
                |> required "s" Json.decoderString
                |> required "nt" decodeNodeType
                |> required "i" (Json.decoderList Json.decoderInt)

        args =
            case Json.decodeValue flagsDecoder flags of
                Ok r ->
                    r

                Err err ->
                    Debug.todo "Flags parse failed; this should be impossible! Please report this issue."

        devbar =
            case LD.debugR "d" devbarInit of
                Nothing ->
                    devbarInit

                Just restoredDevbar ->
                    { restoredDevbar | expanded = False }

        ( ifem, iFeCmds ) =
            userFrontendApp.init url key

        ( ibem, iBeCmds ) =
            userBackendApp.init

        ( fem, newFeCmds ) =
            case LD.debugR "fe" ifem of
                Nothing ->
                    ( ifem, iFeCmds )

                Just rfem ->
                    if devbar.freeze then
                        ( rfem, Cmd.none )

                    else
                        ( ifem, iFeCmds )

        ( bem, newBeCmds ) =
            case args.backendModelIntList of
                [] ->
                    ( ibem, iBeCmds )

                backendModelIntList ->
                    case Wire.bytesDecode Types.w2_decode_BackendModel (Wire.intListToBytes args.backendModelIntList) of
                        Just restoredBem ->
                            let
                                x =
                                    log "☀️ Restored BackendModel" restoredBem
                            in
                            ( restoredBem
                            , Cmd.none
                            )

                        Nothing ->
                            let
                                x =
                                    log "❌ Init error" "Failed to decode provided BackendModel! Resetting to init."
                            in
                            ( ibem, iBeCmds )

        devbarInit =
            { expanded = False
            , location = BottomLeft
            , freeze = False
            , networkDelay = False
            , logging = True
            }
    in
    ( { fem = fem
      , bem = bem
      , bemDirty = True
      , originalKey = key
      , originalUrl = url
      , nodeType = args.nodeType
      , liveStatus = Online
      , sessionId = args.sessionId
      , clientId = args.clientId
      , devbar = devbar
      , showModeChanger = False
      }
    , Cmd.batch
        [ Cmd.map FEMsg newFeCmds
        , if args.nodeType == Leader then
            Cmd.map BEMsg newBeCmds

          else
            Cmd.none
        ]
    )


storeFE m newFem =
    if m.devbar.freeze then
        LD.debugS "fe" newFem

    else
        newFem


type alias NodeInitArgs =
    { clientId : String
    , sessionId : String
    , nodeType : NodeType
    , backendModelIntList : List Int
    }


type NodeType
    = Follower
    | Leader


nodeTypeToString nodeType =
    case nodeType of
        Follower ->
            "Follower"

        Leader ->
            "Leader"


decodeNodeType : Json.Decoder NodeType
decodeNodeType =
    Json.decoderString
        |> Json.map
            (\v ->
                case v of
                    "l" ->
                        Leader

                    "f" ->
                        Follower

                    _ ->
                        let
                            x =
                                Debug.log "error" ("decodeNodeType saw an unexpected value: " ++ v)
                        in
                        Follower
            )


type LiveStatus
    = Online
    | Offline


type alias Payload =
    { t : String
    , s : String
    , c : String
    , i : List Int
    }


payloadDecoder =
    Json.succeed Payload
        |> required "t" Json.decoderString
        |> required "s" Json.decoderString
        |> required "c" Json.decoderString
        |> required "i" (Json.decoderList Json.decoderInt)


type alias ClientJson =
    { sessionId : SessionId, clientId : ClientId }


clientJsonDecoder =
    Json.succeed ClientJson
        |> required "s" Json.decoderString
        |> required "c" Json.decoderString


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    let
        log t v =
            if m.devbar.logging then
                Debug.log t v

            else
                v
    in
    -- case log "msg" msg of
    case msg of
        FEMsg frontendMsg ->
            let
                x =
                    log "F   " frontendMsg

                ( newFem, newFeCmds ) =
                    userFrontendApp.update frontendMsg m.fem
            in
            ( { m | fem = storeFE m newFem }
            , Cmd.map FEMsg newFeCmds
            )

        BEMsg backendMsg ->
            case m.nodeType of
                Follower ->
                    -- Followers don't run BE messages
                    ( m, Cmd.none )

                Leader ->
                    let
                        x =
                            log "  B " backendMsg

                        ( newBem, newBeCmds ) =
                            userBackendApp.update backendMsg m.bem
                    in
                    ( { m | bem = newBem, bemDirty = True }
                    , Cmd.batch
                        [ Cmd.map BEMsg newBeCmds
                        ]
                    )

        BEtoFE clientId toFrontend ->
            case m.nodeType of
                Follower ->
                    -- Followers don't broadcast ToFrontends
                    ( m, Cmd.none )

                Leader ->
                    if m.devbar.networkDelay then
                        ( m, delay 500 (BEtoFEDelayed clientId toFrontend) )

                    else
                        let
                            payload =
                                Json.object
                                    [ ( "t", Json.string "ToFrontend" )
                                    , ( "i"
                                      , toFrontend
                                            |> log " ◀️B "
                                            |> Types.w2_encode_ToFrontend
                                            |> Wire.bytesEncode
                                            |> Wire.intListFromBytes
                                            |> Json.list Json.int
                                      )
                                    , ( "s", Json.string "" )
                                    , ( "c", Json.string clientId )
                                    ]
                        in
                        ( m
                        , Cmd.batch
                            [ sendToFrontend payload
                            ]
                        )

        BEtoFEDelayed clientId toFrontend ->
            case m.nodeType of
                Follower ->
                    ( m, Cmd.none )

                Leader ->
                    let
                        payload =
                            Json.object
                                [ ( "t", Json.string "ToFrontend" )
                                , ( "i"
                                  , toFrontend
                                        |> log " ◀️B⏱"
                                        |> Types.w2_encode_ToFrontend
                                        |> Wire.bytesEncode
                                        |> Wire.intListFromBytes
                                        |> Json.list Json.int
                                  )
                                , ( "s", Json.string "" )
                                , ( "c", Json.string clientId )
                                ]
                    in
                    ( m
                    , Cmd.batch
                        [ sendToFrontend payload
                        ]
                    )

        FEtoBE toBackend ->
            if m.devbar.networkDelay then
                ( m, delay 500 (FEtoBEDelayed toBackend) )

            else
                let
                    _ =
                        log "F▶️  " toBackend

                    payload =
                        Json.object
                            [ ( "t", Json.string "ToBackend" )
                            , ( "s", Json.string m.sessionId )
                            , ( "c", Json.string m.clientId )
                            , ( "i", Json.list Json.int (Wire.intListFromBytes (Wire.bytesEncode (Types.w2_encode_ToBackend toBackend))) )
                            ]
                in
                ( m
                , Cmd.batch
                    [ sendToBackend payload
                    ]
                )

        FEtoBEDelayed toBackend ->
            let
                _ =
                    log "F▶️ ⏱" toBackend

                payload =
                    Json.object
                        [ ( "t", Json.string "ToBackend" )
                        , ( "s", Json.string m.sessionId )
                        , ( "c", Json.string m.clientId )
                        , ( "i", Json.list Json.int (Wire.intListFromBytes (Wire.bytesEncode (Types.w2_encode_ToBackend toBackend))) )
                        ]
            in
            ( m
            , Cmd.batch
                [ sendToBackend payload
                ]
            )

        FENewUrl url ->
            let
                ( newModel, newCmds ) =
                    update (FEMsg (userFrontendApp.onUrlChange url)) m
            in
            ( { newModel | originalUrl = url }, newCmds )

        OnConnection json ->
            case Json.decodeValue clientJsonDecoder json of
                Ok c ->
                    ( m, Lamdera.clientConnected_ c.sessionId c.clientId )

                Err err ->
                    let
                        _ =
                            log "❌" "Connect parse failed; this should be impossible! Please report this issue."
                    in
                    ( m, Cmd.none )

        OnDisconnection json ->
            case Json.decodeValue clientJsonDecoder json of
                Ok c ->
                    ( m, Lamdera.clientDisconnected_ c.sessionId c.clientId )

                Err err ->
                    let
                        _ =
                            log "❌" "Disconnect parse failed; this should be impossible! Please report this issue."
                    in
                    ( m, Cmd.none )

        ReceivedFromFrontend payload ->
            case m.nodeType of
                Follower ->
                    -- Followers don't run BE messages
                    ( m, Cmd.none )

                Leader ->
                    case Json.decodeValue payloadDecoder payload of
                        Ok args ->
                            case Wire.bytesDecode Types.w2_decode_ToBackend (Wire.intListToBytes args.i) of
                                Just toBackend ->
                                    let
                                        _ =
                                            log " ▶️B " toBackend

                                        ( newBem, newBeCmds ) =
                                            userBackendApp.updateFromFrontend m.sessionId args.c toBackend m.bem
                                    in
                                    ( { m | bem = newBem, bemDirty = True }
                                    , Cmd.batch
                                        [ Cmd.map BEMsg newBeCmds
                                        ]
                                    )

                                Nothing ->
                                    let
                                        x =
                                            log "❌ ReceivedFromFrontend" "failed to decode provided msg!"
                                    in
                                    ( m, Cmd.none )

                        Err err ->
                            let
                                x =
                                    log "❌ ReceivedFromFrontend decoding error" (Json.errorToString err)
                            in
                            ( m, Cmd.none )

        ReceivedFromBackend payload ->
            case Json.decodeValue payloadDecoder payload of
                Ok args ->
                    case Wire.bytesDecode Types.w2_decode_ToFrontend (Wire.intListToBytes args.i) of
                        Just toFrontend ->
                            let
                                x =
                                    log "F◀️  " toFrontend

                                ( newFem, newFeCmds ) =
                                    userFrontendApp.updateFromBackend toFrontend m.fem
                            in
                            ( { m | fem = storeFE m newFem }
                            , Cmd.map FEMsg newFeCmds
                            )

                        Nothing ->
                            let
                                x =
                                    log "❌ ReceivedFromBackend" "failed to decode provided msg!"
                            in
                            ( m, Cmd.none )

                Err err ->
                    let
                        x =
                            log "❌ ReceivedFromBackend decoding error" (Json.errorToString err)
                    in
                    ( m, Cmd.none )

        ReceivedBackendModel payload ->
            case Json.decodeValue payloadDecoder payload of
                Ok args ->
                    case Wire.bytesDecode Types.w2_decode_BackendModel (Wire.intListToBytes args.i) of
                        Just newBem ->
                            let
                                x =
                                    log "❇️ ReceivedBackendModel" newBem
                            in
                            ( { m | bem = newBem }
                            , Cmd.none
                            )

                        Nothing ->
                            let
                                x =
                                    log "❌ ReceivedBackendModel" "failed to decode provided msg!"
                            in
                            ( m, Cmd.none )

                Err err ->
                    let
                        x =
                            log "❌ ReceivedBackendModel decoding error" (Json.errorToString err)
                    in
                    ( m, Cmd.none )

        RPCIn rpcArgsJson ->
            -- MKRRC
            ( m, Cmd.none )

        --}
        SetNodeTypeLeader bool ->
            ( { m
                | nodeType =
                    case bool of
                        True ->
                            Leader

                        False ->
                            Follower
              }
            , Cmd.none
            )

        SetLiveStatus bool ->
            ( { m
                | liveStatus =
                    case bool of
                        True ->
                            Online

                        False ->
                            Offline
              }
            , Cmd.none
            )

        ReceivedClientId clientId ->
            ( { m | clientId = clientId }, Cmd.none )

        ExpandedDevbar ->
            let
                devbar =
                    m.devbar
            in
            ( { m | devbar = { devbar | expanded = True } }, Cmd.none )

        CollapsedDevbar ->
            let
                devbar =
                    m.devbar
            in
            ( { m | devbar = { devbar | expanded = False } }, Cmd.none )

        ResetDebugStoreBoth ->
            let
                ( newFem, newFeCmds ) =
                    userFrontendApp.init m.originalUrl m.originalKey

                ( newBem, newBeCmds ) =
                    userBackendApp.init
            in
            ( { m
                | fem = LD.debugS "fe" newFem
                , bem = newBem
                , bemDirty = True
              }
            , Cmd.batch
                [ trigger (PersistBackend True)
                ]
            )

        ResetDebugStoreFE ->
            let
                ( newFem, newFeCmds ) =
                    userFrontendApp.init m.originalUrl m.originalKey
            in
            ( { m
                | fem = LD.debugS "fe" newFem
              }
            , Cmd.batch
                [ Cmd.map FEMsg newFeCmds
                ]
            )

        ResetDebugStoreBE ->
            let
                ( newBem, newBeCmds ) =
                    userBackendApp.init
            in
            ( { m
                | bem = newBem
                , bemDirty = True
              }
            , Cmd.batch
                [ Cmd.map BEMsg newBeCmds
                , trigger (PersistBackend True)
                ]
            )

        ToggledFreezeMode ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | freeze = not m.devbar.freeze }

                newFem =
                    if newDevbar.freeze then
                        LD.debugS "fe" m.fem

                    else
                        m.fem
            in
            ( { m | devbar = LD.debugS "d" newDevbar, fem = newFem }
            , Cmd.none
            )

        ToggledNetworkDelay ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | networkDelay = not m.devbar.networkDelay }
            in
            ( { m | devbar = LD.debugS "d" newDevbar }
            , Cmd.none
            )

        ToggledLogging ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | logging = not m.devbar.logging }
            in
            ( { m | devbar = LD.debugS "d" newDevbar }
            , Cmd.none
            )

        ClickedLocation ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | location = next m.devbar.location }
            in
            ( { m | devbar = LD.debugS "d" newDevbar }
            , Cmd.none
            )

        PersistBackend reload ->
            let
                persistBeState nodeType bem =
                    case nodeType of
                        Follower ->
                            Cmd.none

                        Leader ->
                            [ ( "t", Json.string "BackendModel" )
                            , ( "s", Json.string "system" )
                            , ( "c", Json.string "system" )
                            , ( "f"
                              , if reload then
                                    Json.string "force"

                                else
                                    Json.string ""
                              )
                            , ( "i", Json.list Json.int (Wire.intListFromBytes (Wire.bytesEncode (Types.w2_encode_BackendModel bem))) )
                            ]
                                |> Json.object
                                |> sendToBackend
            in
            if m.bemDirty then
                ( { m | bemDirty = False }
                , Cmd.batch
                    [ persistBeState m.nodeType m.bem
                    , if reload then
                        delay 200 Reload

                      else
                        Cmd.none
                    ]
                )

            else
                ( m, Cmd.none )

        Reload ->
            ( m, LD.browserReload )

        EnvClicked ->
            ( { m | showModeChanger = not m.showModeChanger }, Cmd.none )

        EnvModeSelected env ->
            ( m
            , [ ( "t", Json.string "envMode" )
              , ( "v", Json.string env )
              ]
                |> Json.object
                |> sendToBackend
            )

        Noop ->
            ( m, Cmd.none )


subscriptions { nodeType, fem, bem, bemDirty } =
    Sub.batch
        [ Sub.map FEMsg (userFrontendApp.subscriptions fem)
        , if nodeType == Leader then
            Sub.map BEMsg (userBackendApp.subscriptions bem)

          else
            Sub.none
        , if nodeType == Leader && bemDirty then
            LD.every 1000 (always (PersistBackend False))

          else
            Sub.none
        , setNodeTypeLeader SetNodeTypeLeader
        , setLiveStatus SetLiveStatus
        , setClientId ReceivedClientId
        , receiveFromFrontend ReceivedFromFrontend
        , receiveFromBackend ReceivedFromBackend
        , receiveBackendModel ReceivedBackendModel
        , rpcIn RPCIn
        , onConnection OnConnection
        , onDisconnection OnDisconnection
        ]


xForLocation location =
    case location of
        TopLeft ->
            style "left" "5px"

        TopRight ->
            style "right" "5px"

        BottomRight ->
            style "right" "5px"

        BottomLeft ->
            style "left" "5px"


yForLocation location =
    case location of
        TopLeft ->
            style "top" "5px"

        TopRight ->
            style "top" "5px"

        BottomRight ->
            style "bottom" "5px"

        BottomLeft ->
            style "bottom" "5px"


lamderaUI m =
    case m.liveStatus of
        Online ->
            [ node "style" [] [ text customCss ]
            , lamderaPane m
            , if m.showModeChanger then
                withOverlay
                    [ div
                        [ style "padding" "10px"
                        , style "color" white
                        , style "background-color" charcoal
                        , style "border-radius" "5px"
                        ]
                        [ div [ style "padding" "2px" ] [ text "Select `Env.mode` value:" ]
                        , div
                            [ onClick (EnvModeSelected "Development")
                            , style "cursor" "pointer"
                            , style "padding" "6px 6px"
                            , style "margin" "4px 2px"
                            , style "border-left" "3px solid #85BC7A"
                            ]
                            [ text "Development" ]
                        , div
                            [ onClick (EnvModeSelected "Review")
                            , style "cursor" "pointer"
                            , style "padding" "6px 6px"
                            , style "margin" "4px 2px"
                            , style "border-left" "3px solid #4196ad"
                            ]
                            [ text "Review" ]
                        , div
                            [ onClick (EnvModeSelected "Production")
                            , style "cursor" "pointer"
                            , style "padding" "6px 6px"
                            , style "margin" "4px 2px"
                            , style "border-left" "3px solid #E06C75"
                            ]
                            [ text "Production" ]
                        ]
                    ]

              else
                text ""
            ]

        Offline ->
            [ withOverlay
                [ div
                    [ onClick ClickedLocation
                    , style "padding" "10px"
                    , style "display" "flex"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "color" white
                    , style "background-color" charcoal
                    , style "border-radius" "5px"
                    ]
                    [ icon iconWarning 18 yellow
                    , spacer 8
                    , text "lamdera live is not running!"
                    ]
                ]
            ]


lamderaPane m =
    div
        [ style "font-family" "system-ui, Helvetica Neue, sans-serif"
        , style "font-size" "12px"
        , style "position" "fixed"
        , xForLocation m.devbar.location
        , yForLocation m.devbar.location
        , style "color" white
        , style "background-color" charcoal
        , style "border-radius" "5px"
        , onMouseEnter ExpandedDevbar
        , onMouseLeave CollapsedDevbar
        , style "user-select" "none"
        ]
        (case m.devbar.location of
            TopLeft ->
                devBar True m

            TopRight ->
                devBar True m

            BottomRight ->
                devBar False m

            BottomLeft ->
                devBar False m
        )


withOverlay html =
    div
        [ style "font-family" "system-ui, Helvetica Neue, sans-serif"
        , style "font-size" "14px"
        , style "display" "block"
        , style "position" "fixed"
        , style "top" "0"
        , style "left" "0"
        , style "height" "100vh"
        , style "width" "100vw"
        , style "background-color" "#2e333588"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , onClick EnvClicked
        , class "lamdera-overlay"
        ]
        html


envIndicator =
    let
        ( label, color ) =
            envMeta
    in
    div []
        [ div
            [ style "text-align" "center"
            , style "border-top" "1px solid #393939"
            , style "border-radius" "0px 0px 5px 5px"
            , style "font-size" "10px"
            , style "padding" "1px 4px 2px 4px"
            , style "cursor" "pointer"
            , style "color" "#fff"
            , onClick EnvClicked
            ]
            [ text <| "Env: ", span [ style "color" color ] [ text label ] ]
        ]


envMeta =
    case Env.mode of
        Env.Production ->
            ( "Prod", red )

        Env.Review ->
            ( "Review", blue )

        Env.Development ->
            ( "Dev", green )


devBar topDown m =
    case topDown of
        True ->
            [ collapsedUI m
            , if m.devbar.expanded then
                div
                    [ style "border-top" "1px solid #393939"
                    ]
                    [ expandedUI topDown m
                    ]

              else
                text ""
            ]

        False ->
            [ if m.devbar.expanded then
                div
                    [ style "border-bottom" "1px solid #393939"
                    , style "padding-bottom" "5px"
                    ]
                    [ expandedUI topDown m ]

              else
                text ""
            , collapsedUI m
            ]


collapsedUI m =
    div []
        [ div
            [ style "padding" "5px 7px 2px 5px"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            ]
            [ span
                [ onClick ClickedLocation
                , class "lamderaLogoWhite"
                , style "position" "relative"
                ]
                [ case m.nodeType of
                    Leader ->
                        div
                            [ style "background-color" "#a6f098"
                            , style "height" "4px"
                            , style "width" "4px"
                            , style "border-radius" "10px"
                            , style "position" "absolute"
                            , style "top" "3px"
                            , style "left" "-3px"
                            ]
                            []

                    Follower ->
                        text ""
                ]
            , spacer 5
            , if m.devbar.freeze then
                summaryIcon iconFreeze blue ToggledFreezeMode

              else
                summaryIcon iconFreeze grey ToggledFreezeMode
            , spacer 5
            , if m.devbar.networkDelay then
                summaryIcon iconNetwork yellow ToggledNetworkDelay

              else
                summaryIcon iconNetwork grey ToggledNetworkDelay
            , spacer 5
            , if m.devbar.logging then
                summaryIcon iconLogs white ToggledLogging

              else
                summaryIcon iconLogs grey ToggledLogging
            ]
        , envIndicator
        ]


summaryIcon icon_ color msg =
    span [ onClick msg, style "cursor" "pointer" ] [ icon icon_ 16 color ]


spacer width =
    -- If only we had elm-ui :'(
    span [ style "width" (String.fromInt width ++ "px"), style "display" "inline-block" ] []


expandedUI topDown m =
    let
        modeText =
            case m.devbar.freeze of
                False ->
                    "Inactive"

                True ->
                    "Active"

        envDocs =
            let
                borderPos =
                    if topDown then
                        "border-top"

                    else
                        "border-bottom"
            in
            div
                [ style "display" "flex"
                , style "justify-content" "space-evenly"
                , style borderPos "1px solid #393939"
                ]
                [ let
                    ( label, color ) =
                        envMeta
                  in
                  buttonDevColored "Env" label color EnvClicked
                , div [ style "height" "30px", style "width" "1px", style "background-color" "#393939" ] []
                , buttonDevLink "Docs" "https://dashboard.lamdera.app/docs"
                ]
    in
    div [ style "width" "175px" ]
        [ if topDown then
            text ""

          else
            envDocs
        , case m.devbar.freeze of
            False ->
                buttonDev "Reset Backend" ResetDebugStoreBE

            True ->
                buttonDev "Reset Both" ResetDebugStoreBoth
        , if m.devbar.freeze then
            buttonDev "Reset Frontend" ResetDebugStoreFE

          else
            text ""
        , case m.devbar.freeze of
            False ->
                buttonDevOff "Freeze Mode: Off" iconFreeze ToggledFreezeMode

            True ->
                buttonDevColoredIcon "Freeze Mode" "On" blue iconFreeze ToggledFreezeMode
        , case m.devbar.networkDelay of
            True ->
                buttonDevColoredIcon "Network Delay" "500ms" yellow iconNetwork ToggledNetworkDelay

            False ->
                buttonDevOff "Network Delay: Off" iconNetwork ToggledNetworkDelay
        , case m.devbar.logging of
            True ->
                -- buttonDev "Logging: On"
                buttonDevColoredIcon "Logging" "On" white iconLogs ToggledLogging

            False ->
                buttonDevOff "Logging: Off" iconLogs ToggledLogging
        , if topDown then
            envDocs

          else
            text ""
        ]


buttonDev label msg =
    div
        [ style "color" white
        , style "cursor" "pointer"
        , style "padding" "8px 8px"
        , style "text-align" "center"
        , onClick msg
        ]
        [ text label
        ]


buttonDevColored label value color msg =
    div
        [ style "color" white
        , style "cursor" "pointer"
        , style "padding" "8px 8px"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , onClick msg
        ]
        [ text label
        , text ":"
        , spacer 3
        , span [ style "color" color ] [ text value ]
        ]


buttonDevColoredIcon label value color icon_ msg =
    div
        [ style "color" white
        , style "cursor" "pointer"
        , style "padding" "8px 8px"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , onClick msg
        ]
        [ icon icon_ 16 color
        , spacer 5
        , text label
        , text ":"
        , spacer 3
        , span [ style "color" color ] [ text value ]
        ]


buttonDevOff label icon_ msg =
    div
        [ style "color" grey
        , style "cursor" "pointer"
        , style "padding" "8px 8px"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , onClick msg
        ]
        [ icon icon_ 16 grey
        , spacer 5
        , text label
        ]


buttonDevLink label url =
    a
        [ style "color" white
        , style "cursor" "pointer"
        , style "padding" "8px 8px"
        , style "text-decoration" "none"
        , style "display" "block"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , href url
        , target "_blank"
        ]
        [ text label
        , spacer 5
        , icon iconExternalLink 12 white
        ]


mapDocument model msg { title, body } =
    { title = title
    , body =
        List.map (Html.map msg) body
            ++ lamderaUI model
    }


customCss =
    """
.lamderaLogoWhite {
  margin: -2px 5px 0 5px;
  display: inline-block;
  vertical-align: middle;
  height: 22px;
  width: 13px;
  cursor: pointer;
  background-image:  url("data:image/svg+xml;utf8,<svg width='13px' height='23px' viewBox='0 0 23 27' version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'><g stroke='none' stroke-width='1' fill='none' fill-rule='evenodd'><g transform='translate(-272.000000, -129.000000)' fill='white'><g transform='translate(272.000000, 129.000000)'><path d='M22.721133,26.2285714 C22.3975092,26.7428597 21.8359081,27 21.1720266,27 C20.745075,26.9311782 20.4000491,26.7155717 20.1369487,26.3531804 C19.9207409,26.049077 19.4876467,25.1169484 18.8376663,23.5567944 L11.48425,6.00209059 L3.14198812,25.9651568 C2.85432248,26.6550557 2.3569081,27 1.64973006,27 C1.42199476,27 1.20025582,26.9498263 0.984506591,26.8494774 C0.564994195,26.6613231 0.277332868,26.3477374 0.121513978,25.9087108 C-0.0462909803,25.4696842 -0.040298036,25.0306642 0.139492991,24.5916376 L9.99199199,1.03484321 C10.2796576,0.344944286 10.777072,0 11.48425,0 C12.2034142,0 12.7068215,0.344944286 12.9944871,1.03484321 L22.8469861,24.5916376 C23.0867075,25.1561004 23.0447569,25.7017395 22.721133,26.2285714 Z'></path></g></g></g></svg>");
}
"""


icon fn size hex =
    div
        [ style "display" "inline-block"
        , style "height" (String.fromInt size ++ "px")
        , style "width" (String.fromInt size ++ "px")
        , style "background-image" (fn size hex)
        ]
        []


iconWarning size hex =
    """url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='"""
        ++ String.fromInt size
        ++ """' height='"""
        ++ String.fromInt size
        ++ """' viewBox='0 0 24 24' fill='none' stroke='"""
        ++ String.replace "#" "%23" hex
        ++ """' stroke-width='2' stroke-linecap='round' stroke-linejoin='round' class='feather feather-alert-triangle'><path d='M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z'></path><line x1='12' y1='9' x2='12' y2='13'></line><line x1='12' y1='17' x2='12.01' y2='17'></line></svg>")"""


iconNetwork size hex =
    """url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='"""
        ++ String.fromInt size
        ++ """' height='"""
        ++ String.fromInt size
        ++ """' viewBox='0 0 24 24' fill='none' stroke='"""
        ++ String.replace "#" "%23" hex
        ++ """' stroke-width='2' stroke-linecap='round' stroke-linejoin='round' class='feather feather-zap'><polygon points='13 2 3 14 12 14 11 22 21 10 12 10 13 2'></polygon></svg>")"""


iconFreeze size hex =
    """url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='"""
        ++ String.fromInt size
        ++ """' height='"""
        ++ String.fromInt size
        ++ """' viewBox='0 0 24 24' fill='none' stroke='"""
        ++ String.replace "#" "%23" hex
        ++ """' stroke-width='2' stroke-linecap='round' stroke-linejoin='round' > <path d='M12 2v6.5M10 4l2 1 2-1M3.3 7L9 10.2m-4.9-.5L6 8.5l.1-2.2M3.3 17L9 13.7m-2.9 4L6 15.5l-1.9-1.2M12 22v-6.5m2 4.5l-2-1-2 1m5-6.2l5.6 3.3m-.7-2.8L18 15.5l-.1 2.2M20.7 7L15 10.3m2.9-4l.1 2.2 1.9 1.2M12 8.5l3 1.8v3.5l-3 1.8-3-1.8v-3.5l3-1.8z' /></svg>")"""


iconLogs size hex =
    """url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='"""
        ++ String.fromInt size
        ++ """' height='"""
        ++ String.fromInt size
        ++ """' viewBox='0 0 24 24' fill='none' stroke='"""
        ++ String.replace "#" "%23" hex
        ++ """' stroke-width='2' stroke-linecap='round' stroke-linejoin='round' class='feather feather-align-justify'><line x1='21' y1='10' x2='3' y2='10'></line><line x1='21' y1='6' x2='3' y2='6'></line><line x1='21' y1='14' x2='3' y2='14'></line><line x1='21' y1='18' x2='3' y2='18'></line></svg>")"""


iconExternalLink size hex =
    """url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='"""
        ++ String.fromInt size
        ++ """' height='"""
        ++ String.fromInt size
        ++ """' viewBox='0 0 24 24' fill='none' stroke='"""
        ++ String.replace "#" "%23" hex
        ++ """' stroke-width='2' stroke-linecap='round' stroke-linejoin='round' class='feather feather-external-link'><path d='M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6'></path><polyline points='15 3 21 3 21 9'></polyline><line x1='10' y1='14' x2='21' y2='3'></line></svg>")"""


red =
    "#E06C75"


green =
    "#85BC7A"


blue =
    "#4196AD"


yellow =
    "#FFCB64"


white =
    "#EEE"


grey =
    "#666"


charcoal =
    "#2e3335"


main : Program Json.Value Model Msg
main =
    Browser.application
        { init = init
        , view =
            \model ->
                mapDocument model FEMsg (userFrontendApp.view model.fem)
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \ureq -> FEMsg (userFrontendApp.onUrlRequest ureq)
        , onUrlChange = \url -> FENewUrl url
        }


delay time msg =
    Process.sleep time |> Task.perform (always msg)


trigger msg =
    Process.sleep 0 |> Task.perform (always msg)


required : String -> Json.Decoder a -> Json.Decoder (a -> b) -> Json.Decoder b
required key valDecoder decoder =
    custom (Json.field key valDecoder) decoder


custom : Json.Decoder a -> Json.Decoder (a -> b) -> Json.Decoder b
custom =
    Json.map2 (|>)
