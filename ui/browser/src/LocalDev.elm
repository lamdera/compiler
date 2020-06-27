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


port sendToBackend : Json.Value -> Cmd msg


port sendToFrontend : Json.Value -> Cmd msg


port receiveFromBackend : (Json.Value -> msg) -> Sub msg


port receiveFromFrontend : (Json.Value -> msg) -> Sub msg


port receiveBackendModel : (Json.Value -> msg) -> Sub msg


port nodeTypeSetLeader : (Bool -> msg) -> Sub msg


port liveStatusSet : (Bool -> msg) -> Sub msg


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
    | NodeTypeSetLeader Bool
    | LiveStatusSet Bool
    | ReceivedClientId String
    | DevbarExpand
    | DevbarCollapse
    | ResetDebugStoreBoth
    | ResetDebugStoreFE
    | ResetDebugStoreBE
    | ToggledDevMode
    | ToggledNetworkDelay
    | ToggledLogging
    | ClickedLocation
    | PersistBackend Bool
    | Reload
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
        , devMode : DevMode
        , networkDelay : Bool
        , logging : Bool
        }
    }


type DevMode
    = Normal
    | Freeze


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
                    if devbar.devMode == Freeze then
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
            , devMode = Normal
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
    if m.devbar.devMode == Freeze then
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
        NodeTypeSetLeader bool ->
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

        LiveStatusSet bool ->
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

        DevbarExpand ->
            let
                devbar =
                    m.devbar
            in
            ( { m | devbar = { devbar | expanded = True } }, Cmd.none )

        DevbarCollapse ->
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

        ToggledDevMode ->
            let
                toggleDevMode mode =
                    case mode of
                        Normal ->
                            Freeze

                        Freeze ->
                            Normal

                devbar =
                    m.devbar

                newDevbar =
                    { devbar | devMode = toggleDevMode m.devbar.devMode }

                newFem =
                    if newDevbar.devMode == Freeze then
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

        Noop ->
            ( m, Cmd.none )


subscriptions { nodeType, fem, bem } =
    Sub.batch
        [ Sub.map FEMsg (userFrontendApp.subscriptions fem)
        , if nodeType == Leader then
            Sub.map BEMsg (userBackendApp.subscriptions bem)

          else
            Sub.none
        , if nodeType == Leader then
            LD.every 1000 (always (PersistBackend False))

          else
            Sub.none
        , nodeTypeSetLeader NodeTypeSetLeader
        , liveStatusSet LiveStatusSet
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


lamderaPane m =
    div
        [ style "font-family" "sans-serif"
        , style "font-size" "12px"
        , style "position" "fixed"
        , xForLocation m.devbar.location
        , yForLocation m.devbar.location
        , style "z-index" "100"
        , style "color" "#fff"
        , style "background-color" "#61b6cd"
        , style "background-color" "#2e3335"
        , style "border-radius" "4px"
        , onMouseEnter DevbarExpand
        , onMouseLeave DevbarCollapse
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


devBar topDown m =
    case topDown of
        True ->
            [ collapsedUI m
            , if m.devbar.expanded then
                Html.div [ style "padding-bottom" "5px" ]
                    [ expandedUI m
                    ]

              else
                text ""
            ]

        False ->
            [ if m.devbar.expanded then
                expandedUI m

              else
                text ""
            , collapsedUI m
            ]


collapsedUI m =
    case m.liveStatus of
        Online ->
            div
                [ style "padding" "3px"
                ]
                [ node "style" [] [ text customCss ]
                , Html.span
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
                                , style "top" "1px"
                                , style "left" "-3px"
                                ]
                                []

                        Follower ->
                            text ""
                    ]
                , text " "
                , freezeToggle m
                , if m.devbar.networkDelay then
                    span
                        []
                        [ text " 🐢" ]

                  else
                    text ""
                ]

        Offline ->
            Html.div
                [ onClick ClickedLocation
                , style "padding" "3px 8px"
                ]
                [ text "⚠️ lamdera live is not running!" ]


freezeToggle m =
    let
        checked =
            case m.devbar.devMode of
                Normal ->
                    attribute "data-x" ""

                Freeze ->
                    attribute "checked" "checked"
    in
    div [ class "field", attribute "for" "dev-mode" ]
        [ label [ class "label label-toggle" ]
            [ div [ class "input-toggle" ]
                [ input [ class "input-checkbox", id "dev-mode", type_ "checkbox", onClick ToggledDevMode, checked ] []
                , span [ class "input-toggle-handle" ] []
                ]
            ]
        ]


expandedUI m =
    let
        modeText =
            case m.devbar.devMode of
                Normal ->
                    "Inactive"

                Freeze ->
                    "Active"
    in
    div []
        [ buttonDevInactiveBy (m.devbar.devMode == Freeze) "Reset Frontend" ResetDebugStoreFE
        , case m.devbar.devMode of
            Normal ->
                buttonDev "Reset Backend" ResetDebugStoreBE

            Freeze ->
                buttonDev "Reset Both" ResetDebugStoreBoth
        , case m.devbar.networkDelay of
            True ->
                buttonDev "Network Delay: 500ms" ToggledNetworkDelay

            False ->
                buttonDevOff "Network Delay: Off" ToggledNetworkDelay
        , case m.devbar.logging of
            True ->
                buttonDev "Logging: On" ToggledLogging

            False ->
                buttonDevOff "Logging: Off" ToggledLogging
        ]


buttonDev label msg =
    div
        [ style "background-color" "#555"
        , style "color" "#eee"
        , style "cursor" "pointer"
        , style "padding" "3px 8px"
        , style "margin-top" "5px"
        , style "text-align" "center"
        , onClick msg
        ]
        [ text label
        ]


buttonDevOff label msg =
    div
        [ style "background-color" "#444"
        , style "color" "#888"
        , style "cursor" "pointer"
        , style "padding" "3px 8px"
        , style "margin-top" "5px"
        , style "text-align" "center"
        , onClick msg
        ]
        [ text label
        ]


buttonDevInactiveBy cond label msg =
    if cond then
        buttonDev label msg

    else
        div
            [ style "background-color" "#444"
            , style "color" "#888"
            , style "padding" "3px 8px"
            , style "margin-top" "5px"
            , style "text-align" "center"
            ]
            [ text label
            ]


mapDocument model msg { title, body } =
    { title = title
    , body =
        [ lamderaPane model
        ]
            ++ List.map (Html.map msg) body
    }


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


{-| Modified from <https://iamsteve.me/blog/entry/css-only-ios-style-toggle>
-}
customCss =
    """
.input-toggle {
  position: relative;
  display: inline-block;
  vertical-align: middle;
}
.input-toggle:before {
  content: "on";
  left: 0;
  color: #fff;
}
.input-toggle:after {
  content: "off";
  right: 0;
  color: #888;
}
.input-toggle:before, .input-toggle:after {
  position: absolute;
  top: 50%;
  -webkit-transform: translateY(-50%);
          transform: translateY(-50%);
  padding: 12px 9px;
  font-size: 12px;
  z-index: 0;
}
.input-toggle input {
  -webkit-appearance: none;
     -moz-appearance: none;
          appearance: none;
  position: absolute;
  z-index: 1;
  top: 0;
  right: 0;
  bottom: 0;
  left: 0;
  opacity: 0;
}
.input-toggle input:checked ~ .input-toggle-handle {
  box-shadow: inset 0 1px rgba(0, 0, 0, 0.15), inset 0 0 0 1px #598BA6;
}
.input-toggle input:checked ~ .input-toggle-handle:before {
  left: 29px;
  background: #fff;
  color: #598BA6;
  box-shadow: inset 0 -1px rgba(0, 0, 0, 0.2), inset 0 0 0 1px #598BA6, 0 1px 2px rgba(0, 0, 0, 0.1), 0 6px 12px rgba(0, 0, 0, 0.1);
}

.input-toggle-handle {
  display: block;
  width: 64px;
  height: 25px;
  background-image: linear-gradient(#6FB9E1, #90DDFF);
  border-radius: 4px;
  transition: .2s ease;
  box-shadow: inset 0 1px rgba(0, 0, 0, 0.15), inset 0 0 0 1px rgba(0, 0, 0, 0.15), inset 0 1px 2px rgba(0, 0, 0, 0.15), inset 44px 44px #333;
}

.input-toggle-handle:before {
  content: "❄";
  line-height: 19px;
  font-size: 15px;
  position: absolute;
  z-index: 1;
  top: 3px;
  left: 3px;
  width: 32px;
  height: 19px;
  transition: .4s ease;
  background: #666;
  color: #333;
  border-radius: 4px;
  box-shadow: inset 0 -1px rgba(0, 0, 0, 0.2), inset 0 0 0 1px rgba(0, 0, 0, 0.15), 0 1px 2px rgba(0, 0, 0, 0.1), 0 6px 12px rgba(0, 0, 0, 0.1);
}

.field {
  text-align: center;
  display: inline-block;
}

.label-toggle {
  color: #444;
  font-weight: 700;
  cursor: pointer;
}

.lamderaLogoWhite {
  margin: 0 5px;
  display: inline-block;
  vertical-align: middle;
  height: 23px;
  width: 13px;
  cursor: pointer;
  background-image:  url("data:image/svg+xml;utf8,<svg width='13px' height='23px' viewBox='0 0 23 27' version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'><g stroke='none' stroke-width='1' fill='none' fill-rule='evenodd'><g transform='translate(-272.000000, -129.000000)' fill='white'><g transform='translate(272.000000, 129.000000)'><path d='M22.721133,26.2285714 C22.3975092,26.7428597 21.8359081,27 21.1720266,27 C20.745075,26.9311782 20.4000491,26.7155717 20.1369487,26.3531804 C19.9207409,26.049077 19.4876467,25.1169484 18.8376663,23.5567944 L11.48425,6.00209059 L3.14198812,25.9651568 C2.85432248,26.6550557 2.3569081,27 1.64973006,27 C1.42199476,27 1.20025582,26.9498263 0.984506591,26.8494774 C0.564994195,26.6613231 0.277332868,26.3477374 0.121513978,25.9087108 C-0.0462909803,25.4696842 -0.040298036,25.0306642 0.139492991,24.5916376 L9.99199199,1.03484321 C10.2796576,0.344944286 10.777072,0 11.48425,0 C12.2034142,0 12.7068215,0.344944286 12.9944871,1.03484321 L22.8469861,24.5916376 C23.0867075,25.1561004 23.0447569,25.7017395 22.721133,26.2285714 Z'></path></g></g></g></svg>");
}

"""


required : String -> Json.Decoder a -> Json.Decoder (a -> b) -> Json.Decoder b
required key valDecoder decoder =
    custom (Json.field key valDecoder) decoder


custom : Json.Decoder a -> Json.Decoder (a -> b) -> Json.Decoder b
custom =
    Json.map2 (|>)
