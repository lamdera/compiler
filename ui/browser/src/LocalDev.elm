module LocalDev exposing (main)

import Backend
import Browser
import Browser.Navigation as Navigation
import Frontend
import Html
import Html.Attributes as A
import Html.Events
import Lamdera.Debug as Lamdera
import Lamdera.Frontend exposing (ClientId, Url)
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg, ToBackend, ToFrontend)


type Msg
    = FEMsg Types.FrontendMsg
    | BEMsg Types.BackendMsg
    | BEtoFE ClientId Types.ToFrontend
    | FEtoBE Types.ToBackend
    | DevbarExpand
    | DevbarCollapse
    | ResetDebugStoreBoth
    | ResetDebugStoreFE
    | ResetDebugStoreBE
    | ToggledSnapshotFE
    | ToggledSnapshotBE
    | ClickedLocation


type alias Model =
    { fem : FrontendModel
    , bem : BackendModel
    , originalUrl : Url
    , originalKey : Navigation.Key
    , devbar :
        { expanded : Bool
        , snapshotFE : Bool
        , snapshotBE : Bool
        , location : Location
        }
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


init : flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( ifem, iFeCmds ) =
            userFrontendApp.init url key

        ( ibem, iBeCmds ) =
            userBackendApp.init

        ( fem, newFeCmds ) =
            case Lamdera.debugR "fe" ifem of
                Nothing ->
                    ( ifem, iFeCmds )

                Just rfem ->
                    ( rfem, Cmd.none )

        ( bem, newBeCmds ) =
            case Lamdera.debugR "be" ibem of
                Nothing ->
                    ( ibem, iBeCmds )

                Just rbem ->
                    ( rbem, Cmd.none )

        devbarInit =
            { expanded = False
            , snapshotFE = True
            , snapshotBE = True
            , location = TopLeft
            }
    in
    ( { fem = fem
      , bem = bem
      , originalKey = key
      , originalUrl = url
      , devbar =
            case Lamdera.debugR "d" devbarInit of
                Nothing ->
                    devbarInit

                Just restoredDevbar ->
                    { restoredDevbar | expanded = False }
      }
    , Cmd.batch
        [ Cmd.map FEMsg newFeCmds
        , Cmd.map BEMsg newBeCmds
        ]
    )


storeFE m newFem =
    if m.devbar.snapshotFE then
        Lamdera.debugS "fe" newFem

    else
        newFem


storeBE m newBem =
    if m.devbar.snapshotBE then
        Lamdera.debugS "be" newBem

    else
        newBem


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        FEMsg frontendMsg ->
            let
                x =
                    Debug.log "FEMsg" frontendMsg

                ( newFem, newFeCmds ) =
                    userFrontendApp.update frontendMsg m.fem
            in
            ( { m | fem = storeFE m newFem, bem = m.bem }, Cmd.map FEMsg newFeCmds )

        BEMsg backendMsg ->
            let
                x =
                    Debug.log "BEMsg" backendMsg

                ( newBem, newBeCmds ) =
                    userBackendApp.update backendMsg m.bem
            in
            ( { m | fem = m.fem, bem = storeBE m newBem }, Cmd.map BEMsg newBeCmds )

        BEtoFE clientId toFrontend ->
            let
                x =
                    Debug.log "BEtoFE" toFrontend

                ( newFem, newFeCmds ) =
                    userFrontendApp.updateFromBackend toFrontend m.fem
            in
            ( { m | fem = storeFE m newFem, bem = m.bem }, Cmd.map FEMsg newFeCmds )

        FEtoBE toBackend ->
            let
                _ =
                    Debug.log "FEtoBE" toBackend

                ( newBem, newBeCmds ) =
                    userBackendApp.updateFromFrontend "sessionIdLocalDev" "clientIdLocalDev" toBackend m.bem
            in
            ( { m | fem = m.fem, bem = storeBE m newBem }, Cmd.map BEMsg newBeCmds )

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
                | fem = Lamdera.debugS "fe" newFem
                , bem = Lamdera.debugS "be" newBem
              }
            , Cmd.batch
                [ Cmd.map FEMsg newFeCmds
                , Cmd.map BEMsg newBeCmds
                ]
            )

        ResetDebugStoreFE ->
            let
                ( newFem, newFeCmds ) =
                    userFrontendApp.init m.originalUrl m.originalKey
            in
            ( { m
                | fem = Lamdera.debugS "fe" newFem
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
                | bem = Lamdera.debugS "be" newBem
              }
            , Cmd.batch
                [ Cmd.map BEMsg newBeCmds
                ]
            )

        ToggledSnapshotFE ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | snapshotFE = not m.devbar.snapshotFE }
            in
            ( { m | devbar = Lamdera.debugS "d" newDevbar }
            , if newDevbar.snapshotFE then
                Cmd.none

              else
                Lamdera.debugD "fe"
            )

        ToggledSnapshotBE ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | snapshotBE = not m.devbar.snapshotBE }
            in
            ( { m | devbar = Lamdera.debugS "d" newDevbar }
            , if newDevbar.snapshotBE then
                Cmd.none

              else
                Lamdera.debugD "be"
            )

        ClickedLocation ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | location = next m.devbar.location }
            in
            ( { m | devbar = Lamdera.debugS "d" newDevbar }
            , Cmd.none
            )


subscriptions { fem, bem } =
    Sub.batch
        [ Sub.map FEMsg (userFrontendApp.subscriptions fem)
        , Sub.map BEMsg (userBackendApp.subscriptions bem)
        ]


xForLocation location =
    case location of
        TopLeft ->
            A.style "left" "5px"

        TopRight ->
            A.style "right" "5px"

        BottomRight ->
            A.style "right" "5px"

        BottomLeft ->
            A.style "left" "5px"


yForLocation location =
    case location of
        TopLeft ->
            A.style "top" "5px"

        TopRight ->
            A.style "top" "5px"

        BottomRight ->
            A.style "bottom" "5px"

        BottomLeft ->
            A.style "bottom" "5px"


lamderaPane m =
    Html.div
        [ A.style "font-family" "sans-serif"
        , A.style "font-size" "12px"
        , A.style "position" "absolute"
        , xForLocation m.devbar.location
        , yForLocation m.devbar.location
        , A.style "z-index" "100"
        , A.style "color" "#fff"
        , A.style "background-color" "#61b6cd"
        , A.style "background-color" "#2e3335"
        , Html.Events.onMouseEnter DevbarExpand
        , Html.Events.onMouseLeave DevbarCollapse
        ]
        [ Html.div
            [ A.style "padding" "5px"
            ]
            [ Html.img [ Html.Events.onClick ClickedLocation, A.src "/favicon.ico", A.style "width" "20px", A.align "top" ] []
            , Html.text " "
            , Html.span [ Html.Events.onClick ToggledSnapshotFE ]
                [ if m.devbar.snapshotFE then
                    Html.text "✅"

                  else
                    Html.text "❌"
                ]
            , Html.span [ Html.Events.onClick ToggledSnapshotBE ]
                [ if m.devbar.snapshotBE then
                    Html.text "✅"

                  else
                    Html.text "❌"
                ]
            ]
        , if m.devbar.expanded then
            Html.div [ A.style "padding" "5px" ]
                [ Html.text "💾 Snapshots"
                , Html.div
                    []
                    [ Html.text "FE "
                    , Html.span [ Html.Events.onClick ToggledSnapshotFE ]
                        [ if m.devbar.snapshotFE then
                            Html.text "✅"

                          else
                            Html.text "❌"
                        ]
                    , if m.devbar.snapshotFE then
                        Html.span [ Html.Events.onClick ResetDebugStoreFE ] [ Html.text "🔄" ]

                      else
                        Html.text ""
                    ]
                , Html.div
                    []
                    [ Html.text "BE "
                    , Html.span [ Html.Events.onClick ToggledSnapshotBE ]
                        [ if m.devbar.snapshotBE then
                            Html.text "✅"

                          else
                            Html.text "❌"
                        ]
                    , if m.devbar.snapshotBE then
                        Html.span [ Html.Events.onClick ResetDebugStoreBE ] [ Html.text "🔄" ]

                      else
                        Html.text ""
                    ]
                ]

          else
            Html.text ""
        ]


mapDocument model msg { title, body } =
    { title = title, body = [ lamderaPane model ] ++ List.map (Html.map msg) body }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = \model -> mapDocument model FEMsg (userFrontendApp.view model.fem)
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \url -> FEMsg (userFrontendApp.onUrlRequest url)
        , onUrlChange = \ureq -> FEMsg (userFrontendApp.onUrlChange ureq)
        }
