module LocalDev exposing (main)

import Backend
import Browser
import Frontend
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Lamdera exposing (ClientId, Key, Url)
import Lamdera.Debug
import Process
import Task
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg, ToBackend, ToFrontend)


type Msg
    = FEMsg Types.FrontendMsg
    | BEMsg Types.BackendMsg
    | BEtoFE ClientId Types.ToFrontend
    | BEtoFEDelayed ClientId Types.ToFrontend
    | FEtoBE Types.ToBackend
    | FEtoBEDelayed Types.ToBackend
    | FENewUrl Url
    | DevbarExpand
    | DevbarCollapse
    | ResetDebugStoreBoth
    | ResetDebugStoreFE
    | ResetDebugStoreBE
    | ToggledDevMode
    | ToggledNetworkDelay
    | ClickedLocation
    | Noop


type alias Model =
    { fem : FrontendModel
    , bem : BackendModel
    , originalUrl : Url
    , originalKey : Key
    , devbar :
        { expanded : Bool
        , location : Location
        , devMode : DevMode
        , networkDelay : Bool
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


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        devbar =
            case Lamdera.Debug.debugR "d" devbarInit of
                Nothing ->
                    devbarInit

                Just restoredDevbar ->
                    { restoredDevbar | expanded = False }

        ( ifem, iFeCmds ) =
            userFrontendApp.init url key

        ( ibem, iBeCmds ) =
            userBackendApp.init

        ( fem, newFeCmds ) =
            case Lamdera.Debug.debugR "fe" ifem of
                Nothing ->
                    ( ifem, iFeCmds )

                Just rfem ->
                    if devbar.devMode == Freeze then
                        ( rfem, Cmd.none )

                    else
                        ( ifem, iFeCmds )

        ( bem, newBeCmds ) =
            case Lamdera.Debug.debugR "be" ibem of
                Nothing ->
                    ( ibem, iBeCmds )

                Just rbem ->
                    ( rbem, Cmd.none )

        devbarInit =
            { expanded = False
            , location = BottomLeft
            , devMode = Normal
            , networkDelay = False
            }
    in
    ( { fem = fem
      , bem = bem
      , originalKey = key
      , originalUrl = url
      , devbar = devbar
      }
    , Cmd.batch
        [ Cmd.map FEMsg newFeCmds
        , Cmd.map BEMsg newBeCmds
        ]
    )


storeFE m newFem =
    if m.devbar.devMode == Freeze then
        Lamdera.Debug.debugS "fe" newFem

    else
        newFem


storeBE m newBem =
    Lamdera.Debug.debugS "be" newBem


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
            if m.devbar.networkDelay then
                ( m, delay 500 (BEtoFEDelayed clientId toFrontend) )

            else
                let
                    x =
                        Debug.log "BEtoFE" toFrontend

                    ( newFem, newFeCmds ) =
                        userFrontendApp.updateFromBackend toFrontend m.fem
                in
                ( { m | fem = storeFE m newFem, bem = m.bem }, Cmd.map FEMsg newFeCmds )

        BEtoFEDelayed clientId toFrontend ->
            let
                x =
                    Debug.log "[delayed] BEtoFE" toFrontend

                ( newFem, newFeCmds ) =
                    userFrontendApp.updateFromBackend toFrontend m.fem
            in
            ( { m | fem = storeFE m newFem, bem = m.bem }, Cmd.map FEMsg newFeCmds )

        FEtoBE toBackend ->
            if m.devbar.networkDelay then
                ( m, delay 500 (FEtoBEDelayed toBackend) )

            else
                let
                    _ =
                        Debug.log "FEtoBE" toBackend

                    ( newBem, newBeCmds ) =
                        userBackendApp.updateFromFrontend "sessionIdLocalDev" "clientIdLocalDev" toBackend m.bem
                in
                ( { m | fem = m.fem, bem = storeBE m newBem }, Cmd.map BEMsg newBeCmds )

        FEtoBEDelayed toBackend ->
            let
                _ =
                    Debug.log "[delayed] FEtoBE" toBackend

                ( newBem, newBeCmds ) =
                    userBackendApp.updateFromFrontend "sessionIdLocalDev" "clientIdLocalDev" toBackend m.bem
            in
            ( { m | fem = m.fem, bem = storeBE m newBem }, Cmd.map BEMsg newBeCmds )

        FENewUrl url ->
            let
                ( newModel, newCmds ) =
                    update (FEMsg (userFrontendApp.onUrlChange url)) m
            in
            ( { newModel | originalUrl = url }, newCmds )

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
                | fem = Lamdera.Debug.debugS "fe" newFem
                , bem = Lamdera.Debug.debugS "be" newBem
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
                | fem = Lamdera.Debug.debugS "fe" newFem
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
                | bem = Lamdera.Debug.debugS "be" newBem
              }
            , Cmd.batch
                [ Cmd.map BEMsg newBeCmds
                , Lamdera.Debug.browserReload
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
                        Lamdera.Debug.debugS "fe" m.fem

                    else
                        m.fem
            in
            ( { m | devbar = Lamdera.Debug.debugS "d" newDevbar, fem = newFem }
            , Cmd.none
            )

        ToggledNetworkDelay ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | networkDelay = not m.devbar.networkDelay }
            in
            ( { m | devbar = Lamdera.Debug.debugS "d" newDevbar }
            , Cmd.none
            )

        ClickedLocation ->
            let
                devbar =
                    m.devbar

                newDevbar =
                    { devbar | location = next m.devbar.location }
            in
            ( { m | devbar = Lamdera.Debug.debugS "d" newDevbar }
            , Cmd.none
            )

        Noop ->
            ( m, Cmd.none )


subscriptions { fem, bem } =
    Sub.batch
        [ Sub.map FEMsg (userFrontendApp.subscriptions fem)
        , Sub.map BEMsg (userBackendApp.subscriptions bem)
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
    div
        [ style "padding" "5px"
        ]
        [ node "style" [] [ text customCss ]
        , Html.span
            [ onClick ClickedLocation
            , class "lamderaLogoWhite"
            ]
            []
        , text " "
        , freezeToggle m
        ]


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
    { title = title, body = [ lamderaPane model ] ++ List.map (Html.map msg) body }


main : Program () Model Msg
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
