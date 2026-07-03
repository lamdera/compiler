module Lamdera.TimeTravel exposing
    ( BusMsg
    , Effect(..)
    , Frame
    , History
    , Kind(..)
    , Msg(..)
    , bemUpTo
    , dumpAll
    , femSourcesUpTo
    , frameFromBus
    , frameToBus
    , init
    , previewOrders
    , record
    , restoreBemBus
    , restoreFemBus
    , resumeBus
    , setPoppedOut
    , travellingFem
    , truncateAt
    , update
    , view
    )

{-| Full-stack, multi-client time travel debugger for `lamdera live`.

Every tab broadcasts its state mutations (frontend msg, backend msg,
incoming ToBackend/ToFrontend) on a BroadcastChannel via ports, together
with the wire-encoded resulting model. Every tab — including the detached
popup viewer — accumulates all clients' events into one unified timeline,
so any panel can inspect every client's FrontendModel plus the
BackendModel, and a restore can rewind the whole system.

Local frames keep direct references to the models (O(1), no
serialization); remote frames keep the received bytes (for restores) plus
the decoded model (for inspection).

-}

import Array exposing (Array)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Lazy
import Lamdera.Wire3 as Wire
import Set exposing (Set)
import Types exposing (BackendModel, FrontendModel)


type Kind
    = KindInit
    | KindFrontend
    | KindBackend
    | KindToBackend
    | KindToFrontend
    | KindRestored


type alias Frame =
    { source : String -- clientId of the tab that emitted this event
    , session : String -- session provenance for ToBackend frames, else ""
    , kind : Kind
    , label : String -- Debug.toString of the msg, computed at the source
    , fem : Maybe FrontendModel -- frontend model of `source` after this event
    , femBytes : Maybe Wire.Bytes -- kept on remote frames so we can restore other clients
    , bem : Maybe BackendModel -- backend model after this event (leader events only)
    , bemBytes : Maybe Wire.Bytes
    }


type alias History =
    { frames : Array Frame
    , selected : Maybe Int -- Nothing = tracking the live present
    , open : Bool
    , poppedOut : Bool -- a detached popup window owns the panel right now
    , openSections : Set String
    }


init : History
init =
    { frames = Array.empty
    , selected = Nothing
    , open = False
    , poppedOut = False
    , openSections = Set.empty
    }


{-| The popup window took over (True) or was closed (False). While popped
out the inline panel hides; when the popup closes the inline panel comes
back.
-}
setPoppedOut : Bool -> History -> History
setPoppedOut popped history =
    if popped then
        { history | poppedOut = True, selected = Nothing }

    else
        { history | poppedOut = False, open = True }


{-| ponytail: hard cap so long sessions can't eat unbounded memory;
oldest 200 frames dropped in one block when full
-}
maxFrames : Int
maxFrames =
    1000


record : Frame -> History -> History
record frame history =
    if Array.length history.frames < maxFrames then
        { history | frames = Array.push frame history.frames }

    else
        { history
            | frames = Array.push frame (Array.slice 200 maxFrames history.frames)
            , selected = Maybe.map (\i -> Basics.max 0 (i - 200)) history.selected
        }



-- CROSS-TAB BUS


{-| The message exchanged between clients over the websocket-relayed bus.
t: "f" = frame, "rf" = restore a client's frontend model, "rb" = restore
the backend model (applied by the leader), "tv"/"tvr" = scrub previews.
o is the emitting client (the server echoes broadcasts back to the sender,
so receivers drop messages whose origin is themselves).
-}
type alias BusMsg =
    { t : String
    , o : String
    , k : String
    , c : String
    , s : String
    , l : String
    , f : Maybe Wire.Bytes
    , b : Maybe Wire.Bytes
    }


restoreFemBus : String -> Wire.Bytes -> BusMsg
restoreFemBus clientId bytes =
    { t = "rf", o = "", k = "", c = clientId, s = "", l = "", f = Just bytes, b = Nothing }


restoreBemBus : Wire.Bytes -> BusMsg
restoreBemBus bytes =
    { t = "rb", o = "", k = "", c = "", s = "", l = "", f = Nothing, b = Just bytes }


{-| Non-destructive preview: tell a client to DISPLAY this state while a
panel somewhere is scrubbing the timeline ("master tab" mode). Nothing
means the client had no state at that instant (not born yet): it should
display its init state rather than stay frozen on a stale preview.
-}
previewBus : String -> Maybe Wire.Bytes -> BusMsg
previewBus clientId maybeBytes =
    { t = "tv", o = "", k = "", c = clientId, s = "", l = "", f = maybeBytes, b = Nothing }


{-| One preview order per client ever seen in the timeline (except the
scrubbing tab itself), for the state at the given instant.
-}
previewOrders : String -> Int -> Array Frame -> List BusMsg
previewOrders myClientId index frames =
    let
        atInstant =
            femSourcesUpTo index frames |> Dict.fromList
    in
    femSourcesUpTo (Array.length frames - 1) frames
        |> List.map Tuple.first
        |> List.filter ((/=) myClientId)
        |> List.map
            (\clientId ->
                previewBus clientId
                    (Dict.get clientId atInstant
                        |> Maybe.andThen
                            (\f ->
                                case f.femBytes of
                                    Just bytes ->
                                        Just bytes

                                    Nothing ->
                                        f.fem |> Maybe.map (Types.w3_encode_FrontendModel >> Wire.bytesEncode)
                            )
                    )
            )


{-| End of scrubbing: every tab goes back to rendering its live state.
-}
resumeBus : BusMsg
resumeBus =
    { t = "tvr", o = "", k = "", c = "", s = "", l = "", f = Nothing, b = Nothing }


frameToBus : Frame -> BusMsg
frameToBus frame =
    { t = "f"
    , o = ""
    , k = kindToString frame.kind
    , c = frame.source
    , s = frame.session
    , l = frame.label
    , f = frame.fem |> Maybe.map (Types.w3_encode_FrontendModel >> Wire.bytesEncode)
    , b = frame.bem |> Maybe.map (Types.w3_encode_BackendModel >> Wire.bytesEncode)
    }


{-| Serialize a frame for the history dump sent to a freshly opened popup
viewer. Remote frames reuse their received bytes instead of re-encoding,
which also preserves models we couldn't decode ourselves.
-}
dumpBus : Frame -> BusMsg
dumpBus frame =
    { t = "f"
    , o = ""
    , k = kindToString frame.kind
    , c = frame.source
    , s = frame.session
    , l = frame.label
    , f =
        case frame.femBytes of
            Just bytes ->
                Just bytes

            Nothing ->
                frame.fem |> Maybe.map (Types.w3_encode_FrontendModel >> Wire.bytesEncode)
    , b =
        case frame.bemBytes of
            Just bytes ->
                Just bytes

            Nothing ->
                frame.bem |> Maybe.map (Types.w3_encode_BackendModel >> Wire.bytesEncode)
    }


dumpAll : History -> List BusMsg
dumpAll history =
    history.frames |> Array.toList |> List.map dumpBus


frameFromBus : BusMsg -> Frame
frameFromBus bus =
    { source = bus.c
    , session = bus.s
    , kind = kindFromString bus.k
    , label = bus.l
    , fem = bus.f |> Maybe.andThen (Wire.bytesDecode Types.w3_decode_FrontendModel)
    , femBytes = bus.f
    , bem = bus.b |> Maybe.andThen (Wire.bytesDecode Types.w3_decode_BackendModel)
    , bemBytes = bus.b
    }


kindToString : Kind -> String
kindToString kind =
    case kind of
        KindInit ->
            "init"

        KindFrontend ->
            "fe"

        KindBackend ->
            "be"

        KindToBackend ->
            "tb"

        KindToFrontend ->
            "tf"

        KindRestored ->
            "rs"


kindFromString : String -> Kind
kindFromString s =
    case s of
        "init" ->
            KindInit

        "fe" ->
            KindFrontend

        "be" ->
            KindBackend

        "tb" ->
            KindToBackend

        "tf" ->
            KindToFrontend

        _ ->
            KindRestored



-- TIMELINE QUERIES


{-| Latest frame carrying a frontend model per client, up to (and
including) the given index. This is "the state of every client" at that
point in time.
-}
femSourcesUpTo : Int -> Array Frame -> List ( String, Frame )
femSourcesUpTo index frames =
    frames
        |> Array.slice 0 (index + 1)
        |> Array.foldl
            (\f acc ->
                if f.fem /= Nothing || f.femBytes /= Nothing then
                    Dict.insert f.source f acc

                else
                    acc
            )
            Dict.empty
        |> Dict.toList


{-| Latest frame carrying a backend model, up to the given index.
-}
bemUpTo : Int -> Array Frame -> Maybe Frame
bemUpTo index frames =
    frames
        |> Array.slice 0 (index + 1)
        |> Array.foldl
            (\f acc ->
                if f.bem /= Nothing || f.bemBytes /= Nothing then
                    Just f

                else
                    acc
            )
            Nothing


{-| The frontend model this tab should render while viewing the past.
-}
travellingFem : String -> History -> Maybe FrontendModel
travellingFem myClientId history =
    history.selected
        |> Maybe.andThen
            (\i ->
                femSourcesUpTo i history.frames
                    |> List.filter (\( c, _ ) -> c == myClientId)
                    |> List.head
                    |> Maybe.andThen (\( _, f ) -> f.fem)
            )


truncateAt : Int -> History -> History
truncateAt index history =
    { history
        | frames = Array.slice 0 (index + 1) history.frames
        , selected = Nothing
    }



-- UPDATE


type Msg
    = TogglePanel
    | ClosePanel
    | SelectFrame Int
    | Resume
    | RestoreHere
    | ClearHistory
    | ToggleSection String


type Effect
    = NoEffect
    | RequestRestore Int
    | Scrubbed -- selection moved into the past: broadcast previews to every tab
    | ScrubEnded -- back to live: tell every tab to render its live state again


{-| ScrubEnded only when we actually leave the travelling state, so live
tabs aren't spammed with resume orders.
-}
endScrub : History -> Effect
endScrub history =
    if history.selected /= Nothing then
        ScrubEnded

    else
        NoEffect


update : Msg -> History -> ( History, Effect )
update msg history =
    case msg of
        TogglePanel ->
            if history.open && not history.poppedOut then
                ( { history | open = False, selected = Nothing }, endScrub history )

            else
                -- also reclaims the panel inline if a popup owns it
                ( { history | open = True, poppedOut = False }, NoEffect )

        ClosePanel ->
            ( { history | open = False, selected = Nothing }, endScrub history )

        SelectFrame index ->
            let
                h2 =
                    selectFrame index history
            in
            ( h2
            , if h2.selected /= Nothing then
                Scrubbed

              else
                endScrub history
            )

        Resume ->
            ( { history | selected = Nothing }, endScrub history )

        RestoreHere ->
            case history.selected of
                Just i ->
                    -- Live.elm owns the actual restore (it needs client
                    -- identity and leader status), including truncateAt
                    ( history, RequestRestore i )

                Nothing ->
                    ( history, NoEffect )

        ClearHistory ->
            ( { history | frames = Array.empty, selected = Nothing }, endScrub history )

        ToggleSection key ->
            ( { history
                | openSections =
                    if Set.member key history.openSections then
                        Set.remove key history.openSections

                    else
                        Set.insert key history.openSections
              }
            , NoEffect
            )


selectFrame : Int -> History -> History
selectFrame index history =
    let
        lastIndex =
            Array.length history.frames - 1
    in
    if index >= lastIndex then
        -- The newest frame is the live state, so selecting it resumes
        { history | selected = Nothing }

    else
        { history | selected = Just (Basics.max 0 index) }



-- VIEW


type alias ViewConfig =
    { myClientId : String
    , fullScreen : Bool -- the detached popup viewer
    }


view : ViewConfig -> History -> Html Msg
view config history =
    if not history.open || history.poppedOut then
        text ""

    else
        let
            count =
                Array.length history.frames

            currentIndex =
                history.selected |> Maybe.withDefault (count - 1)

            travelling =
                history.selected /= Nothing
        in
        div []
            [ if travelling && not config.fullScreen then
                veil

              else
                text ""
            , div
                [ style "font-family" "system-ui, Helvetica Neue, sans-serif"
                , style "font-size" "12px"
                , style "position" "fixed"
                , style "bottom" "0"
                , style "left" "0"
                , style "right" "0"
                , style "height"
                    (if config.fullScreen then
                        "100vh"

                     else
                        "40vh"
                    )
                , style "z-index" "2147483646"
                , style "display" "flex"
                , style "flex-direction" "column"
                , style "color" white
                , style "background-color" charcoal
                , style "border-top"
                    ("2px solid "
                        ++ (if travelling then
                                yellow

                            else
                                "#393939"
                           )
                    )
                , style "user-select" "none"
                ]
                [ Html.node "style" [] [ text ".lamdera-tt-row:hover { background-color: #3a4042 !important; }" ]
                , viewHeader config travelling currentIndex count
                , viewSlider currentIndex count
                , div
                    [ style "display" "flex"
                    , style "flex" "1"
                    , style "min-height" "0"
                    ]
                    [ viewRows history.frames currentIndex
                    , if count > 0 then
                        viewDetail config history.openSections currentIndex history.frames

                      else
                        div [ style "padding" "20px", style "color" grey ]
                            [ text "No events yet. Interact with the app in any tab." ]
                    ]
                ]
            ]


veil : Html msg
veil =
    div
        [ style "position" "fixed"
        , style "top" "0"
        , style "left" "0"
        , style "right" "0"
        , style "bottom" "0"
        , style "background-color" "rgba(255, 203, 100, 0.06)"
        , style "z-index" "2147483645"
        , style "pointer-events" "none"
        ]
        []


viewHeader : ViewConfig -> Bool -> Int -> Int -> Html Msg
viewHeader config travelling currentIndex count =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "10px"
        , style "padding" "6px 10px"
        , style "background-color"
            (if travelling then
                "#4d420f"

             else
                "#222"
            )
        , style "border-bottom" "1px solid #393939"
        ]
        [ span [ style "font-weight" "bold" ] [ text "⏱ Time Travel" ]
        , span [ style "color" grey ]
            [ text
                (if travelling then
                    "viewing #" ++ String.fromInt currentIndex ++ " of " ++ String.fromInt (count - 1)

                 else
                    String.fromInt count ++ " events (live)"
                )
            ]
        , headerButton "◀" (SelectFrame (currentIndex - 1))
        , headerButton "▶" (SelectFrame (currentIndex + 1))
        , if travelling then
            span [ style "display" "flex", style "gap" "10px" ]
                [ headerButtonColored "Resume ⏵" green Resume
                , headerButtonColored "Restore all here" yellow RestoreHere
                ]

          else
            text ""
        , span [ style "flex" "1" ] []
        , if config.fullScreen then
            text ""

          else
            -- No Elm onClick here: live.js opens the popup window from a
            -- native click listener on this id, keeping the user gesture
            -- synchronous so the popup blocker stays quiet
            span
                [ id "lamdera-tt-popout"
                , style "color" white
                , style "cursor" "pointer"
                , style "padding" "2px 8px"
                , style "border" "1px solid #555"
                , style "border-radius" "3px"
                ]
                [ text "⧉ Pop out" ]
        , headerButton "Clear" ClearHistory
        , if config.fullScreen then
            text ""

          else
            headerButton "✕" ClosePanel
        ]


headerButton : String -> Msg -> Html Msg
headerButton label msg =
    span
        [ onClick msg
        , style "cursor" "pointer"
        , style "padding" "2px 8px"
        , style "border" "1px solid #555"
        , style "border-radius" "3px"
        ]
        [ text label ]


headerButtonColored : String -> String -> Msg -> Html Msg
headerButtonColored label color msg =
    span
        [ onClick msg
        , style "cursor" "pointer"
        , style "padding" "2px 8px"
        , style "border" ("1px solid " ++ color)
        , style "border-radius" "3px"
        , style "color" color
        , style "font-weight" "bold"
        ]
        [ text label ]


viewSlider : Int -> Int -> Html Msg
viewSlider currentIndex count =
    div
        [ style "padding" "4px 10px"
        , style "border-bottom" "1px solid #393939"
        ]
        [ input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max (String.fromInt (Basics.max 0 (count - 1)))
            , value (String.fromInt currentIndex)
            , onInput (String.toInt >> Maybe.withDefault 0 >> SelectFrame)
            , style "width" "100%"
            , style "accent-color" yellow
            , style "cursor" "pointer"
            ]
            []
        ]


viewRows : Array Frame -> Int -> Html Msg
viewRows frames currentIndex =
    div
        [ style "width" "360px"
        , style "min-width" "240px"
        , style "overflow-y" "auto"
        , style "border-right" "1px solid #393939"
        ]
        (frames
            |> Array.toIndexedList
            |> List.reverse
            |> List.map (\( i, frame ) -> Html.Lazy.lazy3 viewRow i (i == currentIndex) frame)
        )


viewRow : Int -> Bool -> Frame -> Html Msg
viewRow index isCurrent frame =
    div
        [ class "lamdera-tt-row"
        , onClick (SelectFrame index)
        , style "display" "flex"
        , style "align-items" "center"
        , style "gap" "6px"
        , style "padding" "3px 8px"
        , style "cursor" "pointer"
        , style "white-space" "nowrap"
        , style "overflow" "hidden"
        , style "background-color"
            (if isCurrent then
                "#41969D44"

             else
                "transparent"
            )
        ]
        [ span
            [ style "color" grey
            , style "font-size" "10px"
            , style "min-width" "34px"
            , style "text-align" "right"
            , style "flex-shrink" "0"
            ]
            [ text ("#" ++ String.fromInt index) ]
        , kindBadge frame.kind
        , clientBadge frame.source
        , span
            [ style "overflow" "hidden"
            , style "text-overflow" "ellipsis"
            ]
            [ text (String.left 120 frame.label) ]
        ]


viewDetail : ViewConfig -> Set String -> Int -> Array Frame -> Html Msg
viewDetail config openSections currentIndex frames =
    let
        currentFrame =
            Array.get currentIndex frames

        femSections =
            femSourcesUpTo currentIndex frames
                |> List.map
                    (\( clientId, frame ) ->
                        let
                            title =
                                "Frontend model · "
                                    ++ shortClient clientId
                                    ++ (if clientId == config.myClientId then
                                            " (this tab)"

                                        else
                                            ""
                                       )
                        in
                        modelSection ("fem:" ++ clientId)
                            title
                            (clientColor clientId)
                            openSections
                            (\() ->
                                case frame.fem of
                                    Just fem ->
                                        Debug.toString fem

                                    Nothing ->
                                        "(cannot decode — was this event sent by a different version of the app?)"
                            )
                    )

        bemSection =
            case bemUpTo currentIndex frames of
                Just frame ->
                    modelSection "bem"
                        "Backend model"
                        blue
                        openSections
                        (\() ->
                            case frame.bem of
                                Just bem ->
                                    Debug.toString bem

                                Nothing ->
                                    "(cannot decode — was this event sent by a different version of the app?)"
                        )

                Nothing ->
                    div [ style "color" grey, style "padding" "4px 0" ]
                        [ text "▸ Backend model — no backend event seen yet" ]
    in
    div
        [ style "flex" "1"
        , style "overflow-y" "auto"
        , style "padding" "8px 12px"
        , style "user-select" "text"
        ]
        (case currentFrame of
            Nothing ->
                []

            Just frame ->
                [ div
                    [ style "display" "flex"
                    , style "align-items" "center"
                    , style "gap" "8px"
                    , style "margin-bottom" "6px"
                    ]
                    [ kindBadge frame.kind
                    , clientBadge frame.source
                    , if frame.session /= "" then
                        span [ style "color" grey, style "font-size" "10px" ]
                            [ text ("from session " ++ String.left 8 frame.session) ]

                      else
                        text ""
                    ]
                , pre preStyles [ text (cap 100000 frame.label) ]
                ]
                    ++ femSections
                    ++ [ bemSection ]
        )


{-| The thunk means we never Debug.toString a model unless its section is open.
-}
modelSection : String -> String -> String -> Set String -> (() -> String) -> Html Msg
modelSection key title color openSections getText =
    let
        isOpen =
            Set.member key openSections
    in
    div []
        [ div
            [ onClick (ToggleSection key)
            , style "cursor" "pointer"
            , style "color" color
            , style "padding" "4px 0"
            , style "user-select" "none"
            ]
            [ text
                ((if isOpen then
                    "▾ "

                  else
                    "▸ "
                 )
                    ++ title
                )
            ]
        , if isOpen then
            pre preStyles [ text (cap 100000 (getText ())) ]

          else
            text ""
        ]


preStyles : List (Attribute msg)
preStyles =
    [ style "background-color" "#222"
    , style "padding" "8px"
    , style "border-radius" "4px"
    , style "white-space" "pre-wrap"
    , style "word-break" "break-all"
    , style "margin" "0 0 6px 0"
    , style "font-family" "monospace"
    , style "font-size" "11px"
    ]


cap : Int -> String -> String
cap n s =
    if String.length s > n then
        String.left n s ++ "\n… (truncated)"

    else
        s


kindBadge : Kind -> Html msg
kindBadge kind =
    let
        ( label, color ) =
            kindMeta kind
    in
    span
        [ style "background-color" color
        , style "color" "#1b1b1b"
        , style "border-radius" "3px"
        , style "padding" "0 4px"
        , style "font-size" "10px"
        , style "font-weight" "bold"
        , style "min-width" "30px"
        , style "text-align" "center"
        , style "flex-shrink" "0"
        ]
        [ text label ]


kindMeta : Kind -> ( String, String )
kindMeta kind =
    case kind of
        KindInit ->
            ( "init", purple )

        KindFrontend ->
            ( "FE", green )

        KindBackend ->
            ( "BE", blue )

        KindToBackend ->
            ( "→BE", red )

        KindToFrontend ->
            ( "→FE", yellow )

        KindRestored ->
            ( "⏪", purple )


clientBadge : String -> Html msg
clientBadge clientId =
    span
        [ style "color" (clientColor clientId)
        , style "border" ("1px solid " ++ clientColor clientId)
        , style "border-radius" "3px"
        , style "padding" "0 3px"
        , style "font-size" "9px"
        , style "flex-shrink" "0"
        ]
        [ text (shortClient clientId) ]


shortClient : String -> String
shortClient clientId =
    String.left 4 clientId


clientColor : String -> String
clientColor clientId =
    let
        hash =
            clientId
                |> String.toList
                |> List.foldl (\c acc -> acc * 31 + Char.toCode c |> modBy 997) 7
    in
    case modBy 6 hash of
        0 ->
            "#7ec9de"

        1 ->
            "#a6f098"

        2 ->
            "#f0c987"

        3 ->
            "#e8a2ad"

        4 ->
            "#c9a2e8"

        _ ->
            "#98e8d5"



-- Colors matching the Live.elm devbar palette


red =
    "#E06C75"


green =
    "#85BC7A"


blue =
    "#4196AD"


yellow =
    "#FFCB64"


purple =
    "#9966CC"


white =
    "#EEE"


grey =
    "#888"


charcoal =
    "#2e3335"
