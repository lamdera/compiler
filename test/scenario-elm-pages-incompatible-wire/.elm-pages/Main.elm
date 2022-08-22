port module Main exposing (..)

import Api
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Dict
import Effect exposing (Effect)
import ErrorPage exposing (ErrorPage)
import HtmlPrinter
import Lamdera.Wire3
import Pages.FormState
import Pages.Internal.String
import Pages.Internal.Platform.ToJsPayload
import Pages.Internal.ResponseSketch exposing (ResponseSketch)
import Pages.Msg
import Server.Response
import ApiRoute
import Browser.Navigation
import Route exposing (Route)
import Http
import Json.Decode
import Json.Encode
import Pages.Flags
import Pages.Fetcher
import Pages.Internal.Platform
import Shared
import Site
import Head
import Html exposing (Html)
import Pages.Internal.NotFoundReason
import Pages.Transition
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Pages.Internal.RoutePattern
import Pages.ProgramConfig
import Url
import DataSource exposing (DataSource)
import QueryParams
import Task exposing (Task)
import Url exposing (Url)
import View

import Route.Index


type alias Model =
    { global : Shared.Model
    , page : PageModel
    , current :
        Maybe
            { path :
                { path : Path
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : Maybe Route
            , pageUrl : Maybe PageUrl
            }
    }


type PageModel
    = ModelIndex Route.Index.Model

    | ModelErrorPage____ ErrorPage.Model
    | NotFound




type Msg
    = MsgGlobal Shared.Msg
    | OnPageChange
        { protocol : Url.Protocol
        , host : String
        , port_ : Maybe Int
        , path : Path
        , query : Maybe String
        , fragment : Maybe String
        , metadata : Maybe Route
        }
    | MsgErrorPage____ ErrorPage.Msg
    | MsgIndex Route.Index.Msg



type PageData
    = Data404NotFoundPage____
    | DataErrorPage____ ErrorPage
    | DataIndex Route.Index.Data



type ActionData
    = 
    ActionDataIndex Route.Index.ActionData



view :
    Pages.FormState.PageFormState
    -> List Pages.Transition.FetcherState
    -> Maybe Pages.Transition.Transition
    -> { path : Path
    , route : Maybe Route
    }
    -> Maybe PageUrl
    -> Shared.Data
    -> PageData
    -> Maybe ActionData
    ->
        { view : Model -> { title : String, body : Html (Pages.Msg.Msg Msg) }
        , head : List Head.Tag
        }
view pageFormState fetchers transition page maybePageUrl globalData pageData actionData =
    case ( page.route, pageData ) of
        ( _, DataErrorPage____ data ) ->
            { view =
                \model ->
                    case model.page of
                        ModelErrorPage____ subModel ->
                            ErrorPage.view data subModel
                                --maybePageUrl
                                --model.global
                                ----subModel
                                --{ data = data
                                --, sharedData = globalData
                                --, routeParams = {}
                                --, path = page.path
                                --}
                                |> View.map (MsgErrorPage____ >> Pages.Msg.UserMsg)
                                |> Shared.template.view globalData page model.global (MsgGlobal >> Pages.Msg.UserMsg)

                        _ ->
                            { title = "Model mismatch", body = Html.text <| "Model mismatch" }
            , head = []
            }



        ( Just Route.Index, DataIndex data ) ->
                  let
                      actionDataOrNothing =
                          case actionData of
                              Just (ActionDataIndex justActionData) -> Just justActionData
                              _ -> Nothing
                  in
                  { view =
                      \model ->
                          case model.page of
                              ModelIndex subModel ->
                                  Route.Index.route.view
                                      maybePageUrl
                                      model.global
                                      subModel
                                      { data = data
                                      , sharedData = globalData
                                      , routeParams = {}
                                      , action = actionDataOrNothing
                                      , path = page.path
                                      , submit = Pages.Fetcher.submit Route.Index.w3_decode_ActionData
                                      , transition = transition
                                      , fetchers = fetchers
                                      , pageFormState = pageFormState
                                      }
                                      |> View.map (Pages.Msg.map MsgIndex)
                                      |> Shared.template.view globalData page model.global (MsgGlobal >> Pages.Msg.UserMsg)

                              _ ->
                                  { title = "Model mismatch", body = Html.text <| "Model mismatch" }
                  , head = []
                  }

        _ ->
            { head = []
            , view =
                \_ ->
                    { title = "Page not found"
                    , body =
                            Html.div [] 
                            [ Html.text "This page could not be found."
                            ]
                    }

            }



init :
    Maybe Shared.Model
    -> Pages.Flags.Flags
    -> Shared.Data
    -> PageData
    -> Maybe ActionData
    -> Maybe Browser.Navigation.Key
    ->
        Maybe
            { path :
                { path : Path
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : Maybe Route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Effect Msg )
init currentGlobalModel userFlags sharedData pageData actionData navigationKey maybePagePath =
    let
        ( sharedModel, globalCmd ) =
            currentGlobalModel |> Maybe.map (\m -> ( m, Effect.none )) |> Maybe.withDefault (Shared.template.init userFlags maybePagePath)

        ( templateModel, templateCmd ) =
            case ( ( Maybe.map2 Tuple.pair (maybePagePath |> Maybe.andThen .metadata) (maybePagePath |> Maybe.map .path) ), pageData ) of
                ( Just ( Route.Index, justPath ), DataIndex thisPageData ) ->
                    let
                        actionDataOrNothing =
                            case actionData of
                                Just (ActionDataIndex justActionData) -> Just justActionData
                                _ -> Nothing
                    in
                    Route.Index.route.init
                        (Maybe.andThen .pageUrl maybePagePath)
                        sharedModel
                        { data = thisPageData
                        , sharedData = sharedData
                        , action = actionDataOrNothing
                        , routeParams = {}
                        , path = justPath.path
                        , submit = Pages.Fetcher.submit Route.Index.w3_decode_ActionData
                        , transition = Nothing -- TODO is this safe, will this always be Nothing?
                        , fetchers = []
                        , pageFormState = Dict.empty
                        }
                        |> Tuple.mapBoth ModelIndex (Effect.map MsgIndex)

                _ ->
                    (case pageData of
                        DataErrorPage____ errorPage ->
                            errorPage

                        _ ->
                            ErrorPage.notFound
                    )
                        |> ErrorPage.init
                        |> Tuple.mapBoth ModelErrorPage____ (Effect.map MsgErrorPage____)
    in
    ( { global = sharedModel
      , page = templateModel
      , current = maybePagePath
      }
    , Effect.batch
        [ templateCmd
        , globalCmd |> Effect.map MsgGlobal
        ]
    )



update : Pages.FormState.PageFormState  -> List Pages.Transition.FetcherState -> Maybe Pages.Transition.Transition -> Shared.Data -> PageData -> Maybe Browser.Navigation.Key -> Msg -> Model -> ( Model, Effect Msg )
update pageFormState fetchers transition sharedData pageData navigationKey msg model =
    case msg of
        MsgErrorPage____ msg_ ->
            let
                ( updatedPageModel, pageCmd ) =
                    case ( model.page, pageData ) of
                        ( ModelErrorPage____ pageModel, DataErrorPage____ thisPageData ) ->
                            ErrorPage.update
                                -- TODO pass in url or no?
                                --{ data = thisPageData
                                --, sharedData = sharedData
                                --, routeParams = {}
                                --, path = justPage.path
                                --}
                                thisPageData
                                msg_
                                pageModel
                                --model.global -- TODO pass in Shared.Model
                                |> Tuple.mapBoth ModelErrorPage____ (Effect.map MsgErrorPage____)

                        _ ->
                            ( model.page, Effect.none )
            in
            ( { model | page = updatedPageModel }
            , pageCmd
            )


        MsgGlobal msg_ ->
            let
                ( sharedModel, globalCmd ) =
                    Shared.template.update msg_ model.global
            in
            ( { model | global = sharedModel }
            , globalCmd |> Effect.map MsgGlobal
            )

        OnPageChange record ->
            (init (Just model.global) Pages.Flags.PreRenderFlags sharedData pageData Nothing navigationKey <|
                Just
                    { path =
                        { path = record.path
                        , query = record.query
                        , fragment = record.fragment
                        }
                    , metadata = record.metadata
                    , pageUrl =
                        Just
                            { protocol = record.protocol
                            , host = record.host
                            , port_ = record.port_
                            , path = record.path
                            , query = record.query |> Maybe.map QueryParams.fromString
                            , fragment = record.fragment
                            }
                    }
            )
                |> (\( updatedModel, cmd ) ->
                        case Shared.template.onPageChange of
                            Nothing ->
                                ( updatedModel, cmd )

                            Just thingy ->
                                let
                                    ( updatedGlobalModel, globalCmd ) =
                                        Shared.template.update
                                            (thingy
                                                { path = record.path
                                                , query = record.query
                                                , fragment = record.fragment
                                                }
                                            )
                                            model.global
                                in
                                ( { updatedModel
                                    | global = updatedGlobalModel
                                  }
                                , Effect.batch [ cmd, Effect.map MsgGlobal globalCmd ]
                                )
                   )


        
        MsgIndex msg_ ->
            let
                ( updatedPageModel, pageCmd, ( newGlobalModel, newGlobalCmd ) ) =
                    case ( model.page, pageData, Maybe.map3 (\a b c -> ( a, b, c )) (model.current |> Maybe.andThen .metadata) (model.current |> Maybe.andThen .pageUrl) (model.current |> Maybe.map .path) ) of
                        ( ModelIndex pageModel, DataIndex thisPageData, Just ( Route.Index, pageUrl, justPage ) ) ->
                            Route.Index.route.update
                                pageUrl
                                { data = thisPageData
                                , sharedData = sharedData
                                , action = Nothing
                                , routeParams = {}
                                , path = justPage.path
                                , submit = Pages.Fetcher.submit Route.Index.w3_decode_ActionData
                                , transition = transition
                                , fetchers = fetchers
                                , pageFormState = pageFormState
                                }
                                msg_
                                pageModel
                                model.global
                                |> mapBoth ModelIndex (Effect.map MsgIndex)
                                |> (\( a, b, c ) ->
                                        case c of
                                            Just sharedMsg ->
                                                ( a, b, Shared.template.update sharedMsg model.global )

                                            Nothing ->
                                                ( a, b, ( model.global, Effect.none ) )
                                   )

                        _ ->
                            ( model.page, Effect.none, ( model.global, Effect.none ) )
            in
            ( { model | page = updatedPageModel, global = newGlobalModel }
            , Effect.batch [ pageCmd, newGlobalCmd |> Effect.map MsgGlobal ]
            )



templateSubscriptions : Maybe Route -> Path -> Model -> Sub Msg
templateSubscriptions route path model =
    case ( model.page, route ) of
        
        ( ModelIndex templateModel, Just Route.Index ) ->
            Route.Index.route.subscriptions
                Nothing -- TODO wire through value
                {}
                path
                templateModel
                model.global
                |> Sub.map MsgIndex



        _ ->
            Sub.none


main : Pages.Internal.Platform.Program Model Msg PageData ActionData Shared.Data ErrorPage
main =
    Pages.Internal.Platform.application config

config : Pages.ProgramConfig.ProgramConfig Msg Model (Maybe Route) PageData ActionData Shared.Data (Effect Msg) mappedMsg ErrorPage
config =
        { init = init Nothing
        , urlToRoute = Route.urlToRoute
        , routeToPath = \route -> route |> Maybe.map Route.routeToPath |> Maybe.withDefault []
        , site = Nothing
        , globalHeadTags = Nothing
        , getStaticRoutes = DataSource.succeed []
        , handleRoute = handleRoute
        , view = view
        , update = update
        , subscriptions =
            \route path model ->
                Sub.batch
                    [ Shared.template.subscriptions path model.global |> Sub.map MsgGlobal
                    , templateSubscriptions route path model
                    ]
        , onPageChange = OnPageChange
        , toJsPort = toJsPort
        , fromJsPort = fromJsPort identity
        , gotBatchSub = Sub.none
        , data = dataForRoute
        , action = action
        , onActionData = onActionData
        , sharedData = Shared.template.data
        , apiRoutes = \_ -> []
        , pathPatterns = routePatterns3
        , basePath = [  ]
        , sendPageData = sendPageData
        , byteEncodePageData = byteEncodePageData
        , byteDecodePageData = byteDecodePageData
        , hotReloadData = hotReloadData identity
        , encodeResponse = encodeResponse
        , decodeResponse = decodeResponse
        , encodeAction = encodeActionData
        , cmdToEffect = Effect.fromCmd
        , perform = Effect.perform
        , errorStatusCode = ErrorPage.statusCode
        , notFoundPage = ErrorPage.notFound
        , internalError = ErrorPage.internalError
        , errorPageToData = DataErrorPage____
        , notFoundRoute = Nothing
        }

onActionData actionData =
    case actionData of
        ActionDataIndex thisActionData ->
            Route.Index.route.onAction |> Maybe.map (\onAction -> onAction thisActionData) |> Maybe.map MsgIndex





globalHeadTags : DataSource (List Head.Tag)
globalHeadTags =
    (Site.config.head
        :: (Api.routes getStaticRoutes HtmlPrinter.htmlToString
                |> List.filterMap ApiRoute.getGlobalHeadTagsDataSource
           )
    )
        |> DataSource.combine
        |> DataSource.map List.concat


encodeResponse : ResponseSketch PageData ActionData Shared.Data -> Bytes.Encode.Encoder
encodeResponse =
    Pages.Internal.ResponseSketch.w3_encode_ResponseSketch w3_encode_PageData w3_encode_ActionData Shared.w3_encode_Data


decodeResponse : Bytes.Decode.Decoder (ResponseSketch PageData ActionData Shared.Data)
decodeResponse =
    Pages.Internal.ResponseSketch.w3_decode_ResponseSketch w3_decode_PageData w3_decode_ActionData Shared.w3_decode_Data


port hotReloadData : (Bytes -> msg) -> Sub msg


byteEncodePageData : PageData -> Bytes.Encode.Encoder
byteEncodePageData pageData =
    case pageData of
        DataErrorPage____ thisPageData ->
            ErrorPage.w3_encode_ErrorPage thisPageData


        Data404NotFoundPage____ ->
            Bytes.Encode.unsignedInt8 0

        DataIndex thisPageData ->
            Route.Index.w3_encode_Data thisPageData


encodeActionData : ActionData -> Bytes.Encode.Encoder
encodeActionData actionData =
    case actionData of
        ActionDataIndex thisActionData ->
            Route.Index.w3_encode_ActionData thisActionData



port sendPageData : Pages.Internal.Platform.ToJsPayload.NewThingForPort -> Cmd msg


byteDecodePageData : Maybe Route -> Bytes.Decode.Decoder PageData
byteDecodePageData route =
    case route of
        Nothing -> Bytes.Decode.fail
        (Just Route.Index) ->
            Route.Index.w3_decode_Data |> Bytes.Decode.map DataIndex





dataForRoute : Maybe Route -> DataSource (Server.Response.Response PageData ErrorPage)
dataForRoute route =
    case route of
        Nothing ->
            DataSource.succeed (Server.Response.render Data404NotFoundPage____ |> Server.Response.withStatusCode 404 |> Server.Response.mapError never )

        Just Route.Index ->
            Route.Index.route.data {} 
                 |> DataSource.map (Server.Response.map DataIndex)
              

action : Maybe Route -> DataSource (Server.Response.Response ActionData ErrorPage)
action route =
    case route of
        Nothing ->
            DataSource.succeed ( Server.Response.plainText "TODO" )

        Just Route.Index ->
            Route.Index.route.action {} 
                 |> DataSource.map (Server.Response.map ActionDataIndex)
              



handleRoute : Maybe Route -> DataSource (Maybe Pages.Internal.NotFoundReason.NotFoundReason)
handleRoute maybeRoute =
    case maybeRoute of
        Nothing ->
            DataSource.succeed Nothing

        Just (Route.Index) ->
            Route.Index.route.handleRoute { moduleName = [ "Index" ], routePattern = { segments = [  ], ending = Nothing } } (\param -> [  ]) {}


stringToString : String -> String
stringToString string =
    "\"" ++ string ++ "\""


nonEmptyToString : ( String, List String ) -> String
nonEmptyToString ( first, rest ) =
    "( "
        ++ stringToString first
        ++ ", [ "
        ++ (rest
                |> List.map stringToString
                |> String.join ", "
           )
        ++ " ] )"


listToString : List String -> String
listToString strings =
    "[ "
        ++ (strings
                |> List.map stringToString
                |> String.join ", "
           )
        ++ " ]"


maybeToString : Maybe String -> String
maybeToString maybeString =
    case maybeString of
        Just string ->
            "Just " ++ stringToString string

        Nothing ->
            "Nothing"




routePatterns : ApiRoute.ApiRoute ApiRoute.Response
routePatterns =
    ApiRoute.succeed
        (Json.Encode.list
            (\{ kind, pathPattern } ->
                Json.Encode.object
                    [ ( "kind", Json.Encode.string kind )
                    , ( "pathPattern", Json.Encode.string pathPattern )
                    ]
            )
            [ { kind = Route.Index.route.kind, pathPattern = "/" }
          
            ]
            |> (\json -> DataSource.succeed ( Json.Encode.encode 0 json ))
        )
        |> ApiRoute.literal "route-patterns.json"
        |> ApiRoute.single

apiPatterns : ApiRoute.ApiRoute ApiRoute.Response
apiPatterns =
    let
        apiPatternsString =
            Api.routes getStaticRoutes (\_ -> "")
                |> List.map ApiRoute.toJson

    in
    ApiRoute.succeed
        (Json.Encode.list identity apiPatternsString
            |> (\json -> DataSource.succeed ( Json.Encode.encode 0 json ))
        )
        |> ApiRoute.literal "api-patterns.json"
        |> ApiRoute.single


routePatterns2 : List String
routePatterns2 =
    [ "/"
    ]


routePatterns3 : List Pages.Internal.RoutePattern.RoutePattern
routePatterns3 =
    [ { segments = [  ], ending = Nothing }
    ]

getStaticRoutes : DataSource (List Route)
getStaticRoutes =
    DataSource.combine
        [ Route.Index.route.staticRoutes |> DataSource.map (List.map (\_ -> Route.Index))
        ]
        |> DataSource.map List.concat


pathsToGenerateHandler : ApiRoute.ApiRoute ApiRoute.Response
pathsToGenerateHandler =
    ApiRoute.succeed
        (DataSource.map2
            (\pageRoutes apiRoutes ->
                (pageRoutes ++ (apiRoutes |> List.map (\api -> "/" ++ api)))
                    |> Json.Encode.list Json.Encode.string
                    |> Json.Encode.encode 0
            )
            (DataSource.map
                (List.map
                    (\route ->
                        route
                            |> Route.toPath
                            |> Path.toAbsolute
                    )
                )
                getStaticRoutes
            )
            ((routePatterns :: apiPatterns :: Api.routes getStaticRoutes (\_ -> ""))
                |> List.map ApiRoute.getBuildTimeRoutes
                |> DataSource.combine
                |> DataSource.map List.concat
            )
        )
        |> ApiRoute.literal "all-paths.json"
        |> ApiRoute.single


port toJsPort : Json.Encode.Value -> Cmd msg

port fromJsPort : (Json.Decode.Value -> msg) -> Sub msg

port gotBatchSub : (Json.Decode.Value -> msg) -> Sub msg


mapBoth : (a -> b) -> (c -> d) -> ( a, c, e ) -> ( b, d, e )
mapBoth fnA fnB ( a, b, c ) =
    ( fnA a, fnB b, c )

encodeBytes : (b -> Bytes.Encode.Encoder) -> b -> Bytes
encodeBytes bytesEncoder items =
    Bytes.Encode.encode (bytesEncoder items)


decodeBytes : Bytes.Decode.Decoder a -> Bytes -> Result String a
decodeBytes bytesDecoder items =
    Bytes.Decode.decode bytesDecoder items
    -- Lamdera.Wire3.bytesDecodeStrict bytesDecoder items
        |> Result.fromMaybe "Decoding error"
