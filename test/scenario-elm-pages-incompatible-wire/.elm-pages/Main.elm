port module Main exposing (..)

import Api
import ApiRoute
import Browser.Navigation
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import DataSource exposing (DataSource)
import Head
import Html exposing (Html)
import HtmlPrinter
import Http
import Json.Decode
import Json.Encode
import Lamdera.Wire3
import Pages.Flags
import Pages.Internal.NotFoundReason
import Pages.Internal.Platform
import Pages.Internal.Platform.ToJsPayload
import Pages.Internal.ResponseSketch exposing (ResponseSketch)
import Pages.Internal.RoutePattern
import Pages.Internal.String
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import QueryParams
import Route exposing (Route)
import Route.Index
import Server.Response
import Shared
import Site
import Task exposing (Task)
import Url exposing (Url)
import View


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
    | MsgIndex Route.Index.Msg


type PageData
    = Data404NotFoundPage____
    | DataIndex Route.Index.Data


view :
    { path : Path
    , route : Maybe Route
    }
    -> Maybe PageUrl
    -> Shared.Data
    -> PageData
    ->
        { view : Model -> { title : String, body : Html Msg }
        , head : List Head.Tag
        }
view page maybePageUrl globalData pageData =
    case ( page.route, pageData ) of
        ( Just Route.Index, DataIndex data ) ->
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
                                , path = page.path
                                }
                                |> View.map MsgIndex
                                |> Shared.template.view globalData page model.global MsgGlobal

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
    -> ( Model, Cmd Msg )
init currentGlobalModel userFlags sharedData pageData navigationKey maybePagePath =
    let
        ( sharedModel, globalCmd ) =
            currentGlobalModel |> Maybe.map (\m -> ( m, Cmd.none )) |> Maybe.withDefault (Shared.template.init navigationKey userFlags maybePagePath)

        ( templateModel, templateCmd ) =
            case ( Maybe.map2 Tuple.pair (maybePagePath |> Maybe.andThen .metadata) (maybePagePath |> Maybe.map .path), pageData ) of
                ( Just ( Route.Index, justPath ), DataIndex thisPageData ) ->
                    Route.Index.route.init
                        (Maybe.andThen .pageUrl maybePagePath)
                        sharedModel
                        { data = thisPageData
                        , sharedData = sharedData
                        , routeParams = {}
                        , path = justPath.path
                        }
                        |> Tuple.mapBoth ModelIndex (Cmd.map MsgIndex)

                _ ->
                    ( NotFound, Cmd.none )
    in
    ( { global = sharedModel
      , page = templateModel
      , current = maybePagePath
      }
    , Cmd.batch
        [ templateCmd
        , globalCmd |> Cmd.map MsgGlobal
        ]
    )


update : Shared.Data -> PageData -> Maybe Browser.Navigation.Key -> Msg -> Model -> ( Model, Cmd Msg )
update sharedData pageData navigationKey msg model =
    case msg of
        MsgGlobal msg_ ->
            let
                ( sharedModel, globalCmd ) =
                    Shared.template.update msg_ model.global
            in
            ( { model | global = sharedModel }
            , globalCmd |> Cmd.map MsgGlobal
            )

        OnPageChange record ->
            (init (Just model.global) Pages.Flags.PreRenderFlags sharedData pageData navigationKey <|
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
                                , Cmd.batch [ cmd, Cmd.map MsgGlobal globalCmd ]
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
                                , routeParams = {}
                                , path = justPage.path
                                }
                                navigationKey
                                msg_
                                pageModel
                                model.global
                                |> mapBoth ModelIndex (Cmd.map MsgIndex)
                                |> (\( a, b, c ) ->
                                        case c of
                                            Just sharedMsg ->
                                                ( a, b, Shared.template.update sharedMsg model.global )

                                            Nothing ->
                                                ( a, b, ( model.global, Cmd.none ) )
                                   )

                        _ ->
                            ( model.page, Cmd.none, ( model.global, Cmd.none ) )
            in
            ( { model | page = updatedPageModel, global = newGlobalModel }
            , Cmd.batch [ pageCmd, newGlobalCmd |> Cmd.map MsgGlobal ]
            )


templateSubscriptions : Maybe Route -> Path -> Model -> Sub Msg
templateSubscriptions route path model =
    case ( model.page, route ) of
        ( ModelIndex templateModel, Just Route.Index ) ->
            Route.Index.route.subscriptions
                Nothing
                -- TODO wire through value
                {}
                path
                templateModel
                model.global
                |> Sub.map MsgIndex

        _ ->
            Sub.none


main : Pages.Internal.Platform.Program Model Msg PageData Shared.Data
main =
    Pages.Internal.Platform.application
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
        , sharedData = Shared.template.data
        , apiRoutes = \_ -> []
        , pathPatterns = routePatterns3
        , basePath = []
        , fetchPageData = fetchPageData
        , sendPageData = sendPageData
        , byteEncodePageData = byteEncodePageData
        , byteDecodePageData = byteDecodePageData
        , hotReloadData = hotReloadData identity
        , encodeResponse = encodeResponse
        , decodeResponse = decodeResponse
        }


globalHeadTags : DataSource (List Head.Tag)
globalHeadTags =
    (Site.config.head
        :: (Api.routes getStaticRoutes HtmlPrinter.htmlToString
                |> List.filterMap ApiRoute.getGlobalHeadTagsDataSource
           )
    )
        |> DataSource.combine
        |> DataSource.map List.concat


encodeResponse : ResponseSketch PageData Shared.Data -> Bytes.Encode.Encoder
encodeResponse =
    Pages.Internal.ResponseSketch.w3_encode_ResponseSketch w3_encode_PageData Shared.w3_encode_Data


decodeResponse : Bytes.Decode.Decoder (ResponseSketch PageData Shared.Data)
decodeResponse =
    Pages.Internal.ResponseSketch.w3_decode_ResponseSketch w3_decode_PageData Shared.w3_decode_Data


port hotReloadData : (Bytes -> msg) -> Sub msg


byteEncodePageData : PageData -> Bytes.Encode.Encoder
byteEncodePageData pageData =
    case pageData of
        Data404NotFoundPage____ ->
            Bytes.Encode.unsignedInt8 0

        DataIndex thisPageData ->
            Route.Index.w3_encode_Data thisPageData


port sendPageData : Pages.Internal.Platform.ToJsPayload.NewThingForPort -> Cmd msg


fetchPageData : Url -> Maybe { contentType : String, body : String } -> Task Http.Error ( Url, ResponseSketch PageData Shared.Data )
fetchPageData url details =
    Http.task
        { method = details |> Maybe.map (\_ -> "POST") |> Maybe.withDefault "GET"
        , headers = []
        , url =
            url.path
                |> Pages.Internal.String.chopForwardSlashes
                |> String.split "/"
                |> List.filter ((/=) "")
                |> (\l -> l ++ [ "content.dat" ])
                |> String.join "/"
                |> String.append "/"
        , body = details |> Maybe.map (\justDetails -> Http.stringBody justDetails.contentType justDetails.body) |> Maybe.withDefault Http.emptyBody
        , resolver =
            Http.bytesResolver
                (\response ->
                    case response of
                        Http.BadUrl_ url_ ->
                            Err (Http.BadUrl url_)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata _ ->
                            Err (Http.BadStatus metadata.statusCode)

                        Http.GoodStatus_ _ body ->
                            body
                                |> decodeBytes decodeResponse
                                |> Result.mapError Http.BadBody
                                |> Result.map (\okResponse -> ( url, okResponse ))
                )
        , timeout = Nothing
        }
        |> Task.andThen
            (\( _, response ) ->
                case response of
                    Pages.Internal.ResponseSketch.Redirect location ->
                        -- TODO what if it redirects to external URL? Need to handle that somehow, or is it an error?
                        fetchPageData { url | path = location } Nothing

                    _ ->
                        Task.succeed ( url, response )
            )


byteDecodePageData : Maybe Route -> Bytes.Decode.Decoder PageData
byteDecodePageData route =
    case route of
        Nothing ->
            Bytes.Decode.fail

        Just Route.Index ->
            Route.Index.w3_decode_Data |> Bytes.Decode.map DataIndex


dataForRoute : Maybe Route -> DataSource (Server.Response.Response PageData)
dataForRoute route =
    case route of
        Nothing ->
            DataSource.succeed (Server.Response.render Data404NotFoundPage____ |> Server.Response.withStatusCode 404)

        Just Route.Index ->
            Route.Index.route.data {}
                |> DataSource.map (Server.Response.map DataIndex)


handleRoute : Maybe Route -> DataSource (Maybe Pages.Internal.NotFoundReason.NotFoundReason)
handleRoute maybeRoute =
    case maybeRoute of
        Nothing ->
            DataSource.succeed Nothing

        Just Route.Index ->
            Route.Index.route.handleRoute { moduleName = [ "Index" ], routePattern = { segments = [], ending = Nothing } } (\param -> []) {}


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
            |> (\json -> DataSource.succeed (Json.Encode.encode 0 json))
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
            |> (\json -> DataSource.succeed (Json.Encode.encode 0 json))
        )
        |> ApiRoute.literal "api-patterns.json"
        |> ApiRoute.single


routePatterns2 : List String
routePatterns2 =
    [ "/"
    ]


routePatterns3 : List Pages.Internal.RoutePattern.RoutePattern
routePatterns3 =
    [ { segments = [], ending = Nothing }
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
    Lamdera.Wire3.bytesDecodeStrict bytesDecoder items
        |> Result.fromMaybe "Decoding error"
