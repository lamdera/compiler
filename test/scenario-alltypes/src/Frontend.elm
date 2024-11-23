module Frontend exposing (..)

import Array
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Env
import Html
import Html.Attributes as Attr
import Lamdera
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Set
import Test.Wire_Alias_2_Record
import Time
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        _ =
            Env.configBoth

        allTypes : Test.Wire_Alias_2_Record.AllTypes
        allTypes =
            { int = 42
            , float = 3.14
            , bool = True
            , char = 'a'
            , string = "Hello, Elm!"
            , listInt = [ 1, 2, 3, 4, 5 ]
            , setFloat = Set.fromList [ 1.1, 2.2, 3.3 ]
            , arrayString = Array.fromList [ "one", "two", "three" ]
            , dict = Dict.fromList [ ( "key1", [ 1, 2 ] ), ( "key2", [ 3, 4 ] ) ]
            , time = Time.millisToPosix 0
            , order = Basics.LT
            , unit = ()
            , seqDict = SeqDict.fromList [ ( "a", 1 ), ( "b", 2 ) ]
            , seqSet = SeqSet.fromList [ 1, 2, 3 ]
            }

        printout =
            Debug.log "Test.Wire_Alias_2_Record.AllTypes" allTypes
    in
    ( { key = key
      , message = "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding! " ++ Env.configFEOnly
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view model =
    { title = ""
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text model.message ]
            ]
        ]
    }
