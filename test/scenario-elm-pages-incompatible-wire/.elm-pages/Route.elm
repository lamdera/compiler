module Route exposing (baseUrlAsPath, Route(..), link, matchers, routeToPath, toLink, urlToRoute, toPath)

{-|

@docs Route, link, matchers, routeToPath, toLink, urlToRoute, toPath

-}


import Server.Response
import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Path exposing (Path)
import Pages.Internal.Router
import Pattern


{-| -}
type Route
    = Index


{-| -}
urlToRoute : { url | path : String } -> Maybe Route
urlToRoute url =
    url.path
    |> withoutBaseUrl 
    |> Pages.Internal.Router.firstMatch matchers


baseUrl : String
baseUrl =
    "/"


baseUrlAsPath : List String
baseUrlAsPath =
    baseUrl
    |> String.split "/"
    |> List.filter (not << String.isEmpty)


withoutBaseUrl path =
    if (path |> String.startsWith baseUrl) then
      String.dropLeft (String.length baseUrl) path
    else
      path

{-| -}
matchers : List (Pages.Internal.Router.Matcher Route)
matchers =
    [ { pattern = "^\\/$"
      , toRoute = \matches ->
      case matches of
          [  ] ->
              Just Index
          _ ->
              Nothing

  
     }

    ]


{-| -}
routeToPath : Route -> List String
routeToPath route =
    case route of
        Index ->
           List.concat [  ]

{-| -}
toPath : Route -> Path
toPath route =
    (baseUrlAsPath ++ (route |> routeToPath)) |> String.join "/" |> Path.fromString


{-| -}
toString : Route -> String
toString route =
    route |> toPath |> Path.toAbsolute


{-| -}
toLink : (List (Attribute msg) -> tag) -> Route -> tag
toLink toAnchorTag route =
    toAnchorTag
        [ route |> toString |> Attr.href
        , Attr.attribute "elm-pages:prefetch" ""
        ]


{-| -}
link : Route -> List (Attribute msg) -> List (Html msg) -> Html msg
link route attributes children =
    toLink
        (\anchorAttrs ->
            Html.a
                (anchorAttrs ++ attributes)
                children
        )
        route
