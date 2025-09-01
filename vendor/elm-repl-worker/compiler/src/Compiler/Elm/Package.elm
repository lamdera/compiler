{- MANUALLY FORMATTED -}
module Compiler.Elm.Package exposing
  ( Name(..), bName
  , Author
  , Project
  --, Canonical(..)
  , isKernel
  , toChars
  , toUrl
  , toFilePath
  , toJsonString
  --
  , dummyName, kernel, core
  , browser, virtualDom, html
  , json--, http, url
  , webgl--, linearAlgebra
  --
  , suggestions
  , nearbyNames
  --
  , decoder
  , encode
  , keyDecoder
  --
  , parser
  --
  , Comparable, bComparable
  , fromComparable, toComparable
  , toString
  )


import Compiler.Data.Name as Name
import Compiler.Data.Utf8 as Utf8
import Compiler.Json.Decode as D
import Compiler.Json.Encode as E
import Compiler.Json.String as Json
import Compiler.Parse.Primitives as P exposing (Col, Row)
import Compiler.Reporting.Suggest as Suggest
import Extra.Data.Binary as B
import Extra.System.Dir as Dir exposing (FilePath)
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- PACKGE NAMES


type Name =
  Name
    {- author -} Author
    {- project -} Project


type alias Author = Utf8.Utf8
type alias Project = Utf8.Utf8


type alias Comparable = ( String, String )

toComparable : Name -> Comparable
toComparable (Name author project) =
  ( author, project )

fromComparable : Comparable -> Name
fromComparable ( author, project ) =
  Name author project

toString : Name -> String
toString (Name author project) =
  author ++ "/" ++ project



-- HELPERS


isKernel : Name -> Bool
isKernel (Name author _) =
  author == elm || author == elm_explorations


toChars : Name -> String
toChars (Name author project) =
  author ++ "/" ++ project


toUrl : Name -> String
toUrl (Name author project) =
  author ++ "/" ++ project


toFilePath : Name -> FilePath
toFilePath (Name author project) =
  Dir.addName (Dir.fromString author) project


toJsonString : Name -> Json.TString
toJsonString (Name author project) =
  String.join "/" [ author, project ]



-- COMMON PACKAGE NAMES


toName : Author -> String -> Name
toName author project =
  Name author project


dummyName : Name
dummyName =
  toName "author" "project"


kernel : Name
kernel =
  toName elm "kernel"


core : Name
core =
  toName elm "core"


browser : Name
browser =
  toName elm "browser"


virtualDom : Name
virtualDom =
  toName elm "virtual-dom"


html : Name
html =
  toName elm "html"


json : Name
json =
  toName elm "json"


http : Name
http =
  toName elm "http"


url : Name
url =
  toName elm "url"


webgl : Name
webgl =
  toName elm_explorations "webgl"


elm : Author
elm =
  "elm"


elm_explorations : Author
elm_explorations =
  "elm-explorations"



-- PACKAGE SUGGESTIONS


suggestions : Map.Map Name.Name Name
suggestions =
  let
    random = toName elm "random"
    time = toName elm "time"
    file = toName elm "file"
  in
  Map.fromList
    [ ( "Browser", browser )
    , ( "File", file )
    , ( "File.Download", file )
    , ( "File.Select", file )
    , ( "Html", html )
    , ( "Html.Attributes", html )
    , ( "Html.Events", html )
    , ( "Http", http )
    , ( "Json.Decode", json )
    , ( "Json.Encode", json )
    , ( "Random", random )
    , ( "Time", time )
    , ( "Url.Parser", url )
    , ( "Url", url )
    ]



-- NEARBY NAMES


nearbyNames : Name -> TList Name -> TList Name
nearbyNames (Name author1 project1) possibleNames =
  let
    authorDist = authorDistance author1
    projectDist = projectDistance project1

    nameDistance (Name author2 project2) =
      authorDist author2 + projectDist project2
  in
  MList.take 4 <| MList.sortOn nameDistance possibleNames


authorDistance : String -> Author -> Int
authorDistance given possibility =
  if possibility == elm || possibility == elm_explorations
  then 0
  else abs (Suggest.distance given possibility)


projectDistance : String -> Project -> Int
projectDistance given possibility =
  abs (Suggest.distance given possibility)



-- BINARY


bName : B.Binary Name
bName =
  B.bin2 Name (\(Name a b) -> B.T2 a b) Utf8.bUnder256 Utf8.bUnder256


bComparable : B.Binary Comparable
bComparable =
  B.bTuple Utf8.bUnder256 Utf8.bUnder256



-- JSON


decoder : D.Decoder ( Row, Col ) Name
decoder =
  D.customString parser Tuple.pair


encode : Name -> E.Value
encode name =
  E.chars (toChars name)


keyDecoder : (Row -> Col -> x) -> D.KeyDecoder x Comparable
keyDecoder toError =
  let
    keyParser =
      P.specialize (\( r, c ) _ _ -> toError r c) parser |> P.fmap toComparable
  in
  D.KeyDecoder keyParser toError



-- PARSER


parser : P.Parser ( P.Row, P.Col ) Name
parser =
  P.bind (parseName isAlphaOrDigit isAlphaOrDigit) <| \author ->
  P.bind (P.word1 0x2F {- / -} Tuple.pair) <| \_ ->
  P.bind (parseName isLower isLowerOrDigit) <| \project ->
  P.return (Name author project)


parseName : (Int -> Bool) -> (Int -> Bool) -> P.Parser ( P.Row, P.Col ) Utf8.Utf8
parseName isGoodStart isGoodInner =
  P.Parser <| \(P.State src pos end indent row col) ->
    if pos >= end then
      P.Eerr row col Tuple.pair
    else
      let word = P.unsafeIndex src pos in
      if not (isGoodStart word) then
        P.Eerr row col Tuple.pair
      else
        let
          ( isGood, newPos ) = chompName isGoodInner src (pos + 1) end False
          len = newPos - pos
          newCol = col + len
        in
        if isGood && len < 256 then
          let newState = P.State src newPos end indent row newCol in
          P.Cok (Utf8.fromPtr src pos newPos) newState
        else
          P.Cerr row newCol Tuple.pair


isLower : Int -> Bool
isLower word =
  0x61 {-a-} <= word && word <= 0x7A {-z-}

isLowerOrDigit : Int -> Bool
isLowerOrDigit word =
     0x61 {-a-} <= word && word <= 0x7A {-z-}
  || 0x30 {-0-} <= word && word <= 0x39 {-9-}


isAlphaOrDigit : Int -> Bool
isAlphaOrDigit word =
     0x61 {-a-} <= word && word <= 0x7A {-z-}
  || 0x41 {-A-} <= word && word <= 0x5A {-Z-}
  || 0x30 {-0-} <= word && word <= 0x39 {-9-}


chompName : (Int -> Bool) -> String -> Int -> Int -> Bool -> ( Bool, Int )
chompName isGoodChar src pos end prevWasDash =
  if pos >= end then
    ( not prevWasDash, pos )
  else
    let word = P.unsafeIndex src pos in
    if isGoodChar word then
      chompName isGoodChar src (pos + 1) end False
    else if word == 0x2D {---} then
      if prevWasDash then
        ( False, pos )
      else
        chompName isGoodChar src (pos + 1) end True
    else
      ( True, pos )
