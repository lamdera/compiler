{- MANUALLY FORMATTED -}
module Compiler.Elm.ModuleName exposing
  ( Raw, bRaw
  , toChars
  , toFileNames
  , toHyphenName
  --
  --, encode
  , decoder
  --
  , Canonical(..), toString, bCanonical
  , basics, char, string
  , maybe, result, list, array, dict, tuple
  , platform, cmd, sub
  , debug
  , virtualDom
  , jsonDecode, jsonEncode
  , webgl, texture
  --
  , getPackage, getModule
  , Comparable, bComparable
  , fromComparable, toComparable
  , comparison
  )


import Compiler.Data.Name as Name
import Compiler.Data.Utf8 as Utf8
import Compiler.Elm.Package as Pkg
import Compiler.Json.Decode as D
import Compiler.Parse.Primitives as P
import Compiler.Parse.Variable as Var
import Extra.Data.Binary as B
import Extra.System.Dir exposing (FileName)
import Extra.Type.List exposing (TList)



-- RAW


type alias Raw = Name.Name


toChars : Raw -> String
toChars =
  identity


toFileNames : Raw -> TList FileName
toFileNames name =
  String.split "." name


toHyphenName : Raw -> FileName
toHyphenName name =
  String.map (\c -> if c == '.' then '-' else c) name



-- JSON


decoder : D.Decoder (P.Row, P.Col) Raw
decoder =
  D.customString parser Tuple.pair



-- PARSER


parser : P.Parser (P.Row, P.Col) Raw
parser =
  P.Parser <| \(P.State src pos end indent row col) ->
    let
      (isGood, newPos, newCol) = chompStart src pos end col
    in
    if isGood && newPos - pos < 256 then
      let newState = P.State src newPos end indent row newCol in
      P.Cok (Utf8.fromPtr src pos newPos) newState

    else if col == newCol then
      P.Eerr row newCol Tuple.pair

    else
      P.Cerr row newCol Tuple.pair


chompStart : String -> Int -> Int -> P.Col -> (Bool, Int, P.Col)
chompStart src pos end col =
  let
    width = Var.getUpperWidth src pos end
  in
  if width == 0 then
    (False, pos, col)
  else
    chompInner src (pos + width) end (col + 1)


chompInner : String -> Int -> Int -> P.Col -> (Bool, Int, P.Col)
chompInner src pos end col =
  if pos >= end then
    (True, pos, col)
  else
    let
      word = P.unsafeIndex src pos
      width = Var.getInnerWidthHelp word
    in
    if width == 0 then
      if word == 0x2E {-.-} then
        chompStart src (pos + 1) end (col + 1)
      else
        (True, pos, col)
    else
      chompInner src (pos + width) end (col + 1)



-- CANONICAL


type Canonical =
  Canonical
    {- package -} Pkg.Name
    {- module -} Name.Name

getPackage (Canonical pkg _) = pkg
getModule (Canonical _ name) = name


type alias Comparable =
  ( String, Pkg.Comparable )


toComparable : Canonical -> Comparable
toComparable (Canonical pkg name) =
  ( name, Pkg.toComparable pkg )


fromComparable : Comparable -> Canonical
fromComparable ( name, pkg ) =
  Canonical (Pkg.fromComparable pkg) name


comparison : Canonical -> Canonical -> Order
comparison can1 can2 =
  compare (toComparable can1) (toComparable can2)


toString : Canonical -> String
toString (Canonical pkg name) =
  Pkg.toString pkg ++ "/" ++ name



-- INSTANCES


bRaw : B.Binary Raw
bRaw =
  Name.bName


bCanonical : B.Binary Canonical
bCanonical =
  B.bin2 Canonical (\(Canonical a b) -> B.T2 a b)
    Pkg.bName
    Name.bName


bComparable : B.Binary Comparable
bComparable =
  B.iso toComparable fromComparable bCanonical



-- CORE


basics : Canonical
basics = Canonical Pkg.core Name.basics


char : Canonical
char = Canonical Pkg.core Name.char


string : Canonical
string = Canonical Pkg.core Name.string


maybe : Canonical
maybe = Canonical Pkg.core Name.maybe


result : Canonical
result = Canonical Pkg.core Name.result


list : Canonical
list = Canonical Pkg.core Name.list


array : Canonical
array = Canonical Pkg.core Name.array


dict : Canonical
dict = Canonical Pkg.core Name.dict


tuple : Canonical
tuple = Canonical Pkg.core Name.tuple


platform : Canonical
platform = Canonical Pkg.core Name.platform


cmd : Canonical
cmd = Canonical Pkg.core "Platform.Cmd"


sub : Canonical
sub = Canonical Pkg.core "Platform.Sub"


debug : Canonical
debug = Canonical Pkg.core Name.debug



-- HTML


virtualDom : Canonical
virtualDom = Canonical Pkg.virtualDom Name.virtualDom



-- JSON


jsonDecode : Canonical
jsonDecode = Canonical Pkg.json "Json.Decode"


jsonEncode : Canonical
jsonEncode = Canonical Pkg.json "Json.Encode"



-- WEBGL


webgl : Canonical
webgl = Canonical Pkg.webgl "WebGL"


texture : Canonical
texture = Canonical Pkg.webgl "WebGL.Texture"
