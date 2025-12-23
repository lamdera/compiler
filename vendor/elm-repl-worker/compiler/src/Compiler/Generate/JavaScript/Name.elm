{- MANUALLY FORMATTED -}
module Compiler.Generate.JavaScript.Name exposing
  ( Name
  , toBuilder
  , fromIndex
  , fromInt
  , fromLocal
  , fromGlobal
  , fromCycle
  , fromKernel
  , makeF
  , makeA
  , makeLabel
  , makeTemp
  , dollar
  )


import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Extra.Type.List exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Set as Set



-- NAME


type Name =
  Name {- toBuilder -} String

toBuilder : Name -> String
toBuilder (Name name) =
  name



-- CONSTRUCTORS


fromIndex : Index.ZeroBased -> Name
fromIndex index =
  fromInt (Index.toMachine index)


fromInt : Int -> Name
fromInt n =
  Name (Name.toBuilder (intToAscii n))


fromLocal : Name.Name -> Name
fromLocal name =
  if Set.member name reservedNames then
    Name ("_" ++ Name.toBuilder name)
  else
    Name (Name.toBuilder name)


fromGlobal : ModuleName.Canonical -> Name.Name -> Name
fromGlobal home name =
  Name <| homeToBuilder home ++ usd ++ Name.toBuilder name


fromCycle : ModuleName.Canonical -> Name.Name -> Name
fromCycle home name =
  Name <| homeToBuilder home ++ "$cyclic$" ++ Name.toBuilder name


fromKernel : Name.Name -> Name.Name -> Name
fromKernel home name =
  Name ("_" ++ Name.toBuilder home ++ "_" ++ Name.toBuilder name)


homeToBuilder : ModuleName.Canonical -> String
homeToBuilder (ModuleName.Canonical (Pkg.Name author project) home) =
  usd ++
  String.replace "-" "_" author
  ++ usd ++
  String.replace "-" "_" project
  ++ usd ++
  String.replace "." "$" home



-- TEMPORARY NAMES


makeF : Int -> Name
makeF n =
  Name ("F" ++ String.fromInt n)


makeA : Int -> Name
makeA n =
  Name ("A" ++ String.fromInt n)


makeLabel : Name.Name -> Int -> Name
makeLabel name index =
  Name (Name.toBuilder name ++ usd ++ String.fromInt index)


makeTemp : Name.Name -> Name
makeTemp name =
  Name ("$temp$" ++ Name.toBuilder name)


dollar : Name
dollar =
  Name usd


usd : String
usd =
  Name.toBuilder Name.dollar



-- RESERVED NAMES


reservedNames : Set.Set Name.Name
reservedNames =
  Set.union jsReservedWords elmReservedWords


jsReservedWords : Set.Set Name.Name
jsReservedWords =
  Set.fromList
    [ "do", "if", "in"
    , "NaN", "int", "for", "new", "try", "var", "let"
    , "null", "true", "eval", "byte", "char", "goto", "long", "case", "else", "this", "void", "with", "enum"
    , "false", "final", "float", "short", "break", "catch", "throw", "while", "class", "const", "super", "yield"
    , "double", "native", "throws", "delete", "return", "switch", "typeof", "export", "import", "public", "static"
    , "boolean", "default", "finally", "extends", "package", "private"
    , "Infinity", "abstract", "volatile", "function", "continue", "debugger", "function"
    , "undefined", "arguments", "transient", "interface", "protected"
    , "instanceof", "implements"
    , "synchronized"
    ]


elmReservedWords : Set.Set Name.Name
elmReservedWords =
  Set.fromList
    [ "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9"
    , "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"
    ]



-- INT TO ASCII


numStartBytes : Int
numStartBytes =
  54


numInnerBytes : Int
numInnerBytes =
  64


intToAscii : Int -> Name.Name
intToAscii n =
  if n < 53 then -- skip $ as a standalone name
    Name.fromWords [toByte n]

  else
    intToAsciiHelp 2 (numStartBytes * numInnerBytes) allBadFields (n - 53)


intToAsciiHelp : Int -> Int -> TList BadFields -> Int -> Name.Name
intToAsciiHelp width blockSize badFields n =
  case badFields of
    [] ->
      if n < blockSize then
        unsafeIntToAscii width [] n
      else
        intToAsciiHelp (width + 1) (blockSize * numInnerBytes) [] (n - blockSize)

    BadFields renamings :: biggerBadFields ->
      let availableSize = blockSize - Map.size renamings in
      if n < availableSize then
        let name = unsafeIntToAscii width [] n in
        Map.findWithDefault name name renamings
      else
        intToAsciiHelp (width + 1) (blockSize * numInnerBytes) biggerBadFields (n - availableSize)



-- UNSAFE INT TO ASCII


unsafeIntToAscii : Int -> TList Int -> Int -> Name.Name
unsafeIntToAscii width bytes n =
  if width <= 1 then
    Name.fromWords (toByte n :: bytes)
  else
    let
      quotient =
        n // numInnerBytes
      remainder =
        modBy numInnerBytes  n
    in
    unsafeIntToAscii (width - 1) (toByte remainder :: bytes) quotient



-- ASCII BYTES


toByte : Int -> Int
toByte n =
       if n < 26  then (97 + n     ) {- lower -}
  else if n < 52  then (65 + n - 26) {- upper -}
  else if n == 52 then 95 {- _ -}
  else if n == 53 then 36 {- $ -}
  else if n < 64  then (48 + n - 54) {- digit -}
  else Debug.todo <| "cannot convert int " ++ String.fromInt n ++ " to ASCII"



-- BAD FIELDS


type BadFields =
  BadFields {- renamings -} Renamings


type alias Renamings =
  Map.Map Name.Name Name.Name


allBadFields : TList BadFields
allBadFields =
  let
    add keyword dict =
      Map.alter (Just << addRenaming keyword) (String.length keyword) dict
  in
    Map.elems <| Set.foldr add Map.empty jsReservedWords


addRenaming : Name.Name -> Maybe BadFields -> BadFields
addRenaming keyword maybeBadFields =
  let
    width = String.length keyword
    maxName = numStartBytes * numInnerBytes ^ (width - 1) - 1
  in
  case maybeBadFields of
    Nothing ->
      BadFields <| Map.singleton keyword (unsafeIntToAscii width [] maxName)

    Just (BadFields renamings) ->
      BadFields <| Map.insert keyword (unsafeIntToAscii width [] (maxName - Map.size renamings)) renamings
