{- MANUALLY FORMATTED -}
module Compiler.Elm.Kernel exposing
  ( Content(..)
  , Chunk(..), bChunk
  , fromByteString
  , countFields
  )


import Compiler.AST.Source as Src
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Parse.Module as Module
import Compiler.Parse.Primitives as P
import Compiler.Parse.Space as Space
import Compiler.Parse.Variable as Var
import Compiler.Reporting.Annotation as A
import Extra.Data.Binary as B
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map



-- CHUNK


{- NEW: Async -}
type Chunk
  = JS String
  | ElmVar ModuleName.Canonical Name.Name
  | JsVar Name.Name Name.Name
  | ElmField Name.Name
  | JsField Int
  | JsEnum Int
  | Debug
  | Prod
  | Async



-- COUNT FIELDS


countFields : TList Chunk -> Map.Map Name.Name Int
countFields chunks =
  MList.foldr addField Map.empty chunks


addField : Chunk -> Map.Map Name.Name Int -> Map.Map Name.Name Int
addField chunk fields =
  case chunk of
    JS _       -> fields
    ElmVar _ _ -> fields
    JsVar _ _  -> fields
    ElmField f -> Map.insertWith (+) f 1 fields
    JsField _  -> fields
    JsEnum _   -> fields
    Debug      -> fields
    Prod       -> fields
    Async      -> fields



-- FROM FILE


type Content =
  Content (TList Src.Import) (TList Chunk)


type alias Foreigns =
  Map.Map ModuleName.Raw Pkg.Name


fromByteString : Pkg.Name -> Foreigns -> String -> Maybe Content
fromByteString pkg foreigns bytes =
  case P.fromByteString (parser pkg foreigns) toError bytes of
    Right content ->
      Just content

    Left () ->
      Nothing


parser : Pkg.Name -> Foreigns -> P.Parser () Content
parser pkg foreigns =
  P.bind (P.word2 0x2F 0x2A {-/*-} toError) <| \_ ->
  P.bind (Space.chomp ignoreError) <| \_ ->
  P.bind (Space.checkFreshLine toError) <| \_ ->
  P.bind (P.specialize ignoreError (Module.chompImports [])) <| \imports ->
  P.bind (P.word2 0x2A 0x2F {-*/-} toError) <| \_ ->
  P.bind (parseChunks (toVarTable pkg foreigns imports) Map.empty Map.empty) <| \chunks ->
  P.return (Content imports chunks)


toError : P.Row -> P.Col -> ()
toError _ _ =
  ()


ignoreError : a -> P.Row -> P.Col -> ()
ignoreError _ _ _ =
  ()



-- PARSE CHUNKS


parseChunks : VarTable -> Enums -> Fields -> P.Parser () (TList Chunk)
parseChunks vtable enums fields =
  P.Parser <| \(P.State src pos end indent row col) ->
    let
      ( ( chunks, newPos ), ( newRow, newCol ) ) =
        chompChunks vtable enums fields src pos end row col pos []
    in
    if newPos == end then
      P.Cok chunks (P.State src newPos end indent newRow newCol)
    else
      P.Cerr row col toError


chompChunks : VarTable -> Enums -> Fields -> String -> Int -> Int -> P.Row -> P.Col -> Int -> TList Chunk -> ( ( TList Chunk, Int ), ( P.Row, P.Col ) )
chompChunks vs es fs src pos end row col lastPos revChunks =
  if pos >= end then
    let js = toByteString src lastPos end in
    ( ( MList.reverse (JS js :: revChunks), pos ), ( row, col ) )

  else
    let word = P.unsafeIndex src pos in
    if word == 0x5F {-_-} then
      let
        pos1 = pos + 1
        pos3 = pos + 3
      in
      if pos3 <= end && P.unsafeIndex src pos1 == 0x5F {-_-} then
        let js = toByteString src lastPos pos in
        chompTag vs es fs src pos3 end row (col + 3) (JS js :: revChunks)
      else
        chompChunks vs es fs src pos1 end row (col + 1) lastPos revChunks

    else if word == 0x0A {-\n-} then
      chompChunks vs es fs src (pos + 1) end (row + 1) 1 lastPos revChunks

    else
      let
        newPos = pos + (P.getCharWidth word)
      in
      chompChunks vs es fs src newPos end row (col + 1) lastPos revChunks


toByteString : String -> Int -> Int -> String
toByteString src pos end =
  String.slice pos end src



-- relies on external checks in chompChunks
chompTag : VarTable -> Enums -> Fields -> String -> Int -> Int -> P.Row -> P.Col -> TList Chunk -> ( ( TList Chunk, Int ), ( P.Row, P.Col ) )
chompTag vs es fs src pos end row col revChunks =
  let
    ( newPos, newCol ) = Var.chompInnerChars src pos end col
    tagPos = pos - 1
    word = P.unsafeIndex src tagPos
  in
  if word == 0x24 {- $ -} then
    let
      name = Name.fromPtr src pos newPos
    in
    chompChunks vs es fs src newPos end row newCol newPos <|
      ElmField name :: revChunks
  else
    let
      name = Name.fromPtr src tagPos newPos
    in
    if 0x30 {-0-} <= word && word <= 0x39 {-9-} then
      let
        (enum, newEnums) =
          lookupEnum (word - 0x30) name es
      in
      chompChunks vs newEnums fs src newPos end row newCol newPos <|
        JsEnum enum :: revChunks

    else if 0x61 {-a-} <= word && word <= 0x7A {-z-} then
      let
        (field, newFields) =
          lookupField name fs
      in
      chompChunks vs es newFields src newPos end row newCol newPos <|
        JsField field :: revChunks

    else if name == "DEBUG" then
      chompChunks vs es fs src newPos end row newCol newPos (Debug :: revChunks)

    else if name == "PROD" then
      chompChunks vs es fs src newPos end row newCol newPos (Prod :: revChunks)

    else if name == "ASYNC" then
      chompChunks vs es fs src newPos end row newCol newPos (Async :: revChunks)

    else
      case Map.lookup name vs of
        Just chunk ->
          chompChunks vs es fs src newPos end row newCol newPos (chunk :: revChunks)

        Nothing ->
          ( ( revChunks, pos ), ( row, col ) )



-- FIELDS


type alias Fields =
  Map.Map Name.Name Int


lookupField : Name.Name -> Fields -> (Int, Fields)
lookupField name fields =
  case Map.lookup name fields of
    Just n ->
      ( n, fields )

    Nothing ->
      let n = Map.size fields in
      ( n, Map.insert name n fields )



-- ENUMS


type alias Enums =
  Map.Map Int (Map.Map Name.Name Int)


lookupEnum : Int -> Name.Name -> Enums -> (Int, Enums)
lookupEnum word var allEnums =
  let
    enums =
      Map.findWithDefault Map.empty word allEnums
  in
    case Map.lookup var enums of
      Just n ->
        ( n, allEnums )

      Nothing ->
        let n = Map.size enums in
        ( n, Map.insert word (Map.insert var n enums) allEnums )



-- PROCESS IMPORTS


type alias VarTable =
  Map.Map Name.Name Chunk


toVarTable : Pkg.Name -> Foreigns -> TList Src.Import -> VarTable
toVarTable pkg foreigns imports =
  MList.foldl (addImport pkg foreigns) Map.empty imports


addImport : Pkg.Name -> Foreigns -> VarTable -> Src.Import -> VarTable
addImport pkg foreigns vtable (Src.Import (A.At _ importName) maybeAlias exposing_) =
  if Name.isKernel importName then
    case maybeAlias of
      Just _ ->
        Debug.todo ("cannot use `as` with kernel import of: " ++ importName)

      Nothing ->
        let
          home = Name.getKernel importName
          add table name =
            Map.insert (Name.sepBy 0x5F {-_-} home name) (JsVar home name) table
        in
        MList.foldl add vtable (toNames exposing_)

  else
    let
      home = ModuleName.Canonical (Map.findWithDefault pkg importName foreigns) importName
      prefix = toPrefix importName maybeAlias
      add table name =
        Map.insert (Name.sepBy 0x5F {-_-} prefix name) (ElmVar home name) table
    in
    MList.foldl add vtable (toNames exposing_)


toPrefix : Name.Name -> Maybe Name.Name -> Name.Name
toPrefix home maybeAlias =
  case maybeAlias of
    Just alias ->
      alias

    Nothing ->
      if Name.hasDot home then
        Debug.todo ("kernel imports with dots need an alias: " ++ home)
      else
        home


toNames : Src.Exposing -> TList Name.Name
toNames exposing_ =
  case exposing_ of
    Src.Open ->
      Debug.todo "cannot have `exposing (..)` in kernel code."

    Src.Explicit exposedList ->
      MList.map toName exposedList


toName : Src.Exposed -> Name.Name
toName exposed =
  case exposed of
    Src.Lower (A.At _ name) ->
      name

    Src.Upper (A.At _ name) Src.Private ->
      name

    Src.Upper _ (Src.Public _) ->
      Debug.todo "cannot have Maybe(..) syntax in kernel code header"

    Src.Operator _ _ ->
      Debug.todo "cannot use binops in kernel code"



-- BINARY


bChunk : B.Binary Chunk
bChunk =
  B.custom "problem deserializing Elm.Kernel.Chunk"
    (\p0 p1 p2 p3 p4 p5 p6 p7 p8 chunk ->
      case chunk of
        JS a       -> p0 a
        ElmVar a b -> p1 a b
        JsVar a b  -> p2 a b
        ElmField a -> p3 a
        JsField a  -> p4 a
        JsEnum a   -> p5 a
        Debug      -> p6
        Prod       -> p7
        Async      -> p8
    )
    |> B.var1 0 JS B.bString
    |> B.var2 1 ElmVar ModuleName.bCanonical Name.bName
    |> B.var2 2 JsVar Name.bName Name.bName
    |> B.var1 3 ElmField Name.bName
    |> B.var1 4 JsField B.bWord64
    |> B.var1 5 JsEnum B.bWord64
    |> B.var0 6 Debug
    |> B.var0 7 Prod
    |> B.var0 8 Async
    |> B.finish
