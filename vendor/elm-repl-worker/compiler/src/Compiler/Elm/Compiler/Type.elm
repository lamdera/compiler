{- MANUALLY FORMATTED -}
module Compiler.Elm.Compiler.Type exposing
  ( Type(..)
  --, RT.Context(..)
  --, toDoc
  , DebugMetadata(..)
  , Alias(..)
  , Union(..)
  --, encode
  --, decoder
  , encodeMetadata
  )


import Compiler.Data.Name as Name
import Compiler.Json.Encode as E
import Compiler.Json.String as Json
import Compiler.Reporting.Doc as D exposing (d)
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Extra.Type.List as MList exposing (TList)



-- TYPES


type Type
  = Lambda Type Type
  | Var Name.Name
  | Type Name.Name (TList Type)
  | Record (TList (Name.Name, Type)) (Maybe Name.Name)
  | Unit
  | Tuple Type Type (TList Type)


type DebugMetadata =
  DebugMetadata
    {- message -} Type
    {- aliases -} (TList Alias)
    {- unions -} (TList Union)


type Alias = Alias Name.Name (TList Name.Name) Type
type Union = Union Name.Name (TList Name.Name) (TList (Name.Name, TList Type))



-- TO DOC


toDoc : L.Localizer -> RT.Context -> Type -> D.Doc
toDoc localizer context tipe =
  case tipe of
    Lambda _ _ ->
      case MList.map (toDoc localizer RT.Func) (collectLambdas tipe) of
        a :: b :: cs -> RT.lambda context a b cs
        x -> Debug.todo <| "toDoc Lambda " ++ Debug.toString x

    Var name ->
      D.fromName name

    Unit ->
      d"()"

    Tuple a b cs ->
      RT.tuple
        (toDoc localizer RT.None a)
        (toDoc localizer RT.None b)
        (MList.map (toDoc localizer RT.None) cs)

    Type name args ->
      RT.apply
        context
        (D.fromName name)
        (MList.map (toDoc localizer RT.App) args)

    Record fields ext ->
      RT.record
        (MList.map (entryToDoc localizer) fields)
        (Maybe.map D.fromName ext)


entryToDoc : L.Localizer -> (Name.Name, Type) -> (D.Doc, D.Doc)
entryToDoc localizer (field, fieldType) =
  ( D.fromName field, toDoc localizer RT.None fieldType )


collectLambdas : Type -> TList Type
collectLambdas tipe =
  case tipe of
    Lambda arg body ->
      arg :: collectLambdas body

    _ ->
      [tipe]



-- JSON for TYPE


encode : Type -> E.Value
encode tipe =
  E.chars <| D.toLine (toDoc L.empty RT.None tipe)



-- JSON for PROGRAM


encodeMetadata : DebugMetadata -> E.Value
encodeMetadata (DebugMetadata msg aliases unions) =
  E.object
    [ ( "message", encode msg )
    , ( "aliases", E.object (MList.map toTypeAliasField aliases) )
    , ( "unions", E.object (MList.map toCustomTypeField unions) )
    ]


toTypeAliasField : Alias -> ( Json.TString, E.Value )
toTypeAliasField (Alias name args tipe) =
  ( Json.fromName name
  , E.object
      [ ( "args", E.list E.name args )
      , ( "type", encode tipe )
      ]
  )


toCustomTypeField : Union -> ( Json.TString, E.Value )
toCustomTypeField (Union name args constructors) =
  ( Json.fromName name
  , E.object
      [ ( "args", E.list E.name args )
      , ( "tags", E.object (MList.map toVariantObject constructors) )
      ]
  )


toVariantObject : (Name.Name, TList Type) -> ( Json.TString, E.Value )
toVariantObject (name, args) =
  ( Json.fromName name, E.list encode args )
