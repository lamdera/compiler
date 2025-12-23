{- MANUALLY FORMATTED -}
module Compiler.Optimize.Port exposing
  ( toEncoder
  , toFlagsDecoder
  , toDecoder
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Utils.Type as Type
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Optimize.Names as Names
import Extra.Type.List as MList
import Extra.Type.Map as Map



-- ENCODE


toEncoder : Can.Type -> Names.Tracker Opt.Expr
toEncoder tipe =
  case tipe of
    Can.TAlias _ _ args alias_ ->
      toEncoder (Type.dealias args alias_)

    Can.TLambda _ _ ->
      Debug.todo "toEncoder: function"

    Can.TVar _ ->
      Debug.todo "toEncoder: type variable"

    Can.TUnit ->
      Names.fmap (Opt.Function [Name.dollar]) <| encode "null"

    Can.TTuple a b c ->
      encodeTuple a b c

    Can.TType _ name args ->
      let
        otherwise () =
          Debug.todo "toEncoder: bad custom type"
      in
      case args of
        [] ->
          if name == Name.float  then encode "float"
          else if name == Name.int    then encode "int"
          else if name == Name.bool   then encode "bool"
          else if name == Name.string then encode "string"
          else if name == Name.value  then Names.registerGlobal ModuleName.basics Name.identity_
          else otherwise ()

        [arg] ->
          if name == Name.maybe then encodeMaybe arg
          else if name == Name.list  then encodeList arg
          else if name == Name.array then encodeArray arg
          else otherwise ()

        _ ->
          otherwise ()

    Can.TRecord _ (Just _) ->
      Debug.todo "toEncoder: bad record"

    Can.TRecord fields Nothing ->
      let
        encodeField (name, Can.FieldType _ fieldType) =
          Names.bind (toEncoder fieldType) <| \encoder ->
          let value = Opt.Call encoder [Opt.Access (Opt.VarLocal Name.dollar) name] in
          Names.return <| Opt.Tuple (Opt.Str (Name.toElmString name)) value Nothing
      in
        Names.bind (encode "object") <| \object ->
        Names.bind (MList.traverse Names.pure Names.liftA2 encodeField (Map.toList fields)) <| \keyValuePairs ->
        Names.registerFieldDict fields <|
          Opt.Function [Name.dollar] (Opt.Call object [Opt.CList keyValuePairs])



-- ENCODE HELPERS


encodeMaybe : Can.Type -> Names.Tracker Opt.Expr
encodeMaybe tipe =
  Names.bind (encode "null") <| \null ->
  Names.bind (toEncoder tipe) <| \encoder ->
  Names.bind (Names.registerGlobal ModuleName.maybe "destruct") <| \destruct ->
  Names.return <| Opt.Function [Name.dollar] <|
    Opt.Call destruct [ null, encoder, Opt.VarLocal Name.dollar ]


encodeList : Can.Type -> Names.Tracker Opt.Expr
encodeList tipe =
  Names.bind (encode "list") <| \list ->
  Names.bind (toEncoder tipe) <| \encoder ->
  Names.return <| Opt.Call list [ encoder ]


encodeArray : Can.Type -> Names.Tracker Opt.Expr
encodeArray tipe =
  Names.bind (encode "array") <| \array ->
  Names.bind (toEncoder tipe) <| \encoder ->
  Names.return <| Opt.Call array [ encoder ]


encodeTuple : Can.Type -> Can.Type -> Maybe Can.Type -> Names.Tracker Opt.Expr
encodeTuple a b maybeC =
  let
    let_ arg index body =
      Opt.Destruct (Opt.Destructor arg (Opt.Index index (Opt.Root Name.dollar))) body

    encodeArg arg tipe =
      Names.bind (toEncoder tipe) <| \encoder ->
      Names.return <| Opt.Call encoder [ Opt.VarLocal arg ]
  in
  Names.bind (encode "list") <| \list ->
  Names.bind (Names.registerGlobal ModuleName.basics Name.identity_) <| \identity_ ->
  Names.bind (encodeArg "a" a) <| \arg1 ->
  Names.bind (encodeArg "b" b) <| \arg2 ->

  case maybeC of
    Nothing ->
      Names.return <| Opt.Function [Name.dollar] <|
        let_ "a" Index.first <|
        let_ "b" Index.second <|
          Opt.Call list [ identity_, Opt.CList [ arg1, arg2 ] ]

    Just c ->
      Names.bind (encodeArg "c" c) <| \arg3 ->
      Names.return <| Opt.Function [Name.dollar] <|
        let_ "a" Index.first <|
        let_ "b" Index.second <|
        let_ "c" Index.third <|
          Opt.Call list [ identity_, Opt.CList [ arg1, arg2, arg3 ] ]



-- FLAGS DECODER


toFlagsDecoder : Can.Type -> Names.Tracker Opt.Expr
toFlagsDecoder tipe =
  case tipe of
    Can.TUnit ->
      Names.bind (decode "succeed") <| \succeed ->
      Names.return <| Opt.Call succeed [ Opt.Unit ]

    _ ->
      toDecoder tipe



-- DECODE


toDecoder : Can.Type -> Names.Tracker Opt.Expr
toDecoder tipe =
  case tipe of
    Can.TLambda _ _ ->
      Debug.todo "functions should not be allowed through input ports"

    Can.TVar _ ->
      Debug.todo "type variables should not be allowed through input ports"

    Can.TAlias _ _ args alias_ ->
      toDecoder (Type.dealias args alias_)

    Can.TUnit ->
      decodeTuple0

    Can.TTuple a b c ->
      decodeTuple a b c

    Can.TType _ name args ->
      let
        otherwise () =
          Debug.todo "toDecoder: bad type"
      in
      case args of
        [] ->
          if name == Name.float  then decode "float"
          else if name == Name.int    then decode "int"
          else if name == Name.bool   then decode "bool"
          else if name == Name.string then decode "string"
          else if name == Name.value  then decode "value"
          else otherwise ()

        [arg] ->
          if name == Name.maybe then decodeMaybe arg
          else if name == Name.list  then decodeList arg
          else if name == Name.array then decodeArray arg
          else otherwise ()

        _ ->
          otherwise ()

    Can.TRecord _ (Just _) ->
      Debug.todo "toDecoder: bad record"

    Can.TRecord fields Nothing ->
      decodeRecord fields



-- DECODE MAYBE


decodeMaybe : Can.Type -> Names.Tracker Opt.Expr
decodeMaybe tipe =
  Names.bind (Names.registerGlobal ModuleName.maybe "Nothing") <| \nothing ->
  Names.bind (Names.registerGlobal ModuleName.maybe "Just") <| \just ->

  Names.bind (decode "oneOf") <| \oneOf ->
  Names.bind (decode "null") <| \null ->
  Names.bind (decode "map") <| \map_ ->

  Names.bind (toDecoder tipe) <| \subDecoder ->

  Names.return <|
    Opt.Call oneOf
      [ Opt.CList
          [ Opt.Call null [ nothing ]
          , Opt.Call map_ [ just, subDecoder ]
          ]
      ]


-- DECODE LIST


decodeList : Can.Type -> Names.Tracker Opt.Expr
decodeList tipe =
  Names.bind (decode "list") <| \list ->
  Names.bind (toDecoder tipe) <| \decoder ->
  Names.return <| Opt.Call list [ decoder ]



-- DECODE ARRAY


decodeArray : Can.Type -> Names.Tracker Opt.Expr
decodeArray tipe =
  Names.bind (decode "array") <| \array ->
  Names.bind (toDecoder tipe) <| \decoder ->
  Names.return <| Opt.Call array [ decoder ]



-- DECODE TUPLES


decodeTuple0 : Names.Tracker Opt.Expr
decodeTuple0 =
  Names.bind (decode "null") <| \null ->
  Names.return (Opt.Call null [ Opt.Unit ])


decodeTuple : Can.Type -> Can.Type -> Maybe Can.Type -> Names.Tracker Opt.Expr
decodeTuple a b maybeC =
  Names.bind (decode "succeed") <| \succeed ->
  case maybeC of
    Nothing ->
      let tuple = Opt.Tuple (toLocal 0) (toLocal 1) Nothing in
      Names.andThen (indexAndThen 0 a) <|
        indexAndThen 1 b (Opt.Call succeed [tuple])

    Just c ->
      let tuple = Opt.Tuple (toLocal 0) (toLocal 1) (Just (toLocal 2)) in
      Names.andThen (indexAndThen 0 a) <|
        Names.andThen (indexAndThen 1 b) <|
          indexAndThen 2 c (Opt.Call succeed [tuple])


toLocal : Int -> Opt.Expr
toLocal index =
  Opt.VarLocal (Name.fromVarIndex index)


indexAndThen : Int -> Can.Type -> Opt.Expr -> Names.Tracker Opt.Expr
indexAndThen i tipe decoder =
  Names.bind (decode "andThen") <| \andThen ->
  Names.bind (decode "index") <| \index ->
  Names.bind (toDecoder tipe) <| \typeDecoder ->
  Names.return <|
    Opt.Call andThen
      [ Opt.Function [Name.fromVarIndex i] decoder
      , Opt.Call index [ Opt.CInt i, typeDecoder ]
      ]



-- DECODE RECORDS


decodeRecord : Map.Map Name.Name Can.FieldType -> Names.Tracker Opt.Expr
decodeRecord fields =
  let
    toFieldExpr name _ =
      Opt.VarLocal name

    record =
      Opt.Record (Map.mapWithKey toFieldExpr fields)
  in
  Names.bind (decode "succeed") <| \succeed ->
  Names.andThen (MList.foldlM Names.return Names.bind fieldAndThen (Opt.Call succeed [record])) <|
    Names.registerFieldDict fields (Map.toList fields)


fieldAndThen : Opt.Expr -> (Name.Name, Can.FieldType) -> Names.Tracker Opt.Expr
fieldAndThen decoder (key, Can.FieldType _ tipe) =
  Names.bind (decode "andThen") <| \andThen ->
  Names.bind (decode "field") <| \field ->
  Names.bind (toDecoder tipe) <| \typeDecoder ->
  Names.return <|
    Opt.Call andThen
      [ Opt.Function [key] decoder
      , Opt.Call field [ Opt.Str (Name.toElmString key), typeDecoder ]
      ]



-- GLOBALS HELPERS


encode : Name.Name -> Names.Tracker Opt.Expr
encode name =
  Names.registerGlobal ModuleName.jsonEncode name


decode : Name.Name -> Names.Tracker Opt.Expr
decode name =
  Names.registerGlobal ModuleName.jsonDecode name
