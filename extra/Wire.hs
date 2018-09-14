{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wire where

-- import AST.Source (VarType(..), Type_(..), Pattern_(..))
-- import AST.Valid
import Reporting.Annotation (Located(..))
import Reporting.Region
import qualified Elm.Name as N
-- import qualified AST.Valid as AS (Module(..))
import AST.Canonical
import AST.Module.Name (Canonical(..))
import Elm.Package (Name(..))
import qualified Data.Map as Map
import Data.Index

import Control.Monad.Trans (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Prettyprint


imap f l = zipWith f [0..] l


-- Our injection point POC for AllTypes. Search for `Wire.modify`
modify canonical flag pkg importDict interfaces source =
  case canonical of
    Module name docs exports decls unions aliases binops effects ->
      case name of
        Canonical pkg n ->
          case N.toString n of
            "AllTypes_Gen" ->

              -- tracef ("-" ++ N.toString n) valid
              -- tracef ("-" ++ N.toString n) (canonical { _decls = [ encoder, decoder, evg_e_Union, evg_d_Union ] })
              tracef ("-" ++ N.toString n) canonical

            "AllTypes" -> do

              -- tracef ("-" ++ N.toString n) canonical

              let unionDecls = fmap unionToEncoder $ Map.toList unions

              tracef ("-" ++ N.toString n) (canonical { _decls = funtimes unionDecls })

              -- tracef ("-" ++ N.toString n) (canonical { _decls =
              --   case _decls canonical of
              --     DeclareRec d x ->
              --
              --       -- Use this one when we want to see the original schema
              --       -- DeclareRec (d ++ [ staticX ]) x
              --
              --       -- Use this one otherwise
              --       funtimes
              --
              --       -- Canary test
              --       -- DeclareRec [] x
              --
              --     d -> d
              -- })

            _ ->
              canonical


unionToEncoder (unionName_, union_) = do

  let
    _encoderName = "evg_e_" ++ N.toString unionName_

    _genUnion0 _index ctor =
      case ctor of
        Ctor n index numParams params ->
          let _tagNameT = N.toText n
              _tagNameS = N.toString n
          in
          unionCaseBranch unionName_ union_ _index _tagNameS
            ([])
            (call jsonEncodeList
              [ coreBasicsIdentity
              , list [ call jsonEncodeString [ str _tagNameT ] ]
              ]
            )

    _genUnion1 _index ctor =
      case ctor of
        Ctor n index numParams pTypes ->
          let _tagNameT = N.toText n
              _tagNameS = N.toString n
              _pType = head pTypes
          in
          unionCaseBranch unionName_ union_ _index _tagNameS
            [ PatternCtorArg
                { _index = ZeroBased 0
                , _type = _pType
                , _arg = at (PVar (name "evg_v0"))
                }
            ]
             (call jsonEncodeList
               [ coreBasicsIdentity
               , list
                  [ call jsonEncodeString [str _tagNameT]
                  , encodeParamType _pType (vlocal "evg_v0")
                  ]
               ]
             )


    encodeParamType pType vlocal1 =
      case pType of
        TType (Canonical (Name "elm" "core") "Basics") typeName next ->
          case N.toString typeName of
            "Int" ->  call jsonEncodeInt [vlocal1]

        TType (Canonical (Name "elm" "core") "List") typeName next ->
          case N.toString typeName of

            "List" -> call jsonEncodeList (encodeListType (head next) vlocal1)

        TType (Canonical (Name "author" "project") _) typeName next ->
          -- Any types from user, must have encoder ref in this file
          let _targetEncoderName = "evg_e_" ++ N.toString typeName

          in
          call (at (VarTopLevel (canonical "author" "project" "AllTypes")
                                     (name _targetEncoderName))) [vlocal "evg_v0"]

        _ -> error $ "encodeParamType didn't match any existing implementations: " ++ show pType


    encodeListType pType vlocal1 =
      case pType of
        TType (Canonical {_package = Name {_author = "elm" ,_project = "core"} ,_module = "Basics"}) name [] ->
          case N.toString name of
            "Bool" -> [jsonEncodeBool, vlocal1]

        _ -> error $ "encodeListType didn't match any existing implementations: " ++ show pType


    _unionBranches =
      case union_ of
        Union _u_vars _u_alts _u_numAlts _u_opts ->
          imap (\i ctor ->
            case ctor of
              Ctor n index numParams params ->
                case numParams of
                  0 -> _genUnion0 i ctor
                  1 -> _genUnion1 i ctor
                  -- @TODO need to add support for more depths of union types
                  _ -> undefined "unimplemented union parsing for that many params"
          ) _u_alts

    _unionNameString = N.toString unionName_

  TypedDef
    (named _encoderName)
    (Map.fromList [])
    [ (at (PVar (name "evg_p0"))
    , qtyp "author" "project" "AllTypes" _unionNameString [])
    ]
    (at (Case (vlocal "evg_p0") _unionBranches))
    (qtyp "elm" "json" "Json.Encode" "Value" [])


unionCaseBranch unionName union index unionLabel unionArgs expr =
  -- @TODO AllTypes needs to be something else eventually
  CaseBranch
    (at (PCtor { _p_home = canonical "author" "project" "AllTypes"
               , _p_type = unionName
               , _p_union = union
               , _p_name = name unionLabel
               , _p_index = ZeroBased index
               , _p_args = unionArgs
               }
        )
    )
    expr


staticX =
  (Def (at
           (N.fromString "evg"))
       []
       (at
           (Int 123)))


-- AST to file debugger
tracef n a =
  unsafePerformIO $ do
    putStrLn ("can-" ++ n ++ ".txt")
    writeFile ("can-" ++ n ++ ".txt") $ prettyShow a
    pure a


tracer a b =
  unsafePerformIO $ do
    print a
    pure b



-- Helpers for the At/Region prefixes
region =
  Region
    { _start = Position { _line   = 10 , _column = 10 }
    , _end   = Position { _line   = 10 , _column = 10 }
    }


at = At region


canonical author project n =
  Canonical {_package = Name {_author = author ,_project = project} ,_module = name n}


-- An Int literal
int x = at (Int x)


-- A String literal
str x = at (Str x)


-- Unit () literal
unit = at Unit


-- A symbol name
name = N.fromString


-- A name at a region
named n = at (name n)


-- A qualified value
qvar author package module_ n defs =
  at (VarForeign (canonical author package module_) (name n) defs)


-- A qualified canonicalized type in a signature,
-- i.e. elm/core List module's `List` type in `Int -> List a`
-- would be `qtyp "elm" "core" "List" "List"`
qtyp author project module_ n = TType (canonical author project module_) (name n)


-- A type variable in a signature, i.e. the `a` within `List a -> a`
tvar n = TVar (name n)


-- A lamdba in a type signature, i.e. `a -> b`
tlam a b = TLambda a b


-- A function call
call f args = at (Call (f) args)


-- A list literal
list exprs = at (List exprs)


-- A local variable
vlocal n = at (VarLocal (name n))



-- Helpful shortcut definitions for Evergreen derivations

-- JSON.Encode

jsonEncodeList =
  ((qvar "elm" "json" "Json.Encode" "list"
      (Forall (Map.fromList [(name "a"
                        ,())])
              (tlam (tlam (tvar "a")
                          (qtyp "elm" "json" "Json.Encode" "Value" []))
                    (tlam (qtyp "elm" "core" "List" "List" [tvar "a"])
                          (qtyp "elm" "json" "Json.Encode" "Value" [])))
      )))


jsonEncodeString =
  ((qvar "elm" "json" "Json.Encode" "string"
      (Forall (Map.fromList [])
              (tlam (qtyp "elm" "core" "String" "String" [])
                    (qtyp "elm" "json" "Json.Encode" "Value" [])))))


jsonEncodeInt =
  ((qvar "elm" "json" "Json.Encode" "int"
    (Forall (Map.fromList [])
            (tlam (qtyp "elm" "core" "Basics" "Int" [])
                     (qtyp "elm" "json" "Json.Encode" "Value" [])))))

-- JSON.Decode

jsonDecodeInt =
  (qvar "elm" "json" "Json.Decode" "int"
    (Forall (Map.fromList [])
            (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Basics" "Int" []])))



-- Core.Basics

coreBasicsIdentity =
  (qvar "elm" "core" "Basics" "identity"
                  (Forall (Map.fromList [(name "a"
                                    ,())])
                          (tlam (tvar "a")
                                (tvar "a"))))


-- Evergreen

evergreenUnion =
  ((qvar "author" "project" "Evergreen" "union"
    (Forall (Map.fromList [(name "a"
                      ,())])
            (tlam (qtyp "elm" "core" "String" "String" [])
                     (tlam (tvar "a")
                              (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"]))))))


evergreenUnion1 =
  ((qvar "author" "project" "Evergreen" "union1"
    (Forall (Map.fromList [(name "a"
                      ,())
                      ,(name "b"
                      ,())])
            (tlam (qtyp "elm" "core" "String" "String" [])
                     (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                              (tlam (tlam (tvar "a")
                                                (tvar "b"))
                                       (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])))))))


jsonEncodeFloat =
  ((qvar "elm" "json" "Json.Encode" "float"
    (Forall (Map.fromList [])
            (tlam (qtyp "elm" "core" "Basics" "Float" [])
                     (qtyp "elm" "json" "Json.Encode" "Value" [])))))

jsonEncodeBool =
  ((qvar "elm" "json" "Json.Encode" "bool"
    (Forall (Map.fromList [])
            (tlam (qtyp "elm" "core" "Basics" "Bool" [])
                     (qtyp "elm" "json" "Json.Encode" "Value" [])))))

evergreenEncodeChar =
  ((qvar "author" "project" "Evergreen" "e_char"
                            (Forall (Map.fromList [])
                                    (tlam (qtyp "elm" "core" "Char" "Char" [])
                                             (qtyp "elm" "json" "Json.Encode" "Value" [])))))

jsonEncodeSet =
  ((qvar "elm" "json" "Json.Encode" "set"
    (Forall (Map.fromList [(name "a"
                      ,())])
            (tlam (tlam (tvar "a")
                              (qtyp "elm" "json" "Json.Encode" "Value" []))
                     (tlam (qtyp "elm" "core" "Set" "Set" [tvar "a"])
                              (qtyp "elm" "json" "Json.Encode" "Value" []))))))


jsonEncodeArray =
  ((qvar "elm" "json" "Json.Encode" "array"
    (Forall (Map.fromList [(name "a"
                      ,())])
            (tlam (tlam (tvar "a")
                              (qtyp "elm" "json" "Json.Encode" "Value" []))
                     (tlam (qtyp "elm" "core" "Array" "Array" [tvar "a"])
                              (qtyp "elm" "json" "Json.Encode" "Value" []))))))


evergreenEncodeDict =
  ((qvar "author" "project" "Evergreen" "e_dict"
    (Forall (Map.fromList [(name "comparable"
                      ,())
                      ,(name "v"
                      ,())])
            (tlam (tlam (tvar "comparable")
                              (qtyp "elm" "json" "Json.Encode" "Value" []))
                     (tlam (tlam (tvar "v")
                                       (qtyp "elm" "json" "Json.Encode" "Value" []))
                              (tlam (qtyp "elm" "core" "Dict" "Dict" [tvar "comparable"
                                              ,tvar "v"])
                                       (qtyp "elm" "json" "Json.Encode" "Value" [])))))))




funtimes additionalEncoders =
    DeclareRec
    additionalEncoders
    (Declare (TypedDef (named "evg_e_AllTypes")
                      (Map.fromList [])
                      [(at (PVar (name "evg_p0"))
                      ,TAlias (canonical "author" "project" "AllTypes")
                              (name "AllTypes")
                              []
                              (Holey (TRecord (Map.fromList [(name "arrayString"
                                                        ,FieldType 7 (qtyp "elm" "core" "Array" "Array" [qtyp "elm" "core" "String" "String" []]))
                                                        ,(name "bool"
                                                        ,FieldType 2 (qtyp "elm" "core" "Basics" "Bool" []))
                                                        ,(name "char"
                                                        ,FieldType 3 (qtyp "elm" "core" "Char" "Char" []))
                                                        ,(name "dict"
                                                        ,FieldType 8 (qtyp "elm" "core" "Dict" "Dict" [qtyp "elm" "core" "String" "String" []
                                                                            ,qtyp "elm" "core" "List" "List" [qtyp "elm" "core" "Basics" "Int" []]]))
                                                        ,(name "float"
                                                        ,FieldType 1 (qtyp "elm" "core" "Basics" "Float" []))
                                                        ,(name "int"
                                                        ,FieldType 0 (qtyp "elm" "core" "Basics" "Int" []))
                                                        ,(name "listInt"
                                                        ,FieldType 5 (qtyp "elm" "core" "List" "List" [qtyp "elm" "core" "Basics" "Int" []]))
                                                        ,(name "order"
                                                        ,FieldType 10 (qtyp "elm" "core" "Basics" "Order" []))
                                                        ,(name "setFloat"
                                                        ,FieldType 6 (qtyp "elm" "core" "Set" "Set" [qtyp "elm" "core" "Basics" "Float" []]))
                                                        ,(name "string"
                                                        ,FieldType 4 (qtyp "elm" "core" "String" "String" []))
                                                        ,(name "time"
                                                        ,FieldType 9 (qtyp "elm" "time" "Time" "Posix" []))
                                                        ,(name "union"
                                                        ,FieldType 11 (qtyp "author" "project" "AllTypes" "Union" []))
                                                        ,(name "unit"
                                                        ,FieldType 12 TUnit)])
                                              Nothing)))]
                      (at (Call jsonEncodeList
                                [coreBasicsIdentity
                                ,at (List [at (Call jsonEncodeInt
                                                    [at (Access (at (VarLocal (name "evg_p0")))
                                                                (named "int"))])
                                          ,at (Call jsonEncodeFloat
                                                    [at (Access (at (VarLocal (name "evg_p0")))
                                                                (named "float"))])
                                          ,at (Call jsonEncodeBool
                                                    [at (Access (at (VarLocal (name "evg_p0")))
                                                                (named "bool"))])
                                          ,at (Call evergreenEncodeChar
                                                    [at (Access (at (VarLocal (name "evg_p0")))
                                                                (named "char"))])
                                          ,at (Call jsonEncodeString
                                                    [at (Access (at (VarLocal (name "evg_p0")))
                                                                (named "string"))])
                                          ,at (Call jsonEncodeList
                                                    [jsonEncodeInt
                                                    ,at (Access (at (VarLocal (name "evg_p0")))
                                                                (named "listInt"))])
                                          ,at (Call jsonEncodeSet
                                                    [jsonEncodeFloat
                                                    ,at (Access (at (VarLocal (name "evg_p0")))
                                                                (named "setFloat"))])
                                          ,at (Call jsonEncodeArray
                                                    [jsonEncodeString
                                                    ,at (Access (at (VarLocal (name "evg_p0")))
                                                                (named "arrayString"))])
                                          ,at (Call evergreenEncodeDict
                                                    [jsonEncodeString
                                                    ,at (Call jsonEncodeList
                                                              [(qvar "elm" "json" "Json.Encode" "int"
                                                                              (Forall (Map.fromList [])
                                                                                      (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                               (qtyp "elm" "json" "Json.Encode" "Value" []))))])
                                                    ,at (Access (at (VarLocal (name "evg_p0")))
                                                                (named "dict"))])
                                          ,at (Call ((qvar "author" "project" "Evergreen" "e_time"
                                                                    (Forall (Map.fromList [])
                                                                            (tlam (qtyp "elm" "time" "Time" "Posix" [])
                                                                                     (qtyp "elm" "json" "Json.Encode" "Value" [])))))
                                                    [at (Access (at (VarLocal (name "evg_p0")))
                                                                (named "time"))])
                                          ,at (Call ((qvar "author" "project" "Evergreen" "e_order"
                                                                    (Forall (Map.fromList [])
                                                                            (tlam (qtyp "elm" "core" "Basics" "Order" [])
                                                                                     (qtyp "elm" "json" "Json.Encode" "Value" [])))))
                                                    [at (Access (at (VarLocal (name "evg_p0")))
                                                                (named "order"))])
                                          ,at (Call (at (VarTopLevel (canonical "author" "project" "AllTypes")
                                                                     (name "evg_e_Union")))
                                                    [at (Access (at (VarLocal (name "evg_p0")))
                                                                (named "union"))])
                                          ,(qvar "elm" "json" "Json.Encode" "null"
                                                          (Forall (Map.fromList [])
                                                                  (qtyp "elm" "json" "Json.Encode" "Value" [])))])]))
                      (qtyp "elm" "json" "Json.Encode" "Value" []))
            (DeclareRec [TypedDef (named "evg_d_Union")
                                  (Map.fromList [])
                                  []
                                  (at (Call ((qvar "elm" "json" "Json.Decode" "oneOf"
                                                            (Forall (Map.fromList [(name "a"
                                                                              ,())])
                                                                    (tlam (qtyp "elm" "core" "List" "List" [qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"]])
                                                                             (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])))))
                                            [at (List [at (Call evergreenUnion1
                                                                [str "Recursive"
                                                                ,at (Call ((qvar "elm" "json" "Json.Decode" "lazy"
                                                                                          (Forall (Map.fromList [(name "a"
                                                                                                            ,())])
                                                                                                  (tlam (tlam TUnit (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"]))
                                                                                                           (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])))))
                                                                          [at (Lambda [at PAnything]
                                                                                      (at (VarTopLevel (canonical "author" "project" "AllTypes")
                                                                                                       (name "evg_d_Union"))))])
                                                                ,at (VarCtor Normal (canonical "author" "project" "AllTypes")
                                                                                    (name "Recursive")
                                                                                    (ZeroBased 0)
                                                                                    (Forall (Map.fromList [])
                                                                                            (tlam (qtyp "author" "project" "AllTypes" "Union" [])
                                                                                                     (qtyp "author" "project" "AllTypes" "Union" []))))])
                                                      ,at (Call evergreenUnion1
                                                                [str "Valued"
                                                                ,jsonDecodeInt
                                                                ,at (VarCtor Normal (canonical "author" "project" "AllTypes")
                                                                                    (name "Valued")
                                                                                    (ZeroBased 1)
                                                                                    (Forall (Map.fromList [])
                                                                                            (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                     (qtyp "author" "project" "AllTypes" "Union" []))))])
                                                      ,at (Call evergreenUnion1
                                                                [str "DeeplyValued"
                                                                ,at (Call ((qvar "elm" "json" "Json.Decode" "list"
                                                                                          (Forall (Map.fromList [(name "a"
                                                                                                            ,())])
                                                                                                  (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                           (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "List" "List" [tvar "a"]])))))
                                                                          [(qvar "elm" "json" "Json.Decode" "bool"
                                                                                          (Forall (Map.fromList [])
                                                                                                  (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Basics" "Bool" []])))])
                                                                ,at (VarCtor Normal (canonical "author" "project" "AllTypes")
                                                                                    (name "DeeplyValued")
                                                                                    (ZeroBased 2)
                                                                                    (Forall (Map.fromList [])
                                                                                            (tlam (qtyp "elm" "core" "List" "List" [qtyp "elm" "core" "Basics" "Bool" []])
                                                                                                     (qtyp "author" "project" "AllTypes" "Union" []))))])
                                                      ,at (Call evergreenUnion
                                                                [str "Leaf"
                                                                ,at (VarCtor Normal (canonical "author" "project" "AllTypes")
                                                                                    (name "Leaf")
                                                                                    (ZeroBased 3)
                                                                                    (Forall (Map.fromList [])
                                                                                            (qtyp "author" "project" "AllTypes" "Union" [])))])])]))
                                  (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "author" "project" "AllTypes" "Union" []])]
                        (Declare (TypedDef (named "evg_d_AllTypes")
                                           (Map.fromList [])
                                           []
                                           (at (Binop (name "|>")
                                                      (canonical "elm" "core" "Basics")
                                                      (name "apR")
                                                      (Forall (Map.fromList [(name "a"
                                                                        ,())
                                                                        ,(name "b"
                                                                        ,())])
                                                              (tlam (tvar "a")
                                                                       (tlam (tlam (tvar "a")
                                                                                         (tvar "b"))
                                                                                (tvar "b"))))
                                                      (at (Binop (name "|>")
                                                                 (canonical "elm" "core" "Basics")
                                                                 (name "apR")
                                                                 (Forall (Map.fromList [(name "a"
                                                                                   ,())
                                                                                   ,(name "b"
                                                                                   ,())])
                                                                         (tlam (tvar "a")
                                                                                  (tlam (tlam (tvar "a")
                                                                                                    (tvar "b"))
                                                                                           (tvar "b"))))
                                                                 (at (Binop (name "|>")
                                                                            (canonical "elm" "core" "Basics")
                                                                            (name "apR")
                                                                            (Forall (Map.fromList [(name "a"
                                                                                              ,())
                                                                                              ,(name "b"
                                                                                              ,())])
                                                                                    (tlam (tvar "a")
                                                                                             (tlam (tlam (tvar "a")
                                                                                                               (tvar "b"))
                                                                                                      (tvar "b"))))
                                                                            (at (Binop (name "|>")
                                                                                       (canonical "elm" "core" "Basics")
                                                                                       (name "apR")
                                                                                       (Forall (Map.fromList [(name "a"
                                                                                                         ,())
                                                                                                         ,(name "b"
                                                                                                         ,())])
                                                                                               (tlam (tvar "a")
                                                                                                        (tlam (tlam (tvar "a")
                                                                                                                          (tvar "b"))
                                                                                                                 (tvar "b"))))
                                                                                       (at (Binop (name "|>")
                                                                                                  (canonical "elm" "core" "Basics")
                                                                                                  (name "apR")
                                                                                                  (Forall (Map.fromList [(name "a"
                                                                                                                    ,())
                                                                                                                    ,(name "b"
                                                                                                                    ,())])
                                                                                                          (tlam (tvar "a")
                                                                                                                   (tlam (tlam (tvar "a")
                                                                                                                                     (tvar "b"))
                                                                                                                            (tvar "b"))))
                                                                                                  (at (Binop (name "|>")
                                                                                                             (canonical "elm" "core" "Basics")
                                                                                                             (name "apR")
                                                                                                             (Forall (Map.fromList [(name "a"
                                                                                                                               ,())
                                                                                                                               ,(name "b"
                                                                                                                               ,())])
                                                                                                                     (tlam (tvar "a")
                                                                                                                              (tlam (tlam (tvar "a")
                                                                                                                                                (tvar "b"))
                                                                                                                                       (tvar "b"))))
                                                                                                             (at (Binop (name "|>")
                                                                                                                        (canonical "elm" "core" "Basics")
                                                                                                                        (name "apR")
                                                                                                                        (Forall (Map.fromList [(name "a"
                                                                                                                                          ,())
                                                                                                                                          ,(name "b"
                                                                                                                                          ,())])
                                                                                                                                (tlam (tvar "a")
                                                                                                                                         (tlam (tlam (tvar "a")
                                                                                                                                                           (tvar "b"))
                                                                                                                                                  (tvar "b"))))
                                                                                                                        (at (Binop (name "|>")
                                                                                                                                   (canonical "elm" "core" "Basics")
                                                                                                                                   (name "apR")
                                                                                                                                   (Forall (Map.fromList [(name "a"
                                                                                                                                                     ,())
                                                                                                                                                     ,(name "b"
                                                                                                                                                     ,())])
                                                                                                                                           (tlam (tvar "a")
                                                                                                                                                    (tlam (tlam (tvar "a")
                                                                                                                                                                      (tvar "b"))
                                                                                                                                                             (tvar "b"))))
                                                                                                                                   (at (Binop (name "|>")
                                                                                                                                              (canonical "elm" "core" "Basics")
                                                                                                                                              (name "apR")
                                                                                                                                              (Forall (Map.fromList [(name "a"
                                                                                                                                                                ,())
                                                                                                                                                                ,(name "b"
                                                                                                                                                                ,())])
                                                                                                                                                      (tlam (tvar "a")
                                                                                                                                                               (tlam (tlam (tvar "a")
                                                                                                                                                                                 (tvar "b"))
                                                                                                                                                                        (tvar "b"))))
                                                                                                                                              (at (Binop (name "|>")
                                                                                                                                                         (canonical "elm" "core" "Basics")
                                                                                                                                                         (name "apR")
                                                                                                                                                         (Forall (Map.fromList [(name "a"
                                                                                                                                                                           ,())
                                                                                                                                                                           ,(name "b"
                                                                                                                                                                           ,())])
                                                                                                                                                                 (tlam (tvar "a")
                                                                                                                                                                          (tlam (tlam (tvar "a")
                                                                                                                                                                                            (tvar "b"))
                                                                                                                                                                                   (tvar "b"))))
                                                                                                                                                         (at (Binop (name "|>")
                                                                                                                                                                    (canonical "elm" "core" "Basics")
                                                                                                                                                                    (name "apR")
                                                                                                                                                                    (Forall (Map.fromList [(name "a"
                                                                                                                                                                                      ,())
                                                                                                                                                                                      ,(name "b"
                                                                                                                                                                                      ,())])
                                                                                                                                                                            (tlam (tvar "a")
                                                                                                                                                                                     (tlam (tlam (tvar "a")
                                                                                                                                                                                                       (tvar "b"))
                                                                                                                                                                                              (tvar "b"))))
                                                                                                                                                                    (at (Binop (name "|>")
                                                                                                                                                                               (canonical "elm" "core" "Basics")
                                                                                                                                                                               (name "apR")
                                                                                                                                                                               (Forall (Map.fromList [(name "a"
                                                                                                                                                                                                 ,())
                                                                                                                                                                                                 ,(name "b"
                                                                                                                                                                                                 ,())])
                                                                                                                                                                                       (tlam (tvar "a")
                                                                                                                                                                                                (tlam (tlam (tvar "a")
                                                                                                                                                                                                                  (tvar "b"))
                                                                                                                                                                                                         (tvar "b"))))
                                                                                                                                                                               (at (Binop (name "|>")
                                                                                                                                                                                          (canonical "elm" "core" "Basics")
                                                                                                                                                                                          (name "apR")
                                                                                                                                                                                          (Forall (Map.fromList [(name "a"
                                                                                                                                                                                                            ,())
                                                                                                                                                                                                            ,(name "b"
                                                                                                                                                                                                            ,())])
                                                                                                                                                                                                  (tlam (tvar "a")
                                                                                                                                                                                                           (tlam (tlam (tvar "a")
                                                                                                                                                                                                                             (tvar "b"))
                                                                                                                                                                                                                    (tvar "b"))))
                                                                                                                                                                                          (at (Call ((qvar "elm" "json" "Json.Decode" "succeed"
                                                                                                                                                                                                                    (Forall (Map.fromList [(name "a"
                                                                                                                                                                                                                                      ,())])
                                                                                                                                                                                                                            (tlam (tvar "a")
                                                                                                                                                                                                                                     (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])))))
                                                                                                                                                                                                    [at (VarCtor Normal (canonical "author" "project" "AllTypes")
                                                                                                                                                                                                                        (name "AllTypes")
                                                                                                                                                                                                                        (ZeroBased 0)
                                                                                                                                                                                                                        (Forall (Map.fromList [])
                                                                                                                                                                                                                                (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                                                                                                                                                         (tlam (qtyp "elm" "core" "Basics" "Float" [])
                                                                                                                                                                                                                                                  (tlam (qtyp "elm" "core" "Basics" "Bool" [])
                                                                                                                                                                                                                                                           (tlam (qtyp "elm" "core" "Char" "Char" [])
                                                                                                                                                                                                                                                                    (tlam (qtyp "elm" "core" "String" "String" [])
                                                                                                                                                                                                                                                                             (tlam (qtyp "elm" "core" "List" "List" [qtyp "elm" "core" "Basics" "Int" []])
                                                                                                                                                                                                                                                                                      (tlam (qtyp "elm" "core" "Set" "Set" [qtyp "elm" "core" "Basics" "Float" []])
                                                                                                                                                                                                                                                                                               (tlam (qtyp "elm" "core" "Array" "Array" [qtyp "elm" "core" "String" "String" []])
                                                                                                                                                                                                                                                                                                        (tlam (qtyp "elm" "core" "Dict" "Dict" [qtyp "elm" "core" "String" "String" []
                                                                                                                                                                                                                                                                                                                        ,qtyp "elm" "core" "List" "List" [qtyp "elm" "core" "Basics" "Int" []]])
                                                                                                                                                                                                                                                                                                                 (tlam (qtyp "elm" "time" "Time" "Posix" [])
                                                                                                                                                                                                                                                                                                                          (tlam (qtyp "elm" "core" "Basics" "Order" [])
                                                                                                                                                                                                                                                                                                                                   (tlam (qtyp "author" "project" "AllTypes" "Union" [])
                                                                                                                                                                                                                                                                                                                                            (tlam TUnit (TAlias (canonical "author" "project" "AllTypes")
                                                                                                                                                                                                                                                                                                                                                                   (name "AllTypes")
                                                                                                                                                                                                                                                                                                                                                                   []
                                                                                                                                                                                                                                                                                                                                                                   (Filled (TRecord (Map.fromList [(name "arrayString"
                                                                                                                                                                                                                                                                                                                                                                                              ,FieldType 7 (qtyp "elm" "core" "Array" "Array" [qtyp "elm" "core" "String" "String" []]))
                                                                                                                                                                                                                                                                                                                                                                                              ,(name "bool"
                                                                                                                                                                                                                                                                                                                                                                                              ,FieldType 2 (qtyp "elm" "core" "Basics" "Bool" []))
                                                                                                                                                                                                                                                                                                                                                                                              ,(name "char"
                                                                                                                                                                                                                                                                                                                                                                                              ,FieldType 3 (qtyp "elm" "core" "Char" "Char" []))
                                                                                                                                                                                                                                                                                                                                                                                              ,(name "dict"
                                                                                                                                                                                                                                                                                                                                                                                              ,FieldType 8 (qtyp "elm" "core" "Dict" "Dict" [qtyp "elm" "core" "String" "String" []
                                                                                                                                                                                                                                                                                                                                                                                                                  ,qtyp "elm" "core" "List" "List" [qtyp "elm" "core" "Basics" "Int" []]]))
                                                                                                                                                                                                                                                                                                                                                                                              ,(name "float"
                                                                                                                                                                                                                                                                                                                                                                                              ,FieldType 1 (qtyp "elm" "core" "Basics" "Float" []))
                                                                                                                                                                                                                                                                                                                                                                                              ,(name "int"
                                                                                                                                                                                                                                                                                                                                                                                              ,FieldType 0 (qtyp "elm" "core" "Basics" "Int" []))
                                                                                                                                                                                                                                                                                                                                                                                              ,(name "listInt"
                                                                                                                                                                                                                                                                                                                                                                                              ,FieldType 5 (qtyp "elm" "core" "List" "List" [qtyp "elm" "core" "Basics" "Int" []]))
                                                                                                                                                                                                                                                                                                                                                                                              ,(name "order"
                                                                                                                                                                                                                                                                                                                                                                                              ,FieldType 10 (qtyp "elm" "core" "Basics" "Order" []))
                                                                                                                                                                                                                                                                                                                                                                                              ,(name "setFloat"
                                                                                                                                                                                                                                                                                                                                                                                              ,FieldType 6 (qtyp "elm" "core" "Set" "Set" [qtyp "elm" "core" "Basics" "Float" []]))
                                                                                                                                                                                                                                                                                                                                                                                              ,(name "string"
                                                                                                                                                                                                                                                                                                                                                                                              ,FieldType 4 (qtyp "elm" "core" "String" "String" []))
                                                                                                                                                                                                                                                                                                                                                                                              ,(name "time"
                                                                                                                                                                                                                                                                                                                                                                                              ,FieldType 9 (qtyp "elm" "time" "Time" "Posix" []))
                                                                                                                                                                                                                                                                                                                                                                                              ,(name "union"
                                                                                                                                                                                                                                                                                                                                                                                              ,FieldType 11 (qtyp "author" "project" "AllTypes" "Union" []))
                                                                                                                                                                                                                                                                                                                                                                                              ,(name "unit"
                                                                                                                                                                                                                                                                                                                                                                                              ,FieldType 12 TUnit)])
                                                                                                                                                                                                                                                                                                                                                                                    Nothing))))))))))))))))))]))
                                                                                                                                                                                          (at (Call ((qvar "author" "project" "Evergreen" "atIndex"
                                                                                                                                                                                                                    (Forall (Map.fromList [(name "a"
                                                                                                                                                                                                                                      ,())
                                                                                                                                                                                                                                      ,(name "b"
                                                                                                                                                                                                                                      ,())])
                                                                                                                                                                                                                            (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                                                                                                                                                     (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                                                                                                                                              (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tlam (tvar "a")
                                                                                                                                                                                                                                                                       (tvar "b")])
                                                                                                                                                                                                                                                       (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])))))))
                                                                                                                                                                                                    [int 0
                                                                                                                                                                                                    ,(qvar "elm" "json" "Json.Decode" "int"
                                                                                                                                                                                                                    (Forall (Map.fromList [])
                                                                                                                                                                                                                            (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Basics" "Int" []])))]))))
                                                                                                                                                                               (at (Call ((qvar "author" "project" "Evergreen" "atIndex"
                                                                                                                                                                                                         (Forall (Map.fromList [(name "a"
                                                                                                                                                                                                                           ,())
                                                                                                                                                                                                                           ,(name "b"
                                                                                                                                                                                                                           ,())])
                                                                                                                                                                                                                 (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                                                                                                                                          (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                                                                                                                                   (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tlam (tvar "a")
                                                                                                                                                                                                                                                            (tvar "b")])
                                                                                                                                                                                                                                            (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])))))))
                                                                                                                                                                                         [int 1
                                                                                                                                                                                         ,(qvar "elm" "json" "Json.Decode" "float"
                                                                                                                                                                                                         (Forall (Map.fromList [])
                                                                                                                                                                                                                 (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Basics" "Float" []])))]))))
                                                                                                                                                                    (at (Call ((qvar "author" "project" "Evergreen" "atIndex"
                                                                                                                                                                                              (Forall (Map.fromList [(name "a"
                                                                                                                                                                                                                ,())
                                                                                                                                                                                                                ,(name "b"
                                                                                                                                                                                                                ,())])
                                                                                                                                                                                                      (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                                                                                                                               (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                                                                                                                        (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tlam (tvar "a")
                                                                                                                                                                                                                                                 (tvar "b")])
                                                                                                                                                                                                                                 (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])))))))
                                                                                                                                                                              [int 2
                                                                                                                                                                              ,(qvar "elm" "json" "Json.Decode" "bool"
                                                                                                                                                                                              (Forall (Map.fromList [])
                                                                                                                                                                                                      (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Basics" "Bool" []])))]))))
                                                                                                                                                         (at (Call ((qvar "author" "project" "Evergreen" "atIndex"
                                                                                                                                                                                   (Forall (Map.fromList [(name "a"
                                                                                                                                                                                                     ,())
                                                                                                                                                                                                     ,(name "b"
                                                                                                                                                                                                     ,())])
                                                                                                                                                                                           (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                                                                                                                    (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                                                                                                             (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tlam (tvar "a")
                                                                                                                                                                                                                                      (tvar "b")])
                                                                                                                                                                                                                      (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])))))))
                                                                                                                                                                   [int 3
                                                                                                                                                                   ,(qvar "author" "project" "Evergreen" "d_char"
                                                                                                                                                                                   (Forall (Map.fromList [])
                                                                                                                                                                                           (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Char" "Char" []])))]))))
                                                                                                                                              (at (Call ((qvar "author" "project" "Evergreen" "atIndex"
                                                                                                                                                                        (Forall (Map.fromList [(name "a"
                                                                                                                                                                                          ,())
                                                                                                                                                                                          ,(name "b"
                                                                                                                                                                                          ,())])
                                                                                                                                                                                (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                                                                                                         (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                                                                                                  (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tlam (tvar "a")
                                                                                                                                                                                                                           (tvar "b")])
                                                                                                                                                                                                           (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])))))))
                                                                                                                                                        [int 4
                                                                                                                                                        ,(qvar "elm" "json" "Json.Decode" "string"
                                                                                                                                                                        (Forall (Map.fromList [])
                                                                                                                                                                                (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "String" "String" []])))]))))
                                                                                                                                   (at (Call ((qvar "author" "project" "Evergreen" "atIndex"
                                                                                                                                                             (Forall (Map.fromList [(name "a"
                                                                                                                                                                               ,())
                                                                                                                                                                               ,(name "b"
                                                                                                                                                                               ,())])
                                                                                                                                                                     (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                                                                                              (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                                                                                       (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tlam (tvar "a")
                                                                                                                                                                                                                (tvar "b")])
                                                                                                                                                                                                (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])))))))
                                                                                                                                             [int 5
                                                                                                                                             ,at (Call ((qvar "elm" "json" "Json.Decode" "list"
                                                                                                                                                                       (Forall (Map.fromList [(name "a"
                                                                                                                                                                                         ,())])
                                                                                                                                                                               (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                                                                                        (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "List" "List" [tvar "a"]])))))
                                                                                                                                                       [(qvar "elm" "json" "Json.Decode" "int"
                                                                                                                                                                       (Forall (Map.fromList [])
                                                                                                                                                                               (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Basics" "Int" []])))])]))))
                                                                                                                        (at (Call ((qvar "author" "project" "Evergreen" "atIndex"
                                                                                                                                                  (Forall (Map.fromList [(name "a"
                                                                                                                                                                    ,())
                                                                                                                                                                    ,(name "b"
                                                                                                                                                                    ,())])
                                                                                                                                                          (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                                                                                   (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                                                                            (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tlam (tvar "a")
                                                                                                                                                                                                     (tvar "b")])
                                                                                                                                                                                     (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])))))))
                                                                                                                                  [int 6
                                                                                                                                  ,at (Call ((qvar "author" "project" "Evergreen" "d_set"
                                                                                                                                                            (Forall (Map.fromList [(name "comparable"
                                                                                                                                                                              ,())])
                                                                                                                                                                    (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "comparable"])
                                                                                                                                                                             (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Set" "Set" [tvar "comparable"]])))))
                                                                                                                                            [(qvar "elm" "json" "Json.Decode" "float"
                                                                                                                                                            (Forall (Map.fromList [])
                                                                                                                                                                    (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Basics" "Float" []])))])]))))
                                                                                                             (at (Call ((qvar "author" "project" "Evergreen" "atIndex"
                                                                                                                                       (Forall (Map.fromList [(name "a"
                                                                                                                                                         ,())
                                                                                                                                                         ,(name "b"
                                                                                                                                                         ,())])
                                                                                                                                               (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                                                                        (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                                                                 (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tlam (tvar "a")
                                                                                                                                                                                          (tvar "b")])
                                                                                                                                                                          (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])))))))
                                                                                                                       [int 7
                                                                                                                       ,at (Call ((qvar "elm" "json" "Json.Decode" "array"
                                                                                                                                                 (Forall (Map.fromList [(name "a"
                                                                                                                                                                   ,())])
                                                                                                                                                         (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                                                                  (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Array" "Array" [tvar "a"]])))))
                                                                                                                                 [(qvar "elm" "json" "Json.Decode" "string"
                                                                                                                                                 (Forall (Map.fromList [])
                                                                                                                                                         (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "String" "String" []])))])]))))
                                                                                                  (at (Call ((qvar "author" "project" "Evergreen" "atIndex"
                                                                                                                            (Forall (Map.fromList [(name "a"
                                                                                                                                              ,())
                                                                                                                                              ,(name "b"
                                                                                                                                              ,())])
                                                                                                                                    (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                                                             (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                                                      (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tlam (tvar "a")
                                                                                                                                                                               (tvar "b")])
                                                                                                                                                               (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])))))))
                                                                                                            [int 8
                                                                                                            ,at (Call ((qvar "author" "project" "Evergreen" "d_dict"
                                                                                                                                      (Forall (Map.fromList [(name "comparable"
                                                                                                                                                        ,())
                                                                                                                                                        ,(name "v"
                                                                                                                                                        ,())])
                                                                                                                                              (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "comparable"])
                                                                                                                                                       (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "v"])
                                                                                                                                                                (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Dict" "Dict" [tvar "comparable"
                                                                                                                                                                              ,tvar "v"]]))))))
                                                                                                                      [(qvar "elm" "json" "Json.Decode" "string"
                                                                                                                                      (Forall (Map.fromList [])
                                                                                                                                              (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "String" "String" []])))
                                                                                                                      ,at (Call ((qvar "elm" "json" "Json.Decode" "list"
                                                                                                                                                (Forall (Map.fromList [(name "a"
                                                                                                                                                                  ,())])
                                                                                                                                                        (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                                                                 (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "List" "List" [tvar "a"]])))))
                                                                                                                                [(qvar "elm" "json" "Json.Decode" "int"
                                                                                                                                                (Forall (Map.fromList [])
                                                                                                                                                        (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Basics" "Int" []])))])])]))))
                                                                                       (at (Call ((qvar "author" "project" "Evergreen" "atIndex"
                                                                                                                 (Forall (Map.fromList [(name "a"
                                                                                                                                   ,())
                                                                                                                                   ,(name "b"
                                                                                                                                   ,())])
                                                                                                                         (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                                                  (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                                           (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tlam (tvar "a")
                                                                                                                                                                    (tvar "b")])
                                                                                                                                                    (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])))))))
                                                                                                 [int 9
                                                                                                 ,(qvar "author" "project" "Evergreen" "d_time"
                                                                                                                 (Forall (Map.fromList [])
                                                                                                                         (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "time" "Time" "Posix" []])))]))))
                                                                            (at (Call ((qvar "author" "project" "Evergreen" "atIndex"
                                                                                                      (Forall (Map.fromList [(name "a"
                                                                                                                        ,())
                                                                                                                        ,(name "b"
                                                                                                                        ,())])
                                                                                                              (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                                       (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                                (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tlam (tvar "a")
                                                                                                                                                         (tvar "b")])
                                                                                                                                         (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])))))))
                                                                                      [int 10
                                                                                      ,(qvar "author" "project" "Evergreen" "d_order"
                                                                                                      (Forall (Map.fromList [])
                                                                                                              (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Basics" "Order" []])))]))))
                                                                 (at (Call ((qvar "author" "project" "Evergreen" "atIndex"
                                                                                           (Forall (Map.fromList [(name "a"
                                                                                                             ,())
                                                                                                             ,(name "b"
                                                                                                             ,())])
                                                                                                   (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                            (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                                     (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tlam (tvar "a")
                                                                                                                                              (tvar "b")])
                                                                                                                              (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])))))))
                                                                           [int 11
                                                                           ,at (VarTopLevel (canonical "author" "project" "AllTypes")
                                                                                            (name "evg_d_Union"))]))))
                                                      (at (Call ((qvar "author" "project" "Evergreen" "atIndex"
                                                                                (Forall (Map.fromList [(name "a"
                                                                                                  ,())
                                                                                                  ,(name "b"
                                                                                                  ,())])
                                                                                        (tlam (qtyp "elm" "core" "Basics" "Int" [])
                                                                                                 (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                                                                                          (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tlam (tvar "a")
                                                                                                                                   (tvar "b")])
                                                                                                                   (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])))))))
                                                                [int 12
                                                                ,at (Call ((qvar "elm" "json" "Json.Decode" "null"
                                                                                          (Forall (Map.fromList [(name "a"
                                                                                                            ,())])
                                                                                                  (tlam (tvar "a")
                                                                                                           (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])))))
                                                                          [at Unit])]))))
                                           (qtyp "elm" "json" "Json.Decode" "Decoder" [TAlias (canonical "author" "project" "AllTypes")
                                                          (name "AllTypes")
                                                          []
                                                          (Holey (TRecord (Map.fromList [(name "arrayString"
                                                                                    ,FieldType 7 (qtyp "elm" "core" "Array" "Array" [qtyp "elm" "core" "String" "String" []]))
                                                                                    ,(name "bool"
                                                                                    ,FieldType 2 (qtyp "elm" "core" "Basics" "Bool" []))
                                                                                    ,(name "char"
                                                                                    ,FieldType 3 (qtyp "elm" "core" "Char" "Char" []))
                                                                                    ,(name "dict"
                                                                                    ,FieldType 8 (qtyp "elm" "core" "Dict" "Dict" [qtyp "elm" "core" "String" "String" []
                                                                                                        ,qtyp "elm" "core" "List" "List" [qtyp "elm" "core" "Basics" "Int" []]]))
                                                                                    ,(name "float"
                                                                                    ,FieldType 1 (qtyp "elm" "core" "Basics" "Float" []))
                                                                                    ,(name "int"
                                                                                    ,FieldType 0 (qtyp "elm" "core" "Basics" "Int" []))
                                                                                    ,(name "listInt"
                                                                                    ,FieldType 5 (qtyp "elm" "core" "List" "List" [qtyp "elm" "core" "Basics" "Int" []]))
                                                                                    ,(name "order"
                                                                                    ,FieldType 10 (qtyp "elm" "core" "Basics" "Order" []))
                                                                                    ,(name "setFloat"
                                                                                    ,FieldType 6 (qtyp "elm" "core" "Set" "Set" [qtyp "elm" "core" "Basics" "Float" []]))
                                                                                    ,(name "string"
                                                                                    ,FieldType 4 (qtyp "elm" "core" "String" "String" []))
                                                                                    ,(name "time"
                                                                                    ,FieldType 9 (qtyp "elm" "time" "Time" "Posix" []))
                                                                                    ,(name "union"
                                                                                    ,FieldType 11 (qtyp "author" "project" "AllTypes" "Union" []))
                                                                                    ,(name "unit"
                                                                                    ,FieldType 12 TUnit)])
                                                                          Nothing))]))
                                 (Declare (Def (named "evg")
                                               []
                                               (int 123))
                                          SaveTheEnvironment))))
