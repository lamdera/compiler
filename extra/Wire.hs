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


-- modify canonical flag pkg importDict interfaces source =
--   canonical

-- Our injection point POC for AllTypes. Search for `Wire.modify`
modify canonical flag pkg importDict interfaces source =
  case canonical of
    Module name docs exports decls unions aliases binops effects ->
      case name of
        Canonical pkg n ->
          if N.toString n == "AllTypes_Gen" then

            -- tracef ("-" ++ N.toString n) valid
            -- tracef ("-" ++ N.toString n) (canonical { _decls = [ encoder, decoder, evg_e_Union, evg_d_Union ] })
            tracef ("-" ++ N.toString n) canonical

          else if N.toString n == "AllTypes" then

            -- tracef ("-" ++ N.toString n) canonical


            tracef ("-" ++ N.toString n) (canonical { _decls =
              case _decls canonical of
                DeclareRec d x ->

                  -- Use this one when we want to see the original schema
                  -- DeclareRec (d ++ [ staticX ]) x

                  -- Use this one otherwise
                  funtimes

                  -- Canary test
                  -- DeclareRec [] x

                d -> d
            })

          else
            canonical




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


-- unionCase matches WIP breakouts



_u_alts_ =
  [Ctor (name "Recursive")
        (ZeroBased 0)
        1 [qtyp "author" "project" "AllTypes" "Union" []]
  ,Ctor (name "Valued")
        (ZeroBased 1)
        1 [qtyp "elm" "core" "Basics" "Int" []]
  ,Ctor (name "DeeplyValued")
        (ZeroBased 2)
        1 [qtyp "elm" "core" "List" "List" [qtyp "elm" "core" "Basics" "Bool" []]]
  ,Ctor (name "Leaf")
        (ZeroBased 3)
        0 []]


_p_union_ =
  Union {_u_vars = []
        ,_u_alts = _u_alts_
        ,_u_numAlts = 4
        ,_u_opts = Normal}


_ucasebranch_ index unionLabel unionArgs expr =
  CaseBranch (at (PCtor {_p_home = canonical "author" "project" "AllTypes"
                         ,_p_type = name "Union"
                         ,_p_union = _p_union_
                         ,_p_name = name unionLabel
                         ,_p_index = ZeroBased index
                         ,_p_args = unionArgs}))
              expr


funtimes =
    DeclareRec [TypedDef (named "evg_e_Union")
             (Map.fromList [])
             [(at (PVar (name "evg_p0"))
             ,qtyp "author" "project" "AllTypes" "Union" [])]
             (at (Case (at
                       (VarLocal (name "evg_p0")))
                   [

                    _ucasebranch_ 0 "Recursive"
                      ([PatternCtorArg {_index = ZeroBased 0
                                                 ,_type = qtyp "author" "project" "AllTypes" "Union" []
                                                 ,_arg = at (PVar (name "evg_v0"))}])
                      (at (Call jsonEncodeList
                                 [coreBasicsIdentity
                                 ,at (List [call jsonEncodeString [str "Recursive"]
                                           ,call (at (VarTopLevel (canonical "author" "project" "AllTypes")
                                                                      (name "evg_e_Union")))
                                                     [at (VarLocal (name "evg_v0"))]])]))

                   ,_ucasebranch_ 1 "Valued"
                   ([PatternCtorArg {_index = ZeroBased 0
                                              ,_type = qtyp "elm" "core" "Basics" "Int" []
                                              ,_arg = at (PVar (name "evg_v0"))}])
                    (at
                        (Call jsonEncodeList
                              [coreBasicsIdentity
                              ,at (List [at (Call jsonEncodeString [str "Valued"])
                                        ,at (Call jsonEncodeInt
                                                  [at (VarLocal (name "evg_v0"))])])]))

                   ,_ucasebranch_ 2 "DeeplyValued"
                    ([PatternCtorArg {_index = ZeroBased 0
                                              ,_type = qtyp "elm" "core" "List" "List"
                                                             [qtyp "elm" "core" "Basics" "Bool"
                                                                    []]
                                              ,_arg = at
                     (PVar (name "evg_v0"))}])
                     (at
                         (Call jsonEncodeList
                               [coreBasicsIdentity
                               ,at (List [at (Call jsonEncodeString [str "DeeplyValued"])
                                         ,at
                                             (Call jsonEncodeList
                                                   [jsonEncodeBool
                                                   ,at (VarLocal (name "evg_v0"))])])]))

                   ,_ucasebranch_ 3 "Leaf"
                   ([])
                   (at
                       (Call jsonEncodeList
                             [coreBasicsIdentity
                             ,at (List [at (Call jsonEncodeString [str "Leaf"])])]))

             ]))

             (qtyp "elm" "json" "Json.Encode" "Value" [])

    ,Def (named "evg") [] (int 123)]
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
