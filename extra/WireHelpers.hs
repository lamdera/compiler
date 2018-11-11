{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module WireHelpers where

import Reporting.Annotation (Located(..))
import Reporting.Region
import qualified Elm.Name as N
import AST.Canonical
import AST.Module.Name (Canonical(..))
import Elm.Package (Name(..))


import qualified Data.Map as Map

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


-- Local record field access
rfield r f = at (Access (at (VarLocal (name r))) (named f))


-- Helpful shortcut definitions for Evergreen derivations

-- JSON.Encode

jsonEncodeInt =
  ((qvar "elm" "json" "Json.Encode" "int"
    (Forall (Map.fromList [])
            (tlam (qtyp "elm" "core" "Basics" "Int" [])
                     (qtyp "elm" "json" "Json.Encode" "Value" [])))))


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


jsonEncodeString =
  ((qvar "elm" "json" "Json.Encode" "string"
      (Forall (Map.fromList [])
              (tlam (qtyp "elm" "core" "String" "String" [])
                    (qtyp "elm" "json" "Json.Encode" "Value" [])))))


jsonEncodeList =
   ((qvar "elm" "json" "Json.Encode" "list"
       (Forall (Map.fromList [(name "a" ,())])
               (tlam (tlam (tvar "a")
                           (qtyp "elm" "json" "Json.Encode" "Value" []))
                     (tlam (qtyp "elm" "core" "List" "List" [tvar "a"])
                           (qtyp "elm" "json" "Json.Encode" "Value" [])))
       )))


jsonEncodeArray =
   ((qvar "elm" "json" "Json.Encode" "array"
       (Forall (Map.fromList [(name "a" ,())])
               (tlam (tlam (tvar "a")
                           (qtyp "elm" "json" "Json.Encode" "Value" []))
                     (tlam (qtyp "elm" "core" "Array" "Array" [tvar "a"])
                           (qtyp "elm" "json" "Json.Encode" "Value" [])))
       )))


jsonEncodeSet =
   ((qvar "elm" "json" "Json.Encode" "set"
       (Forall (Map.fromList [(name "a" ,())])
               (tlam (tlam (tvar "a")
                           (qtyp "elm" "json" "Json.Encode" "Value" []))
                     (tlam (qtyp "elm" "core" "Set" "Set" [tvar "a"])
                           (qtyp "elm" "json" "Json.Encode" "Value" [])))
       )))


-- JSON.Decode

jsonDecodeSucceed =
  (qvar "elm" "json" "Json.Decode" "succeed"
    (Forall (Map.fromList [(name "a" ,())])
      (tlam (tvar "a")
        (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"]))))


jsonDecodeInt =
  (qvar "elm" "json" "Json.Decode" "int"
    (Forall (Map.fromList [])
            (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Basics" "Int" []])))


jsonDecodeFloat =
  (qvar "elm" "json" "Json.Decode" "float"
    (Forall (Map.fromList [])
            (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Basics" "Float" []])))


jsonDecodeBool =
  (qvar "elm" "json" "Json.Decode" "bool"
    (Forall (Map.fromList [])
      (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Basics" "Bool" []])))


jsonDecodeString =
  (qvar "elm" "json" "Json.Decode" "string"
    (Forall (Map.fromList [])
            (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "String" "String" []])))


jsonDecodeList decoder =
  at (Call ((qvar "elm" "json" "Json.Decode" "list"
                            (Forall (Map.fromList [(name "a" ,())])
                                    (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                             (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "List" "List" [tvar "a"]])))))
            [decoder])


jsonDecodeArray decoder =
  at (Call ((qvar "elm" "json" "Json.Decode" "array"
                            (Forall (Map.fromList [(name "a" ,())])
                                    (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                             (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Array" "Array" [tvar "a"]])))))
            [decoder])


jsonDecodeLazy1Ignore decoder =
  at (Call ((qvar "elm" "json" "Json.Decode" "lazy"
                            (Forall (Map.fromList [(name "a"
                                              ,())])
                                    (tlam (tlam TUnit (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"]))
                                             (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])))))
            [at (Lambda [at PAnything]
                        decoder)])


-- Core.Basics

coreBasicsIdentity =
  (qvar "elm" "core" "Basics" "identity"
                  (Forall (Map.fromList [(name "a"
                                    ,())])
                          (tlam (tvar "a")
                                (tvar "a"))))


-- Evergreen Encoders

-- @TODO should be evergreenDecodeUnion?
evergreenUnion =
  ((qvar "author" "project" "Evergreen" "union"
    (Forall (Map.fromList [(name "a"
                      ,())])
            (tlam (qtyp "elm" "core" "String" "String" [])
                     (tlam (tvar "a")
                              (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"]))))))


evergreenDecodeUnion1 =
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


evergreenDecodeUnion2 =
  ((qvar "author" "project" "Evergreen" "union2"
    (Forall (Map.fromList [(name "a" ,())
                          ,(name "b" ,())
                          ,(name "c" ,())]
                          )
            (tlam (qtyp "elm" "core" "String" "String" [])
                (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                        (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"])
                              (tlam (tlam (tvar "a") (tlam (tvar "b") (tvar "c")))
                                       (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "c"]))))))))


evergreenEncodeChar =
  ((qvar "author" "project" "Evergreen" "e_char"
                            (Forall (Map.fromList [])
                                    (tlam (qtyp "elm" "core" "Char" "Char" [])
                                             (qtyp "elm" "json" "Json.Encode" "Value" [])))))


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


evergreenEncodeTime =
  ((qvar "author" "project" "Evergreen" "e_time"
          (Forall (Map.fromList [])
                  (tlam (qtyp "elm" "time" "Time" "Posix" [])
                           (qtyp "elm" "json" "Json.Encode" "Value" [])))))


evergreenEncodeOrder =
  ((qvar "author" "project" "Evergreen" "e_order"
          (Forall (Map.fromList [])
                  (tlam (qtyp "elm" "core" "Basics" "Order" [])
                           (qtyp "elm" "json" "Json.Encode" "Value" [])))))


evergreenEncodeUnit =
  ((qvar "author" "project" "Evergreen" "e_unit"
          (Forall (Map.fromList [])
                  (qtyp "elm" "json" "Json.Encode" "Value" []))))


-- Evergreen Decoders


evergreenDecodeChar =
  (qvar "author" "project" "Evergreen" "d_char"
    (Forall (Map.fromList [])
            (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Char" "Char" []])))


evergreenDecodeOrder =
  (qvar "author" "project" "Evergreen" "d_order"
    (Forall (Map.fromList [])
            (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Basics" "Order" []])))


evergreenDecodeSet decoder =
  at (Call ((qvar "author" "project" "Evergreen" "d_set"
                            (Forall (Map.fromList [(name "comparable"
                                              ,())])
                                    (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "comparable"])
                                             (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Set" "Set" [tvar "comparable"]])))))
            [decoder])


evergreenDecodeDict keyDecoder valueDecoder =
  at (Call ((qvar "author" "project" "Evergreen" "d_dict"
            (Forall (Map.fromList [(name "comparable" ,())
                              ,(name "v" ,())])
                    (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "comparable"])
                             (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "v"])
                                      (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Dict" "Dict" [tvar "comparable"
                                                    ,tvar "v"]]))))))
            [keyDecoder, valueDecoder])


evergreenDecodeTime =
  (qvar "author" "project" "Evergreen" "d_time"
    (Forall (Map.fromList [])
            (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "time" "Time" "Posix" []])))


evergreenDecodeUnit =
  (qvar "author" "project" "Evergreen" "d_unit"
          (Forall (Map.fromList [])
                  (qtyp "elm" "json" "Json.Decode" "Decoder" [TUnit])))


evergreenAtIndex =
  (qvar "author" "project" "Evergreen" "atIndex"
    (Forall (Map.fromList [(name "a" ,()) ,(name "b" ,())])
      (tlam (qtyp "elm" "core" "Basics" "Int" [])
         (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
            (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tlam (tvar "a") (tvar "b")])
               (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "b"]))))))
