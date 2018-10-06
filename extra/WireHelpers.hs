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



-- Helpful shortcut definitions for Evergreen derivations

-- JSON.Encode

jsonEncodeList =
  ((qvar "elm" "json" "Json.Encode" "list"
      (Forall (Map.fromList [(name "a" ,())])
              (tlam (tlam (tvar "a")
                          (qtyp "elm" "json" "Json.Encode" "Value" []))
                    (tlam (qtyp "elm" "core" "List" "List" [tvar "a"])
                          (qtyp "elm" "json" "Json.Encode" "Value" [])))
      )))


jsonDecodeList decoder =
  at (Call ((qvar "elm" "json" "Json.Decode" "list"
                            (Forall (Map.fromList [(name "a" ,())])
                                    (tlam (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])
                                             (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "List" "List" [tvar "a"]])))))
            [decoder])


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


jsonDecodeBool =
  (qvar "elm" "json" "Json.Decode" "bool"
    (Forall (Map.fromList [])
      (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "elm" "core" "Basics" "Bool" []])))


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
