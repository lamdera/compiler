{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Lamdera.Wire.Helpers where

import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map
import Data.Map ((!?))
import qualified Data.List as List
import qualified Data.Graph as Graph

import Elm.Package
import qualified AST.Source as Src
import qualified Elm.Interface as I
import qualified Elm.ModuleName as Module
import qualified Elm.Package as Pkg
import qualified AST.Canonical as Can
import AST.Canonical
import qualified Data.Name
import qualified Data.Utf8 as Utf8
import qualified Reporting.Annotation as A
import qualified Reporting.Error as E
import qualified Reporting.Doc as D
import qualified Data.Text as T
import qualified Data.Index as Index

import qualified Wire.Source2 as Source2

import Lamdera
import StandaloneInstances
import qualified CanSer.CanSer as ToSource


{- NOTE: Any recursive usage of these types in user-code will get caught in the TypeDiff,
the mapping there has been checked extensively against types in packages that are backed by Kernel.

But we still need to know about them in order to create the right wire encoder/decoder injections
-}
isUnsupportedKernelType tipe =
  case tipe of
    TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" _ -> True
    TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" _ -> True
  -- , (("elm/bytes", "Bytes") "Endianness" _ -> True
    TType (Module.Canonical (Name "elm" "virtualdom") "VirtualDom") "Node" _ -> True
    TType (Module.Canonical (Name "elm" "virtualdom") "VirtualDom") "Attribute" _ -> True
    TType (Module.Canonical (Name "elm" "virtualdom") "VirtualDom") "Handler" _ -> True
  -- Disable for now, but need to revisit these and whether we want actual proper wire support
  -- , (("elm/browser", "Browser") "UrlRequest" _ -> True
  -- , (("elm/browser", "Browser.Navigation") "Key" _ -> True
    TType (Module.Canonical (Name "elm" "file") "File") "File" _ -> True
  -- not implemented yet, but needed by other pkgs
    TType (Module.Canonical (Name "elm" "core") "Process") "Id" _ -> True -- alias of Platform.ProcessId
    TType (Module.Canonical (Name "elm" "core") "Platform") "ProcessId" _ -> True -- time
  -- not needed by anything immediately, but we don't know how to encode these anyway, so let's fail them now
    TType (Module.Canonical (Name "elm" "core") "Platform") "Program" _ -> True -- idk
    TType (Module.Canonical (Name "elm" "core") "Platform") "Router" _ -> True -- idk
    TType (Module.Canonical (Name "elm" "core") "Platform") "Task" _ -> True -- idk
    TType (Module.Canonical (Name "elm" "core") "Platform.Cmd") "Cmd" _ -> True -- idk
    TType (Module.Canonical (Name "elm" "core") "Platform.Sub") "Sub" _ -> True
  -- elm/json
    TType (Module.Canonical (Name "elm" "json") "Json.Encode") "Value" _ -> True -- js type
    TType (Module.Canonical (Name "elm" "json") "Json.Decode") "Decoder" _ -> True -- js type
    TType (Module.Canonical (Name "elm" "json") "Json.Decode") "Value" _ -> True -- js type
  -- elm/core
    TType (Module.Canonical (Name "elm" "core") "Task") "Task" _ -> True -- js type
    _ -> False






foreignTypeTvars :: Module.Raw -> Data.Name.Name -> Map.Map Module.Raw I.Interface -> [Data.Name.Name]
foreignTypeTvars module_ typeName ifaces =
  case ifaces & Map.lookup module_ of
    Just iface ->
      case I._unions iface !? typeName of
        Just union -> unionTvars union
        Nothing ->
          case I._aliases iface !? typeName of
            Just alias -> aliasTvars alias
            Nothing -> []

    Nothing ->
      []

unionTvars union =
  case union of
    I.OpenUnion union_ -> _u_vars union_
    I.ClosedUnion union_ -> _u_vars union_
    I.PrivateUnion union_ -> _u_vars union_

aliasTvars alias =
  case alias of
    I.PublicAlias (Alias tvars tipe) -> tvars
    I.PrivateAlias (Alias tvars tipe) -> tvars


{- Equivalent of writing `functionName = Debug.todo "functionName"` in Elm -}
namedTodo :: Src.Module -> Data.Name.Name -> Def
namedTodo modul functionName =
  let functionName_ = Utf8.fromChars . Data.Name.toChars $ functionName
      moduleName = Src.getName modul
  in
   Def
      (a (functionName))
      []
      (a (Call
            (a (VarDebug
                  (Module.Canonical (Name "author" "project") moduleName)
                  "todo"
                  (Forall (Map.fromList [("a", ())]) (TLambda (TType (Module.Canonical (Name "elm" "core") "String") "String" []) (TVar "a")))))
            [(a (Str functionName_))]))


a v =
  A.at (A.Position 0 0) (A.Position 0 10) v


encodeSequenceWithoutLength list =
  (a (Call (a (VarForeign mLamdera_Wire2 "encodeSequenceWithoutLength"
              (Forall
                 Map.empty
                 (TLambda
                    (TType
                       (Module.Canonical (Name "elm" "core") "List")
                       "List"
                       [ tLamdera_Wire2__Encoder
                       ])
                    tLamdera_Wire2__Encoder))))
        [list]))


encodeUnsignedInt8 value =
  (a (Call (a (VarForeign mLamdera_Wire2 "encodeUnsignedInt8"
                (Forall
                   Map.empty
                   (TLambda
                      (TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])
                      tLamdera_Wire2__Encoder))))
          [value]))


decodeUnsignedInt8 =
  (a (VarForeign mLamdera_Wire2 "decodeUnsignedInt8"
        (Forall
           Map.empty
           (TAlias
              mLamdera_Wire2
              "Decoder"
              [("a", TType (Module.Canonical (Name "elm" "core") "Basics") "Int" [])]
              (Filled
                 (TType
                    (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                    "Decoder"
                    [TType (Module.Canonical (Name "elm" "core") "Basics") "Int" []]))))))


andThenDecode1 lambda =
  (a (Call (a (VarForeign mLamdera_Wire2 "andThenDecode"
              (Forall
                 (Map.fromList [("a", ()), ("b", ())])
                 (TLambda
                    (TLambda
                       (TVar "a")
                       (TAlias
                          mLamdera_Wire2
                          "Decoder"
                          [("a", TVar "b")]
                          (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "b"]))))
                    (TLambda
                       (TAlias
                          mLamdera_Wire2
                          "Decoder"
                          [("a", TVar "a")]
                          (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                       (TAlias
                          mLamdera_Wire2
                          "Decoder"
                          [("a", TVar "b")]
                          (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "b"]))))))))
        [ lambda
        ]))


andMapDecode1 value =
  (a (Call
        (a (VarForeign
              (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
              "andMapDecode"
              (Forall
                 (Map.fromList [("a", ()), ("b", ())])
                 (TLambda
                    (TAlias
                       (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                       "Decoder"
                       [("a", TVar "a")]
                       (Filled
                          (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))
                    (TLambda
                       (TAlias
                          (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                          "Decoder"
                          [("a", TLambda (TVar "a") (TVar "b"))]
                          (Filled
                             (TType
                                (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                "Decoder"
                                [TLambda (TVar "a") (TVar "b")])))
                       (TAlias
                          (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                          "Decoder"
                          [("a", TVar "b")]
                          (Filled
                             (TType
                                (Module.Canonical (Name "elm" "bytes") "Bytes.Decode")
                                "Decoder"
                                [TVar "b"]))))))))
        [ value
        ]))

succeedDecode value =
  (a (Call (a (VarForeign mLamdera_Wire2 "succeedDecode"
              (Forall
                 (Map.fromList [("a", ())])
                 (TLambda
                    (TVar "a")
                    (TAlias
                       mLamdera_Wire2
                       "Decoder"
                       [("a", TVar "a")]
                       (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"])))))))
        [ value
        ]))


failDecode =
  (a (VarForeign mLamdera_Wire2 "failDecode"
        (Forall
           (Map.fromList [("a", ())])
           (TAlias
              mLamdera_Wire2
              "Decoder"
              [("a", TVar "a")]
              (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Decode") "Decoder" [TVar "a"]))))))

failEncode =
   (a (VarForeign
         (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
         "failEncode"
         (Forall
            (Map.fromList [("a", ())])
            (TLambda
               (TVar "a")
               (TAlias
                  (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")
                  "Encoder"
                  []
                  (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])))))))

int value =
  a (Int value)

str value =
  a (Str value)

list values =
  a (List values)

lambda1 pattern expr =
  a (Lambda [ pattern ] expr)

caseof pattern branches =
  a (Case pattern branches)

(–>) pattern expr =
  CaseBranch pattern expr

infixr 0 –>

(|>) expr1 expr2 =
  (a (Binop
        "|>"
        (Module.Canonical (Name "elm" "core") "Basics")
        "apR"
        (Forall (Map.fromList [("a", ()), ("b", ())]) (TLambda (TVar "a") (TLambda (TLambda (TVar "a") (TVar "b")) (TVar "b"))))
        expr1
        expr2
     )
   )

infixr 0 |>

lvar n =
  a (VarLocal n)


-- Patterns

pint i =
  a (PInt i)

pvar n =
  a (PVar n)

p_ =
  a (PAnything)

call fn args =
  (a (Call fn args))


tLamdera_Wire2__Encoder =
  (TAlias
     mLamdera_Wire2
     "Encoder"
     []
       (Filled (TType (Module.Canonical (Name "elm" "bytes") "Bytes.Encode") "Encoder" [])))


mLamdera_Wire2 =
  (Module.Canonical (Name "lamdera" "codecs") "Lamdera.Wire2")


foldlPairs fn list =
  case list of
    [] -> error "Error: foldlPairs called with no items! Please report this with your code."
    x:[] -> x
    x:xs ->
      foldl (\acc item -> fn acc item ) x xs

foldrPairs fn list =
  case list of
    [] -> error "Error: foldrPairs called with no items! Please report this with your code."
    x:[] -> x
    x:xs ->
      fn x (foldrPairs fn xs)
