{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wire where

import Reporting.Annotation (Located(..))
import Reporting.Region
import qualified Elm.Name as N

import AST.Canonical
import AST.Module.Name (Canonical(..))
import Elm.Package (Name(..))
import qualified Data.Map as Map
import Data.Index

import Control.Monad.Trans (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Prettyprint
import WireHelpers


imap f l = zipWith f [0..] l

{-

The main AST dynamic injection logic for Evergreen wire.

Overall todo items remaining∷

- Handle module `exposing (blah)` issues preventing auto-generated definitions from being imported by other modules
- Generic Encoder for records
- Generic Decoder for records
- Remove all references to AllTypes & make the module name dynamic based on context

-}


-- Our injection point POC for AllTypes. Search for `Wire.modify`
modifyCanonical canonical flag pkg importDict interfaces source =
  case canonical of
    Module name docs exports decls customTypes aliases binops effects ->
      case name of
        Canonical pkg n ->
          case N.toString n of
            "AllTypes_Gen" ->

              -- tracef ("-" ++ N.toString n) valid
              -- tracef ("-" ++ N.toString n) (canonical { _decls = [ encoder, decoder, evg_e_Union, evg_d_Union ] })
              tracef ("-" ++ N.toString n) canonical

            "AllTypes" -> do
              -- Keeping this branch for the moment as the test tracking file AllTypes.elm
              -- eventually when everything is done this will be removed and we'll not need to pattern match


              -- tracef ("-" ++ N.toString n) canonical

              let customTypeEncoders = fmap customTypeToEncoder $ Map.toList customTypes
              let customTypeDecoders = fmap customTypeToDecoder $ Map.toList customTypes
              -- let recordEncoders = evg_e_AllTypes

              tracef ("-" ++ N.toString n) (canonical {
                _decls =
                  -- @TODO make a helper that lets us deal with this recursive type more generically, i.e.
                  -- by being able to just pass it a list of ASTs & not caring about how it's transformed
                  DeclareRec
                    (customTypeEncoders ++ customTypeDecoders ++ evg_e_AllTypes)
                    (Declare evg_d_AllTypes SaveTheEnvironment)
              })

              -- tracef ("-" ++ N.toString n) (canonical { _decls =
              --   case _decls canonical of
              --     DeclareRec d x ->
              --
              --       -- Use this one when we want to see the original schema
              --       -- DeclareRec d x
              --
              --       -- Use this one otherwise
              --       funtimes
              --
              --       -- Canary test
              --       -- DeclareRec [] x
              --
              --     d -> d
              -- })

            _ -> do
              -- This will be the final implementation as we converge to it
              let customTypeEncoders = fmap customTypeToEncoder $ Map.toList customTypes
              let customTypeDecoders = fmap customTypeToDecoder $ Map.toList customTypes
              let existingDecls = _decls canonical

              -- Add declarations for our generated encoders/decoders in addition to any existing declarations
              canonical { _decls = DeclareRec (customTypeEncoders ++ customTypeDecoders) existingDecls }



customTypeToEncoder (customTypeName_, customType_) = do

  let
    _encoderName = "evg_e_" ++ N.toString customTypeName_

    _genCustomType0 _index ctor =
      case ctor of
        Ctor n index numParams params ->
          let _tagNameT = N.toText n
              _tagNameS = N.toString n
          in
          customTypeCaseBranch customTypeName_ customType_ _index _tagNameS
            ([])
            (call jsonEncodeList
              [ coreBasicsIdentity
              , list [ call jsonEncodeString [ str _tagNameT ] ]
              ]
            )

    _genCustomType1 _index ctor =
      case ctor of
        Ctor n index numParams pTypes ->
          let _tagNameT = N.toText n
              _tagNameS = N.toString n
              _pType = head pTypes
          in
          customTypeCaseBranch customTypeName_ customType_ _index _tagNameS
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


    encoderForType pType =
      case pType of
        TType (Canonical (Name "elm" "core") "Basics") typeName next ->
          case N.toString typeName of
            "Int" ->  jsonEncodeInt
            "Float" -> jsonEncodeFloat
            "Bool" -> jsonEncodeBool
            "Order" -> evergreenEncodeOrder

            _ -> error $ "encoderForType Basics type didn't match any existing implementations: " ++ show pType

        TType (Canonical (Name "elm" "core") "Char") typeName next ->
          evergreenEncodeChar

        TType (Canonical (Name "elm" "core") "String") typeName next ->
          jsonEncodeString

        TType (Canonical (Name "elm" "core") "List") typeName next ->
          jsonEncodeList

        TType (Canonical (Name "elm" "core") "Array") typeName next ->
          jsonEncodeArray

        TType (Canonical (Name "elm" "core") "Set") typeName next ->
          jsonEncodeSet

        -- @TODO These are broken & need to be carefully reworked & fixed
        -- TType (Canonical (Name "elm" "core") "Dict") typeName next ->
        --   evergreenEncodeDict

        TType (Canonical (Name "elm" "time") "Time") typeName next ->
          case N.toString typeName of
            "Posix" ->
              evergreenEncodeTime

            _ -> error $ "encoderForType Time type didn't match any existing implementations: " ++ show pType

        TUnit ->
          evergreenEncodeUnit

        TType (Canonical (Name "author" "project") _) typeName next ->
          -- Any types from user, must have encoder ref in this file
          let _targetEncoderName = "evg_e_" ++ N.toString typeName
          in
          at (VarTopLevel (canonical "author" "project" "AllTypes") (name _targetEncoderName))

        TAlias (Canonical (Name "author" "project") _) typeName [] (Holey realType) ->
          encoderForType realType

        _ -> error $ "encoderForType didn't match any existing implementations: " ++ show pType



    encodeParamType pType vlocal1 =
      case pType of
        TType (Canonical (Name "elm" "core") "Basics") typeName next ->
          case N.toString typeName of
            "Int" ->  call jsonEncodeInt [vlocal1]
            "Float" -> call jsonEncodeFloat [vlocal1]
            "Bool" -> call jsonEncodeBool [vlocal1]
            "Order" -> call evergreenEncodeOrder [vlocal1]

            _ -> error $ "encodeParamType Basics type didn't match any existing implementations: " ++ show pType

        TType (Canonical (Name "elm" "core") "Char") typeName next ->
          call evergreenEncodeChar [vlocal1]

        TType (Canonical (Name "elm" "core") "String") typeName next ->
          call jsonEncodeString [vlocal1]

        TType (Canonical (Name "elm" "core") "List") typeName next ->
          call jsonEncodeList (encodeListType (head next) vlocal1)

        TType (Canonical (Name "elm" "core") "Array") typeName next ->
          call jsonEncodeArray (encodeListType (head next) vlocal1)

        TType (Canonical (Name "elm" "core") "Set") typeName next ->
          call jsonEncodeSet (encodeListType (head next) vlocal1)

        TType (Canonical (Name "elm" "core") "Dict") typeName next ->
          case next of
            first:second:rest ->
              call evergreenEncodeDict [tracef "dictEncFirst" $ encoderForType first, tracef "dictEncSecond" $ encoderForType second, vlocal1]

        TType (Canonical (Name "elm" "time") "Time") typeName next ->
          case N.toString typeName of
            "Posix" ->
              call evergreenEncodeTime [vlocal1]

            _ -> error $ "encodeParamType Time type didn't match any existing implementations: " ++ show pType

        TUnit ->
          call evergreenEncodeUnit []

        TType (Canonical (Name "author" "project") _) typeName next ->
          -- Any types from user, must have encoder ref in this file
          let _targetEncoderName = "evg_e_" ++ N.toString typeName

          in
          call (at (VarTopLevel (canonical "author" "project" "AllTypes")
                                     (name _targetEncoderName))) [vlocal "evg_v0"]


        TAlias (Canonical (Name "author" "project") _) typeName [] (Holey realType) ->
          encodeParamType realType vlocal1

        _ -> error $ "encodeParamType didn't match any existing implementations: " ++ show pType


    encodeListType pType vlocal1 =
      case pType of
        TType (Canonical {_package = Name {_author = "elm" ,_project = "core"} ,_module = "Basics"}) name next ->
          case N.toString name of
            "Int" -> [jsonEncodeInt, vlocal1]
            "Float" -> [jsonEncodeFloat, vlocal1]
            "Bool" -> [jsonEncodeBool, vlocal1]

            _ -> error $ "encodeListType Basics type didn't match any existing implementations: " ++ show pType

        TType (Canonical (Name "elm" "core") "Char") typeName next ->
          [evergreenEncodeChar, vlocal1]

        TType (Canonical (Name "elm" "core") "String") typeName next ->
          [jsonEncodeString, vlocal1]

        -- TType (Canonical (Name "elm" "core") "List") typeName next ->
        --   [jsonEncodeList, encodeListType (head next) vlocal1]
        --
        -- TType (Canonical (Name "elm" "core") "Array") typeName next ->
        --   [jsonEncodeArray, encodeListType (head next) vlocal1]
        --
        -- TType (Canonical (Name "elm" "core") "Set") typeName next ->
        --   [jsonEncodeSet, encodeListType (head next) vlocal1]

        -- TType (Canonical (Name "author" "project") _) typeName next ->
        --   -- Any types from user, must have encoder ref in this file
        --   let _targetEncoderName = "evg_e_" ++ N.toString typeName
        --
        --   in
        --   call (at (VarTopLevel (canonical "author" "project" "AllTypes") (name _targetEncoderName))) [vlocal "evg_v0"]


        TAlias (Canonical (Name "author" "project") _) typeName [] (Holey realType) ->
          [encodeParamType realType vlocal1, vlocal1]


        _ -> error $ "encodeListType didn't match any existing implementations: " ++ show pType


    _customTypeBranches =
      case customType_ of
        Union _u_vars _u_alts _u_numAlts _u_opts ->
          imap (\i ctor ->
            case ctor of
              Ctor n index numParams params ->
                case numParams of
                  0 -> _genCustomType0 i ctor
                  1 -> _genCustomType1 i ctor
                  -- @TODO need to add support for more depths of custom types
                  x -> undefined $ "Encoder: Custom Type parsing for " ++ show x ++ " params has not yet been implemented"
          ) _u_alts

    _customTypeNameS = N.toString customTypeName_

  TypedDef
    (named _encoderName)
    (Map.fromList [])
    [ (at (PVar (name "evg_p0"))
    , qtyp "author" "project" "AllTypes" _customTypeNameS [])
    ]
    (at (Case (vlocal "evg_p0") _customTypeBranches))
    (qtyp "elm" "json" "Json.Encode" "Value" [])


customTypeCaseBranch customTypeName customType index customTypeLabel customTypeArgs expr =
  CaseBranch
    (at (PCtor { _p_home = canonical "author" "project" "AllTypes"
               , _p_type = customTypeName
               , _p_union = customType
               , _p_name = name customTypeLabel
               , _p_index = ZeroBased index
               , _p_args = customTypeArgs
               }
        )
    )
    expr


{-

This function is responsible for generating the Elm decoder code for a given custom type
It needs to be generic across any possible custom type users are able to define
Right now here we've got the hardcoded desired AST representing the decoders we would want
to generate for the custom type called "Union" from extra/src/AllTypes.elm
We still need to

- Generalise it to work on any type of custom type value
- Generalise it to work for any number of custom type paramaters, i.e type Blah = Derp Int Int Int Int Int Int (q: how many is max?)
- Generalise it to remove reference to AllTypes explicitly and work generically across any module anywhere in the user's codebase, including core libs?

Once this is done, every single SomeModule that defines some CustomType will have a SomeModule.evg_d_CustomType automatically available.

We will be able to leverage this in our wrapper program as well by detecting the type the user is using for their SomeModel, and injecting evg_d_SomeModel
into the core runtime Elm code so it dynamically "write" the write boilerplate at compile time for us.

-}
customTypeToDecoder (customTypeName_, customType_) = do
  let
    _customTypeName = N.toString customTypeName_

    _decoderName = "evg_d_" ++ N.toString customTypeName_

    _genCustomType0 _index ctor =
      case ctor of
        Ctor n index numParams params ->
          let _tagNameT = N.toText n
              _tagNameS = N.toString n
          in
          at (Call evergreenUnion
            [ str _tagNameT
            , at (VarCtor Normal (canonical "author" "project" "AllTypes")
                (name _tagNameS)
                (ZeroBased _index)
                (Forall (Map.fromList []) (qtyp "author" "project" "AllTypes" _customTypeName []))
              )
            ]
          )


    {--

      Generates the following kind of code;

      EG.union1 "ValueInt" D.int ValueInt

      This is then used in the D.oneOf to allow us to decode custom types

    --}
    _genCustomType1 _index ctor =
      case ctor of
        Ctor n index numParams pTypes ->
          let _tagNameT = N.toText n
              _tagNameS = N.toString n
              constructor = generateConstructor _tagNameS _customTypeName (index) pTypes
          in
          call evergreenUnion1 (
            [str _tagNameT] ++ -- The tag name string to parse, i.e. "ValueInt"
            (fmap decodeParamType pTypes) ++ -- The decoder/s to be used (@TODO this sets us up for >1 param custom types?)
            [constructor] -- The constructor, i.e. ValueInt : Int -> Union
          )


    -- @TODO only partially implemented, needs to be extended for all possible types
    decodeParamType pType =
       case pType of
         TType (Canonical (Name "elm" "core") "Basics") typeName next ->
           case N.toString typeName of
             "Int" ->  jsonDecodeInt
             "Float" -> jsonDecodeFloat
             "Bool" -> jsonDecodeBool
             "Order" -> evergreenDecodeOrder

             _ -> error $ "decodeParamType Basics type didn't match any existing implementations: " ++ show pType

         TType (Canonical (Name "elm" "core") "Char") typeName next ->
           evergreenDecodeChar

         TType (Canonical (Name "elm" "core") "String") typeName next ->
           jsonDecodeString

         TType (Canonical (Name "elm" "time") "Time") typeName next ->
           case N.toString typeName of
             "Posix" ->
               evergreenDecodeTime

             _ -> error $ "encodeParamType Time type didn't match any existing implementations: " ++ show pType

         TType (Canonical (Name "elm" "core") "List") typeName next ->
           -- @TODO do we really need to do this? Can we just destructure inline above instead? How many other types does List expose?
           case N.toString typeName of
             "List" -> jsonDecodeList (decodeParamType (head next))

         TType (Canonical (Name "elm" "core") "Array") typeName next ->
           jsonDecodeArray (decodeParamType (head next))

         TType (Canonical (Name "elm" "core") "Set") typeName next ->
           evergreenDecodeSet (decodeParamType (head next))

         TType (Canonical (Name "elm" "core") "Dict") typeName next ->
           case next of
             first:second:rest ->
               evergreenDecodeDict (decodeParamType first) (decodeParamType second)

         TUnit ->
           evergreenDecodeUnit

         TType (Canonical (Name "author" "project") _) typeName next ->
           let _targetDecoderName = "evg_d_" ++ N.toString typeName
               _targetDecoder = at (VarTopLevel (canonical "author" "project" "AllTypes") (name _targetDecoderName))
           in
           if _customTypeName == N.toString typeName then

             {- The parameter type is equal to the custom type, so this is a recursive custom type.

             i.e. if we're dealing with a type like this:

               type Union = Recursive Union

             The branch decoder we'd be generating is:

             EG.union1 "Recursive" (D.lazy (\_ -> evg_d_Union)) Recursive

             So this function will return AST for:

             (D.lazy (\_ -> evg_d_Union))
             -}

             jsonDecodeLazy1Ignore _targetDecoder

          else
           -- Currently, any types from user, must have encoder ref in this file
           -- @TODO we'll need to extend this to be able to call custom types from anywhere in the project
           -- Question: how do we bubble-up the need for additional imports? Do we just transitively make sure we inject explicit
           -- imports of all evergreen wire functions whenever a type is imported into a file? Note (..) will work fine,
           -- just `exposing (SomeModel)` will need special treatment.

           _targetDecoder

         TAlias (Canonical (Name "author" "project") _) typeName [] (Holey realType) ->
           decodeParamType realType

         _ -> error $ "decodeParamType didn't match any existing implementations for: " ++ show pType


    _customTypeBranches =
      case customType_ of
        Union _u_vars _u_alts _u_numAlts _u_opts ->
          imap (\i ctor ->
            case ctor of
              Ctor n index numParams params ->
                case numParams of
                  0 -> _genCustomType0 i ctor
                  1 -> _genCustomType1 i ctor
                  -- @TODO need to add support for more depths of union types
                  x -> undefined $ "Decoder: Custom Type parsing for " ++ show x ++ " params has not yet been implemented"
          ) _u_alts


  TypedDef
    (named _decoderName)
    (Map.fromList [])
    []
    (at (Call ((qvar "elm" "json" "Json.Decode" "oneOf"
                              (Forall (Map.fromList [(name "a", ())])
                                      (tlam (qtyp "elm" "core" "List" "List" [qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"]])
                                               (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])))))
              [at (List _customTypeBranches)]))
    (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "author" "project" "AllTypes" _customTypeName []])



-- Generate a custom type constructor value function, i.e hilighted here:
--
-- type Blah = Derp Int
--
-- x = Derp 1
--     ^^^^
--
generateConstructor :: String -> String -> ZeroBased -> [Type] -> Located Expr_
generateConstructor name_ customTypeName index paramTypes =
  at (VarCtor Normal
    (canonical "author" "project" "AllTypes")
    (name name_)
    (index)
    (generateConstructorAnnotation paramTypes (qtyp "author" "project" "AllTypes" customTypeName []))
  )

-- For a given list of [Type] of params for a custom type constructor, creates the signature for that constructor
-- ending in the actual type of the custom type itself
generateConstructorAnnotation :: [Type] -> Type -> Annotation
generateConstructorAnnotation pTypes customType =
  let typeSignatures pTypes_ =
        case pTypes_ of
          [] -> customType
          x:[] -> tlam x customType
          x:xs -> tlam x (typeSignatures xs)
  in
  Forall (Map.fromList []) (typeSignatures pTypes)


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





evg_e_AllTypes =
  [(TypedDef (named "evg_e_AllTypes")
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
                    (qtyp "elm" "json" "Json.Encode" "Value" []))]



evg_d_AllTypes =
  (TypedDef (named "evg_d_AllTypes")
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
