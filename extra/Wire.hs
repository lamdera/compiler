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
import qualified Data.List as List

import Control.Monad.Trans (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Prettyprint
import WireHelpers


imap f l = zipWith f [0..] l

justs xs = [ x | Just x <- xs ]

{-

The main AST dynamic injection logic for Evergreen wire.

Overall todo items remaining:

- Handle module `exposing (blah)` issues preventing auto-generated definitions from being imported by other modules
- Generic Encoder for records [DONE]
- Generic Decoder for records [WIP]
- Remove all references to AllTypes & make the module name dynamic based on context
- Support more than 1 type param in custom types

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
              -- Keeping this branch for the moment as the test tracks file AllTypes.elm
              -- eventually when everything is done this will be removed and we'll not need to pattern match


              let customTypeEncoders = fmap customTypeToEncoder $ Map.toList customTypes
                  customTypeDecoders = fmap customTypeToDecoder $ Map.toList customTypes
                  recordEncoders = justs $ fmap aliasToEncoder $ Map.toList aliases
                  recordDecoders = justs $ fmap aliasToDecoder $ Map.toList aliases

              tracef ("-" ++ N.toString n) (canonical
                { _decls =
                    -- @TODO make a helper that lets us deal with this recursive type more generically, i.e.
                    -- by being able to just pass it a list of ASTs & not caring about how it's transformed
                    DeclareRec (customTypeEncoders ++ customTypeDecoders ++ recordEncoders ++ recordDecoders) SaveTheEnvironment

                , _aliases = tracef ("-aliases-" ++ N.toString n) aliases
                }
                )

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
                  , encodeForTypeValue _pType (vlocal "evg_v0")
                  ]
               ]
             )

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

             _ -> error $ "decodeParamType Time type didn't match any existing implementations: " ++ show pType

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




-- @TODO only partially implemented, needs to be extended for all possible types
decoderForType pType =
   case pType of
     TType (Canonical (Name "elm" "core") "Basics") typeName next ->
       case N.toString typeName of
         "Int" ->  jsonDecodeInt
         "Float" -> jsonDecodeFloat
         "Bool" -> jsonDecodeBool
         "Order" -> evergreenDecodeOrder

         _ -> error $ "decoderForType Basics type didn't match any existing implementations: " ++ show pType

     TType (Canonical (Name "elm" "core") "Char") typeName next ->
       evergreenDecodeChar

     TType (Canonical (Name "elm" "core") "String") typeName next ->
       jsonDecodeString

     TType (Canonical (Name "elm" "time") "Time") typeName next ->
       case N.toString typeName of
         "Posix" ->
           evergreenDecodeTime

         _ -> error $ "decoderForType Time type didn't match any existing implementations: " ++ show pType

     TType (Canonical (Name "elm" "core") "List") typeName next ->
       -- @TODO do we really need to do this? Can we just destructure inline above instead? How many other types does List expose?
       case N.toString typeName of
         "List" -> jsonDecodeList (decoderForType (head next))

     TType (Canonical (Name "elm" "core") "Array") typeName next ->
       jsonDecodeArray (decoderForType (head next))

     TType (Canonical (Name "elm" "core") "Set") typeName next ->
       evergreenDecodeSet (decoderForType (head next))

     TType (Canonical (Name "elm" "core") "Dict") typeName next ->
       case next of
         first:second:rest ->
           evergreenDecodeDict (decoderForType first) (decoderForType second)

     TUnit ->
       evergreenDecodeUnit

     TType (Canonical (Name "author" "project") _) typeName next ->
       let _targetDecoderName = "evg_d_" ++ N.toString typeName
           _targetDecoder = at (VarTopLevel (canonical "author" "project" "AllTypes") (name _targetDecoderName))
       in
       -- Currently, any types from user, must have encoder ref in this file
       -- @TODO we'll need to extend this to be able to call custom types from anywhere in the project
       -- Question: how do we bubble-up the need for additional imports? Do we just transitively make sure we inject explicit
       -- imports of all evergreen wire functions whenever a type is imported into a file? Note (..) will work fine,
       -- just `exposing (SomeModel)` will need special treatment.

       _targetDecoder

     TAlias (Canonical (Name "author" "project") _) typeName [] (Holey realType) ->
       decoderForType realType

     _ -> error $ "decoderForType didn't match any existing implementations for: " ++ show pType



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


aliasToEncoder alias =
  case alias of
    (typeName, Alias [] (TType (Canonical _ _) (_) [])) ->
      Nothing

    (typeName, Alias [] (TRecord fields Nothing)) ->
      Just $ recordTypeToEncoder alias fields


recordTypeToEncoder record fields =
  case record of
    (typeName, Alias [] (TRecord fields Nothing)) ->
      let
        recordTypeName = N.toString typeName

      in
      (TypedDef (named $ "evg_e_" ++ recordTypeName)
                (Map.fromList [])
                [ (at (PVar (name "evg_p0"))
                , TAlias (canonical "author" "project" "AllTypes")
                         (name recordTypeName)
                         []
                         (Holey (TRecord fields Nothing))
                )
                ]
                (call jsonEncodeList [coreBasicsIdentity, at (List $ encodeRecordFields fields)])
                (qtyp "elm" "json" "Json.Encode" "Value" []))

    _ -> error $ "recordTypeToEncoder: received non-record type, which should be impossible! : " ++ show record


encodeRecordFields fields =
  let
    sortedFields = List.sortOn (\(_, FieldType index _) -> (fromIntegral index) :: Int) (Map.toList fields)

  in fmap encodeRecordField sortedFields


encodeRecordField field =
  case field of
    (fieldName , FieldType index fieldType) ->
      encodeForTypeValue fieldType (rfield "evg_p0" (N.toString fieldName))


encodeForTypeValue typ value =
  case typ of
    TType (Canonical (Name "elm" "core") "Basics") typeName next ->
      case N.toString typeName of
        "Int" ->
          call jsonEncodeInt [value]

        "Float" ->
          call jsonEncodeFloat [value]

        "Bool" ->
          call jsonEncodeBool [value]

        "Order" ->
          call evergreenEncodeOrder [value]

        _ -> error $ "encodeForTypeValue Basics type didn't match any existing implementations: " ++ show typ

    TType (Canonical (Name "elm" "core") "Char") typeName next ->
      call evergreenEncodeChar [value]

    TType (Canonical (Name "elm" "core") "String") typeName next ->
      call jsonEncodeString [value]

    TType (Canonical (Name "elm" "core") "List") typeName next ->
      call jsonEncodeList [encoderForType (head next), value]

    TType (Canonical (Name "elm" "core") "Array") typeName next ->
      call jsonEncodeArray [encoderForType (head next), value]

    TType (Canonical (Name "elm" "core") "Set") typeName next ->
      call jsonEncodeSet [encoderForType (head next), value]

    TType (Canonical (Name "elm" "core") "Dict") typeName next ->
      case next of
        first:second:rest ->
          call evergreenEncodeDict [encoderForType first, encoderForType second, value]

    TType (Canonical (Name "elm" "time") "Time") typeName next ->
      case N.toString typeName of
        "Posix" ->
          call evergreenEncodeTime [value]

        _ -> error $ "encodeForTypeValue Time type didn't match any existing implementations: " ++ show typ

    TUnit ->
      call evergreenEncodeUnit []

    TType (Canonical (Name "author" "project") _) typeName next ->
      -- Any types from user, must have encoder ref in this file
      let _targetEncoderName = "evg_e_" ++ N.toString typeName

      in
      call (at (VarTopLevel (canonical "author" "project" "AllTypes")
                                 (name _targetEncoderName))) [value]

    TAlias (Canonical (Name "author" "project") _) typeName [] (Holey realType) ->
      encodeForTypeValue realType value

    _ -> error $ "encodeForTypeValue didn't match any existing implementations: " ++ show typ


encoderForType pType =
  case pType of
    TType (Canonical (Name "elm" "core") "Basics") typeName next ->
      case N.toString typeName of
        "Int" ->
          jsonEncodeInt

        "Float" ->
          jsonEncodeFloat

        "Bool" ->
          jsonEncodeBool

        "Order" ->
          evergreenEncodeOrder

        _ -> error $ "encoderForType Basics type didn't match any existing implementations: " ++ show pType

    TType (Canonical (Name "elm" "core") "Char") typeName next ->
      evergreenEncodeChar

    TType (Canonical (Name "elm" "core") "String") typeName next ->
      jsonEncodeString

    TType (Canonical (Name "elm" "core") "List") typeName next ->
      call jsonEncodeList [encoderForType (head next)]

    TType (Canonical (Name "elm" "core") "Array") typeName next ->
      call jsonEncodeArray [encoderForType (head next)]

    TType (Canonical (Name "elm" "core") "Set") typeName next ->
      call jsonEncodeSet [encoderForType (head next)]

    TType (Canonical (Name "elm" "core") "Dict") typeName next ->
      case next of
        first:second:rest ->
          call evergreenEncodeDict [encoderForType first, encoderForType second]

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


aliasToDecoder alias =
  case alias of
    (typeName, Alias [] (TType (Canonical _ _) (_) [])) ->
      Nothing

    (typeName, Alias [] (TRecord fields Nothing)) ->
      Just $ recordTypeToDecoder typeName fields


rightpipe first second = at (Binop (name "|>")
           (canonical "elm" "core" "Basics")
           (name "apR")
           (Forall (Map.fromList [(name "a" ,()) ,(name "b" ,())])
                   (tlam (tvar "a")
                            (tlam (tlam (tvar "a")
                                              (tvar "b"))
                                     (tvar "b")))) first second)


recordTypeToDecoder typeName fields =
  let
    sortedFields = List.sortOn (\(_, FieldType index _) -> (fromIntegral index) :: Int) (Map.toList fields)

    fieldDecoders = fmap decodeRecordField sortedFields

    recordDecoder = foldl rightpipe recordDecoderTop fieldDecoders

    recordDecoderTop = call jsonDecodeSucceed [recordConstructor typeName fields]

    decodeRecordField field =
      case field of
        (fieldName , FieldType index fieldType) ->
          call evergreenAtIndex [int ((fromIntegral index) :: Int) , decoderForType fieldType]

    decoderName = "evg_d_" ++ N.toString typeName

  in
  (TypedDef (named decoderName)
     (Map.fromList [])
     []
      recordDecoder
      (qtyp "elm" "json" "Json.Decode" "Decoder" [TAlias (canonical "author" "project" "AllTypes")
        (name (N.toString typeName))
        []
        (Holey (TRecord fields Nothing))]))


recordConstructor typeName fields =
  at (VarCtor Normal (canonical "author" "project" "AllTypes")
      (name (N.toString typeName))
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
                 (name (N.toString typeName))
                 []
                 (Filled (TRecord fields Nothing))))))))))))))))))
