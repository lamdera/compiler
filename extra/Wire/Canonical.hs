{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Wire.Canonical where

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
import Wire.Helpers
import CanSer.CanSer
import qualified Data.Text as Text


-- AST to file debugger
tracef n a =
  unsafePerformIO $ do
    putStrLn ("trace-" ++ n ++ ".txt")
    writeFile ("trace-" ++ n ++ ".txt") $ prettyShow a
    pure a


tracer a b =
  unsafePerformIO $ do
    print a
    pure b

trace a b =
  unsafePerformIO $ do
    putStrLn $ show a ++ ": " ++ show b
    pure b


prettytracer a ast =
  unsafePerformIO $ do
    putStrLn a
    putStrLn $ (Text.unpack $ ppElm ast)
    pure ast

{-

The main AST dynamic injection logic for Evergreen wire.

Overall todo items remaining:

- Handle module `exposing (blah)` issues preventing auto-generated definitions from being imported by other modules
- Generic Encoder for records [DONE]
- Generic Decoder for records [DONE]
- Remove all references to AllTypes & make the module name dynamic based on context [DONE]
- Support more than 1 type param in custom types [DONE]
- Support more than 2 type param in custom types [WIP] - encoders support N, but decoders need more work
- Retain existing declarations in a file we gen into, instead of clobbering [DONE]
- Fix cross-file type references [WIP]

-}


-- Our injection point POC for AllTypes. Search for `Wire.modify`
modifyCanonical canonical flag pkg importDict interfaces source =
  case canonical of
    Module name docs exports decls customTypes aliases binops effects ->
      case name of
        Canonical pkg n ->
          case N.toString n of
            -- "Evergreen" ->
            --   tracef ("can-" ++ N.toString n) canonical

            "AllTypes" ->
              -- Keeping this branch for the moment as the test tracks file AllTypes.elm
              -- eventually when everything is done this will be removed and we'll not need to pattern match
              modifyCanonicalApplied canonical n customTypes aliases
              -- canonical

            "Msg" ->
              -- modifyCanonicalApplied canonical n customTypes aliases
              canonical
            -- "Lamdera.Types" -> modifyCanonicalApplied canonical n customTypes aliases

            _ -> do
              -- -- This will be the final implementation as we converge to it
              -- let customTypeEncoders = fmap customTypeToEncoder $ Map.toList customTypes
              -- let customTypeDecoders = fmap customTypeToDecoder $ Map.toList customTypes
              -- let existingDecls = _decls canonical
              --
              -- -- Add declarations for our generated encoders/decoders in addition to any existing declarations
              -- canonical { _decls = DeclareRec (customTypeEncoders ++ customTypeDecoders) existingDecls }
              -- modifyCanonicalApplied canonical n customTypes aliases
              canonical


-- @TODO this definition is temporary, and once all cases are covered will become the body of `modifyCanonical`
modifyCanonicalApplied canonical n customTypes aliases =
  let
      moduleName = N.toString n
      customTypeEncoders = justs $ fmap (customTypeToEncoder moduleName) $ Map.toList customTypes
      customTypeDecoders = justs $ fmap (customTypeToDecoder moduleName) $ Map.toList customTypes
      recordEncoders = justs $ fmap (aliasToEncoder moduleName) $ Map.toList aliases
      recordDecoders = justs $ fmap (aliasToDecoder moduleName) $ Map.toList aliases

      cleanCanonical = canonical { _decls = canonicalRemoveWireDef $ _decls canonical }
      existingDecls = _decls cleanCanonical

  in
  -- tracer ("can-" ++ N.toString n) $
    cleanCanonical
      { _decls = DeclareRec (
        tracer "customTypeEncoders" customTypeEncoders) existingDecls
        -- tracer "customTypeEncoders" customTypeEncoders ++
        -- tracer "customTypeDecoders" customTypeDecoders ++
        -- tracer "recordEncoders" recordEncoders ++
        -- tracer "recordDecoders" recordDecoders) existingDecls
      }



-- Removes any existing `evg_` prefixed functions.
-- We use this to remove the stub `evg_` functions from the Valid stage and
-- insert the new fully-fledged `evg_` functions from the canonicalisation stage.
canonicalRemoveWireDef decls =
  case decls of
    Declare def declsNext ->
      if isWireDef def then
        DeclareRec [] (canonicalRemoveWireDef declsNext)
      else
        Declare def (canonicalRemoveWireDef declsNext)

    DeclareRec defList declsNext ->
      DeclareRec (filter isWireDef defList) (canonicalRemoveWireDef declsNext)

    SaveTheEnvironment ->
      SaveTheEnvironment


isWireDef def =
  let
    check n =
      if List.isPrefixOf "evg_" $ N.toString n then
        True
      else
        False
  in
  case def of
    TypedDef (At _ name) _ _ _ _ ->
      check name

    Def (At _ name) _ _ ->
      check name


customTypeToEncoder moduleName (customTypeName, customType_) =
  let
    encoderName = "evg_e_" ++ N.toString customTypeName

    customTypeNameS = N.toString customTypeName

    customTypeBranch _index ctor =
      case ctor of
        Ctor n index numParams pTypes ->
          let _tagNameT = N.toText n
              _tagNameS = N.toString n

              patternCtorArgs =
                imap (\i pType ->
                  PatternCtorArg
                      { _index = ZeroBased i
                      , _type = pType
                      , _arg = at (PVar (name $ "evg_v" ++ show i))
                      }
                ) pTypes

              encoders =
                imap (\i pType ->
                  encodeForTypeValue moduleName pType (vlocal $ "evg_v" ++ show i)
                ) pTypes
          in
          -- Encodes custom types, i.e.
          --   ValueTwoParams Bool Char
          -- becomes the following case statement branch:
          --   ValueTwoParams evg_v0 evg_v1 -> E.list identity [ E.string "ValueTwo", E.bool evg_v0, EG.e_char evg_v1 ]
          customTypeCaseBranch moduleName customTypeName customType_ _index _tagNameS
            patternCtorArgs
             (call jsonEncodeList
               [ coreBasicsIdentity
               , list $ [ call jsonEncodeString [str _tagNameT]] ++ encoders
               ]
             )

    customTypeBranches =
      case customType_ of
        Union _u_vars _u_alts _u_numAlts _u_opts ->
          imap (\i ctor ->
            case ctor of
              Ctor n index numParams params ->
                customTypeBranch i ctor
          ) _u_alts

  in
  -- @TODO temporary while we figure out what to do with polymorphic types
  if customTypeName == "FrontendMetaMsg" then
    Nothing
  else
    Just $
    -- prettytracer moduleName $
    TypedDef
      (named encoderName)
      (Map.fromList [])
      [ (at (PVar (name "evg_p0"))
      , qtyp "author" "project" moduleName customTypeNameS [])
      ]
      (at (Case (vlocal "evg_p0") customTypeBranches))
      (qtyp "elm" "json" "Json.Encode" "Value" [])

      -- (tlam (qtyp "author" "project" moduleName customTypeNameS []) (qtyp "elm" "json" "Json.Encode" "Value" []))


customTypeCaseBranch moduleName customTypeName customType index customTypeLabel customTypeArgs expr =
  CaseBranch
    (at (PCtor { _p_home = canonical "author" "project" moduleName
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
customTypeToDecoder moduleName (customTypeName_, customType_) =
  let
    _customTypeName = N.toString customTypeName_

    _decoderName = "evg_d_" ++ N.toString customTypeName_

    genCustomType0 _index ctor =
      case ctor of
        Ctor n index numParams params ->
          let _tagNameT = N.toText n
              _tagNameS = N.toString n
          in
          at (Call evergreenUnion
            [ str _tagNameT
            , at (VarCtor Normal (canonical "author" "project" moduleName)
                (name _tagNameS)
                (ZeroBased _index)
                (Forall (Map.fromList []) (qtyp "author" "project" moduleName _customTypeName []))
              )
            ]
          )


    {--

      Generates the following kind of code;

      EG.union1 "ValueInt" D.int ValueInt

      This is then used in the D.oneOf to allow us to decode custom types

    --}
    genCustomType1 _index ctor =
      case ctor of
        Ctor n index numParams pTypes ->
          let _tagNameT = N.toText n
              _tagNameS = N.toString n
              constructor = generateConstructor moduleName _tagNameS _customTypeName (index) pTypes
          in
          call evergreenDecodeUnion1 (
            [str _tagNameT] ++ -- The tag name string to parse, i.e. "ValueInt"
            (fmap decodeParamType pTypes) ++ -- The decoder/s to be used (@TODO this sets us up for >1 param custom types?)
            [constructor] -- The constructor, i.e. ValueInt : Int -> Union
          )

    genCustomType2 _index ctor =
      case ctor of
        Ctor n index numParams pTypes ->
          let _tagNameT = N.toText n
              _tagNameS = N.toString n
              constructor = generateConstructor moduleName _tagNameS _customTypeName (index) pTypes
          in
          call evergreenDecodeUnion2 (
            [str _tagNameT] ++ -- The tag name string to parse, i.e. "ValueInt"
            (fmap decodeParamType pTypes) ++ -- The decoder/s to be used (@TODO this sets us up for >1 param custom types?)
            [constructor] -- The constructor, i.e. ValueInt : Int -> Union
          )


    -- @TODO only partially implemented, needs to be extended for all possible types
    decodeParamType pType =
       case pType of
         TType (Canonical (Name "author" "project") _) typeName next ->
           let _targetDecoderName = "evg_d_" ++ N.toString typeName
               _targetDecoder = at (VarTopLevel (canonical "author" "project" moduleName) (name _targetDecoderName))
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

         _ -> decoderForType moduleName pType

    _customTypeBranches =
      case customType_ of
        Union _u_vars _u_alts _u_numAlts _u_opts ->
          imap (\i ctor ->
            case ctor of
              Ctor n index numParams params ->
                case numParams of
                  0 -> genCustomType0 i ctor
                  1 -> genCustomType1 i ctor
                  2 -> genCustomType2 i ctor
                  -- @TODO need to add support for more depths of union types
                  x -> undefined $ "Decoder: Custom Type parsing for " ++ show x ++ " params has not yet been implemented"
          ) _u_alts

  in
  -- @TODO temporary while we figure out what to do with polymorphic types
  if customTypeName_ == "FrontendMetaMsg" then
    Nothing
  else
    Just $
    TypedDef
      (named _decoderName)
      (Map.fromList [])
      []
      (at (Call ((qvar "elm" "json" "Json.Decode" "oneOf"
                  (Forall (Map.fromList [(name "a", ())])
                          (tlam (qtyp "elm" "core" "List" "List" [qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"]])
                                   (qtyp "elm" "json" "Json.Decode" "Decoder" [tvar "a"])))))
                [at (List _customTypeBranches)]))
      (qtyp "elm" "json" "Json.Decode" "Decoder" [qtyp "author" "project" moduleName _customTypeName []])




-- @TODO only partially implemented, needs to be extended for all possible types
decoderForType moduleName pType =
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
         "List" -> jsonDecodeList (decoderForType moduleName (head next))

     TType (Canonical (Name "elm" "core") "Array") typeName next ->
       jsonDecodeArray (decoderForType moduleName (head next))

     TType (Canonical (Name "elm" "core") "Set") typeName next ->
       evergreenDecodeSet (decoderForType moduleName (head next))

     TType (Canonical (Name "elm" "core") "Result") "Result" next ->
       case next of
         first:second:rest ->
           evergreenDecodeResult (decoderForType moduleName first) (decoderForType moduleName second)

     TType (Canonical (Name "elm" "core") "Dict") typeName next ->
       case next of
         first:second:rest ->
           evergreenDecodeDict (decoderForType moduleName first) (decoderForType moduleName second)

     TUnit ->
       evergreenDecodeUnit

     TTuple first second _ ->
       evergreenDecodeTuple (decoderForType moduleName first) (decoderForType moduleName second)

     TType (Canonical (Name "author" "project") _) typeName next ->
       let _targetDecoderName = "evg_d_" ++ N.toString typeName
           _targetDecoder = at (VarTopLevel (canonical "author" "project" moduleName) (name _targetDecoderName))
       in
       -- Currently, any types from user, must have encoder ref in this file
       -- @TODO we'll need to extend this to be able to call custom types from anywhere in the project
       -- Question: how do we bubble-up the need for additional imports? Do we just transitively make sure we inject explicit
       -- imports of all evergreen wire functions whenever a type is imported into a file? Note (..) will work fine,
       -- just `exposing (SomeModel)` will need special treatment.

       _targetDecoder

     TAlias (Canonical (Name "author" "project") _) typeName [] (Holey realType) ->
       decoderForType moduleName realType


     -- @TODO temporary
     TType (Canonical (Name "Lamdera" "core") _) typeName next ->
       let _targetDecoderName = "evg_d_" ++ N.toString typeName
           _targetDecoder = at (VarTopLevel (canonical "Lamdera" "core" "Lamdera.Types") (name _targetDecoderName))
       in
       _targetDecoder


     _ -> error $ "decoderForType didn't match any existing implementations for: " ++ show pType



-- Generates a custom type constructor value function, i.e hilighted here:
--
-- type Blah = Derp Int
--
-- x = Derp 1
--     ^^^^
--
generateConstructor :: String -> String -> String -> ZeroBased -> [Type] -> Located Expr_
generateConstructor moduleName name_ customTypeName index paramTypes =
  at (VarCtor Normal
    (canonical "author" "project" moduleName)
    (name name_)
    (index)
    (generateConstructorAnnotation paramTypes (qtyp "author" "project" moduleName customTypeName []))
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



aliasToEncoder moduleName alias =
  case alias of
    (typeName, Alias [] (TType (Canonical _ _) (_) [])) ->
      Nothing

    (typeName, Alias [] (TRecord fields Nothing)) ->
      Just $ recordTypeToEncoder moduleName alias fields

    (typeName, Alias [] typ) ->
      let
        aliasName = N.toString typeName

      in
      Just $
        (TypedDef (named $ "evg_e_" ++ aliasName)
                  (Map.fromList [])
                  [ (at (PVar (name "evg_p0")) , typ ) ]
                  (call (encoderForType moduleName typ) [vlocal "evg_p0"])
                  (qtyp "elm" "json" "Json.Encode" "Value" []))

    _ -> error $ "aliasToEncoder: didn't match any existing implementations: " ++ show alias


recordTypeToEncoder moduleName record fields =
  case record of
    (typeName, Alias [] (TRecord fields Nothing)) ->
      let
        recordTypeName = N.toString typeName

      in
      (TypedDef (named $ "evg_e_" ++ recordTypeName)
                (Map.fromList [])
                [ (at (PVar (name "evg_p0"))
                , TAlias (canonical "author" "project" moduleName)
                         (name recordTypeName)
                         []
                         (Holey (TRecord fields Nothing))
                )
                ]
                (call jsonEncodeList [coreBasicsIdentity, at (List $ encodeRecordFields moduleName fields)])
                (qtyp "elm" "json" "Json.Encode" "Value" []))

    _ -> error $ "recordTypeToEncoder: received non-record type, which should be impossible! : " ++ show record


encodeRecordFields moduleName fields =
  let
    sortedFields = List.sortOn (\(_, FieldType index _) -> (fromIntegral index) :: Int) (Map.toList fields)

  in fmap (encodeRecordField moduleName) sortedFields


encodeRecordField moduleName field =
  case field of
    (fieldName , FieldType index fieldType) ->
      encodeForTypeValue moduleName fieldType (rfield "evg_p0" (N.toString fieldName))


encodeForTypeValue moduleName typ value =
  -- case trace "encodeForTypeValue" typ of
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
      call jsonEncodeList [encoderForType moduleName (head next), value]

    TType (Canonical (Name "elm" "core") "Array") typeName next ->
      call jsonEncodeArray [encoderForType moduleName (head next), value]

    TType (Canonical (Name "elm" "core") "Set") typeName next ->
      call jsonEncodeSet [encoderForType moduleName (head next), value]

    TType (Canonical (Name "elm" "core") "Result") "Result" next ->
      case next of
        first:second:rest ->
          call evergreenEncodeResult [encoderForType moduleName first, encoderForType moduleName second, value]

    TType (Canonical (Name "elm" "core") "Dict") typeName next ->
      case next of
        first:second:rest ->
          call evergreenEncodeDict [encoderForType moduleName first, encoderForType moduleName second, value]

    TType (Canonical (Name "elm" "time") "Time") typeName next ->
      case N.toString typeName of
        "Posix" ->
          call evergreenEncodeTime [value]

        _ -> error $ "encodeForTypeValue Time type didn't match any existing implementations: " ++ show typ

    TUnit ->
      call evergreenEncodeUnit [value]

    TTuple first second _ ->
      call evergreenEncodeTuple [encoderForType moduleName first, encoderForType moduleName second, value]

    TType (Canonical (Name "author" "project") moduleNameLocal) typeName next ->
      -- Any types from user, must have encoder ref in this file
      let
        targetEncoderName = "evg_e_" ++ N.toString typeName

      in
      prettytracer ("➡️  Generating encoder for:"++ show typ) $
        call (at (VarTopLevel (canonical "author" "project" (N.toString moduleNameLocal))
                                 (name targetEncoderName))) [value]

    TAlias (Canonical (Name "author" "project") moduleNameLocal) typeName [] (Holey realType) ->
      encodeForTypeValue (N.toString moduleNameLocal) realType value

    -- @TODO temporary
    TType (Canonical (Name "Lamdera" "core") moduleNameLocal) typeName next ->
      -- Any types from user, must have encoder ref in this file
      let
        targetEncoderName = "evg_e_" ++ N.toString typeName

      in
      trace "LamderaCoreMatched" $
        call ((qvar "Lamdera" "core" (N.toString moduleNameLocal) targetEncoderName
          (Forall (Map.fromList []) (TUnit)))) [value]
          -- @TODO needs a real type signature defined here instead of TUnit...







    _ -> error $ "encodeForTypeValue didn't match any existing implementations: " ++ show typ


encoderForType moduleName pType =
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
      call jsonEncodeList [encoderForType moduleName (head next)]

    TType (Canonical (Name "elm" "core") "Array") typeName next ->
      call jsonEncodeArray [encoderForType moduleName (head next)]

    TType (Canonical (Name "elm" "core") "Set") typeName next ->
      call jsonEncodeSet [encoderForType moduleName (head next)]

    TType (Canonical (Name "elm" "core") "Result") "Result" next ->
      case next of
        first:second:rest ->
          call evergreenEncodeResult [encoderForType moduleName first, encoderForType moduleName second]

    TType (Canonical (Name "elm" "core") "Dict") typeName next ->
      case next of
        first:second:rest ->
          call evergreenEncodeDict [encoderForType moduleName first, encoderForType moduleName second]

    TType (Canonical (Name "elm" "time") "Time") typeName next ->
      case N.toString typeName of
        "Posix" ->
          evergreenEncodeTime

        _ -> error $ "encoderForType Time type didn't match any existing implementations: " ++ show pType

    TUnit ->
      call evergreenEncodeUnit []

    TTuple first second _ ->
      call evergreenEncodeTuple [encoderForType moduleName first, encoderForType moduleName second]

    TType (Canonical (Name "author" "project") _) typeName next ->
      -- Any types from user, must have encoder ref in this file
      let _targetEncoderName = "evg_e_" ++ N.toString typeName
      in
      at (VarTopLevel (canonical "author" "project" moduleName) (name _targetEncoderName))

    TAlias (Canonical (Name "author" "project") _) typeName [] (Holey realType) ->
      encoderForType moduleName realType

    -- @TODO temporary
    TType (Canonical (Name "Lamdera" "core") _) typeName next ->
      -- Any types from user, must have encoder ref in this file
      let _targetEncoderName = "evg_e_" ++ N.toString typeName

      in
      at (VarTopLevel (canonical "Lamdera" "core" "Lamdera.Types") (name _targetEncoderName))


    _ -> error $ "encoderForType didn't match any existing implementations: " ++ show pType


aliasToDecoder moduleName alias =
  case alias of
    (typeName, Alias [] (TType (Canonical _ _) (_) [])) ->
      Nothing

    (typeName, Alias [] (TRecord fields Nothing)) ->
      Just $ recordTypeToDecoder moduleName typeName fields

    (typeName, Alias [] typ) ->
      let
        aliasName = N.toString typeName

      in
      Just $
        (TypedDef (named $ "evg_d_" ++ aliasName)
                  (Map.fromList [])
                  []
                  (decoderForType moduleName typ)
                  (qtyp "elm" "json" "Json.Decode" "Decoder" [typ]))

    _ -> error $ "aliasToDecoder didn't match any existing implementations: " ++ show alias


rightpipe first second = at (Binop (name "|>")
           (canonical "elm" "core" "Basics")
           (name "apR")
           (Forall (Map.fromList [(name "a" ,()) ,(name "b" ,())])
                   (tlam (tvar "a")
                            (tlam (tlam (tvar "a")
                                              (tvar "b"))
                                     (tvar "b")))) first second)


recordTypeToDecoder moduleName typeName fields =
  let
    sortedFields = List.sortOn (\(_, FieldType index _) -> (fromIntegral index) :: Int) (Map.toList fields)

    fieldDecoders = fmap decodeRecordField sortedFields

    recordDecoder = foldl rightpipe recordDecoderTop fieldDecoders

    recordDecoderTop = call jsonDecodeSucceed [recordConstructor moduleName typeName fields]

    decodeRecordField field =
      case field of
        (fieldName , FieldType index fieldType) ->
          call evergreenAtIndex [int ((fromIntegral index) :: Int) , decoderForType moduleName fieldType]

    decoderName = "evg_d_" ++ N.toString typeName

  in
  (TypedDef (named decoderName)
     (Map.fromList [])
     []
      recordDecoder
      (qtyp "elm" "json" "Json.Decode" "Decoder" [TAlias (canonical "author" "project" moduleName)
        (name (N.toString typeName))
        []
        (Holey (TRecord fields Nothing))]))


recordConstructor moduleName typeName fields =
  let
    sortedFields = List.sortOn (\(_, FieldType index _) -> (fromIntegral index) :: Int) (Map.toList fields)

    getFieldType field =
      case field of
        (fieldName , FieldType index fieldType) ->
          fieldType

    constructorTypeSig = foldr (\fieldType accumulator -> tlam fieldType accumulator) finalSigPart (fmap getFieldType sortedFields)

    finalSigPart = (TAlias (canonical "author" "project" moduleName)
                     (name (N.toString typeName))
                     []
                     (Filled (TRecord fields Nothing)))
  in
  at (VarCtor Normal (canonical "author" "project" moduleName)
      (name (N.toString typeName))
      (ZeroBased 0)
      (Forall (Map.fromList []) constructorTypeSig))
