{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Wire.Source2 (generateCodecs, injectEvergreenExposing, isEvergreenCodecName, evergreenCoreCodecs, addImports) where

{-
Wire.Source is responsible for generating Evergreen codecs for all Elm types. It
does this by injecting generated source code, as this was the fastest and safest
way forward at the time. For some codecs it also writes out type annotations.

This module is also responsible for figuring out which of these codecs should be
exposed/imported; if the type is reachable, so is the codec.

Thirdly, we handle the set of built-in codecs here and what code we should
generate in order to interact with them.

ASSUMPTION: no user code defines any values starting with the string `w2_`

-}

import qualified AST.Canonical as Can
import Elm.ModuleName (Canonical(..))
import qualified Elm.ModuleName as ModuleName
import qualified AST.Utils.Type as Type
import qualified AST.Source as Src
import qualified AST.Source as Valid

import qualified Data.Char
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Name as N
import qualified Elm.Package as Pkg
import qualified Reporting.Annotation as A
import qualified Reporting.Annotation as R
import qualified Data.Utf8 as Utf8

-- import CanSer.CanSer as CanSer

import Lamdera
import Wire.PrettyPrint
import StandaloneInstances


textToUf8 =
  Utf8.fromChars . T.unpack


addImports :: [Src.Import] -> Valid.Module -> Valid.Module
addImports imports module_@(Valid.Module _name _exports _docs _imports _values _unions _aliases _binops _effects) =
  module_ { Valid._imports = (imports <> _imports) }


isEvergreenCodecName :: N.Name -> Bool
isEvergreenCodecName name =
  let
    n = N.toText name
  in
  "evg_encode_" `T.isPrefixOf` n ||
  "evg_decode_" `T.isPrefixOf` n ||
  "w2_decode_" `T.isPrefixOf` n ||
  "w2_encode_" `T.isPrefixOf` n


injectEvergreenExposing :: Can.Module -> String -> String
injectEvergreenExposing (Can.Module _ _exports _ _ _ _ _ _) s =
  -- 1. figure out what types are exposed from `can`
  -- 2. inject the encoder/decoders for those types into the exposing statement
  --    by finding the first `)\n\n`, and injecting it before that.
  -- - we can assume there to be no `exposing (..)` or trailing spaces/comments
  --   after the last `)` since elm-format will move/remove those.
  -- - - this implementation only assumes there to be no spaces/comments after
  --     the last `)`, and that there are two free newlines directly after,
  --     which is what elm-format would give us.
  let
    exposingRegion export =
      case export of
        Can.ExportEverything r -> r
        Can.Export m ->
          case Map.elemAt 0 m of -- ASSUMPTION: Export map is non-empty, since
                                 -- parser requires exposing to be non-empty.
            (_, A.At r _) ->
              -- trace (sShow ("exposingRegion", Map.elemAt 0 m))
              r

    startsWithUppercaseCharacter (x:_) | Data.Char.isUpper x = True
    startsWithUppercaseCharacter _ = False

    -- @TODO remove legacy evg_ exports when deprecated
    codecsFor s = ["w2_encode_" ++ s, "w2_decode_" ++ s, "evg_encode_" ++ s, "evg_decode_" ++ s]

  in
  case _exports of
    Can.ExportEverything _ -> s
    Can.Export mapNameExport ->
      insertAfterRegion
        (exposingRegion _exports)
        (mapNameExport
        & Map.keys
        & fmap N.toChars
        & filter startsWithUppercaseCharacter
        & concatMap codecsFor
        & leftPadWith ", "
        & (\v -> "  " ++ v)
        )
        s

insertAfterRegion :: R.Region -> String -> String -> String
insertAfterRegion region@(R.Region _ (R.Position lend cend)) insertion hay =
  let
    -- !_ = trace (sShow ("insertAfterRegion", region)) ()
    regionOffset (line, col) [] = length hay
    regionOffset (line, col) rest | (line, col+1) >= (lend, cend) = length hay - length rest
    regionOffset (line, col) ('\n':xs) = regionOffset (line+1, 0) xs
    regionOffset (line, col) (x:xs) = regionOffset (line, col+1) xs

    offset = regionOffset (1, 0) hay
    (start, end) = splitAt offset hay
  in
    start ++ insertion ++ end

leftPadWith _ [] = ""
leftPadWith delim (x:xs) = delim <> x <> leftPadWith delim xs

generateCodecs :: Map.Map N.Name N.Name -> Can.Module -> T.Text
generateCodecs revImportDict (Can.Module _moduName _docs _exports _decls _unions _aliases _binops _effects) =
  let -- massive let-expr so we can closure in _moduName

    !isDebug = False -- unsafePerformIO $ Lamdera.isDebug

    ifDebugT t = "" -- if isDebug then t else ""

    -- HELPERS
    -- | qualIfNeeded does a reverse lookup from fully qualified module name to
    -- import alias, so we generate valid code when people do e.g.
    -- `import Json.Decode as Decode` or when we're actually referencing
    -- something in the same module which then shouldn't be fully qualified.
    qualIfNeeded :: Canonical -> T.Text
    qualIfNeeded moduName | moduName == _moduName = ""
    qualIfNeeded moduName@(Canonical _ n) =
      case revImportDict Map.!? n of
        Just v -> N.toText v <> "."
        Nothing -> (N.toText n <> ".")
          -- debug_note (sShow ("Error: qualIfNeeded import not found, please report this!", moduName)) (N.toText n <> ".")

    evergreenCoreEncoder tipes = fmap fst <$> evergreenCoreCodecs qualIfNeeded tipes
    evergreenCoreDecoder tipes = fmap snd <$> evergreenCoreCodecs qualIfNeeded tipes

    aliasCodecs :: (N.Name, Can.Alias) -> T.Text
    aliasCodecs (name, (Can.Alias names t)) =
      let
        tName = N.toText name <> leftWrap (N.toText <$> names)
        encoder =
          --"w2_encode_" <> N.toText name <> " : " <> T.intercalate " -> " (((\v -> "(" <> v <> " -> Lamdera.Wire2.Encoder)") <$> N.toText <$> names) ++ [tName, "Lamdera.Wire2.Encoder"]) <> "\n" <>
          "expected_w2_encode_" <> N.toText name <> leftWrap (codec <$> names) <> " =\n" <>
          "  " <> encoderForType Map.empty t
        decoder =
          --"w2_decode_" <> N.toText name <> " : " <> T.intercalate " -> " (((\v -> "(Lamdera.Wire2.Decoder " <> v <> ")") <$> N.toText  <$> names) ++ ["Lamdera.Wire2.Decoder (" <> tName <> ")"]) <> "\n" <>
          "expected_w2_decode_" <> N.toText name <> leftWrap (codec <$> names) <> " =\n"
          <> (ifDebugT $
             "  let _ = Debug.log \"w2_decode_" <> N.toText name <> "\" \"called\"\n"
          <> "  in\n")
          <> "  " <> decoderForType Map.empty t
      in
        encoder <> "\n\n" <> decoder

    unionCodecs :: (N.Name, Can.Union) -> T.Text
    unionCodecs (name, (Can.Union _u_vars _u_alts _u_numAlts _u_opts)) =
      let
        tName = N.toText name <> leftWrap (N.toText <$> _u_vars)

        sorted_u_alts =
          if (List.length _u_alts > 256) then
            -- @TODO put this error somewhere nicer that's not a crash!
            error $ "Error: I ran into a custom type (" <> N.toChars name <> ") with more than 256 values! Please report this!"
          else
            _u_alts
              & List.sortOn
                -- Ctor N.Name Index.ZeroBased Int [Type]
                (\(Can.Ctor name _ _ _) -> N.toChars name)

        encoder =
          "expected_w2_encode_" <> N.toText name <> leftWrap (codec <$> _u_vars) <> " w2v =\n" <>
          "  case w2v of\n    "
          <> T.intercalate "\n    "
            (case _u_opts of
              _ -> encodeUnion _u_vars sorted_u_alts
              --Can.Normal -> codecUnion _u_vars _u_alts
              --Can.Enum -> error "codec Enum notimpl"
              --Can.Unbox -> error "codec Unbox notimpl"
            )

        encodeUnion :: [N.Name] -> [Can.Ctor] -> [T.Text]
        encodeUnion _u_vars _u_alts =
          _u_alts
          & imap
          (\i (Can.Ctor name _ _ tipes) ->
              N.toText name <> leftWrap (fst <$> nargs tipes) <> " -> " <> sequenceEncWithoutLength (("Lamdera.Wire2.encodeUnsignedInt8 " <> (T.pack $ show i)) : ((\(var, t) -> encoderForType Map.empty t <> " " <> var) <$> nargs tipes)))

        decoder =
          "expected_w2_decode_" <> N.toText name <> leftWrap (codec <$> _u_vars) <> " =\n"
          <> "  Lamdera.Wire2.decodeUnsignedInt8 |> Lamdera.Wire2.andThenDecode (\\w2v ->\n"
          <> (ifDebugT $
             "    let _ = Debug.log \"w2_decode_" <> N.toText name <> "\" w2v\n"
          <> "    in\n")
          <> "    case w2v of\n      "
          <> T.intercalate "\n      " (decodeUnion _u_vars sorted_u_alts)
          -- <> "\n      _ -> Lamdera.Wire2.failDecode \"" <> N.toText name <> "\""
          <> "\n      _ -> Lamdera.Wire2.failDecode"
          <> "\n  )"

        decodeUnion :: [N.Name] -> [Can.Ctor] -> [T.Text]
        decodeUnion _u_vars _u_alts =
          _u_alts
          & imap
            (\i (Can.Ctor name _ _ tipes) ->
                (T.pack $ show i) <> " -> " <> T.intercalate " |> Lamdera.Wire2.andMapDecode " ((decodeSucceed $ N.toText name) : ((\(_, t) -> decoderForType Map.empty t) <$> nargs tipes)))

      in
        encoder <> "\n\n" <> decoder


    decoderForType :: Map.Map N.Name Int -> Can.Type -> T.Text
    decoderForType varMap t =
      case t of
        (Can.TVar n) ->
          codec n

        (Can.TUnit) ->
          unitDec

        (Can.TType moduName name tipes) ->
          p ((case evergreenCoreDecoder tipes (moduName, name) of
            Just c -> c
            Nothing -> p $ qualIfNeeded moduName <> "w2_decode_" <> N.toText name
          ) <> leftWrap (p <$> decoderForType varMap <$> tipes))

        (Can.TRecord nameFieldTypeMap (Just _)) ->
          -- We don't allow sending partial records atm, because we haven't fully figured out how to encode/decode them.
          -- "(Lamdera.Wire2.failDecode \"partial record\")"
          "Lamdera.Wire2.failDecode"
          -- error "Wire: disallowed partial record encountered. Please report this issue!"

        (Can.TRecord nameFieldTypeMap Nothing) | Map.null nameFieldTypeMap ->
          p (decodeSucceed "{}")

        (Can.TRecord nameFieldTypeMap Nothing) -> p $
          let
            nameFieldTypes = Can.fieldsToList nameFieldTypeMap
            (newVarMap, vars) = manyVars varMap (fst <$> nameFieldTypes)
          in
            T.intercalate " |> Lamdera.Wire2.andMapDecode " $
              p <$> (decodeSucceed "(\\" <> leftWrap vars <> " -> "
                <> recLit ((\k -> (N.toText k, getVar newVarMap k)) <$> (fst <$> nameFieldTypes)) <> ")")
                : (decoderForType newVarMap <$> (snd <$> nameFieldTypes))

        (Can.TTuple t1 t2 Nothing) ->
          p $ pairDec (decoderForType varMap t1) (decoderForType varMap t2)

        (Can.TTuple t1 t2 (Just t3)) ->
          p $ tripleDec (decoderForType varMap t1) (decoderForType varMap t2) (decoderForType varMap t3)

        (Can.TAlias moduName name nameTypePairs aliasType) ->
          decoderForType varMap (Can.TType moduName name (snd <$> nameTypePairs))

        (Can.TLambda _ _) ->
          -- "(Lamdera.Wire2.failDecode \"lambda\")" -- <> strQuote (T.pack $ show x)
          "Lamdera.Wire2.failDecode" -- <> strQuote (T.pack $ show x)
          -- error "Wire: disallowed function encountered. Please report this issue!"


    encoderForType :: Map.Map N.Name Int -> Can.Type -> T.Text
    encoderForType varMap t =
      case t of
        (Can.TVar n) ->
          codec n

        (Can.TUnit) ->
          unitEnc

        (Can.TType moduName name tipes) ->
          p (
              (case evergreenCoreEncoder tipes (moduName, name) of
                Just c -> c
                Nothing ->
                  qualIfNeeded moduName
                   <> "w2_encode_" <> N.toText name
              ) <> leftWrap (p <$> encoderForType varMap <$> tipes)
            )

        (Can.TRecord nameFieldTypeMap (Just _)) ->
          -- We don't allow sending partial records atm, because we haven't fully figured out how to encode/decode them.
          "Lamdera.Wire2.failEncode"
          -- error "Wire: disallowed partial record encountered. Please report this issue!"

        (Can.TRecord nameFieldTypeMap Nothing) ->
          let
            (newVarMap, recVar) = newRecVar varMap -- store `Map Varname Int` so we can append numbers to varnames to avoid shadowing
          in
            "(\\" <> recVar <> " -> "
              <> sequenceEncWithoutLength ((\(var, t) ->
                encoderForType newVarMap t <> " " <> recAccess newVarMap var) <$> Can.fieldsToList nameFieldTypeMap) <> ")"

        (Can.TTuple t1 t2 Nothing) ->
          pairEnc (encoderForType varMap t1) (encoderForType varMap t2)

        (Can.TTuple t1 t2 (Just t3)) ->
          tripleEnc (encoderForType varMap t1) (encoderForType varMap t2) (encoderForType varMap t3)

        (Can.TAlias moduName name nameTypePairs aliasType) ->
          encoderForType varMap (Can.TType moduName name (snd <$> nameTypePairs))
          -- encoderForType varMap (Type.dealias nameTypePairs aliasType)

        (Can.TLambda _ _) ->
          "Lamdera.Wire2.failEncode" -- <> strQuote (T.pack $ show x)
          -- error "Wire: disallowed partial record encountered. Please report this issue!"
  in
    T.intercalate "\n\n\n" ((unionCodecs <$> Map.toList _unions) <> (aliasCodecs <$> Map.toList _aliases))


nargs :: [Can.Type] -> [(T.Text, Can.Type)]
nargs xs =
  let
    f count t = ("v" <> T.pack (show count), t)
  in
    imap f xs


moduleToText (Canonical _ modu) = N.toText modu

leftWrap [] = ""
leftWrap (x:xs) = " " <> x <> leftWrap xs

sequenceEnc things = "Lamdera.Wire2.encodeSequence [" <> T.intercalate ", " things <> "]"
sequenceEncWithoutLength things = "Lamdera.Wire2.encodeSequenceWithoutLength [" <> T.intercalate ", " things <> "]"

p s = "(" <> s <> ")"

codec n = "w2_x_c_" <> (N.toText n)

decodeSucceed s = "Lamdera.Wire2.succeedDecode " <> s
strQuote s = T.pack (show (T.unpack s))
strEnc s = "Lamdera.Wire2.encodeString " <> strQuote s
unitEnc = "Lamdera.Wire2.encodeUnit"
pairEnc a b = "Lamdera.Wire2.encodePair " <> p a <> " " <> p b
tripleEnc a b c = "Lamdera.Wire2.encodeTriple " <> p a <> " " <> p b <> " " <> p c
unitDec = "Lamdera.Wire2.decodeUnit"
pairDec a b = "Lamdera.Wire2.decodePair " <> p a <> " " <> p b
tripleDec a b c = "Lamdera.Wire2.decodeTriple " <> p a <> " " <> p b <> " " <> p c
recEnc fields = "{ " <> T.intercalate ", " (N.toText <$> fields) <> " }"
recLit kvpairs = "{ " <> T.intercalate ", " ((\(k, v) -> k <> "=" <> v) <$> kvpairs) <> " }"

(-->) = (,)

evergreenCoreCodecs :: (Canonical -> T.Text) -> [Can.Type] -> (Canonical, N.Name) -> Maybe (T.Text, T.Text)
evergreenCoreCodecs qualIfNeeded targs key =
  let
    -- NOTE: Int argument is the number of type variables of the type in question; FIXME: arity is never used, so do we need to write it down? We know arity from targs context, right?
    failCodec n moduName = (typedFailEncode qualIfNeeded targs moduName, typedFailDecode qualIfNeeded targs moduName)

    notimplCodecs =
      ((\((pkg, modu, tipe), arity) ->
        let
          fqType = (Canonical (pkgFromText pkg) modu, tipe)
        in (fqType, failCodec arity fqType)) <$>
      -- non elm/core types
      -- NOTE: Most of these should get caught in the TypeDiff. The mapping there has been checked extensively against types in packages that are backed by Kernel.
      ( [ (("elm/bytes", "Bytes.Encode", "Encoder") --> 0 )
        , (("elm/bytes", "Bytes.Decode", "Decoder") --> 1 )
        -- , (("elm/bytes", "Bytes", "Endianness") --> 0 )
        , (("elm/virtual-dom", "VirtualDom", "Node") --> 1 )
        , (("elm/virtual-dom", "VirtualDom", "Attribute") --> 1 )
        , (("elm/virtual-dom", "VirtualDom", "Handler") --> 1 )
        -- Disable for now, but need to revisit these and whether we want actual proper wire support
        -- , (("elm/browser", "Browser", "UrlRequest") --> 0 )
        -- , (("elm/browser", "Browser.Navigation", "Key") --> 0 )
        , (("elm/file", "File", "File") --> 0 )
        -- not implemented yet, but needed by other pkgs
        , (("elm/core", "Process", "Id") --> 0 ) -- alias of Platform.ProcessId
        , (("elm/core", "Platform", "ProcessId") --> 0 ) -- time
        -- not needed by anything immediately, but we don't know how to encode these anyway, so let's fail them now
        , (("elm/core", "Platform", "Program") --> 3 ) -- idk
        , (("elm/core", "Platform", "Router") --> 2 ) -- idk
        , (("elm/core", "Platform", "Task") --> 2 ) -- idk
        , (("elm/core", "Platform.Cmd", "Cmd") --> 1 ) -- idk
        , (("elm/core", "Platform.Sub", "Sub") --> 1 )
        -- elm/json
        , (("elm/json", "Json.Encode", "Value") --> 0 ) -- js type
        , (("elm/json", "Json.Decode", "Decoder") --> 1 ) -- js type
        , (("elm/json", "Json.Decode", "Value") --> 0 ) -- js type
        -- elm/core
        , (("elm/core", "Task", "Task") --> 2 ) -- js type
        ]))
      <>
      ( (\((pkg, modu, tipe), res) -> ((Canonical (pkgFromText pkg) modu, tipe), res)) <$>
        -- elm/core types; these are exhaustive (but some types are commented out atm)
        (\(modu, tipe) -> ("elm/core", modu, tipe) --> ("Lamdera.Wire2.encode" <> N.toText tipe, "Lamdera.Wire2.decode" <> N.toText tipe)) <$>
        [ -- , ("Platform", "ProcessId")
        -- , ("Platform", "Program")
        -- , ("Platform", "Router")
        -- , ("Platform", "Task")
        -- , ("Platform.Cmd", "Cmd")
        -- , ("Platform.Sub", "Sub")
        ]
      )


    implementedCodecs =
      (\((pkg, modu, tipe), res) -> ((Canonical (pkgFromText pkg) modu, tipe), res)) <$>
      -- non elm/core types
      -- NOTE: Most of these should get caught in the TypeDiff. The mapping there has been checked extensively against types in packages that are backed by Kernel.
      ( [ (("elm/bytes", "Bytes", "Bytes") --> ("Lamdera.Wire2.encodeBytes", "Lamdera.Wire2.decodeBytes") )
        , (("elm/bytes", "Bytes", "Endianness") --> ("Lamdera.Wire2.encodeEndianness", "Lamdera.Wire2.decodeEndianness") )
        , (("elm/time", "Time", "Posix") --> ("(\\t -> Lamdera.Wire2.encodeInt (Time.posixToMillis t))", "Lamdera.Wire2.decodeInt |> Lamdera.Wire2.andThenDecode (\\t -> Lamdera.Wire2.succeedDecode (Time.millisToPosix t))"))
        ] <>
        (
          -- elm/core types; these are exhaustive (but some types are commented out atm)
          (\(modu, tipe) -> ("elm/core", modu, tipe) --> ("Lamdera.Wire2.encode" <> N.toText tipe, "Lamdera.Wire2.decode" <> N.toText tipe)) <$>
          [ ("Array", "Array")
          , ("Basics", "Bool")
          , ("Basics", "Float")
          , ("Basics", "Int")
          , ("Basics", "Never")
          , ("Basics", "Order")
          , ("Char", "Char")
          , ("Dict", "Dict")
          , ("List", "List") -- list type is only defined in kernel code, there's no `type List a = List` or similar in elm land. Accidentally maybe?
          , ("Maybe", "Maybe")
          -- , ("Platform", "ProcessId")
          -- , ("Platform", "Program")
          -- , ("Platform", "Router")
          -- , ("Platform", "Task")
          -- , ("Platform.Cmd", "Cmd")
          -- , ("Platform.Sub", "Sub")
          , ("Result", "Result")
          , ("Set", "Set")
          , ("String", "String")
          ]
        )
      )
  in
  -- NOTE: the injected failEncode and failDecode values follow a simple pattern; they take the (de/en)coders
  -- of type variables as arguments; one argument per type variable, in the same order as the type variables
  -- in the main type. A two-argument type that we want to `fail` must thus be wrapped in a two-argument lambda
  -- that drops both its arguments, for the generated source code to kind-check. Se examples below.
  Map.lookup key $
  Map.fromListWithKey (\k a b -> error (sShow ("Error: got duplicate values for a key", k, "namely", a, b, ". Please report this!"))) $ (notimplCodecs ++ implementedCodecs)


pkgFromText :: T.Text -> Pkg.Name
pkgFromText s =
  case T.splitOn "/" s of
    [p,m] -> Pkg.Name (textToUf8 p) (textToUf8 m)
    _ -> error ("Error: ran into a strange package: " <> T.unpack s <> ". Please report this!")


-- Record helpers

recNameVar varMap = "w2_rec_var" <> T.pack (show (varMap Map.! "w2_rec_var"))
recAccess varMap var = recNameVar varMap <> "." <> N.toText var

-- TODO: manually passing maps around is very verbose and error prone; make a monad wrapping variables and the text output instead

manyVars :: Map.Map N.Name Int -> [N.Name] -> (Map.Map N.Name Int, [T.Text])
manyVars varMap vars =
  let
    fn varMap vars doneVars =
      case vars of
        [] ->
          (varMap, reverse doneVars)
        (v:rest) ->
          let
            (newVarMap, vx) = newGenVar v varMap
          in
            fn newVarMap rest (vx:doneVars)
  in fn varMap vars []

newRecVar :: Map.Map N.Name Int -> (Map.Map N.Name Int, T.Text)
newRecVar =
  newGenVar "w2_rec_var"

getVar :: Map.Map N.Name Int -> N.Name -> T.Text
getVar varMap varName =
  (N.toText varName) <> T.pack (show (varMap Map.! varName))

newGenVar :: N.Name -> Map.Map N.Name Int -> (Map.Map N.Name Int, T.Text)
newGenVar varName varMap =
  let
    (newVarMap, var) =
      case varMap Map.!? varName of
        Just count -> (Map.insert varName (1 + count) varMap, N.toText varName <> T.pack (show (count+1)))
        Nothing -> (Map.insert varName 0 varMap, N.toText varName <> "0")
  in
    (newVarMap, var)

typedFailEncode :: (Canonical -> T.Text) -> [Can.Type] -> (Canonical, N.Name) -> T.Text
typedFailEncode qualIfNeeded targs (can, name) =
  -- We wrap the failEncode decoder in a type annotation to say what that targ should be, and then let type inference handle the type annotation for the whole encoder so we don't have to deal with supertypes.
  -- In some types, super types aren't written out as super types in the type definition, e.g. the elm/core Dict type is defined as `Dict k v` instead of `Dict comparable v`
  let
    failEnc 0 = "w2_typed_failure"
    failEnc n = p ("\\" <> T.replicate n "_ " <> "-> " <> "w2_typed_failure")

    -- CanSer-style Can.Type pretty-printer that respects aliased/exposed imports. It also never generates newlines.
    toElm tipe =
      case tipe of
        Can.TLambda t1 t2 -> p (toElm t1 <> " -> " <> toElm t2)
        Can.TVar name -> N.toText name
        Can.TType moduName name [] -> qualIfNeeded moduName <> N.toText name
        Can.TType moduName name types | elem moduName [ModuleName.list] -> p (N.toText name <> leftPadWith " " (toElm <$> types))
        Can.TType moduName name types -> p (qualIfNeeded moduName <> N.toText name <> leftPadWith " " (toElm <$> types))
        Can.TRecord nameFieldTypeMap Nothing -> "{ " <> T.intercalate ", " (ftMap nameFieldTypeMap) <> " }"
        Can.TRecord nameFieldTypeMap (Just name) -> "{ " <> N.toText name <> " | " <> T.intercalate ", " (ftMap nameFieldTypeMap) <> " }"
        Can.TUnit -> "()"
        Can.TTuple t1 t2 Nothing -> p (T.intercalate ", " (toElm <$> [t1, t2]))
        Can.TTuple t1 t2 (Just t3) -> p (T.intercalate ", " (toElm <$> [t1, t2, t3]))
        Can.TAlias moduName name [] _ -> qualIfNeeded moduName <> N.toText name
        Can.TAlias moduName name nameTypePairs _ -> p (qualIfNeeded moduName <> N.toText name <> leftPadWith " " (toElm <$> snd <$> nameTypePairs))
    ftMap x =
      (\(f, t) -> N.toText f <> " : " <> toElm (fieldType t)) <$> (Map.toList x)
    fieldType (Can.FieldType _ t) = t

  in
  T.replace "%CanType%" (T.intercalate " " ([qualIfNeeded can <> N.toText name] <> (toElm <$> targs))) $
  T.replace "%Lambda%" (failEnc (length targs)) $
  -- 8 space indentation to be indented more than body of case branches in generated code
  -- "(\n\
  -- \        let\n\
  -- \          w2_typed_failure : (%CanType%) -> Lamdera.Wire2.Encoder\n\
  -- \          w2_typed_failure = Lamdera.Wire2.failEncode\n\
  -- \        in %Lambda%\n\
  -- \        )"
  "Lamdera.Wire2.failEncode"

typedFailDecode :: (Canonical -> T.Text) -> [Can.Type] -> (Canonical, N.Name) -> T.Text
typedFailDecode qualIfNeeded targs (can, name) =
  -- since decoders have a type argument in their type, the generated codec has to have the correct type in there or we'll infer it to `a`.
  -- We wrap the failDecode decoder in a type annotation to say what that targ should be, and then let type inference handle the type annotation for the whole encoder so we don't have to deal with supertypes.
  -- In some types, super types aren't written out as super types in the type definition, e.g. the elm/core Dict type is defined as `Dict k v` instead of `Dict comparable v`
  let
    failDec 0 = "w2_typed_failure"
    failDec n = p ("\\" <> T.replicate n "_ " <> "-> " <> "w2_typed_failure")

    -- CanSer-style Can.Type pretty-printer that respects aliased/exposed imports. It also never generates newlines.
    toElm tipe =
      case tipe of
        Can.TLambda t1 t2 -> p (toElm t1 <> " -> " <> toElm t2)
        Can.TVar name -> N.toText name
        Can.TType moduName name [] -> qualIfNeeded moduName <> N.toText name
        Can.TType moduName name types | elem moduName [ModuleName.list] -> p (N.toText name <> leftPadWith " " (toElm <$> types))
        Can.TType moduName name types -> p (qualIfNeeded moduName <> N.toText name <> leftPadWith " " (toElm <$> types))
        Can.TRecord nameFieldTypeMap Nothing -> "{ " <> T.intercalate ", " (ftMap nameFieldTypeMap) <> " }"
        Can.TRecord nameFieldTypeMap (Just name) -> "{ " <> N.toText name <> " | " <> T.intercalate ", " (ftMap nameFieldTypeMap) <> " }"
        Can.TUnit -> "()"
        Can.TTuple t1 t2 Nothing -> p (T.intercalate ", " (toElm <$> [t1, t2]))
        Can.TTuple t1 t2 (Just t3) -> p (T.intercalate ", " (toElm <$> [t1, t2, t3]))
        Can.TAlias moduName name [] _ -> qualIfNeeded moduName <> N.toText name
        Can.TAlias moduName name nameTypePairs _ -> p (qualIfNeeded moduName <> N.toText name <> leftPadWith " " (toElm <$> snd <$> nameTypePairs))
    ftMap x =
      (\(f, t) -> N.toText f <> " : " <> toElm (fieldType t)) <$> (Map.toList x)
    fieldType (Can.FieldType _ t) = t

  in
  T.replace "%CanType%" (T.intercalate " " ([qualIfNeeded can <> N.toText name] <> (toElm <$> targs))) $
  T.replace "%Lambda%" (failDec (length targs)) $
  T.replace "%Name%" (N.toText name) $
  -- 8 space indentation to be indented more than body of case branches in generated code
  -- "(\n\
  -- \        let\n\
  -- \          w2_typed_failure : Lamdera.Wire2.Decoder (%CanType%)\n\
  -- \          w2_typed_failure = Lamdera.Wire2.failDecode \"%Name%\"\n\
  -- \        in %Lambda%\n\
  -- \        )"
  "Lamdera.Wire2.failDecode"