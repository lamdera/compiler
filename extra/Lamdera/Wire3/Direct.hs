{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MagicHash #-}
module Lamdera.Wire3.Direct
  ( addDirectIR
  , replaceCodecs
  , directEnabled
  , CodecCache, emptyCodecCache, replaceCodecsCached -- @LAMDERA the lamdera live session
  ) where

{- Direct JS Wire3 codecs (C2, docs/plans/wire3-codegen-research.md in the runtime repo).

Two halves:

1. `addDirectIR` runs at compile time, per module, right after optimize. It lowers the
   Elm codecs that Wire3.Core just generated (the Can.Expr combinator trees) into a
   small codec IR, and stores it in the module's object graph as an unreachable
   `Opt.Kernel` node named `w3_ir_<Type>`. Lowering from the generated Elm (rather than
   from the types again) reuses every decision Encoder.hs/Decoder.hs already made:
   alias chains, extensible records, unsupported types, tvar plumbing, field and ctor
   order. Both the encoder and the decoder are lowered, and the type only gets IR when
   the two agree, so the frozen semantics of each direction are kept even if the Elm
   pair were ever asymmetric. Anything the lowering doesn't recognise simply gets no IR,
   and that type keeps its Elm codecs.

2. `replaceCodecs` runs at JS generation time (Injection.graphModifications), where the
   Mode is known: dev/prod ctor tags and the --optimize field-name shortening (both are
   per build, so this can't happen earlier). It renders JS for every type that has IR
   and replaces the `w3_encode_<T>` / `w3_decode_<T>` nodes with kernel nodes that
   define the direct codecs plus Elm-facing wrappers of the original types, so every
   existing caller keeps working. Types without IR are called through their Elm codecs
   ("bridged"), in both directions.

On by default. LAMDERA_WIRE3_DIRECT=0 is the emergency escape hatch: it turns the
rendering off (the output is then exactly the Elm codecs, as before this module existed).
The IR is always lowered and stored regardless, so caches built with and without the
escape hatch are interchangeable. Caches are keyed by the version string alone (see
Lamdera.Version.artifacts), so a change to the IR format below needs a new version, or
caches written by the previous binary must be cleared: otherwise their types silently
fall back to the Elm codecs.
LAMDERA_WIRE3_DIRECT_DEBUG=1 reports every codec the lowering gives up on.

Validators and the two decoder chains (Wire3.Core, Wire3.Decoder): every type has a
validating decoder `w3_decode_<T>`, which for a custom type with a user-defined
`w3_validate_<T>` decodes the value and then runs the validator on it (an `Err` logs
its message through `Lamdera.Wire3.debug` and fails the decode), and a non-validating
`w3_unsafe_decode_<T>`. Each chain only references its own chain. Both are lowered and
rendered as separate decoder families (see `Fam`): the validating chain's IR is the
decoder half of `w3_ir_<T>`, the unsafe chain's is stored on its own as `w3_iru_<T>`.
A validated decoder is lowered to `BValidate`, rendered as the plain union decode
followed by the validator call, in the same order and with the same failure as the
Elm decoder's `andThenDecode`.
-}

import Control.Monad.State.Strict (State, runState, get, put, modify')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapS
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.FileEmbed (bsToExp)
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (addDependentFile)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import qualified System.Environment as Env

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified Data.Index as Index
import qualified AST.Prim.Module as PrimModule
import qualified AST.Prim.Name as Name
import qualified AST.Prim.Operator as Op
import qualified Data.String
import qualified Elm.Kernel as K
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Data.Utf8 as Utf8
import qualified Generate.JavaScript.Expression as Expr
import qualified Generate.JavaScript.Name as JsName
import qualified Generate.Mode as Mode
import qualified Reporting.Annotation as A

import qualified Lamdera.Relative
import qualified Lamdera.Wire3.Helpers as Helpers
import qualified CanSer.CanSer as ToSource
import qualified Data.Text as T


-- IR


type Home = (String, String, String) -- author, project, module


data W
  = WInt
  | WFloat
  | WBool
  | WOrder
  | WString
  | WChar
  | WUnit
  | WBytes
  | WTime
  | WFail                       -- failEncode/failDecode: writes nothing, never decodes
  | WElm String String          -- leaf codec bridged through lamdera/codecs (encodeRef/decodeRef)
  | WList W
  | WMaybe W
  | WSet W
  | WArray W
  | WResult W W                 -- err, ok
  | WDict W W
  | WPair W W
  | WTriple W W W
  | WSeqDict W W
  | WSeqSet W
  | WRecord [(String, W)]       -- wire order == sorted Elm field names
  | WTVar String
  | WRef Home String [W]        -- another type's codec, applied to codec args
  deriving (Show, Read, Eq)


data Body
  = BAlias W
  | BUnion String Int [Alt]     -- ctor opts, tag width in bytes, alts in wire order
  | BValidate String Body       -- decode Body, then run the type's own validator (its
                                -- top-level name, in the codec's module) on the value;
                                -- validating decoders only, never encoders
  deriving (Show, Read, Eq)


data Alt = Alt String Int [W]   -- ctor name, declaration index, params
  deriving (Show, Read, Eq)


data Codec = Codec [String] Body -- tvar names in parameter order
  deriving (Show, Read, Eq)


-- The encoder and decoder IR of one type, each lowered from its own Elm codec, so each
-- direction reproduces exactly what its Elm codec does. (They can legitimately differ
-- in shape: the Elm decoder inlines an extensible-record alias chain that the encoder
-- reaches through the alias's own codec.) Either can be missing; that direction then
-- keeps its Elm codec.
data IR = IR (Maybe Codec) (Maybe Codec)
  deriving (Show, Read, Eq)


-- READING STORED IR
--
-- The IR is stored in the .elmo as its derived `show`, and every one in the graph is
-- read back at JS generation time. The derived Read instance made that most of the JS
-- generation of a large app: 21GB of allocation and over 2s for a 920 module app, to
-- read ~6MB of IR. `readStored` reads exactly what `show` writes (constructor
-- applications with parentheses around arguments that are applications, lists,
-- tuples, string literals with Haskell escapes, non-negative Ints) in one pass, and
-- falls back to `reads` for anything it does not recognise, so it reads the same
-- values `reads` would.


readStored :: Read a => Parser a -> BS.ByteString -> Maybe a
readStored parser bs =
  case runParser parser bs of
    Just (value, rest) | BS.null rest ->
      Just value

    _ ->
      case reads (BS8.unpack bs) of
        [(value, "")] -> Just value
        _ -> Nothing


newtype Parser a =
  Parser { runParser :: BS.ByteString -> Maybe (a, BS.ByteString) }


instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \bs -> case p bs of
      Just (a, rest) -> Just (f a, rest)
      Nothing -> Nothing


instance Applicative Parser where
  pure a =
    Parser $ \bs -> Just (a, bs)

  Parser pf <*> Parser pa =
    Parser $ \bs -> case pf bs of
      Just (f, rest) -> case pa rest of
        Just (a, rest2) -> Just (f a, rest2)
        Nothing -> Nothing
      Nothing -> Nothing


instance Monad Parser where
  Parser pa >>= f =
    Parser $ \bs -> case pa bs of
      Just (a, rest) -> runParser (f a) rest
      Nothing -> Nothing


pFail :: Parser a
pFail =
  Parser (const Nothing)


pChar :: Char -> Parser ()
pChar c =
  Parser $ \bs -> case BS8.uncons bs of
    Just (c2, rest) | c2 == c -> Just ((), rest)
    _ -> Nothing


pPeek :: Parser (Maybe Char)
pPeek =
  Parser $ \bs -> Just (fst <$> BS8.uncons bs, bs)


-- A constructor name.
pIdent :: Parser BS.ByteString
pIdent =
  Parser $ \bs ->
    let (name, rest) = BS8.span (\c -> c == '_' || c == '\'' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9')) bs in
    if BS.null name then Nothing else Just (name, rest)


-- An argument of a constructor application (showsPrec 11): an application in
-- parentheses, or a nullary constructor, list or literal as is. (A tuple argument
-- also starts with a parenthesis, so tuples are read with pChar ' ' and pPair /
-- pTriple directly, never through here.)
pArg :: Parser a -> Parser a
pArg p =
  do  next <- pPeek
      case next of
        Just '(' -> pChar '(' *> p <* pChar ')'
        _ -> p


-- One application argument: a space, then the argument.
pSpaced :: Parser a -> Parser a
pSpaced p =
  pChar ' ' *> pArg p


pList :: Parser a -> Parser [a]
pList p =
  do  pChar '['
      next <- pPeek
      case next of
        Just ']' -> pChar ']' *> pure []
        _ ->
          let rest acc =
                do  c <- pPeek
                    case c of
                      Just ',' -> pChar ',' *> (p >>= \x -> rest (x : acc))
                      Just ']' -> pChar ']' *> pure (reverse acc)
                      _ -> pFail
          in
          p >>= \x -> rest [x]


pPair :: Parser a -> Parser b -> Parser (a, b)
pPair pa pb =
  (,) <$> (pChar '(' *> pa) <*> (pChar ',' *> pb <* pChar ')')


pTriple :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
pTriple pa pb pc =
  (,,) <$> (pChar '(' *> pa) <*> (pChar ',' *> pb) <*> (pChar ',' *> pc <* pChar ')')


pInt :: Parser Int
pInt =
  Parser $ \bs -> case BS8.readInt bs of
    Just (n, rest) | n >= 0 -> Just (n, rest)
    _ -> Nothing


-- A string literal as `show` writes it. Only the plain characters, the simple escapes
-- and decimal character codes are handled; anything else fails, and readStored falls
-- back to `reads`.
pString :: Parser String
pString =
  pChar '"' *> go []
  where
    go acc =
      Parser $ \bs ->
        let (plain, rest) = BS8.span (\c -> c /= '"' && c /= '\\') bs
            acc2 = reverse (BS8.unpack plain) ++ acc
        in
        case BS8.uncons rest of
          Just ('"', rest2) -> Just (reverse acc2, rest2)
          Just ('\\', rest2) ->
            case BS8.uncons rest2 of
              Just ('\\', rest3) -> runParser (go ('\\' : acc2)) rest3
              Just ('"', rest3) -> runParser (go ('"' : acc2)) rest3
              Just ('n', rest3) -> runParser (go ('\n' : acc2)) rest3
              Just ('t', rest3) -> runParser (go ('\t' : acc2)) rest3
              Just ('&', rest3) -> runParser (go acc2) rest3
              Just (d, _) | '0' <= d && d <= '9' ->
                -- a character past '\DEL', written as its decimal code (`show` of an
                -- identifier with accents)
                case BS8.readInt rest2 of
                  Just (code, rest3) | code <= 0x10FFFF -> runParser (go (toEnum code : acc2)) rest3
                  _ -> Nothing
              _ -> Nothing
          _ -> Nothing


pMaybe :: Parser a -> Parser (Maybe a)
pMaybe p =
  do  name <- pIdent
      case name of
        "Nothing" -> pure Nothing
        "Just" -> Just <$> pSpaced p
        _ -> pFail


pIR :: Parser IR
pIR =
  do  name <- pIdent
      case name of
        "IR" -> IR <$> pSpaced (pMaybe pCodec) <*> pSpaced (pMaybe pCodec)
        _ -> pFail


pCodec :: Parser Codec
pCodec =
  do  name <- pIdent
      case name of
        "Codec" -> Codec <$> pSpaced (pList pString) <*> pSpaced pBody
        _ -> pFail


pBody :: Parser Body
pBody =
  do  name <- pIdent
      case name of
        "BAlias" -> BAlias <$> pSpaced pW
        "BUnion" -> BUnion <$> pSpaced pString <*> pSpaced pInt <*> pSpaced (pList pAlt)
        "BValidate" -> BValidate <$> pSpaced pString <*> pSpaced pBody
        _ -> pFail


pAlt :: Parser Alt
pAlt =
  do  name <- pIdent
      case name of
        "Alt" -> Alt <$> pSpaced pString <*> pSpaced pInt <*> pSpaced (pList pW)
        _ -> pFail


pW :: Parser W
pW =
  do  name <- pIdent
      case name of
        "WInt" -> pure WInt
        "WFloat" -> pure WFloat
        "WBool" -> pure WBool
        "WOrder" -> pure WOrder
        "WString" -> pure WString
        "WChar" -> pure WChar
        "WUnit" -> pure WUnit
        "WBytes" -> pure WBytes
        "WTime" -> pure WTime
        "WFail" -> pure WFail
        "WElm" -> WElm <$> pSpaced pString <*> pSpaced pString
        "WList" -> WList <$> pSpaced pW
        "WMaybe" -> WMaybe <$> pSpaced pW
        "WSet" -> WSet <$> pSpaced pW
        "WArray" -> WArray <$> pSpaced pW
        "WResult" -> WResult <$> pSpaced pW <*> pSpaced pW
        "WDict" -> WDict <$> pSpaced pW <*> pSpaced pW
        "WPair" -> WPair <$> pSpaced pW <*> pSpaced pW
        "WTriple" -> WTriple <$> pSpaced pW <*> pSpaced pW <*> pSpaced pW
        "WSeqDict" -> WSeqDict <$> pSpaced pW <*> pSpaced pW
        "WSeqSet" -> WSeqSet <$> pSpaced pW
        "WRecord" -> WRecord <$> pSpaced (pList (pPair pString pW))
        "WTVar" -> WTVar <$> pSpaced pString
        "WRef" -> WRef <$> (pChar ' ' *> pTriple pString pString pString) <*> pSpaced pString <*> pSpaced (pList pW)
        _ -> pFail


-- DECODER FAMILIES
--
-- The validating chain (`w3_decode_<T>`, which runs `w3_validate_<T>` where one is
-- defined) and the non-validating chain (`w3_unsafe_decode_<T>`, for trusted data:
-- snapshot restore and Evergreen migrations) are lowered and rendered separately. Each
-- chain only ever references its own chain, so each family's direct functions, helpers
-- and hoisted closures get their own names.


data Fam =
  Fam
    { _famElm :: String     -- the Elm codec's prefix
    , _famDirect :: String  -- the direct decoder function's prefix
    , _famHelper :: String  -- hoisted statement helpers
    , _famArg :: String     -- hoisted closed codec arguments
    }


validatingFam, unsafeFam :: Fam
validatingFam = Fam "w3_decode_" "w3d_" "w3dh_" "w3da_"
unsafeFam = Fam "w3_unsafe_decode_" "w3du_" "w3duh_" "w3dua_"


-- LOWERING (compile time)


addDirectIR :: Pkg.Name -> Can.Module -> Opt.LocalGraph -> Opt.LocalGraph
addDirectIR pkg canonical graph@(Opt.LocalGraph main nodes fields) =
  if not (Helpers.shouldHaveCodecsGenerated pkg) then
    graph
  else
    let
      home = Can._name canonical
      modul = PrimModule.toChars (ModuleName._module home)
      defs = Helpers.declsToList (Can._decls canonical)
      byName = Map.fromList [ (Name.toChars (defNameOf d), d) | d <- defs ]
      codecs =
        Maybe.mapMaybe
          (\(name, encDef) ->
              case List.stripPrefix "w3_encode_" name of
                Just typeName ->
                  do  decDef <- Map.lookup ("w3_decode_" ++ typeName) byName
                      let ir = debugLowering modul typeName encDef decDef (lowerCodec encDef decDef)
                      case ir of
                        IR Nothing Nothing -> Nothing
                        _ -> Just (typeName, ir)
                Nothing -> Nothing
          )
          (Map.toList byName)
      -- The non-validating chain, stored on its own so the IR format of `w3_ir_` is
      -- unchanged.
      unsafeCodecs =
        Maybe.mapMaybe
          (\(name, decDef) ->
              case List.stripPrefix (_famElm unsafeFam) name of
                Just typeName ->
                  do  codec <- debugLoweringUnsafe modul typeName decDef (lowerDecoderDef unsafeFam decDef)
                      Just (typeName, codec)
                Nothing -> Nothing
          )
          (Map.toList byName)
      withIR =
        foldr
          (\(typeName, codec) acc ->
              Map.insert
                (Opt.Global home (Name.fromChars ("w3_ir_" ++ typeName)))
                (Opt.Kernel [K.JS (BS8.pack (show codec))] Set.empty)
                acc
          )
          nodes
          codecs
      newNodes =
        foldr
          (\(typeName, codec) acc ->
              Map.insert
                (Opt.Global home (Name.fromChars ("w3_iru_" ++ typeName)))
                (Opt.Kernel [K.JS (BS8.pack (show codec))] Set.empty)
                acc
          )
          withIR
          unsafeCodecs
    in
    Opt.LocalGraph main newNodes fields


{-# NOINLINE debugEnabled #-}
debugEnabled :: Bool
debugEnabled =
  unsafePerformIO $ fmap (`elem` [Just "1", Just "2"]) (Env.lookupEnv "LAMDERA_WIRE3_DIRECT_DEBUG")


-- LAMDERA_WIRE3_DIRECT_DEBUG=1 reports every codec the lowering gives up on, with the
-- generated Elm it could not lower (LAMDERA_WIRE3_DIRECT_DEBUG=2 for the full source).
debugLowering :: String -> String -> Can.Def -> Can.Def -> IR -> IR
debugLowering modul typeName encDef decDef ir@(IR enc dec) =
  if debugEnabled && (enc == Nothing || dec == Nothing) then
    unsafePerformIO $ do
      verbose <- fmap (== Just "2") (Env.lookupEnv "LAMDERA_WIRE3_DIRECT_DEBUG")
      let src d = if verbose then "\n" ++ T.unpack (ToSource.convert d) else ""
      putStrLn ("[wire3-direct] " ++ modul ++ "." ++ typeName
        ++ ": encoder " ++ maybe ("NOT LOWERED" ++ src encDef) (const "ok") enc
        ++ ", decoder " ++ maybe ("NOT LOWERED" ++ src decDef) (const "ok") dec)
      return ir
  else
    ir


debugLoweringUnsafe :: String -> String -> Can.Def -> Maybe Codec -> Maybe Codec
debugLoweringUnsafe modul typeName decDef codec =
  if debugEnabled && codec == Nothing then
    unsafePerformIO $ do
      verbose <- fmap (== Just "2") (Env.lookupEnv "LAMDERA_WIRE3_DIRECT_DEBUG")
      putStrLn ("[wire3-direct] " ++ modul ++ "." ++ typeName
        ++ ": unsafe decoder NOT LOWERED" ++ (if verbose then "\n" ++ T.unpack (ToSource.convert decDef) else ""))
      return codec
  else
    codec


(&) :: a -> (a -> b) -> b
x & f = f x


defNameOf :: Can.Def -> Name.Name
defNameOf def =
  case def of
    Can.Def (A.At _ n) _ _ -> n
    Can.TypedDef (A.At _ n) _ _ _ _ -> n


defParts :: Can.Def -> ([Can.Pattern], Can.Expr)
defParts def =
  case def of
    Can.Def _ pats body -> (pats, body)
    Can.TypedDef _ _ patsTyped body _ -> (fmap fst patsTyped, body)


patVar :: Can.Pattern -> Maybe String
patVar (A.At _ p) =
  case p of
    Can.PVar n -> Just (Name.toChars n)
    _ -> Nothing


tvarParam :: String -> Maybe String
tvarParam = List.stripPrefix "w3_x_c_"


lowerCodec :: Can.Def -> Can.Def -> IR
lowerCodec encDef decDef =
  IR (lowerEncoderDef encDef) (lowerDecoderDef validatingFam decDef)


lowerEncoderDef :: Can.Def -> Maybe Codec
lowerEncoderDef def =
  do  let (pats, body) = defParts def
      names <- traverse patVar pats
      case reverse names of
        "w3v" : revParams ->
          do  ps <- traverse tvarParam (reverse revParams)
              Codec ps <$> lowerUnionEnc body
        _ ->
          do  ps <- traverse tvarParam names
              Codec ps . BAlias <$> lowerEnc body


lowerDecoderDef :: Fam -> Can.Def -> Maybe Codec
lowerDecoderDef fam def =
  do  let (pats, body) = defParts def
          pfx = _famElm fam
      names <- traverse patVar pats
      ps <- traverse tvarParam names
      case lowerUnionDec pfx body of
        Just b -> Just (Codec ps b)
        Nothing ->
          -- Only the validating chain ever attaches a validator (Wire3.Core.decoderUnion).
          case (if pfx == _famElm validatingFam then lowerValidatedDec pfx body else Nothing) of
            Just b -> Just (Codec ps b)
            Nothing -> Codec ps . BAlias <$> lowerDec pfx body


homeOf :: ModuleName.Canonical -> Home
homeOf (ModuleName.Canonical (Pkg.Name author project) modul) =
  (Pkg.authorToChars author, Pkg.projectToChars project, PrimModule.toChars modul)


isHome :: Home -> ModuleName.Canonical -> Bool
isHome h m = homeOf m == h


wireHome, seqDictHome, seqSetHome, bytesEncodeHome, bytesDecodeHome, timeHome :: Home
wireHome = ("lamdera", "codecs", "Lamdera.Wire3")
seqDictHome = ("lamdera", "containers", "SeqDict")
seqSetHome = ("lamdera", "containers", "SeqSet")
bytesEncodeHome = ("elm", "bytes", "Bytes.Encode")
bytesDecodeHome = ("elm", "bytes", "Bytes.Decode")
timeHome = ("elm", "time", "Time")


-- ENCODER LOWERING


-- An encoder-valued expression (a function `a -> Encoder`).
lowerEnc :: Can.Expr -> Maybe W
lowerEnc expr@(A.At _ e) =
  case e of
    Can.Call f args -> lowerFn f args
    _ -> lowerFn expr []


-- An encoder function applied to its codec arguments (not to the value).
lowerFn :: Can.Expr -> [Can.Expr] -> Maybe W
lowerFn (A.At _ f) args =
  case f of
    Can.VarForeign home name _
      | isHome wireHome home ->
          case (Name.toChars name, args) of
            ("encodeInt", [])        -> Just WInt
            ("encodeFloat", [])      -> Just WFloat
            ("encodeBool", [])       -> Just WBool
            ("encodeOrder", [])      -> Just WOrder
            ("encodeString", [])     -> Just WString
            ("encodeChar", [])       -> Just WChar
            ("encodeUnit", [])       -> Just WUnit
            ("encodeBytes", [])      -> Just WBytes
            ("encodeNever", [])      -> Just WFail
            ("failEncode", [])       -> Just WFail
            ("encodeRef", [])        -> Just (WElm "encodeRef" "decodeRef")
            -- A field of type `Lamdera.Wire3.Bytes` (the codecs' alias of Bytes) makes the
            -- generator reference `Lamdera.Wire3.w3_encode_Bytes`, which doesn't exist. Such
            -- a codec can never be reached in compiled output (it would fail to generate),
            -- so treating it as the Bytes codec it stands for changes nothing observable.
            ("w3_encode_Bytes", [])  -> Just WBytes
            ("encodeList", [a])      -> WList <$> lowerEnc a
            ("encodeMaybe", [a])     -> WMaybe <$> lowerEnc a
            ("encodeSet", [a])       -> WSet <$> lowerEnc a
            ("encodeArray", [a])     -> WArray <$> lowerEnc a
            ("encodeResult", [x, y]) -> WResult <$> lowerEnc x <*> lowerEnc y
            ("encodeDict", [k, v])   -> WDict <$> lowerEnc k <*> lowerEnc v
            ("encodePair", [x, y])   -> WPair <$> lowerEnc x <*> lowerEnc y
            ("encodeTriple", [x, y, z]) -> WTriple <$> lowerEnc x <*> lowerEnc y <*> lowerEnc z
            _ -> Nothing
      | isHome seqDictHome home && Name.toChars name == "encodeDict" ->
          case args of
            [k, v] -> WSeqDict <$> lowerEnc k <*> lowerEnc v
            _ -> Nothing
      | isHome seqSetHome home && Name.toChars name == "encodeSet" ->
          case args of
            [a] -> WSeqSet <$> lowerEnc a
            _ -> Nothing
      | otherwise -> ref "w3_encode_" home name args lowerEnc

    Can.VarTopLevel home name -> ref "w3_encode_" home name args lowerEnc

    Can.VarLocal name | null args -> WTVar <$> tvarParam (Name.toChars name)

    Can.Lambda [A.At _ (Can.PVar v)] body | null args ->
      case lowerRecordBody v body of
        Just w -> Just w
        Nothing -> lowerPosixEnc v body

    _ -> Nothing


ref :: String -> ModuleName.Canonical -> Name.Name -> [Can.Expr] -> (Can.Expr -> Maybe W) -> Maybe W
ref prefix home name args lowerArg =
  case List.stripPrefix prefix (Name.toChars name) of
    Just typeName -> WRef (homeOf home) typeName <$> traverse lowerArg args
    Nothing -> Nothing


-- `\t -> encodeInt (Time.posixToMillis t)`
lowerPosixEnc :: Name.Name -> Can.Expr -> Maybe W
lowerPosixEnc v (A.At _ body) =
  case body of
    Can.Call (A.At _ (Can.VarForeign h1 n1 _)) [A.At _ (Can.Call (A.At _ (Can.VarForeign h2 n2 _)) [A.At _ (Can.VarLocal v2)])]
      | isHome wireHome h1 && Name.toChars n1 == "encodeInt"
          && isHome timeHome h2 && Name.toChars n2 == "posixToMillis" && v2 == v ->
          Just WTime
    _ -> Nothing


-- `\w3_rec_varN -> encodeSequenceWithoutLength [ ... ]`
lowerRecordBody :: Name.Name -> Can.Expr -> Maybe W
lowerRecordBody recVar (A.At _ body) =
  case body of
    Can.Call (A.At _ (Can.VarForeign home name _)) [A.At _ (Can.List fieldExprs)]
      | isHome wireHome home && Name.toChars name == "encodeSequenceWithoutLength" ->
          WRecord <$> traverse (lowerField recVar) fieldExprs
    _ ->
      Nothing


-- One field: `enc args... w3_rec_varN.field`, or `failEncode ()`.
lowerField :: Name.Name -> Can.Expr -> Maybe (String, W)
lowerField recVar expr@(A.At _ e) =
  case e of
    Can.Call _ args@(_:_)
      | A.At _ (Can.Access (A.At _ (Can.VarLocal v)) (A.At _ field)) <- last args
      , v == recVar ->
          fmap (\w -> (Name.toChars field, w)) (lowerApplied expr (const True))
    _ ->
      case lowerApplied expr (const False) of
        Just WFail -> Just ("", WFail)
        _ -> Nothing


-- An encoder applied to a value: `enc codecArgs... value`, where `isValue` recognises
-- the value argument. `failEncode ()` ignores its value.
lowerApplied :: Can.Expr -> (Can.Expr -> Bool) -> Maybe W
lowerApplied (A.At _ expr) isValue =
  case expr of
    Can.Call (A.At _ (Can.VarForeign home name _)) [A.At _ Can.Unit]
      | isHome wireHome home && Name.toChars name == "failEncode" -> Just WFail
    Can.Call f args | not (null args) && isValue (last args) ->
      lowerFn f (init args)
    _ -> Nothing


-- `case w3v of Ctor v0 v1 -> encodeSequenceWithoutLength [ tag, enc v0, enc v1 ]`
lowerUnionEnc :: Can.Expr -> Maybe Body
lowerUnionEnc (A.At _ body) =
  case body of
    Can.Case (A.At _ (Can.VarLocal w3v)) branches@(_:_) | Name.toChars w3v == "w3v" ->
      do  alts <- traverse lowerBranch (zip [0..] branches)
          let (opts, widths, alts_) = unzip3 alts
          case (List.nub opts, List.nub widths) of
            ([o], [wd]) -> Just (BUnion o wd alts_)
            _ -> Nothing
    _ -> Nothing
  where
    lowerBranch :: (Int, Can.CaseBranch) -> Maybe (String, Int, Alt)
    lowerBranch (i, Can.CaseBranch (A.At _ pat) handler) =
      case pat of
        Can.PCtor { Can._p_union = union, Can._p_name = ctorName, Can._p_index = index, Can._p_args = pargs } ->
          do  vars <- traverse (\(Can.PatternCtorArg _ _ p) -> patVar p) pargs
              (width, tag, params) <-
                case handler of
                  A.At _ (Can.Call (A.At _ (Can.VarForeign home name _)) [A.At _ (Can.List (t : ps))])
                    | isHome wireHome home && Name.toChars name == "encodeSequenceWithoutLength" ->
                        do  (wd, tg) <- tagOf t
                            Just (wd, tg, ps)
                  _ ->
                    do  (wd, tg) <- tagOf handler
                        Just (wd, tg, [])
              if tag /= i || length params /= length vars then Nothing else Just ()
              ws <- traverse (\(v, p) -> lowerApplied p (isLocal v)) (zip vars params)
              Just (optsName (Can._u_opts union), width, Alt (Name.toChars ctorName) (Index.toMachine index) ws)
        _ -> Nothing

    isLocal v (A.At _ e) =
      case e of
        Can.VarLocal n -> Name.toChars n == v
        _ -> False

    tagOf (A.At _ e) =
      case e of
        Can.Call (A.At _ (Can.VarForeign home name _)) [A.At _ (Can.Int n)]
          | isHome bytesEncodeHome home && Name.toChars name == "unsignedInt8" -> Just (1, n)
        Can.Call (A.At _ (Can.VarForeign home name _)) [_, A.At _ (Can.Int n)]
          | isHome bytesEncodeHome home && Name.toChars name == "unsignedInt16" -> Just (2, n)
        _ -> Nothing


optsName :: Can.CtorOpts -> String
optsName o =
  case o of
    Can.Normal -> "Normal"
    Can.Enum -> "Enum"
    Can.Unbox -> "Unbox"


-- DECODER LOWERING


-- `pfx` is the decoder family's prefix (`w3_decode_` or `w3_unsafe_decode_`): a reference
-- to another type's decoder must be to the same family, anything else is not lowered.
lowerDec :: String -> Can.Expr -> Maybe W
lowerDec pfx expr@(A.At _ e) =
  case e of
    Can.VarForeign home name _
      | isHome wireHome home ->
          case Name.toChars name of
            "decodeInt"    -> Just WInt
            "decodeFloat"  -> Just WFloat
            "decodeBool"   -> Just WBool
            "decodeOrder"  -> Just WOrder
            "decodeString" -> Just WString
            "decodeChar"   -> Just WChar
            "decodeUnit"   -> Just WUnit
            "decodeBytes"  -> Just WBytes
            "decodeNever"  -> Just WFail
            "failDecode"   -> Just WFail
            "decodeRef"    -> Just (WElm "encodeRef" "decodeRef")
            n | n == pfx ++ "Bytes" -> Just WBytes -- see "w3_encode_Bytes" in lowerFn
            _ -> Nothing
      | otherwise -> ref pfx home name [] (lowerDec pfx)

    Can.VarTopLevel home name -> ref pfx home name [] (lowerDec pfx)

    Can.VarLocal name -> WTVar <$> tvarParam (Name.toChars name)

    -- A record with no fields decodes as `succeedDecode (\ -> {})`, with no andMaps.
    Can.Call _ _ | Just w <- lowerRecordDec pfx expr -> Just w

    Can.Call (A.At _ f) args ->
      case f of
        Can.VarForeign home name _
          | isHome wireHome home ->
              case (Name.toChars name, args) of
                ("decodeList", [a])      -> WList <$> lowerDec pfx a
                ("decodeMaybe", [a])     -> WMaybe <$> lowerDec pfx a
                ("decodeSet", [a])       -> WSet <$> lowerDec pfx a
                ("decodeArray", [a])     -> WArray <$> lowerDec pfx a
                ("decodeResult", [x, y]) -> WResult <$> lowerDec pfx x <*> lowerDec pfx y
                ("decodeDict", [k, v])   -> WDict <$> lowerDec pfx k <*> lowerDec pfx v
                ("decodePair", [x, y])   -> WPair <$> lowerDec pfx x <*> lowerDec pfx y
                ("decodeTriple", [x, y, z]) -> WTriple <$> lowerDec pfx x <*> lowerDec pfx y <*> lowerDec pfx z
                _ -> Nothing
          | isHome seqDictHome home && Name.toChars name == "decodeDict" ->
              case args of
                [k, v] -> WSeqDict <$> lowerDec pfx k <*> lowerDec pfx v
                _ -> Nothing
          | isHome seqSetHome home && Name.toChars name == "decodeSet" ->
              case args of
                [a] -> WSeqSet <$> lowerDec pfx a
                _ -> Nothing
          | otherwise -> ref pfx home name args (lowerDec pfx)
        Can.VarTopLevel home name -> ref pfx home name args (lowerDec pfx)
        _ -> Nothing

    Can.Binop _ _ _ _ _ _ ->
      case lowerPosixDec expr of
        Just w -> Just w
        Nothing -> lowerRecordDec pfx expr

    _ -> Nothing


-- Flatten `a |> f |> g` into (a, [f, g]).
pipeline :: Can.Expr -> (Can.Expr, [Can.Expr])
pipeline expr@(A.At _ e) =
  case e of
    Can.Binop op _ _ _ l r | Op.toChars op == "|>" ->
      let (base, steps) = pipeline l in (base, steps ++ [r])
    _ -> (expr, [])


-- `andMapDecode d`
andMapArg :: Can.Expr -> Maybe Can.Expr
andMapArg (A.At _ e) =
  case e of
    Can.Call (A.At _ (Can.VarForeign home name _)) [d]
      | isHome wireHome home && Name.toChars name == "andMapDecode" -> Just d
    _ -> Nothing


succeedArg :: Can.Expr -> Maybe Can.Expr
succeedArg (A.At _ e) =
  case e of
    Can.Call (A.At _ (Can.VarForeign home name _)) [x]
      | isHome wireHome home && Name.toChars name == "succeedDecode" -> Just x
    _ -> Nothing


-- `decodeInt |> andThenDecode (\t -> succeedDecode (Time.millisToPosix t))`
lowerPosixDec :: Can.Expr -> Maybe W
lowerPosixDec expr =
  case pipeline expr of
    (A.At _ (Can.VarForeign h0 n0 _), [A.At _ (Can.Call (A.At _ (Can.VarForeign h1 n1 _)) [A.At _ (Can.Lambda [A.At _ (Can.PVar t)] body)])])
      | isHome wireHome h0 && Name.toChars n0 == "decodeInt"
          && isHome wireHome h1 && Name.toChars n1 == "andThenDecode" ->
          case succeedArg body of
            Just (A.At _ (Can.Call (A.At _ (Can.VarForeign h2 n2 _)) [A.At _ (Can.VarLocal t2)]))
              | isHome timeHome h2 && Name.toChars n2 == "millisToPosix" && t2 == t -> Just WTime
            _ -> Nothing
    _ -> Nothing


-- `succeedDecode (\a0 b0 -> { a = a0, b = b0 }) |> andMapDecode da |> andMapDecode db`
lowerRecordDec :: String -> Can.Expr -> Maybe W
lowerRecordDec pfx expr =
  let (base, steps) = pipeline expr in
  do  A.At _ (Can.Lambda pats (A.At _ (Can.Record fieldMap))) <- succeedArg base
      vars <- traverse patVar pats
      decs <- traverse andMapArg steps
      if length decs /= length vars || Map.size fieldMap /= length vars then Nothing else Just ()
      let byVar = Map.fromList [ (Name.toChars v, Name.toChars f) | (f, A.At _ (Can.VarLocal v)) <- Map.toList fieldMap ]
      names <- traverse (\v -> Map.lookup v byVar) vars
      ws <- traverse (lowerDec pfx) decs
      Just (WRecord (zip names ws))


-- `tagDecoder |> andThenDecode (\w3v -> case w3v of 0 -> succeedDecode Ctor |> andMapDecode d ...; _ -> failDecode)`
lowerUnionDec :: String -> Can.Expr -> Maybe Body
lowerUnionDec pfx expr =
  case pipeline expr of
    (tagDec, [A.At _ (Can.Call (A.At _ (Can.VarForeign h1 n1 _)) [A.At _ (Can.Lambda [A.At _ (Can.PVar w3v)] (A.At _ (Can.Case (A.At _ (Can.VarLocal w3v2)) branches)))])])
      | isHome wireHome h1 && Name.toChars n1 == "andThenDecode" && w3v == w3v2 ->
          do  width <- tagWidth tagDec
              (ctorBranches, lastBranch) <- unsnoc branches
              case lastBranch of
                Can.CaseBranch (A.At _ Can.PAnything) d | lowerDec pfx d == Just WFail -> Just ()
                _ -> Nothing
              alts <- traverse lowerAlt (zip [0..] ctorBranches)
              let (opts, alts_) = unzip alts
              case List.nub opts of
                [o] -> Just (BUnion o width alts_)
                _ -> Nothing
    _ -> Nothing
  where
    tagWidth (A.At _ e) =
      case e of
        Can.VarForeign home name _
          | isHome bytesDecodeHome home && Name.toChars name == "unsignedInt8" -> Just 1
        Can.Call (A.At _ (Can.VarForeign home name _)) [_]
          | isHome bytesDecodeHome home && Name.toChars name == "unsignedInt16" -> Just 2
        _ -> Nothing

    lowerAlt :: (Int, Can.CaseBranch) -> Maybe (String, Alt)
    lowerAlt (i, Can.CaseBranch (A.At _ pat) chain) =
      case pat of
        Can.PInt n | n == i ->
          let (base, steps) = pipeline chain in
          do  A.At _ (Can.VarCtor opts _ ctorName index _) <- succeedArg base
              decs <- traverse andMapArg steps
              ws <- traverse (lowerDec pfx) decs
              Just (optsName opts, Alt (Name.toChars ctorName) (Index.toMachine index) ws)
        _ -> Nothing

    unsnoc xs = if null xs then Nothing else Just (init xs, last xs)


-- A validated union decoder (Wire3.Decoder.wrapWithValidator), exactly:
--
--   <union decoder>
--     |> andThenDecode
--          (\w3_validate_value ->
--              case w3_validate_<T> w3_validate_value of
--                  Ok _ -> succeedDecode w3_validate_value
--                  Err w3_validate_err ->
--                      let _ = Lamdera.Wire3.debug w3_validate_err in failDecode
--          )
--
-- Anything that differs in any detail is not lowered (and keeps its Elm codec).
lowerValidatedDec :: String -> Can.Expr -> Maybe Body
lowerValidatedDec pfx (A.At _ e) =
  case e of
    Can.Binop op _ _ _ inner (A.At _ (Can.Call (A.At _ (Can.VarForeign h1 n1 _)) [A.At _ (Can.Lambda [A.At _ (Can.PVar x)] handler)]))
      | Op.toChars op == "|>" && isHome wireHome h1 && Name.toChars n1 == "andThenDecode" ->
          do  validator <- validation x handler
              body <- lowerUnionDec pfx inner
              Just (BValidate validator body)
    _ -> Nothing
  where
    resultHome = ("elm", "core", "Result")

    validation x (A.At _ handler) =
      case handler of
        Can.Case (A.At _ (Can.Call (A.At _ (Can.VarTopLevel _ vname)) [A.At _ (Can.VarLocal x1)])) [okBranch, errBranch]
          | x1 == x && List.isPrefixOf Helpers.w3ValidatePrefix (Name.toChars vname) ->
              do  okBody <- ctorBranch "Ok" okBranch (\p -> case p of { Can.PAnything -> Just (); _ -> Nothing })
                  case succeedArg okBody of
                    Just (A.At _ (Can.VarLocal x2)) | x2 == x -> Just ()
                    _ -> Nothing
                  errBody <- ctorBranch "Err" errBranch (\p -> case p of { Can.PVar n -> Just n; _ -> Nothing })
                  errVar <- ctorBranchVar errBranch
                  case errBody of
                    A.At _ (Can.Let (Can.Def (A.At _ under) [] (A.At _ (Can.Call (A.At _ (Can.VarForeign h2 n2 _)) [A.At _ (Can.VarLocal e1)]))) (A.At _ (Can.VarForeign h3 n3 _)))
                      | Name.toChars under == "_" && e1 == errVar
                          && isHome wireHome h2 && Name.toChars n2 == "debug"
                          && isHome wireHome h3 && Name.toChars n3 == "failDecode" ->
                          Just (Name.toChars vname)
                    _ -> Nothing
        _ -> Nothing

    -- `Ctor <arg>` of elm/core's Result, with the argument pattern accepted by `argOk`.
    ctorBranch ctor (Can.CaseBranch (A.At _ pat) body) argOk =
      case pat of
        Can.PCtor { Can._p_home = home, Can._p_name = name, Can._p_args = [Can.PatternCtorArg _ _ (A.At _ p)] }
          | isHome resultHome home && Name.toChars name == ctor ->
              do  _ <- argOk p
                  Just body
        _ -> Nothing

    ctorBranchVar (Can.CaseBranch (A.At _ pat) _) =
      case pat of
        Can.PCtor { Can._p_args = [Can.PatternCtorArg _ _ (A.At _ (Can.PVar n))] } -> Just n
        _ -> Nothing


-- RENDERING (JS generation time)


-- On unless LAMDERA_WIRE3_DIRECT=0 (the escape hatch).
{-# NOINLINE directEnabled #-}
directEnabled :: Bool
directEnabled =
  unsafePerformIO $ fmap (/= Just "0") (Env.lookupEnv "LAMDERA_WIRE3_DIRECT")


runtimeJs :: BS.ByteString
runtimeJs =
  -- addDependentFile: without it GHC doesn't rebuild this module when only the JS
  -- changes, and silently keeps embedding the old runtime.
  $(do  addDependentFile "extra/Lamdera/Wire3/direct-runtime.js"
        bsToExp =<< runIO (Lamdera.Relative.readByteString "extra/Lamdera/Wire3/direct-runtime.js"))


runtimeGlobal :: Opt.Global
runtimeGlobal =
  Opt.Global (keyHome wireHome) (Name.fromChars "$w3direct")


type Key = (Home, String)


data Ctx =
  Ctx
    { _mode :: Mode.Mode
    , _direct :: Map.Map Key Codec  -- the types being replaced
    , _key :: Key                   -- the type being rendered
    , _fam :: Fam                   -- the decoder family being rendered (decoders only)
    }


replaceCodecs :: Mode.Mode -> Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global Opt.Node
replaceCodecs mode graph =
  fst (replaceCodecsWith Nothing mode graph) -- @LAMDERA see replaceCodecsCached


-- CACHED REPLACING
--
-- @LAMDERA `lamdera live` (either mode) runs replaceCodecs on every rebuild, over every module's
-- codecs. Reading the stored IR of every type and rendering the JS of every reachable
-- codec again was a few hundred ms of each rebuild of a 920 module app, for codecs that
-- almost never change. replaceCodecsCached keeps both from the last build:
--
-- - the IR read from each w3_ir_/w3_iru_ node, reused while the node's bytes are the same
-- - each rendered codec node, reused while its Codec is the same and the direct arity of
--   every type it refers to (WRef, the only thing rendering looks up in `_direct`) is too.
--   A codec rendered fresh is rendered with `_direct` cut down to those types, so the kept
--   node (rendered lazily, maybe never) does not hold on to the whole map of that build.
--
-- The output is the same graph replaceCodecs gives. Use it with one Mode only.


data CodecCache =
  CodecCache
    { _ccIrs :: !(Map.Map Opt.Global (BS.ByteString, Maybe IR))
    , _ccUnsafe :: !(Map.Map Opt.Global (BS.ByteString, Maybe Codec))
    , _ccRendered :: !(Map.Map (String, Key) Rendered) -- by node name prefix and type
    }


data Rendered =
  Rendered !Codec ![Maybe Int] Opt.Node -- the codec, the direct arities of its WRefs, its node


emptyCodecCache :: CodecCache
emptyCodecCache =
  CodecCache Map.empty Map.empty Map.empty


replaceCodecsCached :: CodecCache -> Mode.Mode -> Map.Map Opt.Global Opt.Node -> (Map.Map Opt.Global Opt.Node, CodecCache)
replaceCodecsCached cache mode graph =
  case replaceCodecsWith (Just cache) mode graph of
    (graph_, Just cache_) -> (graph_, cache_)
    (graph_, Nothing) -> (graph_, emptyCodecCache)


-- Every type a codec refers to, the only keys rendering looks up (directArity).
codecRefs :: Codec -> [Key]
codecRefs (Codec _ body) =
  Set.toList (goBody body Set.empty)
  where
    goBody b acc =
      case b of
        BAlias w -> goW w acc
        BUnion _ _ alts -> foldr (\(Alt _ _ ws) a -> foldr goW a ws) acc alts
        BValidate _ inner -> goBody inner acc

    goW w acc =
      case w of
        WList a -> goW a acc
        WMaybe a -> goW a acc
        WSet a -> goW a acc
        WArray a -> goW a acc
        WSeqSet a -> goW a acc
        WResult a b -> goW a (goW b acc)
        WDict a b -> goW a (goW b acc)
        WSeqDict a b -> goW a (goW b acc)
        WPair a b -> goW a (goW b acc)
        WTriple a b c -> goW a (goW b (goW c acc))
        WRecord fs -> foldr (goW . snd) acc fs
        WRef home t args -> foldr goW (Set.insert (home, t) acc) args
        _ -> acc


strictArities :: Map.Map Key Codec -> [Key] -> [Maybe Int]
strictArities direct refs =
  let
    arity key =
      case Map.lookup key direct of
        Just (Codec ps _) -> let n = length ps in n `seq` Just n
        Nothing -> Nothing
    go ks =
      case ks of
        [] -> []
        k : rest -> let a = arity k; as = go rest in a `seq` as `seq` (a : as)
  in
  go refs


-- The original replaceCodecs, keeping and reusing a CodecCache when given one.
replaceCodecsWith :: Maybe CodecCache -> Mode.Mode -> Map.Map Opt.Global Opt.Node -> (Map.Map Opt.Global Opt.Node, Maybe CodecCache)
replaceCodecsWith maybeCache mode graph =
  if not directEnabled then (graph, maybeCache) else
  let
    -- @LAMDERA the IR of a node whose bytes are the same as last build's is not read again
    reuse :: Read a => (CodecCache -> Map.Map Opt.Global (BS.ByteString, Maybe a)) -> Parser a -> Opt.Global -> BS.ByteString -> Maybe a
    reuse field parser global bs =
      case maybeCache of
        Just cache
          | Just (oldBs, parsed) <- Map.lookup global (field cache)
          , oldBs == bs ->
              parsed
        _ ->
          readStored parser bs

    storedOf :: Read a => String -> (CodecCache -> Map.Map Opt.Global (BS.ByteString, Maybe a)) -> Parser a -> Map.Map Opt.Global (BS.ByteString, Maybe a)
    storedOf prefix field parser =
      Map.foldrWithKey
        (\global@(Opt.Global _ name) node acc ->
            case (List.isPrefixOf prefix (Name.toChars name), node) of
              (True, Opt.Kernel [K.JS bs] _) ->
                let parsed = reuse field parser global bs in
                parsed `seq` MapS.insert global (bs, parsed) acc
              _ -> acc
        )
        Map.empty
        graph

    storedIrs = storedOf "w3_ir_" _ccIrs pIR
    storedUnsafe = storedOf "w3_iru_" _ccUnsafe pCodec

    byKey :: String -> Map.Map Opt.Global (BS.ByteString, Maybe a) -> Map.Map Key a
    byKey prefix =
      Map.foldrWithKey
        (\(Opt.Global home name) (_, parsed) acc ->
            case (List.stripPrefix prefix (Name.toChars name), parsed) of
              (Just typeName, Just value) -> Map.insert (homeOf home, typeName) value acc
              _ -> acc
        )
        Map.empty

    irs :: Map.Map Key IR
    irs = byKey "w3_ir_" storedIrs

    -- The non-validating decoder chain (w3_unsafe_decode_<T>), stored on its own.
    unsafeIrs :: Map.Map Key Codec
    unsafeIrs = byKey "w3_iru_" storedUnsafe

    encoders = fixReplaceable graph "w3_encode_" (Map.mapMaybe (\(IR e _) -> e) irs)
    decoders = fixReplaceable graph (_famElm validatingFam) (Map.mapMaybe (\(IR _ d) -> d) irs)
    unsafeDecoders = fixReplaceable graph (_famElm unsafeFam) unsafeIrs

    -- @LAMDERA each codec's node, rendered (lazily, as always) or kept from the last build
    renderAll :: String -> (Ctx -> Codec -> (BS.ByteString, Set.Set Opt.Global)) -> Fam -> Map.Map Key Codec -> Map.Map Key Rendered
    renderAll prefix render fam direct =
      Map.mapWithKey
        (\key codec ->
            let
              fresh ctx =
                let (js, deps) = render ctx codec in
                Opt.Kernel [K.JS js] (Set.insert runtimeGlobal deps)
            in
            case maybeCache of
              Nothing ->
                Rendered codec [] (fresh (Ctx mode direct key fam))

              Just cache ->
                let
                  refs = codecRefs codec
                  arities = strictArities direct refs
                in
                case Map.lookup (prefix, key) (_ccRendered cache) of
                  Just kept@(Rendered oldCodec oldArities _)
                    | (isTrue# (reallyUnsafePtrEquality# oldCodec codec) || oldCodec == codec) && oldArities == arities ->
                        kept

                  _ ->
                    let small = Map.restrictKeys direct (Set.fromList refs) in
                    small `seq` Rendered codec arities (fresh (Ctx mode small key fam))
        )
        direct

    renderedEncoders = renderAll "w3_encode_" renderEncoderNode validatingFam encoders
    renderedDecoders = renderAll (_famElm validatingFam) renderDecoderNode validatingFam decoders
    renderedUnsafe = renderAll (_famElm unsafeFam) renderDecoderNode unsafeFam unsafeDecoders

    insertRendered prefix rendered acc0 =
      Map.foldrWithKey
        (\key (Rendered _ _ node) acc -> Map.insert (codecGlobal prefix key) node acc)
        acc0
        rendered

    withEncoders =
      insertRendered "w3_encode_" renderedEncoders graph

    withDecoders =
      insertRendered (_famElm unsafeFam) renderedUnsafe (insertRendered (_famElm validatingFam) renderedDecoders withEncoders)

    newCache =
      fmap
        (\_ ->
            let
              rendered =
                Map.unions
                  [ Map.mapKeysMonotonic ((,) "w3_encode_") renderedEncoders
                  , Map.mapKeysMonotonic ((,) (_famElm validatingFam)) renderedDecoders
                  , Map.mapKeysMonotonic ((,) (_famElm unsafeFam)) renderedUnsafe
                  ]
            in
            Map.foldl' (\a (Rendered _ _ _) -> a) () rendered `seq` CodecCache storedIrs storedUnsafe rendered
        )
        maybeCache

    -- Lamdera.Wire3.bytesEncode (an alias of Bytes.Encode.encode) becomes the runtime's
    -- _W3_bytesEncode, which hands a direct encoder's fresh result over without copying
    -- it again (see direct-runtime.js). Only when the alias is the plain definition.
    bytesEncodeGlobal = elmGlobal wireHome "bytesEncode"

    withBytesEncode =
      case Map.lookup bytesEncodeGlobal graph of
        Just (Opt.Define _ _) | not (Map.null encoders) ->
          Map.insert bytesEncodeGlobal
            (Opt.Kernel [K.JS (BS8.pack ("var " ++ globalJs bytesEncodeGlobal ++ " = _W3_bytesEncode;\n"))] (Set.singleton runtimeGlobal))
            withDecoders
        _ -> withDecoders
    -- LAMDERA_WIRE3_DIRECT_DEBUG=1: every w3_encode_/w3_decode_ definition in the graph
    -- that is NOT rendered direct (no IR, or not replaceable), i.e. every fallback to an
    -- Elm codec. (The graph holds every module's codecs, reachable or not.)
    fallbacks =
      [ PrimModule.toChars m ++ "." ++ n
      | (Opt.Global home@(ModuleName.Canonical _ m) name, _) <- Map.toList graph
      , let n = Name.toChars name
      , (direct, t) <-
          case (List.stripPrefix "w3_encode_" n, List.stripPrefix (_famElm validatingFam) n, List.stripPrefix (_famElm unsafeFam) n) of
            (Just t, _, _) -> [(Map.keysSet encoders, t)]
            (_, Just t, _) -> [(Map.keysSet decoders, t)]
            (_, _, Just t) -> [(Map.keysSet unsafeDecoders, t)]
            _ -> []
      , not (Set.member (homeOf home, t) direct)
      ]
    report =
      if debugEnabled
        then unsafePerformIO $ do
          putStrLn ("[wire3-direct] rendered " ++ show (Map.size encoders) ++ " encoders, "
            ++ show (Map.size decoders) ++ " decoders and " ++ show (Map.size unsafeDecoders)
            ++ " unsafe decoders direct; " ++ show (length fallbacks)
            ++ " codecs fall back to Elm" ++ (if null fallbacks then "" else ": " ++ List.intercalate ", " (take 50 fallbacks)))
          return ()
        else ()
  in
  flip (,) newCache $
  if Map.null encoders && Map.null decoders && Map.null unsafeDecoders then graph else
  report `seq` Map.insert runtimeGlobal (Opt.Kernel [K.JS (BS.append (BS8.pack (prelude mode)) runtimeJs)] runtimeDeps) withBytesEncode


{-| Which codecs (of one direction, named `prefix<Type>`) can be replaced. The node must
be a plain definition, or a member of a recursive group (an `Opt.Cycle` reached through
`Opt.Link`s) in which every member is being replaced too: a group is replaced all or
nothing, since its members are emitted together. Dropping one codec can break another's
group, so iterate to a fixpoint.
-}
fixReplaceable :: Map.Map Opt.Global Opt.Node -> String -> Map.Map Key Codec -> Map.Map Key Codec
fixReplaceable graph prefix candidates =
  let
    memberKeys :: [Name.Name] -> ModuleName.Canonical -> Maybe [Key]
    memberKeys names home =
      traverse
        (\n ->
            let s = Name.toChars n in
            case List.stripPrefix prefix s of
              Just t -> Just (homeOf home, t)
              Nothing -> Nothing
        )
        names

    nodeOk :: Map.Map Key Codec -> Opt.Global -> Bool
    nodeOk cands g@(Opt.Global home _) =
      case Map.lookup g graph of
        Just (Opt.Define _ _) -> True
        Just (Opt.DefineTailFunc _ _ _) -> True
        Just (Opt.Link cycleGlobal) ->
          case Map.lookup cycleGlobal graph of
            Just (Opt.Cycle names _ _ _) ->
              case memberKeys names home of
                Just keys -> all (\k -> Map.member k cands) keys
                Nothing -> False
            _ -> False
        _ -> False

    step cands =
      Map.filterWithKey
        (\key _ -> nodeOk cands (codecGlobal prefix key))
        cands

    go cands =
      let next = step cands in
      if Map.size next == Map.size cands then cands else go next
  in
  go candidates


keyHome :: Home -> ModuleName.Canonical
keyHome (author, project, modul) =
  ModuleName.Canonical (Pkg.Name (Data.String.fromString author) (Data.String.fromString project)) (PrimModule.fromChars modul)


codecGlobal :: String -> Key -> Opt.Global
codecGlobal prefix (home, typeName) =
  Opt.Global (keyHome home) (Name.fromChars (prefix ++ typeName))


elmGlobal :: Home -> String -> Opt.Global
elmGlobal home name =
  Opt.Global (keyHome home) (Name.fromChars name)


globalJs :: Opt.Global -> String
globalJs (Opt.Global home name) =
  jsNameChars (JsName.fromGlobal home name)


directGlobalJs :: Opt.Global -> String
directGlobalJs (Opt.Global home name) =
  jsNameChars (JsName.fromGlobalDirectFn home name)


jsNameChars :: JsName.Name -> String
jsNameChars n =
  BS8.unpack (BSL.toStrict (B.toLazyByteString (JsName.toBuilder n)))


-- Runtime dependencies: kernels and Elm globals the runtime references.
runtimeDeps :: Set.Set Opt.Global
runtimeDeps =
  Set.fromList
    [ Opt.toKernelGlobal (PrimModule.getKernel (PrimModule.fromChars "Elm.Kernel.List"))
    , Opt.toKernelGlobal (PrimModule.getKernel (PrimModule.fromChars "Elm.Kernel.Utils"))
    , Opt.toKernelGlobal (PrimModule.getKernel (PrimModule.fromChars "Elm.Kernel.Char"))
    , elmGlobal ("elm", "core", "Dict") "empty"
    , elmGlobal ("elm", "core", "Dict") "insert"
    , elmGlobal ("elm", "core", "Dict") "RBNode_elm_builtin"
    , elmGlobal ("elm", "core", "Dict") "Red"
    , elmGlobal ("elm", "core", "Dict") "Black"
    , elmGlobal ("elm", "core", "Basics") "LT"
    , elmGlobal ("elm", "core", "Basics") "EQ"
    , elmGlobal ("elm", "core", "Basics") "GT"
    , elmGlobal bytesEncodeHome "getWidth"
    , elmGlobal bytesEncodeHome "write"
    , elmGlobal bytesEncodeHome "encode" -- _Bytes_encode, for _W3_bytesEncode
    ]


isProd :: Mode.Mode -> Bool
isProd m = case m of { Mode.Prod _ -> True; Mode.Dev _ -> False }


-- Mode-specific constants and conversions the runtime needs.
prelude :: Mode.Mode -> String
prelude mode =
  if isProd mode then
    unlines
      [ "var _W3_T_RBNODE = -1, _W3_T_SUBTREE = 0;"
      , "function _W3_bytesNode(dv) { return { $: 10, a: dv }; }"
      , "function _W3_decoderValue(fn) { return fn; }"
      , "function _W3_decoderFnOf(dec) { return dec; }"
      ]
  else
    unlines
      [ "var _W3_T_RBNODE = 'RBNode_elm_builtin', _W3_T_SUBTREE = 'SubTree';"
      , "function _W3_bytesNode(dv) { return { $: 'Bytes', a: dv }; }"
      , "function _W3_decoderValue(fn) { return { $: 'Decoder', a: fn }; }"
      , "function _W3_decoderFnOf(dec) { return dec.a; }"
      ]


fieldJs :: Mode.Mode -> String -> String
fieldJs mode field =
  let name = Name.fromChars field in
  case mode of
    Mode.Dev _ -> jsNameChars (JsName.fromLocal name)
    Mode.Prod fields ->
      case Map.lookup name fields of
        Just short -> jsNameChars short
        -- Every field a codec touches is in the map, because the Elm codec defs that
        -- access it are still in the objects. This is a genuine compiler bug if it fires.
        Nothing -> error ("Wire3.Direct: no short name for field " ++ field)


ctorFieldJs :: Int -> String
ctorFieldJs i = jsNameChars (JsName.fromIndex (Index.ZeroBased i))


-- Mode-specific tags
nothingTag, okTag :: Mode.Mode -> String
nothingTag m = if isProd m then "1" else "'Nothing'"
okTag m = if isProd m then "0" else "'Ok'"


-- RENDER STATE


data RS =
  RS
    { _fresh :: Int
    , _deps :: Set.Set Opt.Global
    , _hoisted :: [String]
    }


type R = State RS


fresh :: R Int
fresh =
  do  s <- get
      put s { _fresh = _fresh s + 1 }
      return (_fresh s)


dep :: Opt.Global -> R ()
dep g = modify' (\s -> s { _deps = Set.insert g (_deps s) })


hoist :: String -> R ()
hoist js = modify' (\s -> s { _hoisted = _hoisted s ++ [js] })


runR :: R a -> (a, RS)
runR r = runState r (RS 0 Set.empty [])


type Env = Map.Map String String -- tvar name -> JS name of its direct codec


isClosed :: W -> Bool
isClosed w =
  case w of
    WTVar _ -> False
    WList a -> isClosed a
    WMaybe a -> isClosed a
    WSet a -> isClosed a
    WArray a -> isClosed a
    WSeqSet a -> isClosed a
    WResult a b -> isClosed a && isClosed b
    WDict a b -> isClosed a && isClosed b
    WSeqDict a b -> isClosed a && isClosed b
    WPair a b -> isClosed a && isClosed b
    WTriple a b c -> isClosed a && isClosed b && isClosed c
    WRecord fs -> all (isClosed . snd) fs
    WRef _ _ args -> all isClosed args
    _ -> True


directArity :: Ctx -> Key -> Maybe Int
directArity ctx key =
  fmap (\(Codec ps _) -> length ps) (Map.lookup key (_direct ctx))


directEncName :: Key -> String
directEncName key = globalJs (codecGlobal "w3e_" key)


directDecName :: Fam -> Key -> String
directDecName fam key = globalJs (codecGlobal (_famDirect fam) key)


hoistName :: Ctx -> String -> R String
hoistName ctx prefix =
  do  i <- fresh
      return (globalJs (codecGlobal prefix (_key ctx)) ++ "_" ++ show i)


indent :: [String] -> [String]
indent = fmap ("  " ++)


-- ENCODER RENDERING


renderEncoderNode :: Ctx -> Codec -> (BS.ByteString, Set.Set Opt.Global)
renderEncoderNode ctx codec@(Codec params body) =
  let
    key = _key ctx
    pnames = zipWith (\i _ -> "e" ++ show i) [(0 :: Int)..] params
    env = Map.fromList (zip params pnames)
    wrapper = globalJs (codecGlobal "w3_encode_" key)
    ((stmts, wrapperJs), rs) =
      runR $
        do  ss <- encBody body
            wj <- encWrapper ctx codec wrapper pnames
            return (ss, wj)
    js =
      unlines (_hoisted rs)
      ++ "function " ++ directEncName key ++ "(" ++ List.intercalate ", " ("v" : pnames) ++ ") {\n"
      ++ unlines (indent stmts)
      ++ "}\n"
      ++ wrapperJs

    -- Encoders never validate (the encoder IR never has BValidate).
    encBody b =
      case b of
        BAlias w -> enc ctx env 0 w "v"
        BUnion opts width alts -> encUnion ctx env opts width alts "v"
        BValidate _ inner -> encBody inner
  in
  (BS8.pack js, _deps rs)


-- Elm-facing `w3_encode_T : (a -> Encoder) -> ... -> T a -> Encoder`, callable with any
-- A<n> call shape (nested unary functions), tagged with `.w3e` for other direct codecs.
encWrapper :: Ctx -> Codec -> String -> [String] -> R String
encWrapper ctx (Codec params _) wrapper pnames =
  let key = _key ctx in
  if null params then
    return ("var " ++ wrapper ++ " = _W3_encWrap(" ++ directEncName key ++ ");\n")
  else
    let
      argNames = fmap ("a" ++) (fmap (drop 1) pnames)
      opens = concatMap (\a -> "function (" ++ a ++ ") { return ") argNames
      closes = concat (replicate (length argNames) "; }")
      conv = List.intercalate ", " (zipWith (\p a -> p ++ " = _W3_toDirectEnc(" ++ a ++ ")") pnames argNames)
      inner =
        "(function () { var " ++ conv ++ "; return _W3_encWrap(function (v) { "
        ++ directEncName key ++ "(" ++ List.intercalate ", " ("v" : pnames) ++ "); }); })()"
    in
    return ("var " ++ wrapper ++ " = " ++ opens ++ inner ++ closes ++ ";\n")


enc :: Ctx -> Env -> Int -> W -> String -> R [String]
enc ctx env d w v =
  let
    mode = _mode ctx
    sd = show d
  in
  case w of
    WInt -> return ["_W3_int(" ++ v ++ ");"]
    WFloat -> return ["_W3_f64(" ++ v ++ ");"]
    WBool -> return ["_W3_u8(" ++ v ++ " ? 1 : 0);"]
    WOrder ->
      if isProd mode
        then return ["_W3_u8(" ++ v ++ ");"]
        else return ["_W3_u8(" ++ v ++ ".$ === 'LT' ? 0 : " ++ v ++ ".$ === 'EQ' ? 1 : 2);"]
    WString -> return ["_W3_str(" ++ v ++ ");"]
    WChar -> return ["_W3_int(_Char_toCode(" ++ v ++ "));"]
    WUnit -> return []
    WBytes -> return ["_W3_bytes(" ++ v ++ ");"]
    WTime ->
      do  let g = elmGlobal timeHome "posixToMillis"
          dep g
          return ["_W3_int(" ++ globalJs g ++ "(" ++ v ++ "));"]
    WFail -> return []
    WElm e _ ->
      do  let g = elmGlobal wireHome e
          dep g
          return ["_W3_elmEnc(" ++ globalJs g ++ "(" ++ v ++ "));"]

    WList e -> encListLike ctx env d e v
    WSeqSet e ->
      do  let g = elmGlobal seqSetHome "toList"
          dep g
          body <- encListLike ctx env (d + 1) e ("l" ++ sd)
          return (("var l" ++ sd ++ " = " ++ globalJs g ++ "(" ++ v ++ ");") : body)
    WSeqDict k e ->
      do  let g = elmGlobal seqDictHome "toList"
          dep g
          body <- encListLike ctx env (d + 1) (WPair k e) ("l" ++ sd)
          return (("var l" ++ sd ++ " = " ++ globalJs g ++ "(" ++ v ++ ");") : body)

    WMaybe e ->
      do  inner <- enc ctx env (d + 1) e ("m" ++ sd ++ ".a")
          return
            ( [ "var m" ++ sd ++ " = " ++ v ++ ";"
              , "if (m" ++ sd ++ ".$ === " ++ nothingTag mode ++ ") { _W3_u8(0); } else {"
              , "  _W3_u8(1);"
              ] ++ indent inner ++ [ "}" ]
            )

    WResult errW okW ->
      do  es <- enc ctx env (d + 1) errW ("rs" ++ sd ++ ".a")
          os <- enc ctx env (d + 1) okW ("rs" ++ sd ++ ".a")
          return
            ( [ "var rs" ++ sd ++ " = " ++ v ++ ";"
              , "if (rs" ++ sd ++ ".$ === " ++ okTag mode ++ ") {"
              , "  _W3_u8(0);"
              ] ++ indent os ++ [ "} else {", "  _W3_u8(1);" ] ++ indent es ++ [ "}" ]
            )

    WDict k e ->
      do  ks <- enc ctx env (d + 1) k ("nd" ++ sd ++ ".b")
          vs <- enc ctx env (d + 1) e ("nd" ++ sd ++ ".c")
          return (dictWalk sd v (ks ++ vs))

    WSet e ->
      do  let dict = if isProd mode then v else v ++ ".a"
          ks <- enc ctx env (d + 1) e ("nd" ++ sd ++ ".b")
          return (dictWalk sd dict ks)

    WArray e ->
      do  inner <- enc ctx env (d + 1) e ("a" ++ sd ++ "[i" ++ sd ++ "]")
          return
            ( [ "var a" ++ sd ++ " = _W3_arrayToJs(" ++ v ++ ");"
              , "_W3_int(a" ++ sd ++ ".length);"
              , "for (var i" ++ sd ++ " = 0; i" ++ sd ++ " < a" ++ sd ++ ".length; i" ++ sd ++ "++) {"
              ] ++ indent inner ++ [ "}" ]
            )

    WPair a b ->
      do  as_ <- enc ctx env (d + 1) a (v ++ ".a")
          bs <- enc ctx env (d + 1) b (v ++ ".b")
          return (as_ ++ bs)

    WTriple a b c ->
      do  as_ <- enc ctx env (d + 1) a (v ++ ".a")
          bs <- enc ctx env (d + 1) b (v ++ ".b")
          cs <- enc ctx env (d + 1) c (v ++ ".c")
          return (as_ ++ bs ++ cs)

    WRecord fields ->
      -- A field whose codec is failEncode writes nothing and is never read, so it
      -- doesn't matter that its name can be "" (encoder-only IR; rendering uses the
      -- decoder's IR, which always has names).
      concat <$> traverse (\(f, fw) -> enc ctx env (d + 1) fw (v ++ "." ++ fieldJs mode f)) fields

    WTVar a ->
      return [lookupEnv env a ++ "(" ++ v ++ ");"]

    WRef home t args ->
      let key = (home, t) in
      if directArity ctx key == Just (length args)
        then
          do  fns <- traverse (encFnExpr ctx env) args
              dep (codecGlobal "w3_encode_" key)
              return [directEncName key ++ "(" ++ List.intercalate ", " (v : fns) ++ ");"]
        else
          do  elmArgs <- traverse (elmEncExpr ctx env) args
              let g = codecGlobal "w3_encode_" key
              dep g
              return ["_W3_elmEnc(" ++ aCall (globalJs g) (elmArgs ++ [v]) ++ ");"]


lookupEnv :: Env -> String -> String
lookupEnv env a =
  case Map.lookup a env of
    Just n -> n
    Nothing -> error ("Wire3.Direct: unbound tvar " ++ a)


encListLike :: Ctx -> Env -> Int -> W -> String -> R [String]
encListLike ctx env d e v =
  do  let sd = show d
      inner <- enc ctx env (d + 1) e ("c" ++ sd ++ ".a")
      return
        ( [ "var n" ++ sd ++ " = 0;"
          , "for (var c" ++ sd ++ " = " ++ v ++ "; c" ++ sd ++ ".b; c" ++ sd ++ " = c" ++ sd ++ ".b) n" ++ sd ++ "++;"
          , "_W3_int(n" ++ sd ++ ");"
          , "for (var c" ++ sd ++ " = " ++ v ++ "; c" ++ sd ++ ".b; c" ++ sd ++ " = c" ++ sd ++ ".b) {"
          ] ++ indent inner ++ [ "}" ]
        )


dictWalk :: String -> String -> [String] -> [String]
dictWalk sd dict body =
  [ "_W3_int(_W3_dictSize(" ++ dict ++ "));"
  , "var st" ++ sd ++ " = [], nd" ++ sd ++ " = " ++ dict ++ ";"
  , "for (;;) {"
  , "  while (nd" ++ sd ++ ".$ === _W3_T_RBNODE) { st" ++ sd ++ ".push(nd" ++ sd ++ "); nd" ++ sd ++ " = nd" ++ sd ++ ".d; }"
  , "  if (st" ++ sd ++ ".length === 0) break;"
  , "  nd" ++ sd ++ " = st" ++ sd ++ ".pop();"
  ] ++ indent body ++
  [ "  nd" ++ sd ++ " = nd" ++ sd ++ ".e;"
  , "}"
  ]


-- A direct encoder function value `(x) -> void` for a codec argument.
encFnExpr :: Ctx -> Env -> W -> R String
encFnExpr ctx env w =
  case w of
    WTVar a -> return (lookupEnv env a)
    WRef home t [] | directArity ctx (home, t) == Just 0 ->
      do  dep (codecGlobal "w3_encode_" (home, t))
          return (directEncName (home, t))
    _ ->
      do  body <- enc ctx env 0 w "x"
          let fn = "function (x) {\n" ++ unlines (indent body) ++ "}"
          if isClosed w
            then
              do  name <- hoistName ctx "w3ea_"
                  hoist ("var " ++ name ++ " = " ++ fn ++ ";")
                  return name
            else return fn


-- An Elm-level encoder `a -> Encoder` for a codec argument of a bridged type.
elmEncExpr :: Ctx -> Env -> W -> R String
elmEncExpr ctx env w =
  case w of
    WRef home t [] ->
      do  let g = codecGlobal "w3_encode_" (home, t)
          dep g
          return (globalJs g)
    _ ->
      do  fn <- encFnExpr ctx env w
          return ("_W3_encWrap(" ++ fn ++ ")")


-- Call an Elm function value with n args (A2..A9 for n >= 2, nested beyond 9).
aCall :: String -> [String] -> String
aCall fn args =
  case args of
    [] -> fn
    [x] -> fn ++ "(" ++ x ++ ")"
    _ | length args <= 9 -> "A" ++ show (length args) ++ "(" ++ List.intercalate ", " (fn : args) ++ ")"
    _ -> aCall (aCall fn (take 9 args)) (drop 9 args)


-- Unions: switch on the runtime representation, write the wire tag, then the params.
encUnion :: Ctx -> Env -> String -> Int -> [Alt] -> String -> R [String]
encUnion ctx env opts width alts v =
  let
    mode = _mode ctx
    (home, _) = _key ctx
    writeTag i = if width == 1 then "_W3_u8(" ++ show i ++ ");" else "_W3_u16(" ++ show i ++ ");"
    prodTag (Alt name index _) = show (Expr.ctorToInt (keyHome home) (Name.fromChars name) (Index.ZeroBased index))
    caseLabel alt@(Alt name _ _) = if isProd mode then prodTag alt else "'" ++ name ++ "'"
  in
  case (opts, alts) of
    ("Unbox", [Alt _ _ [p]]) ->
      do  let payload = if isProd mode then v else v ++ ".a"
          inner <- enc ctx env 1 p payload
          return (writeTag (0 :: Int) : inner)

    -- A single variant is written without looking at the value, as Elm's own `case` on it
    -- does, and so the Elm-level encoder did. Some such values are not that variant at
    -- runtime: Browser.Navigation.Key is declared `type Key = Key` but is a JS function,
    -- whose `.$` matched no case, so nothing was written and the decode failed (lamdera
    -- live freeze mode lost every FrontendModel holding a Key).
    (_, [Alt _ _ params]) ->
      do  inner <- concat <$> traverse (\(pi_, p) -> enc ctx env 1 p (v ++ "." ++ ctorFieldJs pi_)) (zip [0..] params)
          return (writeTag (0 :: Int) : inner)

    ("Enum", _) ->
      let
        subject = if isProd mode then v else v ++ ".$"
        -- prod Enum values are the declaration index (Opt.Enum / VarEnum)
        label alt@(Alt _ index _) = if isProd mode then show index else caseLabel alt
      in
      return
        ( ["switch (" ++ subject ++ ") {"]
          ++ concat (zipWith (\i alt -> ["  case " ++ label alt ++ ": " ++ writeTag i ++ " return;"]) [(0 :: Int)..] alts)
          ++ ["}"]
        )

    _ ->
      do  branches <-
            traverse
              (\(i, alt@(Alt _ _ params)) ->
                  do  inner <- concat <$> traverse (\(pi_, p) -> enc ctx env 1 p (v ++ "." ++ ctorFieldJs pi_)) (zip [0..] params)
                      return (["  case " ++ caseLabel alt ++ ":", "    " ++ writeTag i] ++ indent (indent inner) ++ ["    return;"])
              )
              (zip [(0 :: Int)..] alts)
          return (["switch (" ++ v ++ ".$) {"] ++ concat branches ++ ["}"])


-- DECODER RENDERING
--
-- Decoders are rendered as EXPRESSIONS, evaluated left to right (JS evaluates object
-- literal values and call arguments in order, and wire order is Elm's literal key order).
-- Since Elm 0.19.3 the wire order (Lamdera's byte order of the field names, see
-- Lamdera.Lexical) and Elm's literal key order (Ord on names: shortest first) can differ;
-- a record whose two orders differ decodes its fields into locals in wire order, in a
-- hoisted helper, and then builds the object in Elm's key order (see decE WRecord).
-- Anything that needs statements (a loop, a tag switch) goes into its own small hoisted
-- helper function that takes the codec's decoder parameters. This keeps every JS frame
-- small: a recursive decoder's frame no longer holds a register for every value any
-- branch decodes, so the direct decoder never runs out of stack before the Elm one
-- (see the cold-depth oracle in test/wire3-direct-proto/full/check.js).


renderDecoderNode :: Ctx -> Codec -> (BS.ByteString, Set.Set Opt.Global)
renderDecoderNode ctx codec@(Codec params body) =
  let
    key = _key ctx
    pnames = zipWith (\i _ -> "d" ++ show i) [(0 :: Int)..] params
    env = Map.fromList (zip params pnames)
    dctx = DCtx ctx env pnames
    wrapper = globalJs (codecGlobal (_famElm (_fam ctx)) key)
    ((stmts, wrapperJs), rs) =
      runR $
        do  ss <- decBody dctx body (\e -> "return " ++ e ++ ";")
            wj <- decWrapper ctx codec wrapper pnames
            return (ss, wj)
    js =
      unlines (_hoisted rs)
      ++ "function " ++ directDecName (_fam ctx) key ++ "(" ++ List.intercalate ", " pnames ++ ") {\n"
      ++ unlines (indent stmts)
      ++ "}\n"
      ++ wrapperJs
  in
  (BS8.pack js, _deps rs)


-- Statements that decode a codec body and hand the value to `k` (which makes the
-- statement that uses it, e.g. a return).
decBody :: DCtx -> Body -> (String -> String) -> R [String]
decBody dc body k =
  case body of
    BAlias w ->
      do  e <- decE dc w
          return [k e]

    BUnion _ width alts ->
      decUnion dc width alts k

    -- Wire3.Decoder.wrapWithValidator: the value is decoded completely first (inner
    -- validated values have been validated by then, as they finished decoding), then
    -- the validator runs on it. `Ok _` gives the value; `Err msg` logs msg through
    -- Lamdera.Wire3.debug and fails the decode, like the Elm decoder's
    -- `let _ = debug msg in failDecode` (a failed decode is a throw in both). Nothing is
    -- read after the value, so the bytes consumed are the same as without validation.
    BValidate validator inner ->
      do  let mode = _mode (_dctx dc)
              (home, _) = _key (_dctx dc)
              g = elmGlobal home validator
              gDebug = elmGlobal wireHome "debug"
          dep g
          dep gDebug
          i <- fresh
          let v = "w3v" ++ show i
              r = "w3r" ++ show i
          ss <-
            case inner of
              BUnion _ width alts -> decUnion dc width alts (\e -> v ++ " = " ++ e ++ "; break;")
              _ -> decBody dc inner (\e -> v ++ " = " ++ e ++ ";")
          return
            ( ["var " ++ v ++ ";"]
              ++ ss
              ++ [ "var " ++ r ++ " = " ++ globalJs g ++ "(" ++ v ++ ");"
                 , "if (" ++ r ++ ".$ !== " ++ okTag mode ++ ") { " ++ globalJs gDebug ++ "(" ++ r ++ ".a); throw 0; }"
                 , k v
                 ]
            )


decWrapper :: Ctx -> Codec -> String -> [String] -> R String
decWrapper ctx (Codec params _) wrapper pnames =
  let key = _key ctx in
  if null params then
    return ("var " ++ wrapper ++ " = _W3_decWrap(" ++ directDecName (_fam ctx) key ++ ");\n")
  else
    let
      argNames = fmap ("a" ++) (fmap (drop 1) pnames)
      opens = concatMap (\a -> "function (" ++ a ++ ") { return ") (init argNames)
      lastArg = last argNames
      closes = concat (replicate (length argNames - 1) "; }")
      conv = List.intercalate ", " (zipWith (\p a -> p ++ " = _W3_toDirectDec(" ++ a ++ ")") pnames argNames)
      inner =
        "function (" ++ lastArg ++ ") { var " ++ conv ++ "; return _W3_decWrap(function () { return "
        ++ directDecName (_fam ctx) key ++ "(" ++ List.intercalate ", " pnames ++ "); }); }"
    in
    return ("var " ++ wrapper ++ " = " ++ opens ++ inner ++ closes ++ ";\n")


data DCtx =
  DCtx
    { _dctx :: Ctx
    , _denv :: Env
    , _dparams :: [String] -- the codec's decoder parameters, passed on to every helper
    }


-- A hoisted helper function that decodes `w`; returns the call expression. It takes only
-- the decoder parameters `w` actually uses (its free type variables): helpers are
-- module-level functions, and their call expressions can end up inside module-level
-- hoisted closures (closed codec arguments), where the codec's own parameters are not in
-- scope. A closed `w` gets a helper with no parameters at all.
decHelper :: DCtx -> W -> [String] -> R String
decHelper dc w body =
  do  name <- hoistName (_dctx dc) (_famHelper (_fam (_dctx dc)))
      let used = freeTvars w
          ps = List.intercalate ", " [ n | (tv, n) <- Map.toList (_denv dc), Set.member tv used ]
      hoist ("function " ++ name ++ "(" ++ ps ++ ") {\n" ++ unlines (indent body) ++ "}")
      return (name ++ "(" ++ ps ++ ")")


freeTvars :: W -> Set.Set String
freeTvars w =
  case w of
    WTVar a -> Set.singleton a
    WList a -> freeTvars a
    WMaybe a -> freeTvars a
    WSet a -> freeTvars a
    WArray a -> freeTvars a
    WSeqSet a -> freeTvars a
    WResult a b -> Set.union (freeTvars a) (freeTvars b)
    WDict a b -> Set.union (freeTvars a) (freeTvars b)
    WSeqDict a b -> Set.union (freeTvars a) (freeTvars b)
    WPair a b -> Set.union (freeTvars a) (freeTvars b)
    WTriple a b c -> Set.unions [freeTvars a, freeTvars b, freeTvars c]
    WRecord fs -> Set.unions (fmap (freeTvars . snd) fs)
    WRef _ _ args -> Set.unions (fmap freeTvars args)
    _ -> Set.empty


-- An expression that decodes one value.
decE :: DCtx -> W -> R String
decE dc w =
  let
    ctx = _dctx dc
    env = _denv dc
  in
  case w of
    WInt -> return "_W3_rint()"
    WFloat -> return "_W3_rf64()"
    WString -> return "_W3_rstr()"
    WChar -> return "_Char_fromCode(_W3_rint())"
    WUnit -> return "_Utils_Tuple0"
    WBytes -> return "_W3_rbytes()"
    WBool -> return "_W3_rbool()"
    WOrder -> return "_W3_rorder()"
    WFail -> return "_W3_fail()"
    WTime ->
      do  let g = elmGlobal timeHome "millisToPosix"
          dep g
          return (globalJs g ++ "(_W3_rint())")
    WElm _ dn ->
      do  let g = elmGlobal wireHome dn
          dep g
          return ("_W3_elmDec(" ++ globalJs g ++ ")")

    WList e ->
      do  ee <- decE dc e
          decHelper dc w (listLoop ee)

    WArray e ->
      do  lst <- decE dc (WList e)
          let g = elmGlobal ("elm", "core", "Array") "fromList"
          dep g
          return (globalJs g ++ "(" ++ lst ++ ")")

    WSeqSet e ->
      do  lst <- decE dc (WList e)
          let g = elmGlobal seqSetHome "fromList"
          dep g
          return (globalJs g ++ "(" ++ lst ++ ")")

    WSeqDict k e ->
      do  lst <- decE dc (WList (WPair k e))
          let g = elmGlobal seqDictHome "fromList"
          dep g
          return (globalJs g ++ "(" ++ lst ++ ")")

    WMaybe e ->
      do  ee <- decE dc e
          let gJust = elmGlobal ("elm", "core", "Maybe") "Just"
              gNothing = elmGlobal ("elm", "core", "Maybe") "Nothing"
          dep gJust
          dep gNothing
          decHelper dc w
            [ "switch (_W3_ru8()) {"
            , "  case 0: return " ++ globalJs gNothing ++ ";"
            , "  case 1: return " ++ globalJs gJust ++ "(" ++ ee ++ ");"
            , "  default: throw 0;"
            , "}"
            ]

    WResult errW okW ->
      do  oe <- decE dc okW
          ee <- decE dc errW
          let gOk = elmGlobal ("elm", "core", "Result") "Ok"
              gErr = elmGlobal ("elm", "core", "Result") "Err"
          dep gOk
          dep gErr
          decHelper dc w
            [ "switch (_W3_ru8()) {"
            , "  case 0: return " ++ globalJs gOk ++ "(" ++ oe ++ ");"
            , "  case 1: return " ++ globalJs gErr ++ "(" ++ ee ++ ");"
            , "  default: throw 0;"
            , "}"
            ]

    WDict k e ->
      do  ke <- decE dc k
          ve <- decE dc e
          decHelper dc w (pairsLoop ke ve ++ ["return _W3_dictFromPairs(ks, vs);"])

    WSet e ->
      do  ke <- decE dc e
          let g = elmGlobal ("elm", "core", "Set") "Set_elm_builtin"
          dep g
          decHelper dc w (pairsLoop ke "_Utils_Tuple0" ++ ["return " ++ globalJs g ++ "(_W3_dictFromPairs(ks, vs));"])

    WPair a b ->
      do  ae <- decE dc a
          be <- decE dc b
          return ("_Utils_Tuple2(" ++ ae ++ ", " ++ be ++ ")")

    WTriple a b c ->
      do  ae <- decE dc a
          be <- decE dc b
          ce <- decE dc c
          return ("_Utils_Tuple3(" ++ ae ++ ", " ++ be ++ ", " ++ ce ++ ")")

    WRecord fields ->
      do  parts <- traverse (\(f, fw) -> do { e <- decE dc fw; return (f, e) }) fields
          let
            mode = _mode ctx
            -- Elm's record literal key order (Generate.JavaScript.Expression.generateRecord
            -- emits Map.toList over the field names), so a decoded record has the same
            -- shape (V8 hidden class, key order) as one the program builds itself.
            literalOrder = List.sortOn (\(f, _) -> Name.fromChars f) parts
          if fmap fst literalOrder == fmap fst parts
            then
              return ("{ " ++ List.intercalate ", " (fmap (\(f, e) -> fieldJs mode f ++ ": " ++ e) parts) ++ " }")
            else
              -- Read the fields in wire order into locals, then build the literal.
              let
                locals = zip [(0 :: Int)..] parts
                localOf f = maybe (error ("Wire3.Direct: no local for field " ++ f)) (\(i, _) -> "f" ++ show i) (List.find (\(_, (g, _)) -> g == f) locals)
              in
              decHelper dc w
                ( fmap (\(i, (_, e)) -> "var f" ++ show i ++ " = " ++ e ++ ";") locals
                  ++ [ "return { " ++ List.intercalate ", " (fmap (\(f, _) -> fieldJs mode f ++ ": " ++ localOf f) literalOrder) ++ " };" ]
                )

    WTVar a -> return (lookupEnv env a ++ "()")

    WRef home t args ->
      let key = (home, t) in
      if directArity ctx key == Just (length args)
        then
          do  fns <- traverse (decFnExpr dc) args
              dep (codecGlobal (_famElm (_fam ctx)) key)
              return (directDecName (_fam ctx) key ++ "(" ++ List.intercalate ", " fns ++ ")")
        else
          do  elmArgs <- traverse (elmDecExpr dc) args
              let g = codecGlobal (_famElm (_fam ctx)) key
              dep g
              return ("_W3_elmDec(" ++ aCall (globalJs g) elmArgs ++ ")")


-- A List, built front to back (no reverse, no intermediate array).
listLoop :: String -> [String]
listLoop elemExpr =
  [ "var r = _List_Nil, t = null;"
  , "for (var n = _W3_rint(); n > 0; n--) {"
  , "  var c = _List_Cons(" ++ elemExpr ++ ", _List_Nil);"
  , "  if (t === null) r = c; else t.b = c;"
  , "  t = c;"
  , "}"
  , "return r;"
  ]


-- Keys and values of a Dict/Set, in wire order (key before value). The arrays start as
-- generic-element arrays ([null], then emptied): an array that only ever held numbers
-- gets an unboxed double backing store, which quiets signalling NaNs, and the bits of a
-- decoded Float must survive to be re-encoded identically.
pairsLoop :: String -> String -> [String]
pairsLoop keyExpr valExpr =
  [ "var ks = [null], vs = [null];"
  , "ks.length = 0; vs.length = 0;"
  , "for (var n = _W3_rint(); n > 0; n--) {"
  , "  ks.push(" ++ keyExpr ++ ");"
  , "  vs.push(" ++ valExpr ++ ");"
  , "}"
  ]


-- A direct decoder function value `() -> value` for a codec argument.
decFnExpr :: DCtx -> W -> R String
decFnExpr dc w =
  let ctx = _dctx dc in
  case w of
    WTVar a -> return (lookupEnv (_denv dc) a)
    WRef home t [] | directArity ctx (home, t) == Just 0 ->
      do  dep (codecGlobal (_famElm (_fam ctx)) (home, t))
          return (directDecName (_fam ctx) (home, t))
    _ ->
      do  e <- decE dc w
          let fn = "function () { return " ++ e ++ "; }"
          if isClosed w
            then
              do  name <- hoistName ctx (_famArg (_fam ctx))
                  hoist ("var " ++ name ++ " = " ++ fn ++ ";")
                  return name
            else return fn


elmDecExpr :: DCtx -> W -> R String
elmDecExpr dc w =
  case w of
    WRef home t [] ->
      do  let g = codecGlobal (_famElm (_fam (_dctx dc))) (home, t)
          dep g
          return (globalJs g)
    _ ->
      do  fn <- decFnExpr dc w
          return ("_W3_decWrap(" ++ fn ++ ")")


-- `k` makes the statement that uses a decoded value (a return, or an assignment and a
-- break).
decUnion :: DCtx -> Int -> [Alt] -> (String -> String) -> R [String]
decUnion dc width alts k =
  let
    (home, _) = _key (_dctx dc)
    readTag = if width == 1 then "_W3_ru8()" else "_W3_ru16()"
  in
  do  branches <-
        traverse
          (\(tag, Alt name _ params) ->
              do  args <- traverse (decE dc) params
                  let g = elmGlobal home name
                      ctorExpr =
                        case args of
                          [] -> globalJs g
                          [x] -> globalJs g ++ "(" ++ x ++ ")"
                          _ -> directGlobalJs g ++ "(" ++ List.intercalate ", " args ++ ")"
                  dep g
                  return ("  case " ++ show tag ++ ": " ++ k ctorExpr)
          )
          (zip [(0 :: Int)..] alts)
      return (["switch (" ++ readTag ++ ") {"] ++ branches ++ ["  default: throw 0;", "}"])
