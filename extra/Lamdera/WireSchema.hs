{-# LANGUAGE OverloadedStrings #-}
module Lamdera.WireSchema
  ( Schema(..)
  , Def(..)
  , DefBody(..)
  , fromNodes
  , encode
  , writeForGraph
  , schemaPath
  , roots
  , keyString
  ) where

{- The wire schema: a JSON description of the Wire3 bytes of a Lamdera app's message types,
so tools outside the app (the runtime's event browser, the dashboard, the CLI) can name and
decode logged messages without the app's compiled JS.

Written to `elm-stuff/lamdera/.lamdera-wire-schema.json` whenever JS is generated for a
Lamdera app (`lamdera make` with an output, `lamdera live`, and `lamdera check`, which is
what production builds run). An app is recognised by a `Types` module in the project's own
package whose `ToBackend` has codecs.

The single source of truth is the codec IR Lamdera.Wire3.Direct stores in every module's
object graph (`w3_ir_<Type>`), which the direct JS codecs are rendered from, so the schema
describes exactly the bytes the codecs write. A type whose codecs could not be lowered to
IR (none are known; LAMDERA_WIRE3_DIRECT_DEBUG=1 lists them) is described as missing, and
decoders stop there.

FORMAT (version 1)

  { "version": 1                  -- this format's version, bumped on any incompatible change
  , "compiler": "1.5.1"           -- the Lamdera version that wrote it (informational)
  , "roots": { "ToBackend": "<key>", "BackendMsg": "<key>", "ToFrontend": "<key>", ... }
  , "types": { "<key>": <def>, ... }
  }

`roots` names the Lamdera core types that exist in the app's Types module (ToBackend,
BackendMsg, ToFrontend, FrontendMsg, BackendModel, FrontendModel). A <key> is
"author/project:Module.Name.Type", e.g. "author/project:Types.ToBackend". `types` holds every
type reachable from the roots, recursive ones included (by reference).

<def> is one of

  { "params": ["a", ...], "alias": <wire> }                 -- a type alias, or a kernel type
  { "params": [...], "union": { "tagWidth": 1 | 2, "ctors": [ { "name": "Ctor", "args": [<wire>, ...] }, ... ] } }
  { "params": [...], "missing": true }                      -- no codec IR, cannot be decoded

A union is written as its tag (an unsigned int of tagWidth bytes, little endian) then its
constructor's args in order. The tag is the constructor's index in `ctors`, which is in wire
order (constructors sorted by name), not declaration order. Every union is tagged, also one
with a single constructor.

<wire> describes one value. Leaves are strings:

  "Int"     Lamdera.Wire3.encodeInt64. With z the zigzag of the Int (i >= 0: 2i,
            i < 0: -2i - 1): z <= 215 is one byte z; z <= 9431 is two bytes
            216 + ((z - 216) >> 8), (z - 216) & 255; otherwise a byte 252, 253 or 254 then
            z in 2, 3 or 4 bytes big endian; past 2^32 a byte 255 then the Int itself (not
            z) as a float64 little endian.
  "Float"   float64, little endian
  "Bool"    one byte, 0 or 1
  "Order"   one byte: 0 LT, 1 EQ, 2 GT
  "String"  an Int byte length, then that many UTF-8 bytes
  "Char"    an Int, the code point
  "Unit"    nothing
  "Bytes"   an Int length, then the bytes
  "Posix"   an Int, Time.Posix milliseconds
  "Never"   can never be written or read (Never, and fields the codecs refuse)
  "JsRef"   a uint32 little endian: a reference into the JS context that encoded the value
            (elm/file File, WebGL Texture/Shader/Entity); meaningless anywhere else

and compound values are objects with a "$" kind:

  { "$": "List", "of": <wire> }               an Int count, then the items
  { "$": "Array", "of": <wire> }              same as List
  { "$": "Set", "of": <wire> }                same, in ascending order
  { "$": "SeqSet", "of": <wire> }             same, in insertion order
  { "$": "Dict", "key": <wire>, "value": <wire> }     an Int count, then key, value, ...
  { "$": "SeqDict", "key": <wire>, "value": <wire> }  same
  { "$": "Maybe", "of": <wire> }              one byte 0 Nothing, or 1 then the value
  { "$": "Result", "err": <wire>, "ok": <wire> }      one byte 0 then ok, or 1 then err
  { "$": "Tuple", "items": [<wire>, <wire>(, <wire>)] }  the items in order
  { "$": "Record", "fields": [ { "name": "f", "type": <wire> }, ... ] }  the fields in
                                              this order (wire order, NOT source order)
  { "$": "Floats", "n": 3 }                   linear-algebra Vec2/3/4 (n = 2/3/4) and Mat4
                                              (n = 16): one byte tag (1, or 0 meaning all
                                              zeros, written by 1.5.1 and earlier) then n
                                              float64s when the tag is 1
  { "$": "Var", "name": "a" }                 the def's type parameter `a`
  { "$": "Ref", "type": "<key>", "args": [<wire>, ...] }  another type, with its params

The reference decoder is runtime/js/wireSchema.js in the platform repo.
-}

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text.Encoding as T
import qualified Data.String
import System.FilePath ((</>))

import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as PrimModule
import qualified AST.Prim.Name as Name
import qualified Elm.Kernel as K
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Json.Encode as E

import qualified Lamdera
import qualified Lamdera.Version
import Lamdera.Wire3.Direct (IR(..), Codec(..), Body(..), Alt(..), W(..), Home)
import qualified Lamdera.Wire3.Direct as Direct


type Key = (Home, String)


data Schema =
  Schema
    { _roots :: [(String, Key)]
    , _types :: Map.Map Key Def
    }
    deriving (Show, Eq)


data Def
  = Def [String] DefBody
  | Missing [String]
  deriving (Show, Eq)


data DefBody
  = DAlias W
  | DUnion Int [(String, [W])] -- tag width, ctors in wire order
  deriving (Show, Eq)


-- The Lamdera core types, in the order they are listed in `roots`.
roots :: [String]
roots =
  [ "ToBackend", "BackendMsg", "ToFrontend", "FrontendMsg", "BackendModel", "FrontendModel" ]


schemaPath :: FilePath -> FilePath
schemaPath root =
  Lamdera.lamderaCache root </> ".lamdera-wire-schema.json"


{-| The schema of the app in package `pkg`, from a whole program's object graph, or Nothing
when it is not a Lamdera app. -}
fromNodes :: Pkg.Name -> Map.Map Opt.Global Opt.Node -> Maybe Schema
fromNodes pkg nodes =
  let
    typesHome = homeOf (ModuleName.Canonical pkg (PrimModule.fromChars "Types"))
    rootKeys = [ (r, (typesHome, r)) | r <- roots, Map.member (irGlobal (typesHome, r)) nodes ]
  in
  if not (any ((== "ToBackend") . fst) rootKeys) then
    Nothing
  else
    Just (Schema rootKeys (collect nodes (fmap snd rootKeys) Map.empty))


collect :: Map.Map Opt.Global Opt.Node -> [Key] -> Map.Map Key Def -> Map.Map Key Def
collect nodes todo acc =
  case todo of
    [] -> acc
    key : rest
      | Map.member key acc -> collect nodes rest acc
      | otherwise ->
          let def = defOf nodes key in
          collect nodes (defRefs def ++ rest) (Map.insert key def acc)


defOf :: Map.Map Opt.Global Opt.Node -> Key -> Def
defOf nodes key =
  case Map.lookup (irGlobal key) nodes of
    Just (Opt.Kernel [K.JS bs] _) ->
      case Direct.readIR bs of
        -- The decoder's IR always names every record field (the encoder's leaves the name
        -- of a field it refuses empty), and both describe the same bytes.
        Just (IR _ (Just codec)) -> codecDef codec
        Just (IR (Just codec) Nothing) -> codecDef codec
        _ -> Missing []
    _ -> Missing []


codecDef :: Codec -> Def
codecDef (Codec params body) =
  Def params (bodyDef body)
  where
    bodyDef b =
      case b of
        BAlias w -> DAlias w
        BUnion _ width alts -> DUnion width [ (name, ws) | Alt name _ ws <- alts ]
        BValidate _ inner -> bodyDef inner -- validation reads nothing more


defRefs :: Def -> [Key]
defRefs def =
  case def of
    Missing _ -> []
    Def _ (DAlias w) -> wRefs w
    Def _ (DUnion _ ctors) -> concatMap (concatMap wRefs . snd) ctors


wRefs :: W -> [Key]
wRefs w =
  case w of
    WList a -> wRefs a
    WMaybe a -> wRefs a
    WSet a -> wRefs a
    WArray a -> wRefs a
    WSeqSet a -> wRefs a
    WResult a b -> wRefs a ++ wRefs b
    WDict a b -> wRefs a ++ wRefs b
    WSeqDict a b -> wRefs a ++ wRefs b
    WPair a b -> wRefs a ++ wRefs b
    WTriple a b c -> wRefs a ++ wRefs b ++ wRefs c
    WRecord fs -> concatMap (wRefs . snd) fs
    WRef home t args -> (home, t) : concatMap wRefs args
    _ -> []


irGlobal :: Key -> Opt.Global
irGlobal ((author, project, modul), t) =
  Opt.Global
    (ModuleName.Canonical (Pkg.Name (Data.String.fromString author) (Data.String.fromString project)) (PrimModule.fromChars modul))
    (Name.fromChars ("w3_ir_" ++ t))


homeOf :: ModuleName.Canonical -> Home
homeOf (ModuleName.Canonical (Pkg.Name author project) modul) =
  (Pkg.authorToChars author, Pkg.projectToChars project, PrimModule.toChars modul)


keyString :: Key -> String
keyString ((author, project, modul), t) =
  author ++ "/" ++ project ++ ":" ++ modul ++ "." ++ t



-- JSON


encode :: Schema -> E.Value
encode (Schema rootKeys types) =
  E.object
    [ ("version", E.int 1)
    , ("compiler", E.chars Lamdera.Version.short)
    , ("roots", E.object [ (Data.String.fromString r, E.chars (keyString k)) | (r, k) <- rootKeys ])
    , ("types", E.object [ (Data.String.fromString (keyString k), encodeDef d) | (k, d) <- List.sortOn (keyString . fst) (Map.toList types) ])
    ]


encodeDef :: Def -> E.Value
encodeDef def =
  case def of
    Missing params ->
      E.object [ ("params", E.list E.chars params), ("missing", E.bool True) ]

    Def params (DAlias w) ->
      E.object [ ("params", E.list E.chars params), ("alias", encodeW w) ]

    Def params (DUnion width ctors) ->
      E.object
        [ ("params", E.list E.chars params)
        , ("union", E.object
            [ ("tagWidth", E.int width)
            , ("ctors", E.list (\(name, ws) -> E.object [ ("name", E.chars name), ("args", E.list encodeW ws) ]) ctors)
            ])
        ]


encodeW :: W -> E.Value
encodeW w =
  let
    kind k rest = E.object (("$", E.chars k) : rest)
    one k a = kind k [ ("of", encodeW a) ]
    kv k a b = kind k [ ("key", encodeW a), ("value", encodeW b) ]
  in
  case w of
    WInt -> E.chars "Int"
    WFloat -> E.chars "Float"
    WBool -> E.chars "Bool"
    WOrder -> E.chars "Order"
    WString -> E.chars "String"
    WChar -> E.chars "Char"
    WUnit -> E.chars "Unit"
    WBytes -> E.chars "Bytes"
    WTime -> E.chars "Posix"
    WFail -> E.chars "Never"
    WElm _ _ -> E.chars "JsRef" -- encodeRef/decodeRef, the only bridged leaf codec
    WFloats n -> kind "Floats" [ ("n", E.int n) ]
    WList a -> one "List" a
    WArray a -> one "Array" a
    WSet a -> one "Set" a
    WSeqSet a -> one "SeqSet" a
    WMaybe a -> one "Maybe" a
    WDict a b -> kv "Dict" a b
    WSeqDict a b -> kv "SeqDict" a b
    WResult e o -> kind "Result" [ ("err", encodeW e), ("ok", encodeW o) ]
    WPair a b -> kind "Tuple" [ ("items", E.list encodeW [a, b]) ]
    WTriple a b c -> kind "Tuple" [ ("items", E.list encodeW [a, b, c]) ]
    WRecord fs -> kind "Record" [ ("fields", E.list (\(f, fw) -> E.object [ ("name", E.chars f), ("type", encodeW fw) ]) fs) ]
    WTVar a -> kind "Var" [ ("name", E.chars a) ]
    WRef home t args -> kind "Ref" [ ("type", E.chars (keyString (home, t))), ("args", E.list encodeW args) ]



-- WRITING


{-| Writes the schema of the app being generated, if it is a Lamdera app (see the top). -}
writeForGraph :: Pkg.Name -> Opt.GlobalGraph -> IO ()
writeForGraph pkg graph =
  case fromNodes pkg (Opt._g_nodes graph) of
    Nothing -> pure ()
    Just schema -> do
      root <- Lamdera.getProjectRoot "WireSchema.writeForGraph"
      Lamdera.mkdir (Lamdera.lamderaCache root)
      let text = T.decodeUtf8 (BSL.toStrict (B.toLazyByteString (E.encode (encode schema))))
      Lamdera.writeIfDifferent (schemaPath root) text
