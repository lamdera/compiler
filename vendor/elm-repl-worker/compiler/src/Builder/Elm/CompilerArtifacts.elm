{- MANUALLY FORMATTED -}
module Builder.Elm.CompilerArtifacts exposing
  ( getFingerprints
  , getInterfaces
  , getGlobalGraph
  )


{- @LAMDERA Decoders for the package artifacts the Lamdera compiler writes
(~/.elm/<version>/packages/<pkg>/<vsn>/artifacts.lamdera-<...>.dat), so the REPL
worker can reuse what `lamdera live` already compiled instead of compiling every
package again in the browser.

The compiler is built on Elm 0.19.3, whose binary format is not the Data.Binary one
this worker's own codecs implement (they are a port of 0.19.1/0.19.2): it is
little-endian, counts are u8/u16/u32 sized per field rather than Int64, strings carry
a u8/u32/u64 length, regions are packed 0-based positions, and a few AST
constructors changed. Each decoder below mirrors the compiler's d*/e* functions
(named in the comments) and builds this worker's AST, translating where 0.19.3's
representation differs:

  - Opt.Pair / Opt.Triple and Can.TPair / Can.TTriple become Tuple / TTuple;
  - Chr holds a Char, escaped here the way 0.19.3's code generator escapes it
    (Generate.JavaScript.Expression.charUtf8), since this worker keeps chars as the
    escaped JavaScript string text, like strings;
  - kernel module globals are named #0 (Name.unit) in 0.19.3 and $ here
    (Opt.toKernelGlobal);
  - type variables carry their constraint as a separate byte, dropped here because
    the name already implies it.

Only decoding is needed: whatever the worker compiles itself it keeps under its own
file name, in its own format (see Builder.Elm.Details.verifyDep).
-}


import Array exposing (Array)
import Bitwise
import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Utils.Binop as Binop
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Elm.Interface as I
import Compiler.Elm.Kernel as K
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Optimize.DecisionTree as DT
import Compiler.Reporting.Annotation as A
import Extra.Data.Binary.Get as BG exposing (Get)
import Extra.Type.List exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Set as Set



-- ARTIFACT CACHE (Elm.Details.dArtifactCache)


{-| The fingerprints: set32 (dict32 Pkg.dName V.dVersion), as the worker's
FingerprintComparable (the dependency map as a sorted list).
-}
getFingerprints : Get (Set.Set (TList (Pkg.Comparable, V.Comparable)))
getFingerprints =
  BG.fmap Set.fromList <|
    list32 <|
      BG.fmap (\pairs -> Map.toList (Map.fromList pairs)) <|
        list32 (BG.liftM2 (\p v -> (Pkg.toComparable p, V.toComparable v)) getPkgName getVersion)


{-| dict32 Module.dName I.dDependencyInterface -}
getInterfaces : Get (Map.Map ModuleName.Raw I.DependencyInterface)
getInterfaces =
  dict32 string8 getDependencyInterface


{-| Opt.dGlobalGraph -}
getGlobalGraph : Get Opt.GlobalGraph
getGlobalGraph =
  BG.liftM2 Opt.GlobalGraph
    (dict32 (BG.fmap Opt.toGlobalComparable getGlobal) getNode)
    (dict32 getName int)



-- PRIMITIVES (Bytes.Decode)


u8 : Get Int
u8 =
  BG.getWord8


u16 : Get Int
u16 =
  BG.getWord16LE


u32 : Get Int
u32 =
  BG.getWord32LE


int : Get Int
int =
  BG.getInt64LE


bool : Get Bool
bool =
  BG.fmap (\n -> n /= 0) u8


string8 : Get String
string8 =
  BG.bind count8 BG.getString


string32 : Get String
string32 =
  BG.bind u32 BG.getString


string64 : Get String
string64 =
  BG.bind BG.getWord64LE BG.getString


maybe : Get a -> Get (Maybe a)
maybe getA =
  BG.bind u8 <| \tag ->
  if tag == 0 then BG.return Nothing else BG.fmap Just getA


{-| @LAMDERA Bytes.Decode.count8, the count of every *8 decoder (list8, dict8, string8):
one byte below 255, or 255 then a u32. -}
count8 : Get Int
count8 =
  BG.bind u8 <| \n ->
  if n < 255 then BG.return n else u32


list8 : Get a -> Get (TList a)
list8 getA =
  BG.bind count8 (BG.getSequence getA)


list32 : Get a -> Get (TList a)
list32 getA =
  BG.bind u32 (BG.getSequence getA)


dict8 : Get comparable -> Get v -> Get (Map.Map comparable v)
dict8 getK getV =
  BG.fmap Map.fromList (list8 (BG.liftM2 Tuple.pair getK getV))


dict32 : Get comparable -> Get v -> Get (Map.Map comparable v)
dict32 getK getV =
  BG.fmap Map.fromList (list32 (BG.liftM2 Tuple.pair getK getV))


set32 : Get comparable -> Get (Set.Set comparable)
set32 getA =
  BG.fmap Set.fromList (list32 getA)


tagged : String -> (Int -> Maybe (Get a)) -> Get a
tagged what cases =
  BG.bind u8 <| \tag ->
  case cases tag of
    Just getA -> getA
    Nothing -> BG.fail ("unexpected tag " ++ String.fromInt tag ++ " for " ++ what)



-- NAMES


{-| N.decode, T.decode (type names), Op.decode and Module.dName are all string8. -}
getName : Get Name
getName =
  string8


{-| T.dVar: the name, then a constraint byte this worker does not store. -}
getVar : Get Name
getVar =
  BG.liftM2 (\name _ -> name) string8 u8


getPkgName : Get Pkg.Name
getPkgName =
  BG.liftM2 Pkg.Name string8 string8


getCanonical : Get ModuleName.Canonical
getCanonical =
  BG.liftM2 ModuleName.Canonical getPkgName string8


getVersion : Get V.Version
getVersion =
  BG.bind u8 <| \word ->
  if word == 255 then
    BG.liftM3 (\a b c -> V.fromComparable (a, b, c)) u16 u16 u16
  else
    BG.liftM2 (\b c -> V.fromComparable (word, b, c)) u8 u8


getIndex : Get Index.ZeroBased
getIndex =
  BG.fmap Index.ZeroBased int


{-| A.dRegion: two u64 positions, each (row << 32 | col), 0-based. -}
getRegion : Get A.Region
getRegion =
  BG.liftM2 A.Region getPosition getPosition


getPosition : Get A.Position
getPosition =
  BG.liftM2 (\col row -> A.Position (row + 1) (col + 1)) u32 u32



-- INTERFACES (Elm.Interface)


{-| @LAMDERA Like I.dInterface, a Private interface starts (after its tag) with its own
type table. -}
getDependencyInterface : Get I.DependencyInterface
getDependencyInterface =
  tagged "DependencyInterface" <| \tag ->
  case tag of
    0 -> Just <| BG.fmap I.Public getInterface
    1 -> Just <| BG.bind getTypeTable <| \table -> BG.liftM3 I.Private getPkgName (dict32 getName (getCanUnion table)) (dict32 getName (getCanAlias table))
    _ -> Nothing


getInterface : Get I.Interface
getInterface =
  BG.bind getTypeTable <| \table ->
  BG.liftM5 I.Interface
    getPkgName
    (dict32 getName (getAnnotation table))
    (dict32 getName (getUnion table))
    (dict32 getName (getAlias table))
    (dict32 getName (getBinop table))


getUnion : TypeTable -> Get I.Union
getUnion table =
  tagged "Union" <| \tag ->
  case tag of
    0 -> Just <| BG.fmap I.OpenUnion (getCanUnion table)
    1 -> Just <| BG.fmap I.ClosedUnion (getCanUnion table)
    2 -> Just <| BG.fmap I.PrivateUnion (getCanUnion table)
    _ -> Nothing


getAlias : TypeTable -> Get I.Alias
getAlias table =
  tagged "Alias" <| \tag ->
  case tag of
    0 -> Just <| BG.fmap I.PublicAlias (getCanAlias table)
    1 -> Just <| BG.fmap I.PrivateAlias (getCanAlias table)
    _ -> Nothing


getBinop : TypeTable -> Get I.Binop
getBinop table =
  BG.liftM4 I.Binop getName (getAnnotation table) getAssociativity (BG.fmap Binop.Precedence u8)


getAssociativity : Get Binop.Associativity
getAssociativity =
  tagged "Associativity" <| \tag ->
  case tag of
    0 -> Just <| BG.return Binop.Left
    1 -> Just <| BG.return Binop.Non
    2 -> Just <| BG.return Binop.Right
    _ -> Nothing


getCanAlias : TypeTable -> Get Can.Alias
getCanAlias table =
  BG.liftM2 Can.Alias (list8 getVar) (getTypeRef table)


getCanUnion : TypeTable -> Get Can.Union
getCanUnion table =
  BG.liftM4 Can.Union (list8 getVar) (list32 (getCtor table)) int getCtorOpts


getCtor : TypeTable -> Get Can.Ctor
getCtor table =
  BG.liftM4 Can.Ctor getName getIndex int (list8 (getTypeRef table))


getCtorOpts : Get Can.CtorOpts
getCtorOpts =
  tagged "CtorOpts" <| \tag ->
  case tag of
    0 -> Just <| BG.return Can.Normal
    1 -> Just <| BG.return Can.Enum
    2 -> Just <| BG.return Can.Unbox
    _ -> Nothing


getAnnotation : TypeTable -> Get Can.Annotation
getAnnotation table =
  BG.liftM2 Can.Forall (dict8 getVar (BG.return ())) (getTypeRef table)



-- TYPE TABLE (Elm.Interface dTypeTable)


{-| @LAMDERA Every type in an interface is written once, in a table at its start, and
referred to by index everywhere else (see TYPE TABLE in the compiler's
Elm.Interface): a u32 count, then the entries, each like the inline Type encoding but
with its child types as refs to earlier entries. A ref is a u16 when the table has at
most 65536 entries and a u32 otherwise.
-}
type TypeTable =
  TypeTable (Get Int) (Array Can.Type)


getTypeTable : Get TypeTable
getTypeTable =
  BG.bind u32 <| \size ->
  let ref = if size <= 65536 then u16 else u32 in
  BG.repeatFold size (\types -> BG.fmap (\t -> Array.push t types) (getTypeEntry (TypeTable ref types))) Array.empty
    |> BG.fmap (TypeTable ref)


{-| Only the entries before this one are in the table yet, so a ref forward is
rejected as corrupt rather than followed. -}
getTypeRef : TypeTable -> Get Can.Type
getTypeRef (TypeTable ref types) =
  BG.bind ref <| \i ->
  case Array.get i types of
    Just t -> BG.return t
    Nothing -> BG.fail ("type ref " ++ String.fromInt i ++ " outside the type table")


getTypeEntry : TypeTable -> Get Can.Type
getTypeEntry table =
  let ref = getTypeRef table in
  tagged "Type" <| \tag ->
  case tag of
    0 -> Just <| BG.liftM2 Can.TLambda ref ref
    1 -> Just <| BG.fmap Can.TVar getVar
    2 -> Just <| BG.liftM2 Can.TRecord (dict32 getName (BG.liftM2 Can.FieldType u16 ref)) (maybe getVar)
    3 -> Just <| BG.return Can.TUnit
    4 -> Just <| BG.liftM2 (\a b -> Can.TTuple a b Nothing) ref ref
    5 -> Just <| BG.liftM3 (\a b c -> Can.TTuple a b (Just c)) ref ref ref
    6 -> Just <| BG.liftM4 Can.TAlias getCanonical getName (list8 (BG.liftM2 Tuple.pair getVar ref)) (getAliasType ref)
    7 -> Just <| BG.liftM3 Can.TType getCanonical getName (list8 ref)
    _ -> Nothing


getAliasType : Get Can.Type -> Get Can.AliasType
getAliasType ref =
  tagged "AliasType" <| \tag ->
  case tag of
    0 -> Just <| BG.fmap Can.Holey ref
    1 -> Just <| BG.fmap Can.Filled ref
    _ -> Nothing



-- OBJECTS (AST.Optimized)


getGlobal : Get Opt.Global
getGlobal =
  BG.liftM2 toGlobal getCanonical getName


toGlobal : ModuleName.Canonical -> Name -> Opt.Global
toGlobal ((ModuleName.Canonical pkg _) as home) name =
  if Pkg.toComparable pkg == Pkg.toComparable Pkg.kernel && name == "#0"
    then Opt.Global home Name.dollar
    else Opt.Global home name


getGlobalSet : Get (Set.Set Opt.GlobalComparable)
getGlobalSet =
  set32 (BG.fmap Opt.toGlobalComparable getGlobal)


getNode : Get Opt.Node
getNode =
  tagged "Node" <| \tag ->
  case tag of
    0  -> Just <| BG.liftM2 Opt.Define getExpr getGlobalSet
    1  -> Just <| BG.liftM3 Opt.DefineTailFunc (list32 getName) getExpr getGlobalSet
    2  -> Just <| BG.liftM2 Opt.Ctor getIndex int
    3  -> Just <| BG.fmap Opt.Enum getIndex
    4  -> Just <| BG.return Opt.Box
    5  -> Just <| BG.fmap Opt.Link getGlobal
    6  -> Just <| BG.liftM4 Opt.Cycle (list32 getName) (list32 (BG.liftM2 Tuple.pair getName getExpr)) (list32 getDef) getGlobalSet
    7  -> Just <| BG.fmap Opt.Manager getEffectsType
    8  -> Just <| BG.liftM2 Opt.Kernel (list32 getChunk) getGlobalSet
    9  -> Just <| BG.liftM2 Opt.PortIncoming getExpr getGlobalSet
    10 -> Just <| BG.liftM2 Opt.PortOutgoing getExpr getGlobalSet
    _  -> Nothing


getEffectsType : Get Opt.EffectsType
getEffectsType =
  tagged "EffectsType" <| \tag ->
  case tag of
    0 -> Just <| BG.return Opt.CCmd
    1 -> Just <| BG.return Opt.CSub
    2 -> Just <| BG.return Opt.Fx
    _ -> Nothing


{-| Elm.Kernel.dChunk -}
getChunk : Get K.Chunk
getChunk =
  tagged "Elm.Kernel.Chunk" <| \tag ->
  case tag of
    0 -> Just <| BG.fmap K.JS string64
    1 -> Just <| BG.liftM2 K.ElmVar getCanonical getName
    2 -> Just <| BG.liftM2 K.JsVar string8 getName
    3 -> Just <| BG.fmap K.ElmField getName
    4 -> Just <| BG.fmap K.JsField int
    5 -> Just <| BG.fmap K.JsEnum int
    6 -> Just <| BG.return K.Debug
    7 -> Just <| BG.return K.Prod
    _ -> Nothing


getExpr : Get Opt.Expr
getExpr offset =
  (tagged "Expr" <| \tag ->
    case tag of
      0  -> Just <| BG.fmap Opt.CBool bool
      1  -> Just <| BG.fmap (Opt.Chr << escapeChar) BG.getUtf8Char
      2  -> Just <| BG.fmap Opt.Str string32
      3  -> Just <| BG.fmap Opt.CInt int
      4  -> Just <| BG.fmap Opt.CFloat string8
      5  -> Just <| BG.fmap Opt.VarLocal getName
      6  -> Just <| BG.fmap Opt.VarGlobal getGlobal
      7  -> Just <| BG.liftM2 Opt.VarEnum getGlobal getIndex
      8  -> Just <| BG.fmap Opt.VarBox getGlobal
      9  -> Just <| BG.liftM2 Opt.VarCycle getCanonical getName
      10 -> Just <| BG.liftM4 Opt.VarDebug getName getCanonical getRegion (maybe getName)
      11 -> Just <| BG.liftM2 Opt.VarKernel string8 getName
      12 -> Just <| BG.fmap Opt.CList (list32 getExpr)
      13 -> Just <| BG.liftM2 Opt.Function (list32 getName) getExpr
      14 -> Just <| BG.liftM2 Opt.Call getExpr (list32 getExpr)
      15 -> Just <| BG.liftM2 Opt.TailCall getName (list32 (BG.liftM2 Tuple.pair getName getExpr))
      16 -> Just <| BG.liftM2 Opt.If (list32 (BG.liftM2 Tuple.pair getExpr getExpr)) getExpr
      17 -> Just <| BG.liftM2 Opt.Let getDef getExpr
      18 -> Just <| BG.liftM2 Opt.Destruct getDestructor getExpr
      19 -> Just <| BG.liftM4 Opt.Case getName getName (getDecider getChoice) (list32 (BG.liftM2 Tuple.pair int getExpr))
      20 -> Just <| BG.fmap Opt.Accessor getName
      21 -> Just <| BG.liftM2 Opt.Access getExpr getName
      22 -> Just <| BG.liftM2 Opt.Update getExpr (dict32 getName getExpr)
      23 -> Just <| BG.fmap Opt.Record (dict32 getName getExpr)
      24 -> Just <| BG.return Opt.Unit
      25 -> Just <| BG.liftM2 (\a b -> Opt.Tuple a b Nothing) getExpr getExpr
      26 -> Just <| BG.liftM3 (\a b c -> Opt.Tuple a b (Just c)) getExpr getExpr getExpr
      27 -> Just <| BG.liftM3 Opt.Shader (BG.fmap Shader.Source string64) (set32 getName) (set32 getName)
      _  -> Nothing
  ) offset


getDef : Get Opt.Def
getDef offset =
  (tagged "Def" <| \tag ->
    case tag of
      0 -> Just <| BG.liftM2 Opt.Def getName getExpr
      1 -> Just <| BG.liftM3 Opt.TailDef getName (list32 getName) getExpr
      _ -> Nothing
  ) offset


getDestructor : Get Opt.Destructor
getDestructor =
  BG.liftM2 Opt.Destructor getName getPath


getPath : Get Opt.Path
getPath offset =
  (tagged "Path" <| \tag ->
    case tag of
      0 -> Just <| BG.liftM2 Opt.Index getIndex getPath
      1 -> Just <| BG.liftM2 Opt.Field getName getPath
      2 -> Just <| BG.fmap Opt.Unbox getPath
      3 -> Just <| BG.fmap Opt.Root getName
      _ -> Nothing
  ) offset


getDecider : Get a -> Get (Opt.Decider a)
getDecider getA offset =
  (tagged "Decider" <| \tag ->
    case tag of
      0 -> Just <| BG.fmap Opt.Leaf getA
      1 -> Just <| BG.liftM3 Opt.Chain (list32 (BG.liftM2 Tuple.pair getDtPath getTest)) (getDecider getA) (getDecider getA)
      2 -> Just <| BG.liftM3 Opt.FanOut getDtPath (list32 (BG.liftM2 Tuple.pair getTest (getDecider getA))) (getDecider getA)
      _ -> Nothing
  ) offset


getChoice : Get Opt.Choice
getChoice =
  tagged "Choice" <| \tag ->
  case tag of
    0 -> Just <| BG.fmap Opt.Inline getExpr
    1 -> Just <| BG.fmap Opt.Jump int
    _ -> Nothing


{-| Optimize.DecisionTree.dTest -}
getTest : Get DT.Test
getTest =
  tagged "DecisionTree.Test" <| \tag ->
  case tag of
    0 -> Just <| BG.liftM5 DT.IsCtor getCanonical getName getIndex int getCtorOpts
    1 -> Just <| BG.return DT.IsCons
    2 -> Just <| BG.return DT.IsNil
    3 -> Just <| BG.return DT.IsTuple
    4 -> Just <| BG.fmap (DT.IsChr << escapeChar) BG.getUtf8Char
    5 -> Just <| BG.fmap DT.IsStr string32
    6 -> Just <| BG.fmap DT.IsInt int
    7 -> Just <| BG.fmap DT.IsBool bool
    _ -> Nothing


{-| Optimize.DecisionTree.dPath -}
getDtPath : Get DT.Path
getDtPath offset =
  (tagged "DecisionTree.Path" <| \tag ->
    case tag of
      0 -> Just <| BG.liftM2 DT.Index getIndex getDtPath
      1 -> Just <| BG.fmap DT.Unbox getDtPath
      2 -> Just <| BG.return DT.Empty
      _ -> Nothing
  ) offset



-- CHARS


{-| The text of a Char inside a single-quoted JavaScript string, exactly as 0.19.3's
Generate.JavaScript.Expression.charUtf8 writes it.
-}
escapeChar : Char -> String
escapeChar char =
  let code = Char.toCode char in
  if char == '\\' then "\\\\"
  else if char == '\'' then "\\'"
  else if code >= 0x20 then String.fromChar char
  else if code == 0x08 then "\\b"
  else if code == 0x0C then "\\f"
  else if char == '\n' then "\\n"
  else if char == '\u{000D}' then "\\r"
  else if char == '\t' then "\\t"
  else "\\u00" ++ String.fromList [ hexDigit (Bitwise.shiftRightBy 4 code), hexDigit (Bitwise.and code 15) ]


hexDigit : Int -> Char
hexDigit n =
  if n < 10 then Char.fromCode (0x30 + n) else Char.fromCode (0x57 + n)
