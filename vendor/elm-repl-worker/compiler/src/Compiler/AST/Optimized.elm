{- MANUALLY FORMATTED -}
module Compiler.AST.Optimized exposing
  ( Def(..)
  , Expr(..)
  , Global(..), GlobalComparable, toGlobalComparable, fromGlobalComparable
  , Path(..)
  , Destructor(..)
  , Decider(..)
  , Choice(..)
  , GlobalGraph(..), bGlobalGraph
  , LocalGraph(..), bLocalGraph
  , Main(..)
  , Node(..)
  , EffectsType(..)
  , empty
  , addGlobalGraph
  , addLocalGraph
  , addKernel
  , toKernelGlobal
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name exposing (Name)
import Compiler.Elm.Float as EF
import Compiler.Elm.Kernel as K
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.String as ES
import Compiler.Optimize.DecisionTree as DT
import Compiler.Reporting.Annotation as A
import Extra.Data.Binary as B
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Set as Set



-- EXPRESSIONS


type Expr
  = CBool Bool
  | Chr ES.TString
  | Str ES.TString
  | CInt Int
  | CFloat EF.TFloat
  | VarLocal Name
  | VarGlobal Global
  | VarEnum Global Index.ZeroBased
  | VarBox Global
  | VarCycle ModuleName.Canonical Name
  | VarDebug Name ModuleName.Canonical A.Region (Maybe Name)
  | VarKernel Name Name
  | CList (TList Expr)
  | Function (TList Name) Expr
  | Call Expr (TList Expr)
  | TailCall Name (TList (Name, Expr))
  | If (TList (Expr, Expr)) Expr
  | Let Def Expr
  | Destruct Destructor Expr
  | Case Name Name (Decider Choice) (TList (Int, Expr))
  | Accessor Name
  | Access Expr Name
  | Update Expr (Map.Map Name Expr)
  | Record (Map.Map Name Expr)
  | Unit
  | Tuple Expr Expr (Maybe Expr)
  | Shader Shader.Source (Set.Set Name) (Set.Set Name)


type Global = Global ModuleName.Canonical Name


type alias GlobalComparable = ( ModuleName.Comparable, Name )


toGlobalComparable : Global -> GlobalComparable
toGlobalComparable (Global moduleName name) =
  ( ModuleName.toComparable moduleName, name )


fromGlobalComparable : GlobalComparable -> Global
fromGlobalComparable (moduleName, name) =
  Global (ModuleName.fromComparable moduleName) name



-- DEFINITIONS


type Def
  = Def Name Expr
  | TailDef Name (TList Name) Expr


type Destructor =
  Destructor Name Path


type Path
  = Index Index.ZeroBased Path
  | Field Name Path
  | Unbox Path
  | Root Name



-- BRANCHING


type Decider a
  = Leaf a
  | Chain
      {- testChain -} (TList (DT.Path, DT.Test))
      {- success -} (Decider a)
      {- failure -} (Decider a)
  | FanOut
      {- path -} DT.Path
      {- tests -} (TList (DT.Test, Decider a))
      {- fallback -} (Decider a)


type Choice
  = Inline Expr
  | Jump Int



-- OBJECT GRAPH


type GlobalGraph =
  GlobalGraph
    {- g_nodes -} (Map.Map GlobalComparable Node)
    {- g_fields -} (Map.Map Name Int)


type LocalGraph =
  LocalGraph
    {- l_main -} (Maybe Main)
    {- l_nodes -} (Map.Map GlobalComparable Node)  -- PERF profile switching Global to Name
    {- l_fields -} (Map.Map Name Int)


type Main
  = Static
  | Dynamic
      {- message-} Can.Type
      {- decoder-} Expr


type Node
  = Define Expr (Set.Set GlobalComparable)
  | DefineTailFunc (TList Name) Expr (Set.Set GlobalComparable)
  | Ctor Index.ZeroBased Int
  | Enum Index.ZeroBased
  | Box
  | Link Global
  | Cycle (TList Name) (TList (Name, Expr)) (TList Def) (Set.Set GlobalComparable)
  | Manager EffectsType
  | Kernel (TList K.Chunk) (Set.Set GlobalComparable)
  | PortIncoming Expr (Set.Set GlobalComparable)
  | PortOutgoing Expr (Set.Set GlobalComparable)


type EffectsType = CCmd | CSub | Fx



-- GRAPHS


empty : GlobalGraph
empty =
  GlobalGraph Map.empty Map.empty


addGlobalGraph : GlobalGraph -> GlobalGraph -> GlobalGraph
addGlobalGraph (GlobalGraph nodes1 fields1) (GlobalGraph nodes2 fields2) =
  GlobalGraph
    {- g_nodes -} (Map.union nodes1 nodes2)
    {- g_fields -} (Map.union fields1 fields2)


addLocalGraph : LocalGraph -> GlobalGraph -> GlobalGraph
addLocalGraph (LocalGraph _ nodes1 fields1) (GlobalGraph nodes2 fields2) =
  GlobalGraph
    {- g_nodes -} (Map.union nodes1 nodes2)
    {- g_fields -} (Map.union fields1 fields2)


addKernel : Name.Name -> TList K.Chunk -> GlobalGraph -> GlobalGraph
addKernel shortName chunks (GlobalGraph nodes fields) =
  let
    global = toKernelGlobal shortName
    node = Kernel chunks (MList.foldr addKernelDep Set.empty chunks)
  in
  GlobalGraph
    {- g_nodes -} (Map.insert (toGlobalComparable global) node nodes)
    {- g_fields -} (Map.union (K.countFields chunks) fields)


addKernelDep : K.Chunk -> Set.Set GlobalComparable -> Set.Set GlobalComparable
addKernelDep chunk deps =
  case chunk of
    K.JS _              -> deps
    K.ElmVar home name  -> Set.insert (toGlobalComparable <| Global home name) deps
    K.JsVar shortName _ -> Set.insert (toGlobalComparable <| toKernelGlobal shortName) deps
    K.ElmField _        -> deps
    K.JsField _         -> deps
    K.JsEnum _          -> deps
    K.Debug             -> deps
    K.Prod              -> deps
    K.Async             -> deps


toKernelGlobal : Name -> Global
toKernelGlobal shortName =
  Global (ModuleName.Canonical Pkg.kernel shortName) Name.dollar



-- BINARY


bGlobal : B.Binary Global
bGlobal =
  B.bin2 Global (\(Global a b) -> B.T2 a b) ModuleName.bCanonical Name.bName


bGlobalComparable : B.Binary GlobalComparable
bGlobalComparable =
  B.bTuple ModuleName.bComparable Name.bName


bExpr : B.Binary Expr
bExpr =
  B.custom "problem getting Opt.Expr binary"
    (\p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 expr -> case expr of
      CBool a          -> p0 a
      Chr a            -> p1 a
      Str a            -> p2 a
      CInt a           -> p3 a
      CFloat a         -> p4 a
      VarLocal a       -> p5 a
      VarGlobal a      -> p6 a
      VarEnum a b      -> p7 a b
      VarBox a         -> p8 a
      VarCycle a b     -> p9 a b
      VarDebug a b c d -> p10 a b c d
      VarKernel a b    -> p11 a b
      CList a          -> p12 a
      Function a b     -> p13 a b
      Call a b         -> p14 a b
      TailCall a b     -> p15 a b
      If a b           -> p16 a b
      Let a b          -> p17 a b
      Destruct a b     -> p18 a b
      Case a b c d     -> p19 a b c d
      Accessor a       -> p20 a
      Access a b       -> p21 a b
      Update a b       -> p22 a b
      Record a         -> p23 a
      Unit             -> p24
      Tuple a b c      -> p25 a b c
      Shader a b c     -> p26 a b c
    )
    |> B.var1 0  CBool B.bBool
    |> B.var1 1  Chr ES.bTString
    |> B.var1 2  Str ES.bTString
    |> B.var1 3  CInt B.bWord64
    |> B.var1 4  CFloat EF.bTFloat
    |> B.var1 5  VarLocal Name.bName
    |> B.var1 6  VarGlobal bGlobal
    |> B.var2 7  VarEnum bGlobal Index.bZeroBased
    |> B.var1 8  VarBox bGlobal
    |> B.var2 9  VarCycle ModuleName.bCanonical Name.bName
    |> B.var4 10 VarDebug Name.bName ModuleName.bCanonical A.bRegion (B.bMaybe Name.bName)
    |> B.var2 11 VarKernel Name.bName Name.bName
    |> B.var1 12 CList (B.bTList (B.lazy <| \() -> bExpr))
    |> B.var2 13 Function (B.bTList Name.bName) (B.lazy <| \() -> bExpr)
    |> B.var2 14 Call (B.lazy <| \() -> bExpr) (B.bTList (B.lazy <| \() -> bExpr))
    |> B.var2 15 TailCall Name.bName (B.bTList (B.bTuple Name.bName (B.lazy <| \() -> bExpr)))
    |> B.var2 16 If (B.bTList (B.bTuple (B.lazy <| \() -> bExpr) (B.lazy <| \() -> bExpr))) (B.lazy <| \() -> bExpr)
    |> B.var2 17 Let bDef (B.lazy <| \() -> bExpr)
    |> B.var2 18 Destruct bDestructor (B.lazy <| \() -> bExpr)
    |> B.var4 19 Case Name.bName Name.bName (bDecider bChoice) (B.bTList (B.bTuple B.bWord64 (B.lazy <| \() -> bExpr)))
    |> B.var1 20 Accessor Name.bName
    |> B.var2 21 Access (B.lazy <| \() -> bExpr) Name.bName
    |> B.var2 22 Update (B.lazy <| \() -> bExpr) (B.bMap Name.bName (B.lazy <| \() -> bExpr))
    |> B.var1 23 Record (B.bMap Name.bName (B.lazy <| \() -> bExpr))
    |> B.var0 24 Unit
    |> B.var3 25 Tuple (B.lazy <| \() -> bExpr) (B.lazy <| \() -> bExpr) (B.bMaybe (B.lazy <| \() -> bExpr))
    |> B.var3 26 Shader Shader.bSource (B.bSet Name.bName) (B.bSet Name.bName)
    |> B.finish


bDef : B.Binary Def
bDef =
  B.custom "problem getting Opt.Def binary"
    (\p0 p1 def ->
      case def of
        Def     a b   -> p0 a b
        TailDef a b c -> p1 a b c
    )
    |> B.var2 0 Def Name.bName (B.lazy <| \() -> bExpr)
    |> B.var3 1 TailDef Name.bName (B.bTList Name.bName) (B.lazy <| \() -> bExpr)
    |> B.finish


bDestructor : B.Binary Destructor
bDestructor =
  B.bin2 Destructor (\(Destructor a b) -> B.T2 a b) Name.bName bPath


bPath : B.Binary Path
bPath =
  B.custom "problem getting Opt.Path binary"
    (\p0 p1 p2 p3 path ->
      case path of
        Index a b -> p0 a b
        Field a b -> p1 a b
        Unbox a   -> p2 a
        Root  a   -> p3 a
    )
    |> B.var2 0 Index Index.bZeroBased (B.lazy <| \() -> bPath)
    |> B.var2 1 Field Name.bName (B.lazy <| \() -> bPath)
    |> B.var1 2 Unbox (B.lazy <| \() -> bPath)
    |> B.var1 3 Root Name.bName
    |> B.finish


bDecider : B.Binary a -> B.Binary (Decider a)
bDecider binA =
  B.custom "problem getting Opt.Decider binary"
    (\p0 p1 p2 decider ->
      case decider of
        Leaf   a     -> p0 a
        Chain  a b c -> p1 a b c
        FanOut a b c -> p2 a b c
    )
    |> B.var1 0 Leaf binA
    |> B.var3 1 Chain (B.bTList (B.bTuple DT.bPath DT.bTest)) (B.lazy <| \() -> bDecider binA) (B.lazy <| \() -> bDecider binA)
    |> B.var3 2 FanOut DT.bPath (B.bTList (B.bTuple DT.bTest (B.lazy <| \() -> bDecider binA))) (B.lazy <| \() -> bDecider binA)
    |> B.finish


bChoice : B.Binary Choice
bChoice =
  B.custom "problem getting Opt.Choice binary"
    (\p0 p1 choice ->
      case choice of
        Inline a -> p0 a
        Jump   a -> p1 a
    )
    |> B.var1 0 Inline (B.lazy <| \() -> bExpr)
    |> B.var1 1 Jump B.bWord64
    |> B.finish


bGlobalGraph : B.Binary GlobalGraph
bGlobalGraph =
  B.bin2 GlobalGraph (\(GlobalGraph a b) -> B.T2 a b)
    (B.bMap bGlobalComparable bNode)
    (B.bMap Name.bName B.bWord64)


bLocalGraph : B.Binary LocalGraph
bLocalGraph =
  B.bin3 LocalGraph (\(LocalGraph a b c) -> B.T3 a b c)
    (B.bMaybe bMain)
    (B.bMap bGlobalComparable bNode)
    (B.bMap Name.bName B.bWord64)


bMain : B.Binary Main
bMain =
  B.custom "problem getting Opt.Main binary"
    (\p0 p1 main ->
      case main of
        Static      -> p0
        Dynamic a b -> p1 a b
    )
    |> B.var0 0 Static
    |> B.var2 1 Dynamic Can.bType bExpr
    |> B.finish


bNode : B.Binary Node
bNode =
  B.custom "problem getting Opt.Node binary"
    (\p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 node ->
      case node of
        Define         a b     -> p0  a b
        DefineTailFunc a b c   -> p1  a b c
        Ctor           a b     -> p2  a b
        Enum           a       -> p3  a
        Box                    -> p4
        Link           a       -> p5  a
        Cycle          a b c d -> p6  a b c d
        Manager        a       -> p7  a
        Kernel         a b     -> p8  a b
        PortIncoming   a b     -> p9  a b
        PortOutgoing   a b     -> p10 a b
    )
    |> B.var2 0 Define bExpr (B.bSet bGlobalComparable)
    |> B.var3 1 DefineTailFunc (B.bTList Name.bName) bExpr (B.bSet bGlobalComparable)
    |> B.var2 2 Ctor Index.bZeroBased B.bWord64
    |> B.var1 3 Enum Index.bZeroBased
    |> B.var0 4 Box
    |> B.var1 5 Link bGlobal
    |> B.var4 6 Cycle (B.bTList Name.bName) (B.bTList (B.bTuple Name.bName bExpr)) (B.bTList bDef) (B.bSet bGlobalComparable)
    |> B.var1 7 Manager bEffectsType
    |> B.var2 8 Kernel (B.bTList K.bChunk) (B.bSet bGlobalComparable)
    |> B.var2 9 PortIncoming bExpr (B.bSet bGlobalComparable)
    |> B.var2 10 PortOutgoing bExpr (B.bSet bGlobalComparable)
    |> B.finish


bEffectsType : B.Binary EffectsType
bEffectsType =
  B.enum "problem getting Opt.EffectsType binary" [ CCmd, CSub, Fx ]
