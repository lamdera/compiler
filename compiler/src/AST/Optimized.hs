{-# OPTIONS_GHC -Wall #-}
module AST.Optimized
  ( Def(..)
  , Expr(..)
  , Global(..)
  , kernel
  , Path(..)
  , Destructor(..)
  , Decider(..)
  , Choice(..)
  , Graph(..)
  , Main(..)
  , Node(..)
  , EffectsType(..)
  , KContent(..)
  , KChunk(..)
  )
  where


import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Region as R



-- EXPRESSIONS


data Expr
  = Bool Bool
  | Chr Text
  | Str Text
  | Int Int
  | Float Double
  | VarLocal N.Name
  | VarGlobal Global
  | VarEnum Global Index.ZeroBased
  | VarBox Global
  | VarCycle ModuleName.Canonical N.Name
  | VarDebug N.Name ModuleName.Canonical R.Region (Maybe N.Name)
  | VarKernel N.Name N.Name
  | List [Expr]
  | Function [N.Name] Expr -- single-argument functions, returning functions(closures are native)
  | Call Expr [Expr] -- just call a function, nothing special
  | TailCall N.Name [(N.Name, Expr)]
  | If [(Expr, Expr)] Expr
  | Let Def Expr
  | Destruct Destructor Expr
  | Case N.Name N.Name (Decider Choice) [(Int, Expr)] -- is each branch optimized down to an int?
  | Accessor N.Name -- lambda
  | Access Expr N.Name -- interface with getter function?
  | Update Expr (Map.Map N.Name Expr) -- interface with setter function?
  | Record (Map.Map N.Name Expr) -- struct
  | Unit
  | Tuple Expr Expr (Maybe Expr) -- struct with names `f0`, `f1`, ... for indices
  | Shader Text (Set.Set N.Name) (Set.Set N.Name)
  deriving (Show)

{-
- union types
- functions/lambdas (closures)
- pattern matching (destructors) js: thing.fieldname1[2].fieldname2
- tuples (1, "happy", True)
- records {x=2, y=3}
- persistent lists, dicts, sets, others
- accessor functions (.field)
- accessing fields (record.field)
- record update
- extensible records {r | x : Int, y : Int}
  - interfaces whenever we use extensible records, otherwise use structs with names derived from the types it holds, with fields sorted by name. Gather up all types used anywhere, and generate all those structs somewhere else.
  - always fetch fieldnames using functions. If they're interfaces or pure structs doesn't matter.
    - always use interfaces, otherwise we might not know when to pass pointers or not around.

golang:
- adt <-> interfaces
  - https://blog.merovius.de/2018/02/25/persistent_datastructures_with_go.html

-}


-- TODO: how do we represent boxing/unboxing?

data Global = Global ModuleName.Canonical N.Name
  deriving (Eq, Ord)

instance Show Global where
  show (Global can name) = show can ++ "." ++ show name

-- Provide "List" not "Elm.Kernel.List"
--
kernel :: N.Name -> Global
kernel home =
  Global (ModuleName.Canonical Pkg.kernel home) N.dollar



-- DEFINITIONS


data Def
  = Def N.Name Expr
  | TailDef N.Name [N.Name] Expr
  deriving (Show)


data Destructor =
  -- let varname = this thing over at this path
  -- Destructor varname path
  Destructor N.Name Path
  deriving (Show)


data Path
  = Index Index.ZeroBased Path
  | Field N.Name Path
  | Unbox Path
  | Root N.Name
  deriving (Show)



-- BRANCHING


data Decider a
  = Leaf a
  | Chain
      { _testChain :: [(DT.Path, DT.Test)]
      , _success :: Decider a
      , _failure :: Decider a
      }
  | FanOut
      { _path :: DT.Path
      , _tests :: [(DT.Test, Decider a)]
      , _fallback :: Decider a
      }
  deriving (Eq, Show)


data Choice
  = Inline Expr
  | Jump Int -- what does this int mean?
  deriving (Show)



-- OBJECT GRAPH


data Graph =
  Graph
    { _mains :: Map.Map ModuleName.Canonical Main
    , _nodes :: Map.Map Global Node
    , _fields :: Map.Map N.Name Int
    }
    deriving (Show)


data Main
  = Static
  | Dynamic
      { _message :: Can.Type
      , _decoder :: Expr
      }
      deriving (Show)

type GlobalDeps
  -- Set.Set Global is the set of things referenced in the Expr(s), with `elm/kernel~Utils.$` being function application
  = (Set.Set Global)

data Node
  = Define Expr GlobalDeps
  | DefineTailFunc [N.Name] Expr GlobalDeps
  | Ctor Index.ZeroBased Int
  | Enum Index.ZeroBased
  | Box
  | Link Global
  | Cycle [N.Name] [(N.Name, Expr)] [Def] GlobalDeps
  | Manager EffectsType
  | Kernel KContent (Maybe KContent)
  | PortIncoming Expr GlobalDeps
  | PortOutgoing Expr GlobalDeps
  deriving (Show)


data EffectsType = Cmd | Sub | Fx deriving (Show)


data KContent =
  KContent [KChunk] (Set.Set Global) deriving (Show)


data KChunk
  = JS BS.ByteString
  | ElmVar ModuleName.Canonical N.Name
  | JsVar N.Name N.Name
  | ElmField N.Name
  | JsField Int
  | JsEnum Int
  | Debug
  | Prod
  deriving (Show)



-- BINARY


instance Binary Global where
  get = liftM2 Global get get
  put (Global a b) = put a >> put b


instance Binary Expr where
  put expr =
    case expr of
      Bool a           -> putWord8  0 >> put a
      Chr a            -> putWord8  1 >> put a
      Str a            -> putWord8  2 >> put a
      Int a            -> putWord8  3 >> put a
      Float a          -> putWord8  4 >> put a
      VarLocal a       -> putWord8  5 >> put a
      VarGlobal a      -> putWord8  6 >> put a
      VarEnum a b      -> putWord8  7 >> put a >> put b
      VarBox a         -> putWord8  8 >> put a
      VarCycle a b     -> putWord8  9 >> put a >> put b
      VarDebug a b c d -> putWord8 10 >> put a >> put b >> put c >> put d
      VarKernel a b    -> putWord8 11 >> put a >> put b
      List a           -> putWord8 12 >> put a
      Function a b     -> putWord8 13 >> put a >> put b
      Call a b         -> putWord8 14 >> put a >> put b
      TailCall a b     -> putWord8 15 >> put a >> put b
      If a b           -> putWord8 16 >> put a >> put b
      Let a b          -> putWord8 17 >> put a >> put b
      Destruct a b     -> putWord8 18 >> put a >> put b
      Case a b c d     -> putWord8 19 >> put a >> put b >> put c >> put d
      Accessor a       -> putWord8 20 >> put a
      Access a b       -> putWord8 21 >> put a >> put b
      Update a b       -> putWord8 22 >> put a >> put b
      Record a         -> putWord8 23 >> put a
      Unit             -> putWord8 24
      Tuple a b c      -> putWord8 25 >> put a >> put b >> put c
      Shader a b c     -> putWord8 26 >> put a >> put b >> put c

  get =
    do  word <- getWord8
        case word of
          0  -> liftM  Bool get
          1  -> liftM  Chr get
          2  -> liftM  Str get
          3  -> liftM  Int get
          4  -> liftM  Float get
          5  -> liftM  VarLocal get
          6  -> liftM  VarGlobal get
          7  -> liftM2 VarEnum get get
          8  -> liftM  VarBox get
          9  -> liftM2 VarCycle get get
          10 -> liftM4 VarDebug get get get get
          11 -> liftM2 VarKernel get get
          12 -> liftM  List get
          13 -> liftM2 Function get get
          14 -> liftM2 Call get get
          15 -> liftM2 TailCall get get
          16 -> liftM2 If get get
          17 -> liftM2 Let get get
          18 -> liftM2 Destruct get get
          19 -> liftM4 Case get get get get
          20 -> liftM  Accessor get
          21 -> liftM2 Access get get
          22 -> liftM2 Update get get
          23 -> liftM  Record get
          24 -> pure   Unit
          25 -> liftM3 Tuple get get get
          26 -> liftM3 Shader get get get
          _  -> error "problem getting Opt.Expr binary"


instance Binary Def where
  put def =
    case def of
      Def a b       -> putWord8 0 >> put a >> put b
      TailDef a b c -> putWord8 1 >> put a >> put b >> put c

  get =
    do  word <- getWord8
        case word of
          0 -> liftM2 Def get get
          1 -> liftM3 TailDef get get get
          _ -> error "problem getting Opt.Def binary"


instance Binary Destructor where
  get = liftM2 Destructor get get
  put (Destructor a b) = put a >> put b


instance Binary Path where
  put destructor =
    case destructor of
      Index a b -> putWord8 0 >> put a >> put b
      Field a b -> putWord8 1 >> put a >> put b
      Unbox a   -> putWord8 2 >> put a
      Root a    -> putWord8 3 >> put a

  get =
    do  word <- getWord8
        case word of
          0 -> liftM2 Index get get
          1 -> liftM2 Field get get
          2 -> liftM  Unbox get
          3 -> liftM  Root get
          _ -> error "problem getting Opt.Path binary"


instance (Binary a) => Binary (Decider a) where
  put decider =
    case decider of
      Leaf a       -> putWord8 0 >> put a
      Chain a b c  -> putWord8 1 >> put a >> put b >> put c
      FanOut a b c -> putWord8 2 >> put a >> put b >> put c

  get =
    do  word <- getWord8
        case word of
          0 -> liftM  Leaf get
          1 -> liftM3 Chain get get get
          2 -> liftM3 FanOut get get get
          _ -> error "problem getting Opt.Decider binary"


instance Binary Choice where
  put choice =
    case choice of
      Inline expr -> putWord8 0 >> put expr
      Jump index  -> putWord8 1 >> put index

  get =
    do  word <- getWord8
        case word of
          0 -> liftM Inline get
          1 -> liftM Jump get
          _ -> error "problem getting Opt.Choice binary"



instance Binary Graph where
  get = liftM3 Graph get get get
  put (Graph a b c) = put a >> put b >> put c


instance Binary Main where
  put main =
    case main of
      Static      -> putWord8 0
      Dynamic a b -> putWord8 1 >> put a >> put b

  get =
    do  word <- getWord8
        case word of
          0 -> return Static
          1 -> liftM2 Dynamic get get
          _ -> error "problem getting Opt.Main binary"


instance Binary Node where
  put node =
    case node of
      Define a b           -> putWord8  0 >> put a >> put b
      DefineTailFunc a b c -> putWord8  1 >> put a >> put b >> put c
      Ctor a b             -> putWord8  2 >> put a >> put b
      Enum a               -> putWord8  3 >> put a
      Box                  -> putWord8  4
      Link a               -> putWord8  5 >> put a
      Cycle a b c d        -> putWord8  6 >> put a >> put b >> put c >> put d
      Manager a            -> putWord8  7 >> put a
      Kernel a b           -> putWord8  8 >> put a >> put b
      PortIncoming a b     -> putWord8  9 >> put a >> put b
      PortOutgoing a b     -> putWord8 10 >> put a >> put b

  get =
    do  word <- getWord8
        case word of
          0  -> liftM2 Define get get
          1  -> liftM3 DefineTailFunc get get get
          2  -> liftM2 Ctor get get
          3  -> liftM  Enum get
          4  -> return Box
          5  -> liftM  Link get
          6  -> liftM4 Cycle get get get get
          7  -> liftM  Manager get
          8  -> liftM2 Kernel get get
          9  -> liftM2 PortIncoming get get
          10 -> liftM2 PortOutgoing get get
          _  -> error "problem getting Opt.Node binary"


instance Binary EffectsType where
  put effectsType =
    case effectsType of
      Cmd -> putWord8 0
      Sub -> putWord8 1
      Fx  -> putWord8 2

  get =
    do  word <- getWord8
        case word of
          0 -> return Cmd
          1 -> return Sub
          2 -> return Fx
          _ -> error "problem getting Opt.EffectsType binary"


instance Binary KContent where
  get = liftM2 KContent get get
  put (KContent a b) = put a >> put b


instance Binary KChunk where
  put chunk =
    case chunk of
      JS a       -> putWord8 0 >> put a
      ElmVar a b -> putWord8 1 >> put a >> put b
      JsVar a b  -> putWord8 2 >> put a >> put b
      ElmField a -> putWord8 3 >> put a
      JsField a  -> putWord8 4 >> put a
      JsEnum a   -> putWord8 5 >> put a
      Debug      -> putWord8 6
      Prod       -> putWord8 7

  get =
    do  word <- getWord8
        case word of
          0 -> liftM  JS get
          1 -> liftM2 ElmVar get get
          2 -> liftM2 JsVar get get
          3 -> liftM  ElmField get
          4 -> liftM  JsField get
          5 -> liftM  JsEnum get
          6 -> return Debug
          7 -> return Prod
          _ -> error "problem deserializing AST.Optimized.KChunk"
