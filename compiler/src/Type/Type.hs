{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Type.Type
  ( Constraint(..)
  , exists
  , Variable
  , FlatType(..)
  , Type(..)
  , Descriptor(Descriptor)
  , Content(..)
  , SuperType(..)
  , noRank
  , outermostRank
  , Mark(..) -- @LAMDERA exposed
  , noMark
  , nextMark
  , (==>)
  , int, float, char, string, bool, never
  , vec2, vec3, vec4, mat4, texture
  , mkFlexVar
  , mkFlexNumber
  , unnamedFlexVar
  , unnamedFlexSuper
  , nameToFlex
  , nameToRigid
  , toAnnotation
  , toAnnotations -- @LAMDERA
  , toErrorType
  -- @LAMDERA deferred alias bodies, see Deferred below
  , Deferred(..)
  , DeferredKey
  , DeferredOrigin
  , AliasSource
  , deferredVariables
  , traverseDeferred
  , expandDeferred
  , typeToVarWith
  , srcTypeToVarWith
  )
  where


import Control.Monad.State.Strict (StateT, liftIO)
import qualified Control.Monad.State.Strict as State
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Utils as Map
import Control.Exception (evaluate)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.IntMap.Strict as IntMap
import System.Mem.StableName (StableName, makeStableName, hashStableName)
import Data.Word (Word32)

import qualified Crash

import qualified AST.Canonical as Can
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified AST.Utils.Type as Type
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as E
import qualified Type.Error as ET
import qualified Type.UnionFind as UF



-- CONSTRAINTS


data Constraint
  = CTrue
  | CSaveTheEnvironment
  | CEqual A.Region E.Category Type (E.Expected Type)
  | CLocal A.Region N.Name (E.Expected Type)
  | CForeign A.Region N.Name Can.Annotation (E.Expected Type)
  | COperator A.Region Op.Name Can.Annotation (E.Expected Type)
  | CPattern A.Region E.PCategory Type (E.PExpected Type)
  | CAnd [Constraint]
  | CLet
      { _rigidVars :: [Variable]
      , _flexVars :: [Variable]
      , _header :: Map.Map N.Name (A.Located Type)
      , _headerCon :: Constraint
      , _bodyCon :: Constraint
      }


exists :: [Variable] -> Constraint -> Constraint
exists flexVars constraint =
  CLet [] flexVars Map.empty constraint CTrue



-- TYPE PRIMITIVES
--
-- TODO is PlaceHolder really unused now?


type Variable =
    UF.Point Descriptor


data FlatType
    = App1 ModuleName.Canonical T.Name [Variable]
    | Fun1 Variable Variable
    | EmptyRecord1
    | Record1 (Map.Map N.Name Variable) Variable
    | Unit1
    | Pair1 Variable Variable
    | Triple1 Variable Variable Variable


data Type
    = PlaceHolder T.Var
    | AliasN ModuleName.Canonical T.Name [(T.Var, Type)] Type (Maybe AliasSource) -- @LAMDERA source, see DeferredKey
    | VarN Variable
    | AppN ModuleName.Canonical T.Name [Type]
    | FunN Type Type
    | EmptyRecordN
    | RecordN (Map.Map N.Name Type) Type
    | UnitN
    | PairN Type Type
    | TripleN Type Type Type



-- DESCRIPTORS


data Descriptor =
  Descriptor
    { _content :: Content
    , _rank :: Int
    , _mark :: Mark
    , _copy :: Maybe Variable
    }


data Content
    = FlexVar (Maybe T.Var)
    | FlexSuper SuperType (Maybe T.Var)
    | RigidVar T.Var
    | RigidSuper SuperType T.Var
    | Structure FlatType
    | Alias ModuleName.Canonical T.Name [(T.Var,Variable)] Variable
    | Deferred Deferred -- @LAMDERA only ever the real type of an Alias, see Deferred
    | Error


{-| @LAMDERA The real type of an alias, not expanded yet.

Upstream builds a variable for every node of an alias's real type each time a type
mentioning the alias is turned into variables: every annotation, every reference to
a foreign value, every pattern and expectation. Aliases unify with each other by name
and arguments, so most of those real types are never looked at. An app with a large
Model record (aliases of records of aliases...) spent most of its type checking
building them: 215M variables for 4M unifications on a 390 module rebuild.

So the real type starts out as a Deferred: the body, plus the variables its free type
variables stand for. It is expanded, building exactly the variables upstream would
have built up front, the first time something needs to look inside it: unifying it
with a structure or a different alias (Type.Unify forces it), gathering record fields,
or turning it back into a type (toAnnotation, toErrorType). Anything else only ever
deals with an alias's arguments, which are built as before.

  DeferredType: from Type (constraint types). Only used when the alias arguments have
    no variables in them, so the body (which only refers to the arguments) has none
    either, and Variables embedded in the body never need remapping when copied.

  DeferredSrc: from Can.Type (annotations of foreign values, operators). The map is
    what the body's type variables stand for: the alias arguments for a Holey body,
    the annotation's free variables for a Filled one.
-}
data Deferred
    = DeferredType (Maybe DeferredKey) (Map.Map T.Var Variable) Type
    | DeferredSrc DeferredOrigin (Map.Map T.Var Variable) Can.Type


{-| Where an alias node of a constraint type came from (Type.Instantiate): the Can.Type
inside its Holey / Filled (True = Holey), and its arguments as written.
-}
type AliasSource =
  (Can.Type, Bool, [Can.Type])


{-| Identifies what a still-deferred real type turns into, for toAnnotation's
AliasCache. Two Deferreds with equal keys convert to equal Can.Types:

  FromConstraint: a DeferredType's body is built (Type.Instantiate) from the Can.Type
    of the alias definition (or Filled body) and the alias arguments as written,
    which are closed, so the body is decided by that Can.Type and those arguments.

  FromAnnotation: a DeferredSrc's body is a Can.Type whose type variables stand for
    the alias arguments (Holey: they are the arguments; Filled: an alias's real type
    only has the type variables of its arguments). Given the arguments as converted
    for this annotation, the conversion is decided by that Can.Type.

The Can.Type is compared by StableName (its identity), not by value: equal identity
is all that is needed, it is cheap, and the bodies in question are shared already
(alias definitions come from one place, foreign annotations from one interface).
-}
data DeferredKey =
  DeferredKey !DeferredKind !(StableName Can.Type) !Bool ![Can.Type]
  deriving (Eq)


data DeferredKind = FromConstraint | FromAnnotation
  deriving (Eq)


data DeferredOrigin =
  DeferredOrigin !(StableName Can.Type) !Bool


makeSourceKey :: AliasSource -> IO DeferredKey
makeSourceKey (inner, isHoley, args) =
  do  name <- makeStableName =<< evaluate inner
      return (DeferredKey FromConstraint name isHoley args)


makeOrigin :: Can.Type -> Bool -> IO DeferredOrigin
makeOrigin body isHoley =
  do  name <- makeStableName =<< evaluate body
      return (DeferredOrigin name isHoley)


deferredKey :: Deferred -> [Can.Type] -> Maybe DeferredKey
deferredKey deferred canArgs =
  case deferred of
    DeferredType maybeKey _ _ ->
      maybeKey

    DeferredSrc (DeferredOrigin name isHoley) _ _ ->
      Just (DeferredKey FromAnnotation name isHoley canArgs)


deferredVariables :: Deferred -> [Variable]
deferredVariables deferred =
  case deferred of
    DeferredType _ dict _ -> Map.elems dict
    DeferredSrc _ dict _ -> Map.elems dict


traverseDeferred :: (Variable -> IO Variable) -> Deferred -> IO Deferred
traverseDeferred f deferred =
  case deferred of
    DeferredType key dict tipe -> (\d -> DeferredType key d tipe) <$> traverse f dict
    DeferredSrc origin dict tipe -> (\d -> DeferredSrc origin d tipe) <$> traverse f dict


{-| Build the variables for a deferred real type, registering each new variable with
the given action, and return the variable for its root.
-}
expandDeferred :: (Content -> IO Variable) -> Deferred -> IO Variable
expandDeferred register deferred =
  case deferred of
    DeferredType _ dict tipe -> typeToVarWith register dict tipe
    DeferredSrc _ dict tipe -> srcTypeToVarWith register dict tipe


{-| What used to be Type.Solve.typeToVar, with registering abstracted out so that a
Deferred can be expanded outside the solver, and alias bodies deferred when their
arguments are closed.
-}
typeToVarWith :: (Content -> IO Variable) -> Map.Map T.Var Variable -> Type -> IO Variable
typeToVarWith register aliasDict tipe =
  let go = typeToVarWith register aliasDict in
  case tipe of
    VarN v ->
      return v

    AppN home name args ->
      do  argVars <- traverse go args
          register (Structure (App1 home name argVars))

    FunN a b ->
      do  aVar <- go a
          bVar <- go b
          register (Structure (Fun1 aVar bVar))

    AliasN home name args aliasType source ->
      do  argVars <- traverse (traverse go) args
          let argDict = Map.fromList argVars
          aliasVar <-
            if all (isClosedType . snd) args
              then
                do  key <- traverse makeSourceKey source
                    register (Deferred (DeferredType key argDict aliasType))
              else typeToVarWith register argDict aliasType
          register (Alias home name argVars aliasVar)

    PlaceHolder name ->
      return $ $(Map.require 'typeToVarWith) name aliasDict T.varToChars

    RecordN fields ext ->
      do  fieldVars <- traverse go fields
          extVar <- go ext
          register (Structure (Record1 fieldVars extVar))

    EmptyRecordN ->
      register emptyRecord1

    UnitN ->
      register unit1

    PairN a b ->
      do  aVar <- go a
          bVar <- go b
          register (Structure (Pair1 aVar bVar))

    TripleN a b c ->
      do  aVar <- go a
          bVar <- go b
          cVar <- go c
          register (Structure (Triple1 aVar bVar cVar))


{-| Whether a type has no variables in it. An alias body only refers to the alias
arguments, so an alias is closed when its arguments are, and the body is not looked at
(it is built lazily, see Type.Instantiate).
-}
isClosedType :: Type -> Bool
isClosedType tipe =
  case tipe of
    PlaceHolder _ -> False
    AliasN _ _ args _ _ -> all (isClosedType . snd) args
    VarN _ -> False
    AppN _ _ args -> all isClosedType args
    FunN a b -> isClosedType a && isClosedType b
    EmptyRecordN -> True
    RecordN fields ext -> all isClosedType fields && isClosedType ext
    UnitN -> True
    PairN a b -> isClosedType a && isClosedType b
    TripleN a b c -> isClosedType a && isClosedType b && isClosedType c


{-| What used to be Type.Solve.srcTypeToVar, with registering abstracted out, and
every alias body deferred.
-}
srcTypeToVarWith :: (Content -> IO Variable) -> Map.Map T.Var Variable -> Can.Type -> IO Variable
srcTypeToVarWith register flexVars srcType =
  let go = srcTypeToVarWith register flexVars in
  case srcType of
    Can.TLambda argument result ->
      do  argVar <- go argument
          resultVar <- go result
          register (Structure (Fun1 argVar resultVar))

    Can.TVar name ->
      return $ $(Map.require 'srcTypeToVarWith) name flexVars T.varToChars

    Can.TType home name args ->
      do  argVars <- traverse go args
          register (Structure (App1 home name argVars))

    Can.TRecord fields maybeExt ->
      do  fieldVars <- traverse (\(Can.FieldType _ srcTipe) -> go srcTipe) fields
          extVar <-
            case maybeExt of
              Nothing -> register emptyRecord1
              Just ext -> return $ $(Map.require 'srcTypeToVarWith) ext flexVars T.varToChars
          register (Structure (Record1 fieldVars extVar))

    Can.TUnit ->
      register unit1

    Can.TPair a b ->
      do  aVar <- go a
          bVar <- go b
          register (Structure (Pair1 aVar bVar))

    Can.TTriple a b c ->
      do  aVar <- go a
          bVar <- go b
          cVar <- go c
          register (Structure (Triple1 aVar bVar cVar))

    Can.TAlias home name args aliasType ->
      do  argVars <- traverse (traverse go) args
          aliasVar <-
            case aliasType of
              Can.Holey tipe ->
                do  origin <- makeOrigin tipe True
                    register $ Deferred $ DeferredSrc origin (Map.fromList argVars) tipe

              Can.Filled tipe ->
                do  origin <- makeOrigin tipe False
                    register $ Deferred $ DeferredSrc origin flexVars tipe

          register (Alias home name argVars aliasVar)


{-# NOINLINE emptyRecord1 #-}
emptyRecord1 :: Content
emptyRecord1 =
  Structure EmptyRecord1


{-# NOINLINE unit1 #-}
unit1 :: Content
unit1 =
  Structure Unit1


-- @LAMDERA a Deferred that is only being looked at (toAnnotation, toErrorType) is
-- expanded into variables nobody else can see, and left deferred.
temporaryVariable :: Content -> IO Variable
temporaryVariable content =
  UF.fresh (Descriptor content noRank noMark Nothing)


data SuperType
  = Number
  | Comparable
  | Appendable
  | CompAppend
  deriving (Eq)


makeDescriptor :: Content -> Descriptor
makeDescriptor content =
  Descriptor content noRank noMark Nothing



-- RANKS


noRank :: Int
noRank =
  0


outermostRank :: Int
outermostRank =
  1



-- MARKS


newtype Mark = Mark Word32
  deriving (Eq, Ord)


noMark :: Mark
noMark =
  Mark 2


occursMark :: Mark
occursMark =
  Mark 1


getVarNamesMark :: Mark
getVarNamesMark =
  Mark 0


{-# INLINE nextMark #-}
nextMark :: Mark -> Mark
nextMark (Mark mark) =
  Mark (mark + 1)



-- FUNCTION TYPES


infixr 9 ==>


{-# INLINE (==>) #-}
(==>) :: Type -> Type -> Type
(==>) =
  FunN



-- PRIMITIVE TYPES


{-# NOINLINE int    #-}; int    :: Type; int    = AppN ModuleName.basics T.int    []
{-# NOINLINE float  #-}; float  :: Type; float  = AppN ModuleName.basics T.float  []
{-# NOINLINE char   #-}; char   :: Type; char   = AppN ModuleName.char   T.char   []
{-# NOINLINE string #-}; string :: Type; string = AppN ModuleName.string T.string []
{-# NOINLINE bool   #-}; bool   :: Type; bool   = AppN ModuleName.basics T.bool   []
{-# NOINLINE never  #-}; never  :: Type; never  = AppN ModuleName.basics T.never  []



-- WEBGL TYPES


{-# NOINLINE vec2    #-}; vec2    :: Type; vec2    = AppN ModuleName.vector2 T.vec2    []
{-# NOINLINE vec3    #-}; vec3    :: Type; vec3    = AppN ModuleName.vector3 T.vec3    []
{-# NOINLINE vec4    #-}; vec4    :: Type; vec4    = AppN ModuleName.vector4 T.vec4    []
{-# NOINLINE mat4    #-}; mat4    :: Type; mat4    = AppN ModuleName.matrix4 T.mat4    []
{-# NOINLINE texture #-}; texture :: Type; texture = AppN ModuleName.texture T.texture []



-- MAKE FLEX VARIABLES


mkFlexVar :: IO Variable
mkFlexVar =
  UF.fresh flexVarDescriptor


{-# NOINLINE flexVarDescriptor #-}
flexVarDescriptor :: Descriptor
flexVarDescriptor =
  makeDescriptor unnamedFlexVar


{-# NOINLINE unnamedFlexVar #-}
unnamedFlexVar :: Content
unnamedFlexVar =
  FlexVar Nothing



-- MAKE FLEX NUMBERS


mkFlexNumber :: IO Variable
mkFlexNumber =
  UF.fresh flexNumberDescriptor


{-# NOINLINE flexNumberDescriptor #-}
flexNumberDescriptor :: Descriptor
flexNumberDescriptor =
  makeDescriptor (unnamedFlexSuper Number)


unnamedFlexSuper :: SuperType -> Content
unnamedFlexSuper super =
  FlexSuper super Nothing



-- MAKE NAMED VARIABLES


nameToFlex :: T.Var -> IO Variable
nameToFlex name =
  UF.fresh $ makeDescriptor $
    maybe FlexVar FlexSuper (toSuper name) (Just name)


nameToRigid :: T.Var -> IO Variable
nameToRigid name =
  UF.fresh $ makeDescriptor $
    maybe RigidVar RigidSuper (toSuper name) name


toSuper :: T.Var -> Maybe SuperType
toSuper (T.Var _ c) =
  case c of
    T.Any        -> Nothing
    T.Comparable -> Just Comparable
    T.Appendable -> Just Appendable
    T.CompAppend -> Just CompAppend
    T.Number     -> Just Number



-- TO TYPE ANNOTATION


toAnnotation :: Variable -> IO Can.Annotation
toAnnotation variable =
  do  cache <- newAliasCache
      toAnnotationWith cache variable


{-| @LAMDERA The annotations of a whole module, sharing one AliasCache, so that every
use of an alias with the same arguments (say a large Model record) in the module's
annotations is converted once, and is the same Can.Type in memory.
-}
toAnnotations :: Map.Map N.Name Variable -> IO (Map.Map N.Name Can.Annotation)
toAnnotations env =
  do  cache <- newAliasCache
      traverse (toAnnotationWith cache) env


toAnnotationWith :: AliasCache -> Variable -> IO Can.Annotation
toAnnotationWith cache variable =
  do  userNames <- getVarNames variable Map.empty
      (tipe, NameState freeVars _ _ _ _ _) <-
        State.runStateT (variableToCanType cache variable) (makeNameState userNames)
      return $ Can.Forall freeVars tipe


{-| @LAMDERA The conversion of each still-deferred real type seen so far, by
DeferredKey (bucketed by the StableName's hash).

Most real types in a module's annotations were never looked at by the solver, so they
are still Deferred, and the same few (a Model record, a page context) come up again in
annotation after annotation. Converting each one once, and sharing the result, gives
values == to converting it at every use, without expanding and walking it every time,
and leaves one copy of it in memory. A real type that has been expanded is converted
as before: once unification has been inside it, it can differ (a record unified with
another one keeps the other's field variables, so an alias there can turn into its
expansion), so it is never cached.
-}
type AliasCache =
  IORef (IntMap.IntMap [(DeferredKey, Can.Type)])


newAliasCache :: IO AliasCache
newAliasCache =
  newIORef IntMap.empty


aliasCacheKeyHash :: DeferredKey -> Int
aliasCacheKeyHash (DeferredKey _ name _ _) =
  hashStableName name


variableToCanType :: AliasCache -> Variable -> StateT NameState IO Can.Type
variableToCanType cache variable =
  do  (Descriptor content _ _ _) <- liftIO $ UF.get variable
      case content of
        Structure term ->
            termToCanType cache term

        FlexVar maybeName ->
          case maybeName of
            Just name ->
              return (Can.TVar name)

            Nothing ->
              do  name <- getFreshVarName
                  liftIO $ UF.modify variable (\desc -> desc { _content = FlexVar (Just name) })
                  return (Can.TVar name)

        FlexSuper super maybeName ->
          case maybeName of
            Just name ->
              return (Can.TVar name)

            Nothing ->
              do  name <- getFreshSuperName super
                  liftIO $ UF.modify variable (\desc -> desc { _content = FlexSuper super (Just name) })
                  return (Can.TVar name)

        RigidVar name ->
            return (Can.TVar name)

        RigidSuper _ name ->
            return (Can.TVar name)

        Alias home name args realVariable ->
            do  canArgs <- traverse (traverse (variableToCanType cache)) args
                -- @LAMDERA see AliasCache
                (Descriptor realContent _ _ _) <- liftIO $ UF.get realVariable
                let maybeKey =
                      case realContent of
                        Deferred deferred -> deferredKey deferred (map snd canArgs)
                        _ -> Nothing
                canType <-
                  case maybeKey of
                    Just key ->
                      do  let hash = aliasCacheKeyHash key
                          bucket <- liftIO $ IntMap.findWithDefault [] hash <$> readIORef cache
                          case lookup key bucket of
                            Just canType ->
                              return canType

                            Nothing ->
                              do  canType <- variableToCanType cache realVariable
                                  liftIO $ modifyIORef' cache (IntMap.insertWith (++) hash [(key, canType)])
                                  return canType

                    Nothing ->
                      variableToCanType cache realVariable
                return (Can.TAlias home name canArgs (Can.Filled canType))

        Deferred deferred ->
            -- @LAMDERA see Deferred
            variableToCanType cache =<< liftIO (expandDeferred temporaryVariable deferred)

        Error ->
            $(Crash.crash 'variableToCanType) "cannot handle Error types in variableToCanType"


termToCanType :: AliasCache -> FlatType -> StateT NameState IO Can.Type
termToCanType cache term =
  case term of
    App1 home name args ->
      Can.TType home name <$> traverse (variableToCanType cache) args

    Fun1 a b ->
      Can.TLambda
        <$> variableToCanType cache a
        <*> variableToCanType cache b

    EmptyRecord1 ->
      return $ Can.TRecord Map.empty Nothing

    Record1 fields extension ->
      do  canFields <- traverse (fieldToCanType cache) fields
          canExt <- Type.iteratedDealias <$> variableToCanType cache extension
          return $
              case canExt of
                Can.TRecord subFields subExt ->
                    Can.TRecord (Map.union subFields canFields) subExt

                Can.TVar name ->
                    Can.TRecord canFields (Just name)

                _ ->
                    $(Crash.crash 'termToCanType) "Used toAnnotation on a type that is not well-formed"

    Unit1 ->
      return Can.TUnit

    Pair1 a b ->
      Can.TPair
        <$> variableToCanType cache a
        <*> variableToCanType cache b

    Triple1 a b c ->
      Can.TTriple
        <$> variableToCanType cache a
        <*> variableToCanType cache b
        <*> variableToCanType cache c


fieldToCanType :: AliasCache -> Variable -> StateT NameState IO Can.FieldType
fieldToCanType cache variable =
  do  tipe <- variableToCanType cache variable
      return (Can.FieldType 0 tipe)



-- TO ERROR TYPE


toErrorType :: Variable -> IO ET.Type
toErrorType variable =
  do  userNames <- getVarNames variable Map.empty
      State.evalStateT (variableToErrorType variable) (makeNameState userNames)


variableToErrorType :: Variable -> StateT NameState IO ET.Type
variableToErrorType variable =
  do  descriptor <- liftIO $ UF.get variable
      let mark = _mark descriptor
      if mark == occursMark
        then
          return ET.Infinite

        else
          do  liftIO $ UF.modify variable (\desc -> desc { _mark = occursMark })
              errType <- contentToErrorType variable (_content descriptor)
              liftIO $ UF.modify variable (\desc -> desc { _mark = mark })
              return errType


contentToErrorType :: Variable -> Content -> StateT NameState IO ET.Type
contentToErrorType variable content =
  case content of
    Structure term ->
        termToErrorType term

    FlexVar maybeName ->
      case maybeName of
        Just name ->
          return (ET.FlexVar name)

        Nothing ->
          do  name <- getFreshVarName
              liftIO $ UF.modify variable (\desc -> desc { _content = FlexVar (Just name) })
              return (ET.FlexVar name)

    FlexSuper super maybeName ->
      case maybeName of
        Just name ->
          return (ET.FlexSuper (superToSuper super) name)

        Nothing ->
          do  name <- getFreshSuperName super
              liftIO $ UF.modify variable (\desc -> desc { _content = FlexSuper super (Just name) })
              return (ET.FlexSuper (superToSuper super) name)

    RigidVar name ->
        return (ET.RigidVar name)

    RigidSuper super name ->
        return (ET.RigidSuper (superToSuper super) name)

    Alias home name args realVariable ->
        do  errArgs <- traverse (traverse variableToErrorType) args
            errType <- variableToErrorType realVariable
            return (ET.Alias home name errArgs errType)

    Deferred deferred ->
        -- @LAMDERA see Deferred
        variableToErrorType =<< liftIO (expandDeferred temporaryVariable deferred)

    Error ->
        return ET.Error


superToSuper :: SuperType -> ET.Super
superToSuper super =
  case super of
    Number -> ET.Number
    Comparable -> ET.Comparable
    Appendable -> ET.Appendable
    CompAppend -> ET.CompAppend


termToErrorType :: FlatType -> StateT NameState IO ET.Type
termToErrorType term =
  case term of
    App1 home name args ->
      ET.Type home name <$> traverse variableToErrorType args

    Fun1 a b ->
      do  arg <- variableToErrorType a
          result <- variableToErrorType b
          return $
            case result of
              ET.Lambda arg1 arg2 others ->
                ET.Lambda arg arg1 (arg2:others)

              _ ->
                ET.Lambda arg result []

    EmptyRecord1 ->
      return $ ET.Record Map.empty ET.Closed

    Record1 fields extension ->
      do  errFields <- traverse variableToErrorType fields
          errExt <- ET.iteratedDealias <$> variableToErrorType extension
          return $
              case errExt of
                ET.Record subFields subExt ->
                    ET.Record (Map.union subFields errFields) subExt

                ET.FlexVar ext ->
                    ET.Record errFields (ET.FlexOpen ext)

                ET.RigidVar ext ->
                    ET.Record errFields (ET.RigidOpen ext)

                _ ->
                    $(Crash.crash 'termToErrorType) "Used toErrorType on a type that is not well-formed"

    Unit1         -> return ET.Unit
    Pair1   a b   -> ET.Pair   <$> variableToErrorType a <*> variableToErrorType b
    Triple1 a b c -> ET.Triple <$> variableToErrorType a <*> variableToErrorType b <*> variableToErrorType c



-- MANAGE FRESH VARIABLE NAMES


data NameState =
  NameState
    { _taken :: Map.Map T.Var ()
    , _normals :: Int
    , _numbers :: Int
    , _comparables :: Int
    , _appendables :: Int
    , _compAppends :: Int
    }


makeNameState :: Map.Map T.Var Variable -> NameState
makeNameState taken =
  NameState (Map.map (const ()) taken) 0 0 0 0 0



-- FRESH VAR NAMES


getFreshVarName :: (Monad m) => StateT NameState m T.Var
getFreshVarName =
  do  index <- State.gets _normals
      taken <- State.gets _taken
      let (name, newIndex, newTaken) = getFreshVarNameHelp index taken
      State.modify $ \state -> state { _taken = newTaken, _normals = newIndex }
      return name


getFreshVarNameHelp :: Int -> Map.Map T.Var () -> (T.Var, Int, Map.Map T.Var ())
getFreshVarNameHelp index taken =
  let
    name = T.genAny index
  in
  if Map.member name taken then
    getFreshVarNameHelp (index + 1) taken
  else
    ( name, index + 1, Map.insert name () taken )



-- FRESH SUPER NAMES


getFreshSuperName :: (Monad m) => SuperType -> StateT NameState m T.Var
getFreshSuperName super =
  case super of
    Number     -> getFreshSuper T.genNumber     _numbers     (\i s -> s { _numbers     = i })
    Comparable -> getFreshSuper T.genComparable _comparables (\i s -> s { _comparables = i })
    Appendable -> getFreshSuper T.genAppendable _appendables (\i s -> s { _appendables = i })
    CompAppend -> getFreshSuper T.genCompAppend _compAppends (\i s -> s { _compAppends = i })


getFreshSuper :: (Monad m) => (Int -> T.Var) -> (NameState -> Int) -> (Int -> NameState -> NameState) -> StateT NameState m T.Var
getFreshSuper gen getter setter =
  do  index <- State.gets getter
      taken <- State.gets _taken
      let (name, newIndex, newTaken) = getFreshSuperHelp gen index taken
      State.modify (\state -> setter newIndex state { _taken = newTaken })
      return name


getFreshSuperHelp :: (Int -> T.Var) -> Int -> Map.Map T.Var () -> (T.Var, Int, Map.Map T.Var ())
getFreshSuperHelp gen index taken =
  let
    name = gen index
  in
    if Map.member name taken then
      getFreshSuperHelp gen (index + 1) taken

    else
      ( name, index + 1, Map.insert name () taken )



-- GET ALL VARIABLE NAMES


getVarNames :: Variable -> Map.Map T.Var Variable -> IO (Map.Map T.Var Variable)
getVarNames var takenNames =
  do  (Descriptor content rank mark copy) <- UF.get var
      if mark == getVarNamesMark
        then return takenNames
        else
        do  UF.set var (Descriptor content rank getVarNamesMark copy)
            case content of
              Error ->
                return takenNames

              FlexVar maybeName ->
                case maybeName of
                  Nothing   -> return takenNames
                  Just name -> addName 0 name var (FlexVar . Just) takenNames

              FlexSuper super maybeName ->
                case maybeName of
                  Nothing   -> return takenNames
                  Just name -> addName 0 name var (FlexSuper super . Just) takenNames

              RigidVar name ->
                addName 0 name var RigidVar takenNames

              RigidSuper super name ->
                addName 0 name var (RigidSuper super) takenNames

              Alias _ _ args _ ->
                foldrM getVarNames takenNames (map snd args)

              Deferred _ ->
                -- @LAMDERA only reachable through an Alias, which looks at its args
                return takenNames

              Structure flatType ->
                case flatType of
                  App1 _ _ xs   -> foldrM getVarNames takenNames xs
                  Fun1 x e      -> getVarNames x =<< getVarNames e takenNames
                  EmptyRecord1  -> return takenNames
                  Record1 fs x  -> getVarNames x =<< foldrM getVarNames takenNames (Map.elems fs)
                  Unit1         -> return takenNames
                  Pair1   a b   -> getVarNames a =<< getVarNames b takenNames
                  Triple1 a b c -> getVarNames a =<< getVarNames b =<< getVarNames c takenNames



-- REGISTER NAME / RENAME DUPLICATES


addName :: Int -> T.Var -> Variable -> (T.Var -> Content) -> Map.Map T.Var Variable -> IO (Map.Map T.Var Variable)
addName index givenName@(T.Var v c) var makeContent takenNames =
  let
    indexedName =
      T.Var (T.genIndexed v index) c
  in
    case Map.lookup indexedName takenNames of
      Nothing ->
        do  if indexedName == givenName then return () else
              UF.modify var $ \(Descriptor _ rank mark copy) ->
                Descriptor (makeContent indexedName) rank mark copy
            return $ Map.insert indexedName var takenNames

      Just otherVar ->
        do  same <- UF.equivalent var otherVar
            if same
              then return takenNames
              else addName (index + 1) givenName var makeContent takenNames

