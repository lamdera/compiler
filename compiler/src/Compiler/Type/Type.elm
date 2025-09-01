{- MANUALLY FORMATTED -}
module Compiler.Type.Type exposing
  ( Constraint(..)
  , exists
  , Variable
  , FlatType(..)
  --, Type(..)
  , Descriptor(..)
  --, Content(..)
  , SuperType(..)
  , noRank
  , outermostRank
  , Mark
  , noMark
  , nextMark
  --, (==>)
  , int, float, char, string, bool, never
  , texture
  , mkFlexVar
  , mkFlexNumber
  , unnamedFlexVar
  , unnamedFlexSuper
  , nameToFlex
  , nameToRigid
  , toAnnotation
  , toErrorType
  , Content(..)
  , Type(..)
  , IO
  , State
  , init
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Type as Type
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Type as E
import Compiler.Type.Error as ET
import Compiler.Type.UnionFind as UF
import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Class.StateT as StateT
import Extra.System.IO.Pure as IO
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe
import Extra.Type.Tuple as MTuple



-- IO


type alias IO t a =
  UF.IO Descriptor t a


type alias State =
  UF.LocalState Descriptor


init : State
init =
  UF.init



-- STATE OVER IO


type alias StateT t a = StateT.StateT NameState (IO t (a, NameState))

andMap : Applicative.AndMap (StateT t a) (StateT t (a -> b)) (StateT t b)
andMap = StateT.andMap IO.fmap IO.bind

bind : Monad.Bind a (StateT t a) (StateT t b)
bind = StateT.bind IO.bind

evalStateT : StateT t a -> NameState -> IO t a
evalStateT = StateT.evalStateT IO.fmap

fmap : Functor.Fmap a (StateT t a) b (StateT t b)
fmap = StateT.fmap IO.fmap

gets : (NameState -> a) -> StateT t a
gets = StateT.gets IO.return

liftA2 : Applicative.LiftA2 a (StateT t a) b (StateT t b) c (StateT t c)
liftA2 = StateT.liftA2 IO.fmap IO.fmap IO.bind

liftIO : IO t a -> StateT t a
liftIO ia s = IO.fmap (\a -> (a, s)) ia

modify : (NameState -> NameState) -> StateT t ()
modify = StateT.modify IO.return

pure : Applicative.Pure a (StateT t a)
pure = StateT.pure IO.return

return : a -> StateT t a
return = StateT.return IO.return



-- CONSTRAINTS


type Constraint
  = CTrue
  | CSaveTheEnvironment
  | CEqual A.Region E.Category Type (E.Expected Type)
  | CLocal A.Region Name.Name (E.Expected Type)
  | CForeign A.Region Name.Name Can.Annotation (E.Expected Type)
  | CPattern A.Region E.PCategory Type (E.PExpected Type)
  | CAnd (TList Constraint)
  | CLet
      {- rigidVars -} (TList Variable)
      {- flexVars -} (TList Variable)
      {- header -} (Map.Map Name.Name (A.Located Type))
      {- headerCon -} Constraint
      {- bodyCon -} Constraint


exists : TList Variable -> Constraint -> Constraint
exists flexVars constraint =
  CLet [] flexVars Map.empty constraint CTrue



-- TYPE PRIMITIVES


type alias Variable =
  UF.Point Descriptor


type FlatType
  = App1 ModuleName.Canonical Name.Name (TList Variable)
  | Fun1 Variable Variable
  | EmptyRecord1
  | Record1 (Map.Map Name.Name Variable) Variable
  | Unit1
  | Tuple1 Variable Variable (Maybe Variable)


type Type
  = AliasN ModuleName.Canonical Name.Name (TList (Name.Name, Type)) Type
  | VarN Variable
  | AppN ModuleName.Canonical Name.Name (TList Type)
  | FunN Type Type
  | EmptyRecordN
  | RecordN (Map.Map Name.Name Type) Type
  | UnitN
  | TupleN Type Type (Maybe Type)



-- DESCRIPTORS


type Descriptor =
  Descriptor
    {- content -} Content
    {- rank -} Int
    {- mark -} Mark
    {- copy -} (Maybe Variable)


getContent (Descriptor content _ _ _) = content
getMark (Descriptor _ _ mark _) = mark

setContent content (Descriptor _ a b c) = Descriptor content a b c
setMark mark (Descriptor a b _ c) = Descriptor a b mark c


type Content
  = FlexVar (Maybe Name.Name)
  | FlexSuper SuperType (Maybe Name.Name)
  | RigidVar Name.Name
  | RigidSuper SuperType Name.Name
  | Structure FlatType
  | Alias ModuleName.Canonical Name.Name (TList (Name.Name,Variable)) Variable
  | Error


type SuperType
  = Number
  | Comparable
  | Appendable
  | CompAppend


makeDescriptor : Content -> Descriptor
makeDescriptor content =
  Descriptor content noRank noMark Nothing



-- RANKS


noRank : Int
noRank =
  0


outermostRank : Int
outermostRank =
  1



-- MARKS


type Mark = Mark Int


noMark : Mark
noMark =
  Mark 2


occursMark : Mark
occursMark =
  Mark 1


getVarNamesMark : Mark
getVarNamesMark =
  Mark 0


nextMark : Mark -> Mark
nextMark (Mark mark) =
  Mark (mark + 1)



-- PRIMITIVE TYPES


int : Type
int = AppN ModuleName.basics "Int" []


float : Type
float = AppN ModuleName.basics "Float" []


char : Type
char = AppN ModuleName.char "Char" []


string : Type
string = AppN ModuleName.string "String" []


bool : Type
bool = AppN ModuleName.basics "Bool" []


never : Type
never = AppN ModuleName.basics "Never" []



-- WEBGL TYPES


texture : Type
texture = AppN ModuleName.texture "Texture" []



-- MAKE FLEX VARIABLES


mkFlexVar : IO t Variable
mkFlexVar =
  UF.fresh flexVarDescriptor


flexVarDescriptor : Descriptor
flexVarDescriptor =
  makeDescriptor unnamedFlexVar


unnamedFlexVar : Content
unnamedFlexVar =
  FlexVar Nothing



-- MAKE FLEX NUMBERS


mkFlexNumber : IO t Variable
mkFlexNumber =
  UF.fresh flexNumberDescriptor


flexNumberDescriptor : Descriptor
flexNumberDescriptor =
  makeDescriptor (unnamedFlexSuper Number)


unnamedFlexSuper : SuperType -> Content
unnamedFlexSuper super =
  FlexSuper super Nothing



-- MAKE NAMED VARIABLES


nameToFlex : Name.Name -> IO t Variable
nameToFlex name =
  UF.fresh <| makeDescriptor <|
    MMaybe.maybe FlexVar FlexSuper (toSuper name) (Just name)


nameToRigid : Name.Name -> IO t Variable
nameToRigid name =
  UF.fresh <| makeDescriptor <|
    MMaybe.maybe RigidVar RigidSuper (toSuper name) name


toSuper : Name.Name -> Maybe SuperType
toSuper name =
  if Name.isNumberType name then
    Just Number

  else if Name.isComparableType name then
    Just Comparable

  else if Name.isAppendableType name then
    Just Appendable

  else if Name.isCompappendType name then
    Just CompAppend

  else
    Nothing



-- TO TYPE ANNOTATION


toAnnotation : Variable -> IO t Can.Annotation
toAnnotation variable =
  IO.bind (getVarNames variable Map.empty) <| \userNames ->
  IO.bind
    ((variableToCanType variable) (makeNameState userNames)) <| \(tipe, NameState freeVars _ _ _ _ _) ->
  IO.return <| Can.Forall freeVars tipe


variableToCanType : Variable -> StateT t Can.Type
variableToCanType variable =
  bind (liftIO <| UF.get variable) <| \(Descriptor content _ _ _) ->
  case content of
    Structure term ->
      termToCanType term

    FlexVar maybeName ->
      case maybeName of
        Just name ->
          return (Can.TVar name)

        Nothing ->
          bind getFreshVarName <| \name ->
          bind (liftIO <| UF.modify variable (\(Descriptor _ a b c) -> Descriptor (FlexVar (Just name)) a b c)) <| \() ->
          return (Can.TVar name)

    FlexSuper super maybeName ->
      case maybeName of
        Just name ->
          return (Can.TVar name)

        Nothing ->
          bind (getFreshSuperName super) <| \name ->
          bind (liftIO <| UF.modify variable (\(Descriptor _ a b c) -> Descriptor (FlexSuper super (Just name)) a b c)) <| \() ->
          return (Can.TVar name)

    RigidVar name ->
      return (Can.TVar name)

    RigidSuper _ name ->
      return (Can.TVar name)

    Alias home name args realVariable ->
      bind (MList.traverse pure liftA2 (MTuple.traverseSecond fmap variableToCanType) args) <| \canArgs ->
      bind (variableToCanType realVariable) <| \canType ->
      return (Can.TAlias home name canArgs (Can.Filled canType))

    Error ->
      Debug.todo "cannot handle Error types in variableToCanType"


termToCanType : FlatType -> StateT t Can.Type
termToCanType term =
  case term of
    App1 home name args ->
      fmap (Can.TType home name) <| MList.traverse pure liftA2 variableToCanType args

    Fun1 a b ->
      pure Can.TLambda
        |> andMap (variableToCanType a)
        |> andMap (variableToCanType b)

    EmptyRecord1 ->
      return <| Can.TRecord Map.empty Nothing

    Record1 fields extension ->
      bind (Map.traverse pure liftA2 fieldToCanType fields) <| \canFields ->
      bind (fmap Type.iteratedDealias <| variableToCanType extension) <| \canExt ->
      return <|
        case canExt of
          Can.TRecord subFields subExt ->
            Can.TRecord (Map.union subFields canFields) subExt

          Can.TVar name ->
            Can.TRecord canFields (Just name)

          _ ->
            Debug.todo "Used toAnnotation on a type that is not well-formed"

    Unit1 ->
      return Can.TUnit

    Tuple1 a b maybeC ->
      pure Can.TTuple
        |> andMap (variableToCanType a)
        |> andMap (variableToCanType b)
        |> andMap (MMaybe.traverse pure fmap variableToCanType maybeC)


fieldToCanType : Variable -> StateT t Can.FieldType
fieldToCanType variable =
  bind (variableToCanType variable) <| \tipe ->
  return (Can.FieldType 0 tipe)



-- TO ERROR TYPE


toErrorType : Variable -> IO t ET.Type
toErrorType variable =
  IO.bind (getVarNames variable Map.empty) <| \userNames ->
  evalStateT (variableToErrorType variable) (makeNameState userNames)


variableToErrorType : Variable -> StateT t ET.Type
variableToErrorType variable =
  bind (liftIO <| UF.get variable) <| \descriptor ->
  let mark = getMark descriptor in
  if mark == occursMark
    then
      return ET.Infinite

    else
      bind (liftIO <| UF.modify variable (setMark occursMark)) <| \() ->
      bind (contentToErrorType variable (getContent descriptor)) <| \errType ->
      bind (liftIO <| UF.modify variable (setMark mark)) <| \() ->
      return errType


superToSuper : SuperType -> ET.Super
superToSuper super =
  case super of
    Number -> ET.Number
    Comparable -> ET.Comparable
    Appendable -> ET.Appendable
    CompAppend -> ET.CompAppend


contentToErrorType : Variable -> Content -> StateT t ET.Type
contentToErrorType variable content =
  case content of
    Structure term ->
      termToErrorType term

    FlexVar maybeName ->
      case maybeName of
        Just name ->
          return (ET.FlexVar name)

        Nothing ->
          bind getFreshVarName <| \name ->
          bind (liftIO <| UF.modify variable (setContent <| FlexVar (Just name))) <| \() ->
          return (ET.FlexVar name)

    FlexSuper super maybeName ->
      case maybeName of
        Just name ->
          return (ET.FlexSuper (superToSuper super) name)

        Nothing ->
          bind (getFreshSuperName super) <| \name ->
          bind (liftIO <| UF.modify variable (setContent <| FlexSuper super (Just name))) <| \() ->
          return (ET.FlexSuper (superToSuper super) name)

    RigidVar name ->
      return (ET.RigidVar name)

    RigidSuper super name ->
      return (ET.RigidSuper (superToSuper super) name)

    Alias home name args realVariable ->
      bind (MList.traverse pure liftA2 (MTuple.traverseSecond fmap variableToErrorType) args) <| \errArgs ->
      bind (variableToErrorType realVariable) <| \errType ->
      return (ET.Alias home name errArgs errType)

    Error ->
      return ET.Error


termToErrorType : FlatType -> StateT t ET.Type
termToErrorType term =
  case term of
    App1 home name args ->
      fmap (ET.Type home name) <| MList.traverse pure liftA2 variableToErrorType args

    Fun1 a b ->
      bind (variableToErrorType a) <| \arg ->
      bind (variableToErrorType b) <| \result ->
      return <|
        case result of
          ET.Lambda arg1 arg2 others ->
            ET.Lambda arg arg1 (arg2::others)

          _ ->
            ET.Lambda arg result []

    EmptyRecord1 ->
      return <| ET.Record Map.empty ET.Closed

    Record1 fields extension ->
      bind (Map.traverse pure liftA2 variableToErrorType fields) <| \errFields ->
      bind (fmap ET.iteratedDealias <| variableToErrorType extension) <| \errExt ->
      return <|
        case errExt of
          ET.Record subFields subExt ->
            ET.Record (Map.union subFields errFields) subExt

          ET.FlexVar ext ->
            ET.Record errFields (ET.FlexOpen ext)

          ET.RigidVar ext ->
            ET.Record errFields (ET.RigidOpen ext)

          _ ->
            Debug.todo "Used toErrorType on a type that is not well-formed"

    Unit1 ->
      return ET.Unit

    Tuple1 a b maybeC ->
      pure ET.Tuple
        |> andMap (variableToErrorType a)
        |> andMap (variableToErrorType b)
        |> andMap (MMaybe.traverse pure fmap variableToErrorType maybeC)



-- MANAGE FRESH VARIABLE NAMES


type NameState =
  NameState
    {- taken -} (Map.Map Name.Name ())
    {- normals -} Int
    {- numbers -} Int
    {- comparables -} Int
    {- appendables -} Int
    {- compAppends -} Int


getTaken (NameState taken _ _ _ _ _) = taken
getNormals (NameState _ normals _ _ _ _) = normals
getNumbers (NameState _ _ numbers _ _ _) = numbers
getComparables (NameState _ _ _ comparables _ _) = comparables
getAppendables (NameState _ _ _ _ appendables _) = appendables
getCompAppends (NameState _ _ _ _ _ compAppends) = compAppends

setTaken taken (NameState _ a b c d e) = NameState taken a b c d e
setNormals normals (NameState a _ b c d e) = NameState a normals b c d e
setNumbers numbers (NameState a b _ c d e) = NameState a b numbers c d e
setComparables comparables (NameState a b c _ d e) = NameState a b c comparables d e
setAppendables appendables (NameState a b c d _ e) = NameState a b c d appendables e
setCompAppends compAppends (NameState a b c d e _) = NameState a b c d e compAppends


makeNameState : Map.Map Name.Name Variable -> NameState
makeNameState taken =
  NameState (Map.map (always ()) taken) 0 0 0 0 0



-- FRESH VAR NAMES


getFreshVarName : StateT t Name.Name
getFreshVarName =
  bind (gets getNormals) <| \index ->
  bind (gets getTaken) <| \taken ->
  let (name, newIndex, newTaken) = getFreshVarNameHelp index taken in
  bind (modify <| (setTaken newTaken >> setNormals newIndex)) <| \() ->
  return name


getFreshVarNameHelp : Int -> Map.Map Name.Name () -> (Name.Name, Int, Map.Map Name.Name ())
getFreshVarNameHelp index taken =
  let
    name =
      Name.fromTypeVariableScheme index
  in
  if Map.member name taken then
    getFreshVarNameHelp (index + 1) taken
  else
    ( name, index + 1, Map.insert name () taken )



-- FRESH SUPER NAMES


getFreshSuperName : SuperType -> StateT t Name.Name
getFreshSuperName super =
  case super of
    Number ->
      getFreshSuper "number" getNumbers setNumbers

    Comparable ->
      getFreshSuper "comparable" getComparables setComparables

    Appendable ->
      getFreshSuper "appendable" getAppendables setAppendables

    CompAppend ->
      getFreshSuper "compappend" getCompAppends setCompAppends


getFreshSuper : Name.Name -> (NameState -> Int) -> (Int -> NameState -> NameState) -> StateT t Name.Name
getFreshSuper prefix getter setter =
  bind (gets getter) <| \index ->
  bind (gets getTaken) <| \taken ->
  let (name, newIndex, newTaken) = getFreshSuperHelp prefix index taken in
  bind (modify (setTaken newTaken >> setter newIndex)) <| \() ->
  return name


getFreshSuperHelp : Name.Name -> Int -> Map.Map Name.Name () -> (Name.Name, Int, Map.Map Name.Name ())
getFreshSuperHelp prefix index taken =
  let
    name =
      Name.fromTypeVariable prefix index
  in
    if Map.member name taken then
      getFreshSuperHelp prefix (index + 1) taken

    else
      ( name, index + 1, Map.insert name () taken )



-- GET ALL VARIABLE NAMES


getVarNames : Variable -> Map.Map Name.Name Variable -> IO t (Map.Map Name.Name Variable)
getVarNames var takenNames =
  IO.bind (UF.get var) <| \(Descriptor content rank mark copy) ->
  if mark == getVarNamesMark
    then IO.return takenNames
    else
      IO.bind (UF.set var (Descriptor content rank getVarNamesMark copy)) <| \() ->
      case content of
        Error ->
          IO.return takenNames

        FlexVar maybeName ->
          case maybeName of
            Nothing ->
              IO.return takenNames

            Just name ->
              addName 0 name var (FlexVar << Just) takenNames

        FlexSuper super maybeName ->
          case maybeName of
            Nothing ->
              IO.return takenNames

            Just name ->
              addName 0 name var (FlexSuper super << Just) takenNames

        RigidVar name ->
          addName 0 name var RigidVar takenNames

        RigidSuper super name ->
          addName 0 name var (RigidSuper super) takenNames

        Alias _ _ args _ ->
          IO.foldrMList getVarNames takenNames (MList.map Tuple.second args)

        Structure flatType ->
          case flatType of
            App1 _ _ args ->
              IO.foldrMList getVarNames takenNames args

            Fun1 arg body ->
              IO.andThen (getVarNames arg) <| getVarNames body takenNames

            EmptyRecord1 ->
              IO.return takenNames

            Record1 fields extension ->
              IO.andThen (getVarNames extension) <|
                IO.foldrMList getVarNames takenNames (Map.elems fields)

            Unit1 ->
              IO.return takenNames

            Tuple1 a b Nothing ->
              IO.andThen (getVarNames a) <| getVarNames b takenNames

            Tuple1 a b (Just c) ->
              IO.andThen (getVarNames a) <| IO.andThen (getVarNames b) <| getVarNames c takenNames



-- REGISTER NAME / RENAME DUPLICATES


addName : Int -> Name.Name -> Variable -> (Name.Name -> Content) -> Map.Map Name.Name Variable -> IO t (Map.Map Name.Name Variable)
addName index givenName var makeContent takenNames =
  let
    indexedName =
      Name.fromTypeVariable givenName index
  in
    case Map.lookup indexedName takenNames of
      Nothing ->
        IO.bind (if indexedName == givenName then IO.return () else
          UF.modify var <| \(Descriptor _ rank mark copy) ->
            Descriptor (makeContent indexedName) rank mark copy) <| \() ->
        IO.return <| Map.insert indexedName var takenNames

      Just otherVar ->
        IO.bind (UF.equivalent var otherVar) <| \same ->
        if same
          then IO.return takenNames
          else addName (index + 1) givenName var makeContent takenNames
