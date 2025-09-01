{- MANUALLY FORMATTED -}
module Compiler.Type.Unify exposing
  ( Answer(..)
  , unify
  )


import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Type.Error as Error
import Compiler.Type.Occurs as Occurs
import Compiler.Type.Type as Type
import Compiler.Type.UnionFind as UF
import Extra.System.IO.Pure as IO
import Extra.Type.List exposing (TList)
import Extra.Type.Map as Map
import Extra.Class.Applicative as Applicative



-- IO


type alias IO t a = UF.IO Type.Descriptor t a


-- UNIFY


type Answer
  = COk (TList Type.Variable)
  | CErr (TList Type.Variable) Error.Type Error.Type


unify : Type.Variable -> Type.Variable -> IO t Answer
unify v1 v2 =
  case guardedUnify v1 v2 of
    Unify k ->
      k [] onSuccess <| \vars () ->
        IO.bind (Type.toErrorType v1) <| \t1 ->
        IO.bind (Type.toErrorType v2) <| \t2 ->
        IO.bind (UF.union v1 v2 errorDescriptor) <| \_ ->
        IO.return (CErr vars t1 t2)


onSuccess : (TList Type.Variable) -> () -> IO t Answer
onSuccess vars () =
  IO.return (COk vars)


errorDescriptor : Type.Descriptor
errorDescriptor =
  Type.Descriptor Type.Error Type.noRank Type.noMark Nothing



-- CPS UNIFIER


type Unify t z a =
  Unify
    ((TList Type.Variable)
    -> ((TList Type.Variable) -> a -> IO t z)
    -> ((TList Type.Variable) -> () -> IO t z)
    -> IO t z
    )


fmap func (Unify kv) =
  Unify <| \vars ok err ->
    let
      ok1 vars1 value =
        ok vars1 (func value)
    in
    kv vars ok1 err


andMap (Unify kv) (Unify kf) =
  Unify <| \vars ok err ->
    let
      ok1 vars1 func =
        let
          ok2 vars2 value =
            ok vars2 (func value)
        in
        kv vars1 ok2 err
    in
    kf vars ok1 err


liftA2 = Applicative.liftA2 fmap andMap


return a =
  Unify <| \vars ok _ ->
    ok vars a


bind (Unify ka) callback =
  Unify <| \vars ok err ->
    let
      ok1 vars1 a =
        case callback a of
          Unify kb -> kb vars1 ok err
    in
    ka vars ok1 err


register : IO t Type.Variable -> Unify t z Type.Variable
register mkVar =
  Unify <| \vars ok _ ->
    IO.bind mkVar <| \var ->
    ok (var::vars) var


mismatch : Unify t z a
mismatch =
  Unify <| \vars _ err ->
    err vars ()



-- UNIFICATION HELPERS


type Context =
  Context
    {- first -} Type.Variable
    {- firstDesc -} Type.Descriptor
    {- second -} Type.Variable
    {- secondDesc -} Type.Descriptor


getFirst (Context first _ _ _) = first
getSecond (Context _ _ second _) = second


reorient : Context -> Context
reorient (Context var1 desc1 var2 desc2) =
  Context var2 desc2 var1 desc1



-- MERGE


merge : Context -> Type.Content -> Unify t z ()
merge (Context var1 (Type.Descriptor _ rank1 _ _) var2 (Type.Descriptor _ rank2 _ _)) content =
  Unify <| \vars ok _ ->
    IO.andThen (ok vars) <|
      UF.union var1 var2 (Type.Descriptor content (min rank1 rank2) Type.noMark Nothing)


fresh : Context -> Type.Content -> Unify t z Type.Variable
fresh (Context _ (Type.Descriptor _ rank1 _ _) _ (Type.Descriptor _ rank2 _ _)) content =
  register <| UF.fresh <|
    Type.Descriptor content (min rank1 rank2) Type.noMark Nothing



-- ACTUALLY UNIFY THINGS


guardedUnify : Type.Variable -> Type.Variable -> Unify t z ()
guardedUnify left right =
  Unify <| \vars ok err ->
    IO.bind (UF.equivalent left right) <| \equivalent ->
    if equivalent
      then ok vars ()
      else
        IO.bind (UF.get left) <| \leftDesc ->
        IO.bind (UF.get right) <| \rightDesc ->
        case actuallyUnify (Context left leftDesc right rightDesc) of
          Unify k ->
            k vars ok err


subUnify : Type.Variable -> Type.Variable -> Unify t z ()
subUnify var1 var2 =
  guardedUnify var1 var2


actuallyUnify : Context -> Unify t z ()
actuallyUnify ((Context _ (Type.Descriptor firstContent _ _ _) _ (Type.Descriptor secondContent _ _ _)) as context) =
  case firstContent of
    Type.FlexVar _ ->
      unifyFlex context firstContent secondContent

    Type.FlexSuper super _ ->
      unifyFlexSuper context super firstContent secondContent

    Type.RigidVar _ ->
      unifyRigid context Nothing firstContent secondContent

    Type.RigidSuper super _ ->
      unifyRigid context (Just super) firstContent secondContent

    Type.Alias home name args realVar ->
      unifyAlias context home name args realVar secondContent

    Type.Structure flatType ->
      unifyStructure context flatType firstContent secondContent

    Type.Error ->
      -- If there was an error, just pretend it is okay. This lets us avoid
      -- "cascading" errors where one problem manifests as multiple message.
      merge context Type.Error



-- UNIFY FLEXIBLE VARIABLES


unifyFlex : Context -> Type.Content -> Type.Content -> Unify t z ()
unifyFlex context content otherContent =
  case otherContent of
    Type.Error ->
      merge context Type.Error

    Type.FlexVar maybeName ->
      merge context <|
        case maybeName of
          Nothing ->
            content

          Just _ ->
            otherContent

    Type.FlexSuper _ _ ->
      merge context otherContent

    Type.RigidVar _ ->
      merge context otherContent

    Type.RigidSuper _ _ ->
      merge context otherContent

    Type.Alias _ _ _ _ ->
      merge context otherContent

    Type.Structure _ ->
      merge context otherContent



-- UNIFY RIGID VARIABLES


unifyRigid : Context -> Maybe Type.SuperType -> Type.Content -> Type.Content -> Unify t z ()
unifyRigid context maybeSuper content otherContent =
  case otherContent of
    Type.FlexVar _ ->
      merge context content

    Type.FlexSuper otherSuper _ ->
      case maybeSuper of
        Just super ->
          if combineRigidSupers super otherSuper then
            merge context content
          else
            mismatch

        Nothing ->
          mismatch

    Type.RigidVar _ ->
      mismatch

    Type.RigidSuper _ _ ->
      mismatch

    Type.Alias _ _ _ _ ->
      mismatch

    Type.Structure _ ->
      mismatch

    Type.Error ->
      merge context Type.Error



-- UNIFY SUPER VARIABLES


unifyFlexSuper : Context -> Type.SuperType -> Type.Content -> Type.Content -> Unify t z ()
unifyFlexSuper context super content otherContent =
  case otherContent of
    Type.Structure flatType ->
      unifyFlexSuperStructure context super flatType

    Type.RigidVar _ ->
      mismatch

    Type.RigidSuper otherSuper _ ->
      if combineRigidSupers otherSuper super then
        merge context otherContent
      else
        mismatch

    Type.FlexVar _ ->
      merge context content

    Type.FlexSuper otherSuper _ ->
      case super of
        Type.Number ->
          case otherSuper of
            Type.Number     -> merge context content
            Type.Comparable -> merge context content
            Type.Appendable -> mismatch
            Type.CompAppend -> mismatch

        Type.Comparable ->
          case otherSuper of
            Type.Comparable -> merge context otherContent
            Type.Number     -> merge context otherContent
            Type.Appendable -> merge context (Type.unnamedFlexSuper Type.CompAppend)
            Type.CompAppend -> merge context otherContent

        Type.Appendable ->
          case otherSuper of
            Type.Appendable -> merge context otherContent
            Type.Comparable -> merge context (Type.unnamedFlexSuper Type.CompAppend)
            Type.CompAppend -> merge context otherContent
            Type.Number     -> mismatch

        Type.CompAppend ->
          case otherSuper of
            Type.Comparable -> merge context content
            Type.Appendable -> merge context content
            Type.CompAppend -> merge context content
            Type.Number     -> mismatch

    Type.Alias _ _ _ realVar ->
      subUnify (getFirst context) realVar

    Type.Error ->
      merge context Type.Error


combineRigidSupers : Type.SuperType -> Type.SuperType -> Bool
combineRigidSupers rigid flex =
  rigid == flex
  || (rigid == Type.Number && flex == Type.Comparable)
  || (rigid == Type.CompAppend && (flex == Type.Comparable || flex == Type.Appendable))


atomMatchesSuper : Type.SuperType -> ModuleName.Canonical -> Name.Name -> Bool
atomMatchesSuper super home name =
  case super of
    Type.Number ->
      isNumber home name

    Type.Comparable ->
      isNumber home name
      || Error.isString home name
      || Error.isChar home name

    Type.Appendable ->
      Error.isString home name

    Type.CompAppend ->
      Error.isString home name


isNumber : ModuleName.Canonical -> Name.Name -> Bool
isNumber home name =
  home == ModuleName.basics
  &&
  (name == Name.int || name == Name.float)


unifyFlexSuperStructure : Context -> Type.SuperType -> Type.FlatType -> Unify t z ()
unifyFlexSuperStructure context super flatType =
  case flatType of
    Type.App1 home name [] ->
      if atomMatchesSuper super home name then
        merge context (Type.Structure flatType)
      else
        mismatch

    Type.App1 home name [variable] -> if home == ModuleName.list && name == Name.list then
      case super of
        Type.Number ->
          mismatch

        Type.Appendable ->
          merge context (Type.Structure flatType)

        Type.Comparable ->
          bind (comparableOccursCheck context) <| \() ->
          bind (unifyComparableRecursive variable) <| \() ->
          merge context (Type.Structure flatType)

        Type.CompAppend ->
          bind (comparableOccursCheck context) <| \() ->
          bind (unifyComparableRecursive variable) <| \() ->
          merge context (Type.Structure flatType)
      else mismatch

    Type.Tuple1 a b maybeC ->
      case super of
        Type.Number ->
          mismatch

        Type.Appendable ->
          mismatch

        Type.Comparable ->
          bind (comparableOccursCheck context) <| \() ->
          bind (unifyComparableRecursive a) <| \() ->
          bind (unifyComparableRecursive b) <| \() ->
          bind (case maybeC of
                  Nothing -> return ()
                  Just c  -> unifyComparableRecursive c
               ) <| \() ->
          merge context (Type.Structure flatType)

        Type.CompAppend ->
          mismatch

    _ ->
      mismatch


-- TODO: is there some way to avoid doing this?
-- Do type classes require occurs checks?
comparableOccursCheck : Context -> Unify t z ()
comparableOccursCheck (Context _ _ var _) =
  Unify <| \vars ok err ->
    IO.bind (Occurs.occurs var) <| \hasOccurred ->
    if hasOccurred
      then err vars ()
      else ok vars ()


unifyComparableRecursive : Type.Variable -> Unify t z ()
unifyComparableRecursive var =
  bind (register <|
    IO.bind (UF.get var) <| \(Type.Descriptor _ rank _ _) ->
    UF.fresh <| Type.Descriptor (Type.unnamedFlexSuper Type.Comparable) rank Type.noMark Nothing) <| \compVar ->
  guardedUnify compVar var



-- UNIFY ALIASES


unifyAlias : Context -> ModuleName.Canonical -> Name.Name -> (TList (Name.Name, Type.Variable)) -> Type.Variable -> Type.Content -> Unify t z ()
unifyAlias context home name args realVar otherContent =
  case otherContent of
    Type.FlexVar _ ->
      merge context (Type.Alias home name args realVar)

    Type.FlexSuper _ _ ->
      subUnify realVar (getSecond context)

    Type.RigidVar _ ->
      subUnify realVar (getSecond context)

    Type.RigidSuper _ _ ->
      subUnify realVar (getSecond context)

    Type.Alias otherHome otherName otherArgs otherRealVar ->
      if name == otherName && home == otherHome then
        Unify <| \vars ok err ->
          let
            ok1 vars1 () =
              case merge context otherContent of
                Unify k ->
                  k vars1 ok err
          in
          unifyAliasArgs vars args otherArgs ok1 err

      else
        subUnify realVar otherRealVar

    Type.Structure _ ->
      subUnify realVar (getSecond context)

    Type.Error ->
      merge context Type.Error


unifyAliasArgs : (TList Type.Variable) -> (TList (Name.Name,Type.Variable)) -> (TList (Name.Name,Type.Variable)) -> ((TList Type.Variable) -> () -> IO t z) -> ((TList Type.Variable) -> () -> IO t z) -> IO t z
unifyAliasArgs vars args1 args2 ok err =
  case args1 of
    (_,arg1)::others1 ->
      case args2 of
        (_,arg2)::others2 ->
          case subUnify arg1 arg2 of
            Unify k ->
              k vars
                (\vs () -> unifyAliasArgs vs others1 others2 ok err)
                (\vs () -> unifyAliasArgs vs others1 others2 err err)

        _ ->
          err vars ()

    [] ->
      case args2 of
        [] ->
          ok vars ()

        _ ->
          err vars ()



-- UNIFY STRUCTURES


unifyStructure : Context -> Type.FlatType -> Type.Content -> Type.Content -> Unify t z ()
unifyStructure context flatType content otherContent =
  case otherContent of
    Type.FlexVar _ ->
      merge context content

    Type.FlexSuper super _ ->
      unifyFlexSuperStructure (reorient context) super flatType

    Type.RigidVar _ ->
      mismatch

    Type.RigidSuper _ _ ->
      mismatch

    Type.Alias _ _ _ realVar ->
      subUnify (getFirst context) realVar

    Type.Structure otherFlatType ->
      case (flatType, otherFlatType) of
        (Type.App1 home name args, Type.App1 otherHome otherName otherArgs) -> if home == otherHome && name == otherName then
          Unify <| \vars ok err ->
            let
              ok1 vars1 () =
                case merge context otherContent of
                  Unify k ->
                    k vars1 ok err
            in
            unifyArgs vars args otherArgs ok1 err
          else mismatch

        (Type.Fun1 arg1 res1, Type.Fun1 arg2 res2) ->
          bind (subUnify arg1 arg2) <| \() ->
          bind (subUnify res1 res2) <| \() ->
          merge context otherContent

        (Type.EmptyRecord1, Type.EmptyRecord1) ->
          merge context otherContent

        (Type.Record1 fields ext, Type.EmptyRecord1) -> if Map.null fields then
          subUnify ext (getSecond context)
          else mismatch

        (Type.EmptyRecord1, Type.Record1 fields ext)-> if Map.null fields then
          subUnify (getFirst context) ext
          else mismatch

        (Type.Record1 fields1 ext1, Type.Record1 fields2 ext2) ->
          Unify <| \vars ok err ->
            IO.bind (gatherFields fields1 ext1) <| \structure1 ->
            IO.bind (gatherFields fields2 ext2) <| \structure2 ->
            case unifyRecord context structure1 structure2 of
              Unify k ->
                k vars ok err

        (Type.Tuple1 a b Nothing, Type.Tuple1 x y Nothing) ->
          bind (subUnify a x) <| \() ->
          bind (subUnify b y) <| \() ->
          merge context otherContent

        (Type.Tuple1 a b (Just c), Type.Tuple1 x y (Just z)) ->
          bind (subUnify a x) <| \() ->
          bind (subUnify b y) <| \() ->
          bind (subUnify c z) <| \() ->
          merge context otherContent

        (Type.Unit1, Type.Unit1) ->
          merge context otherContent

        _ ->
          mismatch

    Type.Error ->
      merge context Type.Error



-- UNIFY ARGS


unifyArgs : (TList Type.Variable) -> (TList Type.Variable) -> (TList Type.Variable) -> ((TList Type.Variable) -> () -> IO t z) -> ((TList Type.Variable) -> () -> IO t z) -> IO t z
unifyArgs vars args1 args2 ok err =
  case args1 of
    arg1::others1 ->
      case args2 of
        arg2::others2 ->
          case subUnify arg1 arg2 of
            Unify k ->
              k vars
                (\vs () -> unifyArgs vs others1 others2 ok err)
                (\vs () -> unifyArgs vs others1 others2 err err)

        _ ->
          err vars ()

    [] ->
      case args2 of
        [] ->
          ok vars ()

        _ ->
          err vars ()



-- UNIFY RECORDS


unifyRecord : Context -> RecordStructure -> RecordStructure -> Unify t z ()
unifyRecord context (RecordStructure fields1 ext1) (RecordStructure fields2 ext2) =
  let
    sharedFields = Map.intersectionWith Tuple.pair fields1 fields2
    uniqueFields1 = Map.difference fields1 fields2
    uniqueFields2 = Map.difference fields2 fields1
  in
  if Map.null uniqueFields1 then

    if Map.null uniqueFields2 then
      bind (subUnify ext1 ext2) <| \() ->
      unifySharedFields context sharedFields Map.empty ext1

    else
      bind (fresh context (Type.Structure (Type.Record1 uniqueFields2 ext2))) <| \subRecord ->
      bind (subUnify ext1 subRecord) <| \() ->
      unifySharedFields context sharedFields Map.empty subRecord

  else

    if Map.null uniqueFields2 then
      bind (fresh context (Type.Structure (Type.Record1 uniqueFields1 ext1))) <| \subRecord ->
      bind (subUnify subRecord ext2) <| \() ->
      unifySharedFields context sharedFields Map.empty subRecord

    else
      let otherFields = Map.union uniqueFields1 uniqueFields2 in
      bind (fresh context Type.unnamedFlexVar) <| \ext ->
      bind (fresh context (Type.Structure (Type.Record1 uniqueFields1 ext))) <| \sub1 ->
      bind (fresh context (Type.Structure (Type.Record1 uniqueFields2 ext))) <| \sub2 ->
      bind (subUnify ext1 sub2) <| \() ->
      bind (subUnify sub1 ext2) <| \() ->
      unifySharedFields context sharedFields otherFields ext


unifySharedFields : Context -> Map.Map Name.Name (Type.Variable, Type.Variable) -> Map.Map Name.Name Type.Variable -> Type.Variable -> Unify t z ()
unifySharedFields context sharedFields otherFields ext =
  bind (Map.traverseMaybeWithKey return liftA2 unifyField sharedFields) <| \matchingFields ->
  if Map.size sharedFields == Map.size matchingFields
    then merge context (Type.Structure (Type.Record1 (Map.union matchingFields otherFields) ext))
    else mismatch


unifyField : Name.Name -> (Type.Variable, Type.Variable) -> Unify t z (Maybe Type.Variable)
unifyField _ (actual, expected) =
  Unify <| \vars ok _ ->
    case subUnify actual expected of
      Unify k ->
        k vars
          (\vs () -> ok vs (Just actual))
          (\vs () -> ok vs Nothing)



-- GATHER RECORD STRUCTURE


type RecordStructure =
  RecordStructure
    {- fields -} (Map.Map Name.Name Type.Variable)
    {- extension -} Type.Variable


gatherFields : Map.Map Name.Name Type.Variable -> Type.Variable -> IO t RecordStructure
gatherFields fields variable =
  IO.bind (UF.get variable) <| \(Type.Descriptor content _ _ _) ->
  case content of
    Type.Structure (Type.Record1 subFields subExt) ->
      gatherFields (Map.union fields subFields) subExt

    Type.Alias _ _ _ var ->
      -- TODO may be dropping useful alias info here
      gatherFields fields var

    _ ->
      IO.return (RecordStructure fields variable)
