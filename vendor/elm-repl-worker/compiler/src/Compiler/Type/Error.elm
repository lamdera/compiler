{- MANUALLY FORMATTED -}
module Compiler.Type.Error exposing
  ( Type(..)
  , Super(..)
  , Extension(..)
  , iteratedDealias
  , toDoc
  , Problem(..)
  , Direction(..)
  , toComparison
  , isInt
  , isFloat
  , isString
  , isChar
  , isList
  )


import Compiler.Elm.ModuleName as ModuleName
import Compiler.Data.Bag as Bag
import Compiler.Data.Name as Name
import Compiler.Reporting.Doc as D exposing (d, da)
import Compiler.Reporting.Render.Type as RT
import Compiler.Reporting.Render.Type.Localizer as L
import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe



-- ERROR TYPES


type Type
  = Lambda Type Type (TList Type)
  | Infinite
  | Error
  | FlexVar Name.Name
  | FlexSuper Super Name.Name
  | RigidVar Name.Name
  | RigidSuper Super Name.Name
  | Type ModuleName.Canonical Name.Name (TList Type)
  | Record (Map.Map Name.Name Type) Extension
  | Unit
  | Tuple Type Type (Maybe Type)
  | Alias ModuleName.Canonical Name.Name (TList (Name.Name, Type)) Type


type Super
  = Number
  | Comparable
  | Appendable
  | CompAppend


type Extension
  = Closed
  | FlexOpen Name.Name
  | RigidOpen Name.Name


iteratedDealias : Type -> Type
iteratedDealias tipe =
  case tipe of
    Alias _ _ _ real ->
      iteratedDealias real

    _ ->
      tipe



-- TO DOC


toDoc : L.Localizer -> RT.Context -> Type -> D.Doc
toDoc localizer ctx tipe =
  case tipe of
    Lambda a b cs ->
      RT.lambda ctx
        (toDoc localizer RT.Func a)
        (toDoc localizer RT.Func b)
        (MList.map (toDoc localizer RT.Func) cs)

    Infinite ->
      D.fromChars "∞"

    Error ->
      D.fromChars "?"

    FlexVar name ->
      D.fromName name

    FlexSuper _ name ->
      D.fromName name

    RigidVar name ->
      D.fromName name

    RigidSuper _ name ->
      D.fromName name

    Type home name args ->
      RT.apply ctx
        (L.toDoc localizer home name)
        (MList.map (toDoc localizer RT.App) args)

    Record fields ext ->
      RT.record (fieldsToDocs localizer fields) (extToDoc ext)

    Unit ->
      D.fromChars "()"

    Tuple a b maybeC ->
      RT.tuple
        (toDoc localizer RT.None a)
        (toDoc localizer RT.None b)
        (MList.map (toDoc localizer RT.None) (MMaybe.maybeToList maybeC))

    Alias home name args _ ->
      aliasToDoc localizer ctx home name args


aliasToDoc : L.Localizer -> RT.Context -> ModuleName.Canonical -> Name.Name -> TList (Name.Name, Type) -> D.Doc
aliasToDoc localizer ctx home name args =
  RT.apply ctx
    (L.toDoc localizer home name)
    (MList.map (toDoc localizer RT.App << Tuple.second) args)


fieldsToDocs : L.Localizer -> Map.Map Name.Name Type -> TList (D.Doc, D.Doc)
fieldsToDocs localizer fields =
  Map.foldrWithKey (addField localizer) [] fields


addField : L.Localizer -> Name.Name -> Type -> TList (D.Doc, D.Doc) -> TList (D.Doc, D.Doc)
addField localizer fieldName fieldType docs =
  let
    f = D.fromName fieldName
    t = toDoc localizer RT.None fieldType
  in
  (f,t) :: docs


extToDoc : Extension -> Maybe D.Doc
extToDoc ext =
  case ext of
    Closed -> Nothing
    FlexOpen x -> Just (D.fromName x)
    RigidOpen x -> Just (D.fromName x)



-- DIFF


type Diff a =
  Diff a a Status


type Status
  = Similar
  | Different (Bag.Bag Problem)


type Problem
  = IntFloat
  | StringFromInt
  | StringFromFloat
  | StringToInt
  | StringToFloat
  | AnythingToBool
  | AnythingFromMaybe
  | ArityMismatch Int Int
  | BadFlexSuper Direction Super Type
  | BadRigidVar Name.Name Type
  | BadRigidSuper Super Name.Name Type
  | FieldTypo Name.Name (TList Name.Name)
  | FieldsMissing (TList Name.Name)


type Direction = Have | Need


fmap : Functor.Fmap a (Diff a) b (Diff b)
fmap func (Diff a b status) =
  Diff (func a) (func b) status


pure : Applicative.Pure a (Diff a)
pure a =
  Diff a a Similar


andMap : Applicative.AndMap (Diff a) (Diff (a -> b)) (Diff b)
andMap (Diff aArg bArg status2) (Diff aFunc bFunc status1) =
  Diff (aFunc aArg) (bFunc bArg) (merge status1 status2)


liftA2 : Applicative.LiftA2 a (Diff a) b (Diff b) c (Diff c)
liftA2 =
  Applicative.liftA2 fmap andMap


merge : Status -> Status -> Status
merge status1 status2 =
  case status1 of
    Similar ->
      status2

    Different problems1 ->
      case status2 of
        Similar ->
          status1

        Different problems2 ->
          Different (Bag.append problems1 problems2)



-- COMPARISON


toComparison : L.Localizer -> Type -> Type -> (D.Doc, D.Doc, TList Problem)
toComparison localizer tipe1 tipe2 =
  case toDiff localizer RT.None tipe1 tipe2 of
    Diff doc1 doc2 Similar ->
      (doc1, doc2, [])

    Diff doc1 doc2 (Different problems) ->
      (doc1, doc2, Bag.toList problems)


toDiff : L.Localizer -> RT.Context -> Type -> Type -> Diff D.Doc
toDiff localizer ctx tipe1 tipe2 =
  case (tipe1, tipe2) of
    (Unit    , Unit    ) -> same localizer ctx tipe1
    (Error   , Error   ) -> same localizer ctx tipe1
    (Infinite, Infinite) -> same localizer ctx tipe1

    (FlexVar      x, FlexVar      y) -> if x == y then same localizer ctx tipe1 else toDiff2 localizer ctx tipe1 tipe2
    (FlexSuper _  x, FlexSuper _  y) -> if x == y then same localizer ctx tipe1 else toDiff2 localizer ctx tipe1 tipe2
    (RigidVar     x, RigidVar     y) -> if x == y then same localizer ctx tipe1 else toDiff2 localizer ctx tipe1 tipe2
    (RigidSuper _ x, RigidSuper _ y) -> if x == y then same localizer ctx tipe1 else toDiff2 localizer ctx tipe1 tipe2
    _ -> toDiff2 localizer ctx tipe1 tipe2


toDiff2 : L.Localizer -> RT.Context -> Type -> Type -> Diff D.Doc
toDiff2 localizer ctx tipe1 tipe2 =
  case (tipe1, tipe2) of
    (FlexVar _, _        ) -> similar localizer ctx tipe1 tipe2
    (_        , FlexVar _) -> similar localizer ctx tipe1 tipe2

    (FlexSuper s _, t            ) -> if isSuper s t then similar localizer ctx tipe1 tipe2 else toDiff3 localizer ctx tipe1 tipe2
    _ -> toDiff3 localizer ctx tipe1 tipe2


toDiff3 : L.Localizer -> RT.Context -> Type -> Type -> Diff D.Doc
toDiff3 localizer ctx tipe1 tipe2 =
  case (tipe1, tipe2) of
    (t            , FlexSuper s _) -> if isSuper s t then similar localizer ctx tipe1 tipe2 else toDiff4 localizer ctx tipe1 tipe2

    (Lambda a b cs, Lambda x y zs) ->
      if MList.length cs == MList.length zs then
        pure (RT.lambda ctx)
          |> andMap (toDiff localizer RT.Func a x)
          |> andMap (toDiff localizer RT.Func b y)
          |> andMap (MList.sequenceA pure liftA2 (MList.zipWith (toDiff localizer RT.Func) cs zs))
      else
        let f = toDoc localizer RT.Func in
        different
          (D.dullyellow (RT.lambda ctx (f a) (f b) (MList.map f cs)))
          (D.dullyellow (RT.lambda ctx (f x) (f y) (MList.map f zs)))
          (Bag.one (ArityMismatch (2 + MList.length cs) (2 + MList.length zs)))

    (Tuple a b Nothing, Tuple x y Nothing) ->
      pure RT.tuple
        |> andMap (toDiff localizer RT.None a x)
        |> andMap (toDiff localizer RT.None b y)
        |> andMap (pure [])

    (Tuple a b (Just c), Tuple x y (Just z)) ->
      pure RT.tuple
        |> andMap (toDiff localizer RT.None a x)
        |> andMap (toDiff localizer RT.None b y)
        |> andMap (fmap (\d -> [d]) <| toDiff localizer RT.None c z)

    (Record fields1 ext1, Record fields2 ext2) ->
      diffRecord localizer fields1 ext1 fields2 ext2

    (Type home1 name1 args1, Type home2 name2 args2) -> if home1 == home2 && name1 == name2 then
      fmap (RT.apply ctx (L.toDoc localizer home1 name1))
        <| MList.sequenceA pure liftA2 (MList.zipWith (toDiff localizer RT.App) args1 args2) else toDiff4 localizer ctx tipe1 tipe2

    (Alias home1 name1 args1 _, Alias home2 name2 args2 _) -> if home1 == home2 && name1 == name2 then
      fmap (RT.apply ctx (L.toDoc localizer home1 name1))
        <| MList.sequenceA pure liftA2 (MList.zipWith (toDiff localizer RT.App) (MList.map Tuple.second args1) (MList.map Tuple.second args2)) else toDiff4 localizer ctx tipe1 tipe2
    _ -> toDiff4 localizer ctx tipe1 tipe2


toDiff4 : L.Localizer -> RT.Context -> Type -> Type -> Diff D.Doc
toDiff4 localizer ctx tipe1 tipe2 =
  case (tipe1, tipe2) of

    -- start trying to find specific problems

    (Type home1 name1 args1, Type home2 name2 args2) -> if L.toChars localizer home1 name1 == L.toChars localizer home2 name2 then
      different
        (nameClashToDoc ctx localizer home1 name1 args1)
        (nameClashToDoc ctx localizer home2 name2 args2)
        Bag.empty else toDiff5 localizer ctx tipe1 tipe2
    _ -> toDiff5 localizer ctx tipe1 tipe2


toDiff5 : L.Localizer -> RT.Context -> Type -> Type -> Diff D.Doc
toDiff5 localizer ctx tipe1 tipe2 =
  case (tipe1, tipe2) of
    (Type home name [t1], t2) -> if isMaybe home name && isSimilar (toDiff localizer ctx t1 t2) then
      different
        (RT.apply ctx (D.dullyellow (L.toDoc localizer home name)) [toDoc localizer RT.App t1])
        (toDoc localizer ctx t2)
        (Bag.one AnythingFromMaybe) else toDiff6 localizer ctx tipe1 tipe2
    _ -> toDiff6 localizer ctx tipe1 tipe2


toDiff6 : L.Localizer -> RT.Context -> Type -> Type -> Diff D.Doc
toDiff6 localizer ctx tipe1 tipe2 =
  case (tipe1, tipe2) of
    (t1, Type home name [t2]) -> if isList home name && isSimilar (toDiff localizer ctx t1 t2) then
      different
        (toDoc localizer ctx t1)
        (RT.apply ctx (D.dullyellow (L.toDoc localizer home name)) [toDoc localizer RT.App t2])
        Bag.empty else toDiff7 localizer ctx tipe1 tipe2
    _ -> toDiff7 localizer ctx tipe1 tipe2


toDiff7 : L.Localizer -> RT.Context -> Type -> Type -> Diff D.Doc
toDiff7 localizer ctx tipe1 tipe2 =
  case (tipe1, tipe2) of
    (Alias home1 name1 args1 t1, t2) ->
      case diffAliasedRecord localizer t1 t2 of
        Just (Diff _ doc2 status) ->
          Diff (D.dullyellow (aliasToDoc localizer ctx home1 name1 args1)) doc2 status

        Nothing ->
          let
            otherwise () =
              different
                (D.dullyellow (toDoc localizer ctx tipe1))
                (D.dullyellow (toDoc localizer ctx tipe2))
                Bag.empty
          in
          case t2 of
            Type home2 name2 args2 -> if L.toChars localizer home1 name1 == L.toChars localizer home2 name2 then
              different
                (nameClashToDoc ctx localizer home1 name1 (MList.map Tuple.second args1))
                (nameClashToDoc ctx localizer home2 name2 args2)
                Bag.empty else otherwise ()

            _ ->
              otherwise ()

    (t1, Alias home2 name2 args2 t2) ->
      case diffAliasedRecord localizer t1 t2 of
        Just (Diff doc1 _ status) ->
          Diff doc1 (D.dullyellow (aliasToDoc localizer ctx home2 name2 args2)) status

        Nothing ->
          let
            otherwise () =
              different
                (D.dullyellow (toDoc localizer ctx tipe1))
                (D.dullyellow (toDoc localizer ctx tipe2))
                Bag.empty
          in
          case t1 of
            Type home1 name1 args1 -> if L.toChars localizer home1 name1 == L.toChars localizer home2 name2 then
              different
                (nameClashToDoc ctx localizer home1 name1 args1)
                (nameClashToDoc ctx localizer home2 name2 (MList.map Tuple.second args2))
                Bag.empty else otherwise ()

            _ ->
              otherwise ()

    pair ->
      let
        doc1 = D.dullyellow (toDoc localizer ctx tipe1)
        doc2 = D.dullyellow (toDoc localizer ctx tipe2)
      in
      different doc1 doc2 <|
        case pair of
          (RigidVar     x, other) -> Bag.one <| BadRigidVar x other
          (FlexSuper  s _, other) -> Bag.one <| BadFlexSuper Have s other
          (RigidSuper s x, other) -> Bag.one <| BadRigidSuper s x other
          (other, RigidVar     x) -> Bag.one <| BadRigidVar x other
          (other, FlexSuper  s _) -> Bag.one <| BadFlexSuper Need s other
          (other, RigidSuper s x) -> Bag.one <| BadRigidSuper s x other

          (Type home1 name1 [], Type home2 name2 []) ->
            if isInt   home1 name1 && isFloat  home2 name2 then Bag.one IntFloat
            else if isFloat home1 name1 && isInt    home2 name2 then Bag.one IntFloat
            else if isInt   home1 name1 && isString home2 name2 then Bag.one StringFromInt
            else if isFloat home1 name1 && isString home2 name2 then Bag.one StringFromFloat
            else if isString home1 name1 && isInt   home2 name2 then Bag.one StringToInt
            else if isString home1 name1 && isFloat home2 name2 then Bag.one StringToFloat
            else if isBool home2 name2 then Bag.one AnythingToBool else Bag.empty

          _ ->
            Bag.empty



-- DIFF HELPERS


same : L.Localizer -> RT.Context -> Type -> Diff D.Doc
same localizer ctx tipe =
  let
    doc = toDoc localizer ctx tipe
  in
  Diff doc doc Similar


similar : L.Localizer -> RT.Context -> Type -> Type -> Diff D.Doc
similar localizer ctx t1 t2 =
  Diff (toDoc localizer ctx t1) (toDoc localizer ctx t2) Similar


different : a -> a -> Bag.Bag Problem -> Diff a
different a b problems =
  Diff a b (Different problems)


isSimilar : Diff a -> Bool
isSimilar (Diff _ _ status) =
  case status of
    Similar -> True
    Different _ -> False



-- IS TYPE?


isBool : ModuleName.Canonical -> Name.Name -> Bool
isBool home name =
  home == ModuleName.basics && name == Name.bool


isInt : ModuleName.Canonical -> Name.Name -> Bool
isInt home name =
  home == ModuleName.basics && name == Name.int


isFloat : ModuleName.Canonical -> Name.Name -> Bool
isFloat home name =
  home == ModuleName.basics && name == Name.float


isString : ModuleName.Canonical -> Name.Name -> Bool
isString home name =
  home == ModuleName.string && name == Name.string


isChar : ModuleName.Canonical -> Name.Name -> Bool
isChar home name =
  home == ModuleName.char && name == Name.char


isMaybe : ModuleName.Canonical -> Name.Name -> Bool
isMaybe home name =
  home == ModuleName.maybe && name == Name.maybe


isList : ModuleName.Canonical -> Name.Name -> Bool
isList home name =
  home == ModuleName.list && name == Name.list



-- IS SUPER?


isSuper : Super -> Type -> Bool
isSuper super tipe =
  case iteratedDealias tipe of
    Type h n args ->
      case super of
        Number     -> isInt h n || isFloat h n
        Comparable -> isInt h n || isFloat h n || isString h n || isChar h n || isList h n && isSuper super (MList.head args)
        Appendable -> isString h n || isList h n
        CompAppend -> isString h n || isList h n && isSuper Comparable (MList.head args)

    Tuple a b maybeC ->
      case super of
        Number     -> False
        Comparable -> isSuper super a && isSuper super b && MMaybe.maybe True (isSuper super) maybeC
        Appendable -> False
        CompAppend -> False

    _ ->
      False



-- NAME CLASH


nameClashToDoc : RT.Context -> L.Localizer -> ModuleName.Canonical -> Name.Name -> TList Type -> D.Doc
nameClashToDoc ctx localizer (ModuleName.Canonical _ home) name args =
  RT.apply ctx
    (da[D.yellow (D.fromName home), D.dullyellow (da[d".", D.fromName name])])
    (MList.map (toDoc localizer RT.App) args)



-- DIFF ALIASED RECORD


diffAliasedRecord : L.Localizer -> Type -> Type -> Maybe (Diff D.Doc)
diffAliasedRecord localizer t1 t2 =
  case (iteratedDealias t1, iteratedDealias t2) of
    (Record fields1 ext1, Record fields2 ext2) ->
      Just (diffRecord localizer fields1 ext1 fields2 ext2)

    _ ->
      Nothing



-- RECORD DIFFS


diffRecord : L.Localizer -> Map.Map Name.Name Type -> Extension -> Map.Map Name.Name Type -> Extension -> Diff D.Doc
diffRecord localizer fields1 ext1 fields2 ext2 =
  let
    toUnknownDocs field tipe =
      ( D.dullyellow (D.fromName field), toDoc localizer RT.None tipe )

    toOverlapDocs field t1 t2 =
      fmap (Tuple.pair (D.fromName field)) <| toDiff localizer RT.None t1 t2

    left = Map.mapWithKey toUnknownDocs (Map.difference fields1 fields2)
    both = Map.intersectionWithKey toOverlapDocs fields1 fields2
    right = Map.mapWithKey toUnknownDocs (Map.difference fields2 fields1)

    fieldsDiff =
      fmap Map.elems <|
        if Map.null left && Map.null right then
          Map.sequenceA pure liftA2 both
        else
          pure Map.union
            |> andMap (Map.sequenceA pure liftA2 both)
            |> andMap (Diff left right (Different Bag.empty))

    (Diff doc1 doc2 status) =
      pure RT.record
        |> andMap fieldsDiff
        |> andMap (extToDiff ext1 ext2)
  in
  Diff doc1 doc2 <| merge status <|
    case (hasFixedFields ext1, hasFixedFields ext2) of
      (True, True) ->
        case Map.lookupMin left of
          Just (f,_) -> Different <| Bag.one <| FieldTypo f (Map.keys fields2)
          Nothing ->
            if Map.null right
              then Similar
              else Different <| Bag.one <| FieldsMissing (Map.keys right)

      (False, True) ->
        case Map.lookupMin left of
          Just (f,_) -> Different <| Bag.one <| FieldTypo f (Map.keys fields2)
          Nothing    -> Similar

      (True, False) ->
        case Map.lookupMin right of
          Just (f,_) -> Different <| Bag.one <| FieldTypo f (Map.keys fields1)
          Nothing    -> Similar

      (False, False) ->
        Similar


hasFixedFields : Extension -> Bool
hasFixedFields ext =
  case ext of
    Closed      -> True
    FlexOpen _  -> False
    RigidOpen _ -> True



-- DIFF RECORD EXTENSION


extToDiff : Extension -> Extension -> Diff (Maybe D.Doc)
extToDiff ext1 ext2 =
  let
    status = extToStatus ext1 ext2
    extDoc1 = extToDoc ext1
    extDoc2 = extToDoc ext2
  in
  case status of
    Similar ->
      Diff extDoc1 extDoc2 status

    Different _ ->
      Diff (Maybe.map D.dullyellow extDoc1) (Maybe.map D.dullyellow extDoc2) status


extToStatus : Extension -> Extension -> Status
extToStatus ext1 ext2 =
  case ext1 of
    Closed ->
      case ext2 of
        Closed      -> Similar
        FlexOpen  _ -> Similar
        RigidOpen _ -> Different Bag.empty

    FlexOpen _ ->
      Similar

    RigidOpen x ->
      case ext2 of
        Closed      -> Different Bag.empty
        FlexOpen  _ -> Similar
        RigidOpen y ->
          if x == y
            then Similar
            else Different <| Bag.one <| BadRigidVar x (RigidVar y)
