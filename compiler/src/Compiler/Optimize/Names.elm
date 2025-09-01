{- MANUALLY FORMATTED -}
module Compiler.Optimize.Names exposing
  ( Tracker
  , run
  , generate
  , registerKernel
  , registerGlobal
  , registerDebug
  , registerCtor
  , registerField
  , registerFieldDict
  , registerFieldList
  , fmap, pure, andMap, liftA2, return, bind, andThen
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Extra.Class.Applicative as Applicative
import Extra.Class.Functor as Functor
import Extra.Class.Monad as Monad
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Set as Set
import Compiler.AST.Optimized exposing (toGlobalComparable)



-- GENERATOR


type Tracker a =
  Tracker (
    Int
    -> Set.Set Opt.GlobalComparable
    -> Map.Map Name.Name Int
    -> TStep a
  )


type TStep a =
  Cok Int (Set.Set Opt.GlobalComparable) (Map.Map Name.Name Int) a


run : Tracker a -> (Set.Set Opt.GlobalComparable, Map.Map Name.Name Int, a)
run (Tracker k) =
  case k 0 Set.empty Map.empty of
    Cok _ deps fields value -> (deps, fields, value)


generate : Tracker Name.Name
generate =
  Tracker <| \uid deps fields ->
    Cok (uid + 1) deps fields (Name.fromVarIndex uid)


registerKernel : Name.Name -> a -> Tracker a
registerKernel home value =
  Tracker <| \uid deps fields ->
    Cok uid (Set.insert (Opt.toGlobalComparable <| Opt.toKernelGlobal home) deps) fields value


registerGlobal : ModuleName.Canonical -> Name.Name -> Tracker Opt.Expr
registerGlobal home name =
  Tracker <| \uid deps fields ->
    let global = Opt.Global home name in
    Cok uid (Set.insert (toGlobalComparable global) deps) fields (Opt.VarGlobal global)


registerDebug : Name.Name -> ModuleName.Canonical -> A.Region -> Tracker Opt.Expr
registerDebug name home region =
  Tracker <| \uid deps fields ->
    let global = Opt.Global ModuleName.debug name in
    Cok uid (Set.insert (toGlobalComparable global) deps) fields (Opt.VarDebug name home region Nothing)


registerCtor : ModuleName.Canonical -> Name.Name -> Index.ZeroBased -> Can.CtorOpts -> Tracker Opt.Expr
registerCtor home name index opts =
  Tracker <| \uid deps fields ->
    let
      global = Opt.Global home name
      newDeps = Set.insert (toGlobalComparable global) deps
    in
    case opts of
      Can.Normal ->
        Cok uid newDeps fields (Opt.VarGlobal global)

      Can.Enum ->
        Cok uid newDeps fields <|
          let
            otherwise () = Opt.VarEnum global index
          in
          case name of
            "True"  -> if home == ModuleName.basics then Opt.CBool True  else otherwise ()
            "False" -> if home == ModuleName.basics then Opt.CBool False else otherwise ()
            _ -> otherwise ()

      Can.Unbox ->
        Cok uid (Set.insert (toGlobalComparable identity_) newDeps) fields (Opt.VarBox global)


identity_ : Opt.Global
identity_ =
  Opt.Global ModuleName.basics Name.identity_


registerField : Name.Name -> a -> Tracker a
registerField name value =
  Tracker <| \uid d fields ->
    Cok uid d (Map.insertWith (+) name 1 fields) value


registerFieldDict : Map.Map Name.Name v -> a -> Tracker a
registerFieldDict newFields value =
  Tracker <| \uid d fields ->
    Cok uid d (Map.unionWith (+) fields (Map.map toOne newFields)) value


toOne : a -> Int
toOne _ = 1


registerFieldList : TList Name.Name -> a -> Tracker a
registerFieldList names value =
  Tracker <| \uid deps fields ->
    Cok uid deps (MList.foldr addOne fields names) value


addOne : Name.Name -> Map.Map Name.Name Int -> Map.Map Name.Name Int
addOne name fields =
  Map.insertWith (+) name 1 fields



-- INSTANCES


fmap : Functor.Fmap a (Tracker a) b (Tracker b)
fmap func (Tracker kv) =
  Tracker <| \n d f ->
    case kv n d f of
      Cok n1 d1 f1 value ->
        Cok n1 d1 f1 (func value)


pure : Applicative.Pure a (Tracker a)
pure value =
  Tracker <| \n d f -> Cok n d f value


andMap : Applicative.AndMap (Tracker a) (Tracker (a -> b)) (Tracker b)
andMap (Tracker kv) (Tracker kf) =
  Tracker <| \n d f ->
    case kf n d f of
      Cok n1 d1 f1 func ->
        case kv n1 d1 f1 of
          Cok n2 d2 f2 value ->
            Cok n2 d2 f2 (func value)


liftA2 : Applicative.LiftA2 a (Tracker a) b (Tracker b) c (Tracker c)
liftA2 =
  Applicative.liftA2 fmap andMap


return : Monad.Return a (Tracker a)
return =
  pure


bind : Monad.Bind a (Tracker a) (Tracker b)
bind (Tracker k) callback =
  Tracker <| \n d f ->
    case k n d f of
      Cok n1 d1 f1 a ->
        case callback a of
          Tracker kb -> kb n1 d1 f1


andThen : Monad.AndThen a (Tracker a) (Tracker b)
andThen =
  Monad.andThen bind
