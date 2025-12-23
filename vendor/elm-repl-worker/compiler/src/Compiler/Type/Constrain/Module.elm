{- MANUALLY FORMATTED -}
module Compiler.Type.Constrain.Module exposing
  ( constrain
  )


import Compiler.AST.Canonical as Can
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Data.Name as Name
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Type as E
import Compiler.Type.Constrain.Expression as Expr
import Compiler.Type.Instantiate as Instantiate
import Compiler.Type.Type as Type
import Extra.System.IO.Pure as IO
import Extra.Type.Map as Map



-- IO


type alias IO t a = Type.IO t a



-- CONSTRAIN


constrain : Can.Module -> IO t Type.Constraint
constrain (Can.Module home _ decls _ _ _ effects) =
  case effects of
    Can.NoEffects ->
      constrainDecls decls Type.CSaveTheEnvironment

    Can.Ports ports ->
      Map.foldrWithKey letPort (constrainDecls decls Type.CSaveTheEnvironment) ports

    Can.Manager r0 r1 r2 manager ->
      case manager of
        Can.CCmd cmdName ->
          IO.andThen (letCmd home cmdName) <|
            IO.andThen (constrainDecls decls) <| constrainEffects home r0 r1 r2 manager

        Can.CSub subName ->
          IO.andThen (letSub home subName) <|
            IO.andThen (constrainDecls decls) <| constrainEffects home r0 r1 r2 manager

        Can.Fx cmdName subName ->
          IO.andThen (letCmd home cmdName) <|
          IO.andThen (letSub home subName) <|
            IO.andThen (constrainDecls decls) <| constrainEffects home r0 r1 r2 manager



-- CONSTRAIN DECLARATIONS


constrainDecls : Can.Decls -> Type.Constraint -> IO t Type.Constraint
constrainDecls decls finalConstraint =
  constrainDeclsHelp decls finalConstraint identity


constrainDeclsHelp : Can.Decls -> Type.Constraint -> (IO t Type.Constraint -> IO t Type.Constraint) -> IO t Type.Constraint
constrainDeclsHelp decls finalConstraint cont =
  case decls of
    Can.Declare def otherDecls ->
      constrainDeclsHelp otherDecls finalConstraint (IO.andThen (Expr.constrainDef Map.empty def) >> cont)

    Can.DeclareRec def defs otherDecls ->
      constrainDeclsHelp otherDecls finalConstraint (IO.andThen (Expr.constrainRecursiveDefs Map.empty (def::defs)) >> cont)

    Can.SaveTheEnvironment ->
      cont (IO.return finalConstraint)



-- PORT HELPERS


letPort : Name.Name -> Can.Port -> IO t Type.Constraint -> IO t Type.Constraint
letPort name port_ makeConstraint =
  case port_ of
    Can.Incoming freeVars _ srcType ->
      IO.bind (IO.traverseWithKey (\k _ -> Type.nameToRigid k) freeVars) <| \vars ->
      IO.bind (Instantiate.fromSrcType (Map.map Type.VarN vars) srcType) <| \tipe ->
      let header = Map.singleton name (A.At A.zero tipe) in
      IO.fmap (Type.CLet (Map.elems vars) [] header Type.CTrue) <| makeConstraint

    Can.Outgoing freeVars _ srcType ->
      IO.bind (IO.traverseWithKey (\k _ -> Type.nameToRigid k) freeVars) <| \vars ->
      IO.bind (Instantiate.fromSrcType (Map.map Type.VarN vars) srcType) <| \tipe ->
      let header = Map.singleton name (A.At A.zero tipe) in
      IO.fmap (Type.CLet (Map.elems vars) [] header Type.CTrue) <| makeConstraint



-- EFFECT MANAGER HELPERS


letCmd : ModuleName.Canonical -> Name.Name -> Type.Constraint -> IO t Type.Constraint
letCmd home tipe constraint =
  IO.bind Type.mkFlexVar <| \msgVar ->
  let msg = Type.VarN msgVar
      cmdType = Type.FunN (Type.AppN home tipe [msg]) (Type.AppN ModuleName.cmd Name.cmd [msg])
      header = Map.singleton "command" (A.At A.zero cmdType) in
  IO.return <| Type.CLet [msgVar] [] header Type.CTrue constraint


letSub : ModuleName.Canonical -> Name.Name -> Type.Constraint -> IO t Type.Constraint
letSub home tipe constraint =
  IO.bind Type.mkFlexVar <| \msgVar ->
  let msg = Type.VarN msgVar
      subType = Type.FunN (Type.AppN home tipe [msg]) (Type.AppN ModuleName.sub Name.sub [msg])
      header = Map.singleton "subscription" (A.At A.zero subType) in
  IO.return <| Type.CLet [msgVar] [] header Type.CTrue constraint


constrainEffects : ModuleName.Canonical -> A.Region -> A.Region -> A.Region -> Can.Manager -> IO t Type.Constraint
constrainEffects home r0 r1 r2 manager =
  IO.bind Type.mkFlexVar <| \s0 ->
  IO.bind Type.mkFlexVar <| \s1 ->
  IO.bind Type.mkFlexVar <| \s2 ->
  IO.bind Type.mkFlexVar <| \m1 ->
  IO.bind Type.mkFlexVar <| \m2 ->
  IO.bind Type.mkFlexVar <| \sm1 ->
  IO.bind Type.mkFlexVar <| \sm2 ->

  let state0 = Type.VarN s0
      state1 = Type.VarN s1
      state2 = Type.VarN s2
      msg1 = Type.VarN m1
      msg2 = Type.VarN m2
      self1 = Type.VarN sm1
      self2 = Type.VarN sm2

      onSelfMsg = Type.FunN (router msg2 self2) (Type.FunN self2 (Type.FunN state2 (task state2)))
      onEffects =
        case manager of
          Can.CCmd cmd   -> Type.FunN (router msg1 self1) (Type.FunN (effectList home cmd msg1) (Type.FunN state1 (task state1)))
          Can.CSub sub   -> Type.FunN (router msg1 self1) (Type.FunN (effectList home sub msg1) (Type.FunN state1 (task state1)))
          Can.Fx cmd sub -> Type.FunN (router msg1 self1) (Type.FunN (effectList home cmd msg1) (Type.FunN (effectList home sub msg1) (Type.FunN state1 (task state1))))

      effectCons =
        Type.CAnd
          [ Type.CLocal r0 "init" (E.NoExpectation (task state0))
          , Type.CLocal r1 "onEffects" (E.NoExpectation onEffects)
          , Type.CLocal r2 "onSelfMsg" (E.NoExpectation onSelfMsg)
          , Type.CEqual r1 E.Effects state0 (E.NoExpectation state1)
          , Type.CEqual r2 E.Effects state0 (E.NoExpectation state2)
          , Type.CEqual r2 E.Effects self1 (E.NoExpectation self2)
          ] in

  IO.fmap (Type.CLet [] [s0,s1,s2,m1,m2,sm1,sm2] Map.empty effectCons) <|
    case manager of
      Can.CCmd cmd ->
        checkMap "cmdMap" home cmd Type.CSaveTheEnvironment

      Can.CSub sub ->
        checkMap "subMap" home sub Type.CSaveTheEnvironment

      Can.Fx cmd sub ->
        IO.andThen (checkMap "cmdMap" home cmd) <|
          checkMap "subMap" home sub Type.CSaveTheEnvironment


effectList : ModuleName.Canonical -> Name.Name -> Type.Type -> Type.Type
effectList home name msg =
  Type.AppN ModuleName.list Name.list [Type.AppN home name [msg]]


task : Type.Type -> Type.Type
task answer =
  Type.AppN ModuleName.platform Name.task [ Type.never, answer ]


router : Type.Type -> Type.Type -> Type.Type
router msg self =
  Type.AppN ModuleName.platform Name.router [ msg, self ]


checkMap : Name.Name -> ModuleName.Canonical -> Name.Name -> Type.Constraint -> IO t Type.Constraint
checkMap name home tipe constraint =
  IO.bind Type.mkFlexVar <| \a ->
  IO.bind Type.mkFlexVar <| \b ->
  let mapType = toMapType home tipe (Type.VarN a) (Type.VarN b)
      mapCon = Type.CLocal A.zero name (E.NoExpectation mapType) in
  IO.return <| Type.CLet [a,b] [] Map.empty mapCon constraint


toMapType : ModuleName.Canonical -> Name.Name -> Type.Type -> Type.Type -> Type.Type
toMapType home tipe a b =
  Type.FunN (Type.FunN a b) (Type.FunN (Type.AppN home tipe [a]) (Type.AppN home tipe [b]))
