{- MANUALLY FORMATTED -}
module Compiler.Canonicalize.Environment.Foreign exposing
  ( createInitialEnv
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Source as Src
import Compiler.Canonicalize.Environment as Env
import Compiler.Data.Name as Name
import Compiler.Elm.Interface as I
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Canonicalize as Error
import Compiler.Reporting.Result as MResult
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Maybe as MMaybe



-- RESULT


type alias TResult i w a =
  MResult.TResult i w Error.Error a


createInitialEnv : ModuleName.Canonical -> Map.Map ModuleName.Raw I.Interface -> TList Src.Import -> TResult i w Env.Env
createInitialEnv home ifaces imports =
  MResult.bind (MResult.foldM (addImport ifaces) emptyState (toSafeImports home imports)) <| \(State vs ts cs bs qvs qts qcs) ->
  MResult.ok (Env.Env home (Map.map infoToVar vs) ts cs bs qvs qts qcs)


infoToVar : Env.Info Can.Annotation -> Env.Var
infoToVar info =
  case info of
    Env.Specific home tipe -> Env.Foreign home tipe
    Env.Ambiguous h hs     -> Env.Foreigns h hs



-- STATE


type State =
  State
    {- vars -} (Env.Exposed Can.Annotation)
    {- types -} (Env.Exposed Env.Type)
    {- ctors -} (Env.Exposed Env.Ctor)
    {- binops -} (Env.Exposed Env.Binop)
    {- q_vars -} (Env.Qualified Can.Annotation)
    {- q_types -} (Env.Qualified Env.Type)
    {- q_ctors -} (Env.Qualified Env.Ctor)


emptyState : State
emptyState =
  State Map.empty emptyTypes Map.empty Map.empty Map.empty Map.empty Map.empty


emptyTypes : Env.Exposed Env.Type
emptyTypes =
  Map.singleton "List" (Env.Specific ModuleName.list (Env.Union 1 ModuleName.list))



-- TO SAFE IMPORTS


toSafeImports : ModuleName.Canonical -> TList Src.Import -> TList Src.Import
toSafeImports (ModuleName.Canonical pkg _) imports =
  if Pkg.isKernel pkg
  then MList.filter isNormal imports
  else imports


isNormal : Src.Import -> Bool
isNormal (Src.Import (A.At _ name) maybeAlias _) =
  if Name.isKernel name
  then
    case maybeAlias of
      Nothing -> False
      Just _ -> Debug.todo "kernel imports cannot use `as`"
  else
    True



-- ADD IMPORTS


addImport : Map.Map ModuleName.Raw I.Interface -> State -> Src.Import -> TResult i w State
addImport ifaces (State vs ts cs bs qvs qts qcs) (Src.Import (A.At _ name) maybeAlias exposing_) =
  let
    (I.Interface pkg defs unions aliases binops) = Map.ex ifaces name
    prefix = MMaybe.maybe name identity maybeAlias
    home = ModuleName.Canonical pkg name

    rawTypeInfo =
      Map.union
        (Map.mapMaybeWithKey (unionToType home) unions)
        (Map.mapMaybeWithKey (aliasToType home) aliases)

    vars = Map.map (Env.Specific home) defs
    types = Map.map (Env.Specific home << Tuple.first) rawTypeInfo
    ctors = Map.foldr (addExposed << Tuple.second) Map.empty rawTypeInfo

    qvs2 = addQualified prefix vars qvs
    qts2 = addQualified prefix types qts
    qcs2 = addQualified prefix ctors qcs
  in
  case exposing_ of
    Src.Open ->
      let
        vs2 = addExposed vs vars
        ts2 = addExposed ts types
        cs2 = addExposed cs ctors
        bs2 = addExposed bs (Map.mapWithKey (binopToBinop home) binops)
      in
      MResult.ok (State vs2 ts2 cs2 bs2 qvs2 qts2 qcs2)

    Src.Explicit exposedList ->
      MResult.foldM
        (addExposedValue home vars rawTypeInfo binops)
        (State vs ts cs bs qvs2 qts2 qcs2)
        exposedList


addExposed : Env.Exposed a -> Env.Exposed a -> Env.Exposed a
addExposed =
  Map.unionWith Env.mergeInfo


addQualified : Name.Name -> Env.Exposed a -> Env.Qualified a -> Env.Qualified a
addQualified prefix exposed qualified =
  Map.insertWith addExposed prefix exposed qualified



-- UNION


unionToType : ModuleName.Canonical -> Name.Name -> I.Union -> Maybe (Env.Type, Env.Exposed Env.Ctor)
unionToType home name union =
  Maybe.map (unionToTypeHelp home name) (I.toPublicUnion union)


unionToTypeHelp : ModuleName.Canonical -> Name.Name -> Can.Union -> (Env.Type, Env.Exposed Env.Ctor)
unionToTypeHelp home name ((Can.Union vars ctors _ _) as union) =
  let
    addCtor dict (Can.Ctor ctor index _ args) =
      Map.insert ctor (Env.Specific home (Env.Ctor home name union index args)) dict
  in
  ( Env.Union (MList.length vars) home
  , MList.foldl addCtor Map.empty ctors
  )



-- ALIAS


aliasToType : ModuleName.Canonical -> Name.Name -> I.Alias -> Maybe (Env.Type, Env.Exposed Env.Ctor)
aliasToType home name alias =
  Maybe.map (aliasToTypeHelp home name) (I.toPublicAlias alias)


aliasToTypeHelp : ModuleName.Canonical -> Name.Name -> Can.Alias -> (Env.Type, Env.Exposed Env.Ctor)
aliasToTypeHelp home name (Can.Alias vars tipe) =
  (
    Env.Alias (MList.length vars) home vars tipe
  ,
    case tipe of
      Can.TRecord fields Nothing ->
        let
          avars = MList.map (\var -> (var, Can.TVar var)) vars
          alias_ =
            MList.foldr
              (\(_,t1) t2 -> Can.TLambda t1 t2)
              (Can.TAlias home name avars (Can.Filled tipe))
              (Can.fieldsToList fields)
        in
        Map.singleton name (Env.Specific home (Env.RecordCtor home vars alias_))

      _ ->
        Map.empty
  )



-- BINOP


binopToBinop : ModuleName.Canonical -> Name.Name -> I.Binop -> Env.Info Env.Binop
binopToBinop home op (I.Binop name annotation associativity precedence) =
  Env.Specific home (Env.Binop op home name annotation associativity precedence)



-- ADD EXPOSED VALUE


addExposedValue
  : ModuleName.Canonical
  -> Env.Exposed Can.Annotation
  -> Map.Map Name.Name (Env.Type, Env.Exposed Env.Ctor)
  -> Map.Map Name.Name I.Binop
  -> State
  -> Src.Exposed
  -> TResult i w State
addExposedValue home vars types binops (State vs ts cs bs qvs qts qcs) exposed =
  case exposed of
    Src.Lower (A.At region name) ->
      case Map.lookup name vars of
        Just info ->
          MResult.ok (State (Map.insertWith Env.mergeInfo name info vs) ts cs bs qvs qts qcs)

        Nothing ->
          MResult.throw (Error.ImportExposingNotFound region home name (Map.keys vars))

    Src.Upper (A.At region name) privacy ->
      case privacy of
        Src.Private ->
          case Map.lookup name types of
            Just (tipe, ctors) ->
              case tipe of
                Env.Union _ _ ->
                  let
                    ts2 = Map.insert name (Env.Specific home tipe) ts
                  in
                  MResult.ok (State vs ts2 cs bs qvs qts qcs)

                Env.Alias _ _ _ _ ->
                  let
                    ts2 = Map.insert name (Env.Specific home tipe) ts
                    cs2 = addExposed cs ctors
                  in
                  MResult.ok (State vs ts2 cs2 bs qvs qts qcs)

            Nothing ->
              case checkForCtorMistake name types of
                tipe::_ ->
                  MResult.throw <| Error.ImportCtorByName region name tipe

                [] ->
                  MResult.throw <| Error.ImportExposingNotFound region home name (Map.keys types)

        Src.Public dotDotRegion ->
          case Map.lookup name types of
            Just (tipe, ctors) ->
              case tipe of
                Env.Union _ _ ->
                  let
                    ts2 = Map.insert name (Env.Specific home tipe) ts
                    cs2 = addExposed cs ctors
                  in
                  MResult.ok (State vs ts2 cs2 bs qvs qts qcs)

                Env.Alias _ _ _ _ ->
                  MResult.throw (Error.ImportOpenAlias dotDotRegion name)

            Nothing ->
              MResult.throw (Error.ImportExposingNotFound region home name (Map.keys types))

    Src.Operator region op ->
      case Map.lookup op binops of
        Just binop ->
          let
            bs2 = Map.insert op (binopToBinop home op binop) bs
          in
          MResult.ok (State vs ts cs bs2 qvs qts qcs)

        Nothing ->
          MResult.throw (Error.ImportExposingNotFound region home op (Map.keys binops))


checkForCtorMistake : Name.Name -> Map.Map Name.Name (Env.Type, Env.Exposed Env.Ctor) -> TList Name.Name
checkForCtorMistake givenName types =
  let
    addMatches (_, exposedCtors) matches =
      Map.foldrWithKey addMatch matches exposedCtors

    addMatch ctorName info matches =
      if ctorName /= givenName
      then matches
      else
        case info of
          Env.Specific _ (Env.Ctor _ tipeName _ _ _) ->
            tipeName :: matches

          Env.Specific _ (Env.RecordCtor _ _ _) ->
            matches

          Env.Ambiguous _ _ ->
            matches
  in
  Map.foldr addMatches [] types
