{- MANUALLY FORMATTED -}
module Compiler.Type.Constrain.Expression exposing
  ( {- constrain
  ,-} constrainDef
  , constrainRecursiveDefs
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Reporting.Annotation as A
import Compiler.Reporting.Error.Type as E
import Compiler.Type.Constrain.Pattern as Pattern
import Compiler.Type.Instantiate as Instantiate
import Compiler.Type.Type as Type
import Extra.System.IO.Pure as IO
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Set as Set



-- IO


type alias IO t a = Type.IO t a



-- CONSTRAIN


-- As we step past type annotations, the free type variables are added to
-- the "rigid type variables" dict. Allowing sharing of rigid variables
-- between nested type annotations.
--
-- So if you have a top-level type annotation like (func : a -> b) the RTV
-- dictionary will hold variables for `a` and `b`
--
type alias RTV =
  Map.Map Name.Name Type.Type


constrain : RTV -> Can.Expr -> E.Expected Type.Type -> IO t Type.Constraint
constrain rtv (A.At region expression) expected =
  case expression of
    Can.VarLocal name ->
      IO.return (Type.CLocal region name expected)

    Can.VarTopLevel _ name ->
      IO.return (Type.CLocal region name expected)

    Can.VarKernel _ _ ->
      IO.return Type.CTrue

    Can.VarForeign _ name annotation ->
      IO.return <| Type.CForeign region name annotation expected

    Can.VarCtor _ _ name _ annotation ->
      IO.return <| Type.CForeign region name annotation expected

    Can.VarDebug _ name annotation ->
      IO.return <| Type.CForeign region name annotation expected

    Can.VarOperator op _ _ annotation ->
      IO.return <| Type.CForeign region op annotation expected

    Can.Str _ ->
      IO.return <| Type.CEqual region E.CString Type.string expected

    Can.Chr _ ->
      IO.return <| Type.CEqual region E.CChar Type.char expected

    Can.CInt _ ->
      IO.bind Type.mkFlexNumber <| \var ->
      IO.return <| Type.exists [var] <| Type.CEqual region E.Number (Type.VarN var) expected

    Can.CFloat _ ->
      IO.return <| Type.CEqual region E.CFloat Type.float expected

    Can.CList elements ->
      constrainList rtv region elements expected

    Can.Negate expr ->
      IO.bind Type.mkFlexNumber <| \numberVar ->
      let numberType = Type.VarN numberVar in
      IO.bind (constrain rtv expr (E.FromContext region E.Negate numberType)) <| \numberCon ->
      let negateCon = Type.CEqual region E.Number numberType expected in
      IO.return <| Type.exists [numberVar] <| Type.CAnd [ numberCon, negateCon ]

    Can.Binop op _ _ annotation leftExpr rightExpr ->
      constrainBinop rtv region op annotation leftExpr rightExpr expected

    Can.Lambda args body ->
      constrainLambda rtv region args body expected

    Can.Call func args ->
      constrainCall rtv region func args expected

    Can.If branches finally ->
      constrainIf rtv region branches finally expected

    Can.Case expr branches ->
      constrainCase rtv region expr branches expected

    Can.Let def body ->
      IO.andThen (constrainDef rtv def)
      <| constrain rtv body expected

    Can.LetRec defs body ->
      IO.andThen (constrainRecursiveDefs rtv defs)
      <| constrain rtv body expected

    Can.LetDestruct pattern expr body ->
      IO.andThen (constrainDestruct rtv region pattern expr)
      <| constrain rtv body expected

    Can.Accessor field ->
      IO.bind Type.mkFlexVar <| \extVar ->
      IO.bind Type.mkFlexVar <| \fieldVar ->
      let extType = Type.VarN extVar
          fieldType = Type.VarN fieldVar
          recordType = Type.RecordN (Map.singleton field fieldType) extType in
      IO.return <| Type.exists [ fieldVar, extVar ] <|
        Type.CEqual region (E.Accessor field) (Type.FunN recordType fieldType) expected

    Can.Access expr (A.At accessRegion field) ->
      IO.bind Type.mkFlexVar <| \extVar ->
      IO.bind Type.mkFlexVar <| \fieldVar ->
      let extType = Type.VarN extVar
          fieldType = Type.VarN fieldVar
          recordType = Type.RecordN (Map.singleton field fieldType) extType

          context = E.RecordAccess (A.toRegion expr) (getAccessName expr) accessRegion field in
      IO.bind (constrain rtv expr (E.FromContext region context recordType)) <| \recordCon ->

      IO.return <| Type.exists [ fieldVar, extVar ] <|
        Type.CAnd
          [ recordCon
          , Type.CEqual region (E.Access field) fieldType expected
          ]

    Can.Update name expr fields ->
      constrainUpdate rtv region name expr fields expected

    Can.Record fields ->
      constrainRecord rtv region fields expected

    Can.Unit ->
      IO.return <| Type.CEqual region E.Unit Type.UnitN expected

    Can.Tuple a b maybeC ->
      constrainTuple rtv region a b maybeC expected

    Can.Shader _ types ->
      constrainShader region types expected



-- CONSTRAIN LAMBDA


constrainLambda : RTV -> A.Region -> TList Can.Pattern -> Can.Expr -> E.Expected Type.Type -> IO t Type.Constraint
constrainLambda rtv region args body expected =
  IO.bind
    (constrainArgs args) <| \(Args vars tipe resultType (Pattern.State headers pvars revCons)) ->

  IO.bind
    (constrain rtv body (E.NoExpectation resultType)) <| \bodyCon ->

  IO.return <| Type.exists vars <|
    Type.CAnd
      [ Type.CLet
          {- rigidVars -} []
          {- flexVars -} pvars
          {- header -} headers
          {- headerCon -} (Type.CAnd (MList.reverse revCons))
          {- bodyCon -} bodyCon
      , Type.CEqual region E.Lambda tipe expected
      ]



-- CONSTRAIN CALL


constrainCall : RTV -> A.Region -> Can.Expr -> TList Can.Expr -> E.Expected Type.Type -> IO t Type.Constraint
constrainCall rtv region ((A.At funcRegion _) as func) args expected =
  let maybeName = getName func in

  IO.bind Type.mkFlexVar <| \funcVar ->
  IO.bind Type.mkFlexVar <| \resultVar ->
  let funcType = Type.VarN funcVar
      resultType = Type.VarN resultVar in

  IO.bind (constrain rtv func (E.NoExpectation funcType)) <| \funcCon ->

  IO.bind
    (IO.fmap MList.unzip3 <| indexedForA args (constrainArg rtv region maybeName)) <| \(argVars, argTypes, argCons) ->

  let arityType = MList.foldr Type.FunN resultType argTypes
      category = E.CallResult maybeName in

  IO.return <| Type.exists (funcVar::resultVar::argVars) <|
    Type.CAnd
      [ funcCon
      , Type.CEqual funcRegion category funcType (E.FromContext region (E.CallArity maybeName (MList.length args)) arityType)
      , Type.CAnd argCons
      , Type.CEqual region category resultType expected
      ]


constrainArg : RTV -> A.Region -> E.MaybeName -> Index.ZeroBased -> Can.Expr -> IO t (Type.Variable, Type.Type, Type.Constraint)
constrainArg rtv region maybeName index arg =
  IO.bind Type.mkFlexVar <| \argVar ->
  let argType = Type.VarN argVar in
  IO.bind (constrain rtv arg (E.FromContext region (E.CallArg maybeName index) argType)) <| \argCon ->
  IO.return (argVar, argType, argCon)


getName : Can.Expr -> E.MaybeName
getName (A.At _ expr) =
  case expr of
    Can.VarLocal name        -> E.FuncName name
    Can.VarTopLevel _ name   -> E.FuncName name
    Can.VarForeign _ name _  -> E.FuncName name
    Can.VarCtor _ _ name _ _ -> E.CtorName name
    Can.VarOperator op _ _ _ -> E.OpName op
    Can.VarKernel _ name     -> E.FuncName name
    _                        -> E.NoName


getAccessName : Can.Expr -> Maybe Name.Name
getAccessName (A.At _ expr) =
  case expr of
    Can.VarLocal name       -> Just name
    Can.VarTopLevel _ name  -> Just name
    Can.VarForeign _ name _ -> Just name
    _                       -> Nothing



-- CONSTRAIN BINOP


constrainBinop : RTV -> A.Region -> Name.Name -> Can.Annotation -> Can.Expr -> Can.Expr -> E.Expected Type.Type -> IO t Type.Constraint
constrainBinop rtv region op annotation leftExpr rightExpr expected =
  IO.bind Type.mkFlexVar <| \leftVar ->
  IO.bind Type.mkFlexVar <| \rightVar ->
  IO.bind Type.mkFlexVar <| \answerVar ->
  let leftType = Type.VarN leftVar
      rightType = Type.VarN rightVar
      answerType = Type.VarN answerVar
      binopType = Type.FunN leftType (Type.FunN rightType answerType)

      opCon = Type.CForeign region op annotation (E.NoExpectation binopType) in

  IO.bind (constrain rtv leftExpr (E.FromContext region (E.OpLeft op) leftType)) <| \leftCon ->
  IO.bind (constrain rtv rightExpr (E.FromContext region (E.OpRight op) rightType)) <| \rightCon ->

  IO.return <| Type.exists [ leftVar, rightVar, answerVar ] <|
    Type.CAnd
      [ opCon
      , leftCon
      , rightCon
      , Type.CEqual region (E.CallResult (E.OpName op)) answerType expected
      ]



-- CONSTRAIN LISTS


constrainList : RTV -> A.Region -> TList Can.Expr -> E.Expected Type.Type -> IO t Type.Constraint
constrainList rtv region entries expected =
  IO.bind Type.mkFlexVar <| \entryVar ->
  let entryType = Type.VarN entryVar
      listType = Type.AppN ModuleName.list Name.list [entryType] in

  IO.bind
    (indexedForA entries (constrainListEntry rtv region entryType)) <| \entryCons ->

  IO.return <| Type.exists [entryVar] <|
    Type.CAnd
      [ Type.CAnd entryCons
      , Type.CEqual region E.CList listType expected
      ]


constrainListEntry : RTV -> A.Region -> Type.Type -> Index.ZeroBased -> Can.Expr -> IO t Type.Constraint
constrainListEntry rtv region tipe index expr =
  constrain rtv expr (E.FromContext region (E.ListEntry index) tipe)



-- CONSTRAIN IF EXPRESSIONS


constrainIf : RTV -> A.Region -> TList (Can.Expr, Can.Expr) -> Can.Expr -> E.Expected Type.Type -> IO t Type.Constraint
constrainIf rtv region branches final expected =
  let boolExpect = E.FromContext region E.IfCondition Type.bool
      (conditions, exprs) = MList.foldr (\(c,e) (cs,es) -> (c::cs,e::es)) ([],[final]) branches in

  IO.bind
    (IO.traverseList (\c -> constrain rtv c boolExpect) conditions) <| \condCons ->

  case expected of
    E.FromAnnotation name arity _ tipe ->
      IO.bind (indexedForA exprs <| \index expr ->
        constrain rtv expr (E.FromAnnotation name arity (E.TypedIfBranch index) tipe)) <| \branchCons ->
      IO.return <|
        Type.CAnd
          [ Type.CAnd condCons
          , Type.CAnd branchCons
          ]

    _ ->
      IO.bind Type.mkFlexVar <| \branchVar ->
      let branchType = Type.VarN branchVar in

      IO.bind (indexedForA exprs <| \index expr ->
        constrain rtv expr (E.FromContext region (E.IfBranch index) branchType)) <| \branchCons ->

      IO.return <| Type.exists [branchVar] <|
        Type.CAnd
          [ Type.CAnd condCons
          , Type.CAnd branchCons
          , Type.CEqual region E.If branchType expected
          ]



-- CONSTRAIN CASE EXPRESSIONS


constrainCase : RTV -> A.Region -> Can.Expr -> TList Can.CaseBranch -> E.Expected Type.Type -> IO t Type.Constraint
constrainCase rtv region expr branches expected =
  IO.bind Type.mkFlexVar <| \ptrnVar ->
  let ptrnType = Type.VarN ptrnVar in
  IO.bind (constrain rtv expr (E.NoExpectation ptrnType)) <| \exprCon ->

  case expected of
    E.FromAnnotation name arity _ tipe ->
      IO.bind (indexedForA branches <| \index branch ->
        constrainCaseBranch rtv branch
          (E.PFromContext region (E.PCaseMatch index) ptrnType)
          (E.FromAnnotation name arity (E.TypedCaseBranch index) tipe)) <| \branchCons ->

      IO.return <| Type.exists [ptrnVar] <| Type.CAnd (exprCon::branchCons)

    _ ->
      IO.bind Type.mkFlexVar <| \branchVar ->
      let branchType = Type.VarN branchVar in

      IO.bind (indexedForA branches <| \index branch ->
        constrainCaseBranch rtv branch
          (E.PFromContext region (E.PCaseMatch index) ptrnType)
          (E.FromContext region (E.CaseBranch index) branchType)) <| \branchCons ->

      IO.return <| Type.exists [ptrnVar,branchVar] <|
        Type.CAnd
          [ exprCon
          , Type.CAnd branchCons
          , Type.CEqual region E.Case branchType expected
          ]


constrainCaseBranch : RTV -> Can.CaseBranch -> E.PExpected Type.Type -> E.Expected Type.Type -> IO t Type.Constraint
constrainCaseBranch rtv (Can.CaseBranch pattern expr) pExpect bExpect =
  IO.bind
    (Pattern.add pattern pExpect Pattern.emptyState) <| \(Pattern.State headers pvars revCons) ->

  IO.fmap (Type.CLet [] pvars headers (Type.CAnd (MList.reverse revCons)))
    <| constrain rtv expr bExpect



-- CONSTRAIN RECORD


constrainRecord : RTV -> A.Region -> Map.Map Name.Name Can.Expr -> E.Expected Type.Type -> IO t Type.Constraint
constrainRecord rtv region fields expected =
  IO.bind (IO.traverseMap (constrainField rtv) fields) <| \dict ->

  let getType (_, t, _) = t
      recordType = Type.RecordN (Map.map getType dict) Type.EmptyRecordN
      recordCon = Type.CEqual region E.Record recordType expected

      vars = Map.foldr (\(v,_,_) vs -> v::vs) [] dict
      cons = Map.foldr (\(_,_,c) cs -> c::cs) [recordCon] dict in

  IO.return <| Type.exists vars (Type.CAnd cons)


constrainField : RTV -> Can.Expr -> IO t (Type.Variable, Type.Type, Type.Constraint)
constrainField rtv expr =
  IO.bind Type.mkFlexVar <| \var ->
  let tipe = Type.VarN var in
  IO.bind (constrain rtv expr (E.NoExpectation tipe)) <| \con ->
  IO.return (var, tipe, con)



-- CONSTRAIN RECORD UPDATE


constrainUpdate : RTV -> A.Region -> Name.Name -> Can.Expr -> Map.Map Name.Name Can.FieldUpdate -> E.Expected Type.Type -> IO t Type.Constraint
constrainUpdate rtv region name expr fields expected =
  IO.bind Type.mkFlexVar <| \extVar ->
  IO.bind (IO.traverseWithKey (constrainUpdateField rtv region) fields) <| \fieldDict ->

  IO.bind Type.mkFlexVar <| \recordVar ->
  let recordType = Type.VarN recordVar
      fieldsType = Type.RecordN (Map.map (\(_,t,_) -> t) fieldDict) (Type.VarN extVar)

      -- NOTE: fieldsType is separate so that Error propagates better
      fieldsCon = Type.CEqual region E.Record recordType (E.NoExpectation fieldsType)
      recordCon = Type.CEqual region E.Record recordType expected

      vars = Map.foldr (\(v,_,_) vs -> v::vs) [recordVar,extVar] fieldDict
      cons = Map.foldr (\(_,_,c) cs -> c::cs) [recordCon] fieldDict in

  IO.bind (constrain rtv expr (E.FromContext region (E.RecordUpdateKeys name fields) recordType)) <| \con ->

  IO.return <| Type.exists vars <| Type.CAnd (fieldsCon::con::cons)


constrainUpdateField : RTV -> A.Region -> Name.Name -> Can.FieldUpdate -> IO t (Type.Variable, Type.Type, Type.Constraint)
constrainUpdateField rtv region field (Can.FieldUpdate _ expr) =
  IO.bind Type.mkFlexVar <| \var ->
  let tipe = Type.VarN var in
  IO.bind (constrain rtv expr (E.FromContext region (E.RecordUpdateValue field) tipe)) <| \con ->
  IO.return (var, tipe, con)



-- CONSTRAIN TUPLE


constrainTuple : RTV -> A.Region -> Can.Expr -> Can.Expr -> Maybe Can.Expr -> E.Expected Type.Type -> IO t Type.Constraint
constrainTuple rtv region a b maybeC expected =
  IO.bind Type.mkFlexVar <| \aVar ->
  IO.bind Type.mkFlexVar <| \bVar ->
  let aType = Type.VarN aVar
      bType = Type.VarN bVar in

  IO.bind (constrain rtv a (E.NoExpectation aType)) <| \aCon ->
  IO.bind (constrain rtv b (E.NoExpectation bType)) <| \bCon ->

  case maybeC of
    Nothing ->
      let tupleType = Type.TupleN aType bType Nothing
          tupleCon = Type.CEqual region E.Tuple tupleType expected in
      IO.return <| Type.exists [ aVar, bVar ] <| Type.CAnd [ aCon, bCon, tupleCon ]

    Just c ->
      IO.bind Type.mkFlexVar <| \cVar ->
      let cType = Type.VarN cVar in

      IO.bind (constrain rtv c (E.NoExpectation cType)) <| \cCon ->

      let tupleType = Type.TupleN aType bType (Just cType)
          tupleCon = Type.CEqual region E.Tuple tupleType expected in

      IO.return <| Type.exists [ aVar, bVar, cVar ] <| Type.CAnd [ aCon, bCon, cCon, tupleCon ]



-- CONSTRAIN SHADER


constrainShader : A.Region -> Shader.Types -> E.Expected Type.Type -> IO t Type.Constraint
constrainShader region (Shader.Types attributes uniforms varyings) expected =
  IO.bind Type.mkFlexVar <| \attrVar ->
  IO.bind Type.mkFlexVar <| \unifVar ->
  let attrType = Type.VarN attrVar
      unifType = Type.VarN unifVar

      shaderType =
        Type.AppN ModuleName.webgl Name.shader
          [ toShaderRecord attributes attrType
          , toShaderRecord uniforms unifType
          , toShaderRecord varyings Type.EmptyRecordN
          ] in

  IO.return <| Type.exists [ attrVar, unifVar ] <|
    Type.CEqual region E.Shader shaderType expected


toShaderRecord : Set.Set Name.Name -> Type.Type -> Type.Type
toShaderRecord types baseRecType =
  if Set.null types then
    baseRecType
  else
    Type.RecordN (glToType types) baseRecType


glToType : Set.Set Name.Name -> Map.Map Name.Name Type.Type
glToType types =
  Set.foldl (\map name -> Map.insert name Type.texture map) Map.empty types



-- CONSTRAIN DESTRUCTURES


constrainDestruct : RTV -> A.Region -> Can.Pattern -> Can.Expr -> Type.Constraint -> IO t Type.Constraint
constrainDestruct rtv region pattern expr bodyCon =
  IO.bind Type.mkFlexVar <| \patternVar ->
  let patternType = Type.VarN patternVar in

  IO.bind
    (Pattern.add pattern (E.PNoExpectation patternType) Pattern.emptyState) <| \(Pattern.State headers pvars revCons) ->

  IO.bind
    (constrain rtv expr (E.FromContext region E.Destructure patternType)) <| \exprCon ->

  IO.return <| Type.CLet [] (patternVar::pvars) headers (Type.CAnd (MList.reverse (exprCon::revCons))) bodyCon



-- CONSTRAIN DEF


constrainDef : RTV -> Can.Def -> Type.Constraint -> IO t Type.Constraint
constrainDef rtv def bodyCon =
  case def of
    Can.Def (A.At region name) args expr ->
      IO.bind
        (constrainArgs args) <| \(Args vars tipe resultType (Pattern.State headers pvars revCons)) ->

      IO.bind
        (constrain rtv expr (E.NoExpectation resultType)) <| \exprCon ->

      IO.return <|
        Type.CLet
          {- rigidVars -} []
          {- flexVars -} vars
          {- header -} (Map.singleton name (A.At region tipe))
          {- headerCon -}
            (Type.CLet
              {- rigidVars -} []
              {- flexVars -} pvars
              {- header -} headers
              {- headerCon -} (Type.CAnd (MList.reverse revCons))
              {- bodyCon -} exprCon)
          {- bodyCon -} bodyCon

    Can.TypedDef (A.At region name) freeVars typedArgs expr srcResultType ->
      let newNames = Map.difference freeVars rtv in
      IO.bind (IO.traverseWithKey (\n _ -> Type.nameToRigid n) newNames) <| \newRigids ->
      let newRtv = Map.union rtv (Map.map Type.VarN newRigids) in

      IO.bind
        (constrainTypedArgs newRtv name typedArgs srcResultType) <| \(TypedArgs tipe resultType (Pattern.State headers pvars revCons)) ->

      let expected = E.FromAnnotation name (MList.length typedArgs) E.TypedBody resultType in
      IO.bind
        (constrain newRtv expr expected) <| \exprCon ->

      IO.return <|
        Type.CLet
          {- rigidVars -} (Map.elems newRigids)
          {- flexVars -} []
          {- header -} (Map.singleton name (A.At region tipe))
          {- headerCon -}
            (Type.CLet
              {- rigidVars -} []
              {- flexVars -} pvars
              {- header -} headers
              {- headerCon -} (Type.CAnd (MList.reverse revCons))
              {- bodyCon -} exprCon)
          {- bodyCon -} bodyCon



-- CONSTRAIN RECURSIVE DEFS


type Info =
  Info
    {- vars -} (TList Type.Variable)
    {- cons -} (TList Type.Constraint)
    {- headers -} (Map.Map Name.Name (A.Located Type.Type))


emptyInfo : Info
emptyInfo =
  Info [] [] Map.empty


constrainRecursiveDefs : RTV -> TList Can.Def -> Type.Constraint -> IO t Type.Constraint
constrainRecursiveDefs rtv defs bodyCon =
  recDefsHelp rtv defs bodyCon emptyInfo emptyInfo


recDefsHelp : RTV -> TList Can.Def -> Type.Constraint -> Info -> Info -> IO t Type.Constraint
recDefsHelp rtv defs bodyCon rigidInfo flexInfo =
  case defs of
    [] ->
      let (Info rigidVars rigidCons rigidHeaders) = rigidInfo
          (Info flexVars  flexCons  flexHeaders ) = flexInfo in
      IO.return <|
        Type.CLet rigidVars [] rigidHeaders Type.CTrue <|
          Type.CLet [] flexVars flexHeaders (Type.CLet [] [] flexHeaders Type.CTrue (Type.CAnd flexCons)) <|
            Type.CAnd [ Type.CAnd rigidCons, bodyCon ]

    def :: otherDefs ->
      case def of
        Can.Def (A.At region name) args expr ->
          let (Info flexVars flexCons flexHeaders) = flexInfo in

          IO.bind
            (argsHelp args (Pattern.State Map.empty flexVars [])) <| \(Args newFlexVars tipe resultType (Pattern.State headers pvars revCons)) ->

          IO.bind
            (constrain rtv expr (E.NoExpectation resultType)) <| \exprCon ->

          let defCon =
                Type.CLet
                  {- rigidVars -} []
                  {- flexVars -} pvars
                  {- header -} headers
                  {- headerCon -} (Type.CAnd (MList.reverse revCons))
                  {- bodyCon -} exprCon in

          recDefsHelp rtv otherDefs bodyCon rigidInfo <|
            Info
              {- vars -} newFlexVars
              {- cons -} (defCon :: flexCons)
              {- headers -} (Map.insert name (A.At region tipe) flexHeaders)

        Can.TypedDef (A.At region name) freeVars typedArgs expr srcResultType ->
          let newNames = Map.difference freeVars rtv in
          IO.bind (IO.traverseWithKey (\n _ -> Type.nameToRigid n) newNames) <| \newRigids ->
          let newRtv = Map.union rtv (Map.map Type.VarN newRigids) in

          IO.bind
            (constrainTypedArgs newRtv name typedArgs srcResultType) <| \(TypedArgs tipe resultType (Pattern.State headers pvars revCons)) ->

          IO.bind
            (constrain newRtv expr <|
              E.FromAnnotation name (MList.length typedArgs) E.TypedBody resultType) <| \exprCon ->

          let defCon =
                Type.CLet
                  {- rigidVars -} []
                  {- flexVars -} pvars
                  {- header -} headers
                  {- headerCon -} (Type.CAnd (MList.reverse revCons))
                  {- bodyCon -} exprCon in

          let (Info rigidVars rigidCons rigidHeaders) = rigidInfo in
          recDefsHelp rtv otherDefs bodyCon
            ( Info
                {- vars -} (Map.foldr (::) rigidVars newRigids)
                {- cons -} (Type.CLet (Map.elems newRigids) [] Map.empty defCon Type.CTrue :: rigidCons)
                {- headers -} (Map.insert name (A.At region tipe) rigidHeaders)
            )
            flexInfo



-- CONSTRAIN ARGS


type Args =
  Args
    {- a_vars -} (TList Type.Variable)
    {- a_type -} Type.Type
    {- a_result -} Type.Type
    {- a_state -} Pattern.State


constrainArgs : TList Can.Pattern -> IO t Args
constrainArgs args =
  argsHelp args Pattern.emptyState


argsHelp : TList Can.Pattern -> Pattern.State -> IO t Args
argsHelp args state =
  case args of
    [] ->
      IO.bind Type.mkFlexVar <| \resultVar ->
      let resultType = Type.VarN resultVar in
      IO.return <| Args [resultVar] resultType resultType state

    pattern :: otherArgs ->
      IO.bind Type.mkFlexVar <| \argVar ->
      let argType = Type.VarN argVar in

      IO.bind
        (IO.andThen (argsHelp otherArgs) <|
          Pattern.add pattern (E.PNoExpectation argType) state) <| \(Args vars tipe result newState) ->

      IO.return (Args (argVar::vars) (Type.FunN argType tipe) result newState)



-- CONSTRAIN TYPED ARGS


type TypedArgs =
  TypedArgs
    {- t_type -} Type.Type
    {- t_result -} Type.Type
    {- t_state -} Pattern.State


constrainTypedArgs : Map.Map Name.Name Type.Type -> Name.Name -> TList (Can.Pattern, Can.Type) -> Can.Type -> IO t TypedArgs
constrainTypedArgs rtv name args srcResultType =
  typedArgsHelp rtv name Index.first args srcResultType Pattern.emptyState


typedArgsHelp : Map.Map Name.Name Type.Type -> Name.Name -> Index.ZeroBased -> TList (Can.Pattern, Can.Type) -> Can.Type -> Pattern.State -> IO t TypedArgs
typedArgsHelp rtv name index args srcResultType state =
  case args of
    [] ->
      IO.bind (Instantiate.fromSrcType rtv srcResultType) <| \resultType ->
      IO.return <| TypedArgs resultType resultType state

    (((A.At region _) as pattern), srcType) :: otherArgs ->
      IO.bind (Instantiate.fromSrcType rtv srcType) <| \argType ->
      let expected = E.PFromContext region (E.PTypedArg name index) argType in

      IO.bind
        (IO.andThen (typedArgsHelp rtv name (Index.next index) otherArgs srcResultType) <|
          Pattern.add pattern expected state) <| \(TypedArgs tipe resultType newState) ->

      IO.return (TypedArgs (Type.FunN argType tipe) resultType newState)



-- HELPER


indexedForA : TList a -> (Index.ZeroBased -> a -> IO s b) -> IO s (TList b)
indexedForA list func =
  IO.loop (indexedForAHelp func) (list, 0, [])


indexedForAHelp : (Index.ZeroBased -> a -> IO s b) -> (TList a, Int, TList b) -> IO s (IO.Step (TList a, Int, TList b) (TList b))
indexedForAHelp callback (list, index, result) =
  case list of
    [] ->
      IO.return (IO.Done (MList.reverse result))

    a::rest ->
      IO.fmap (\b -> IO.Loop (rest, index + 1, b::result)) (callback (Index.ZeroBased index) a)