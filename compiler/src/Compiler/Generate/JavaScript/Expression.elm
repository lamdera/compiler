{- MANUALLY FORMATTED -}
module Compiler.Generate.JavaScript.Expression exposing
  ( generate
  , generateCtor
  , generateField
  , generateTailDef
  , generateMain
  , Code
  , codeToExpr
  , codeToStmtList
  --
  , isBreakpointDef
  , generateAsync
  )


import Compiler.AST.Canonical as Can
import Compiler.AST.Optimized as Opt
import Compiler.AST.Utils.Shader as Shader
import Compiler.Data.Index as Index
import Compiler.Data.Name as Name
import Compiler.Elm.Compiler.Type as Type
import Compiler.Elm.Compiler.Type.Extract as Extract
import Compiler.Elm.ModuleName as ModuleName
import Compiler.Elm.Package as Pkg
import Compiler.Elm.Version as V
import Compiler.Generate.JavaScript.Builder as JS
import Compiler.Generate.JavaScript.Name as JsName
import Compiler.Generate.Mode as Mode
import Compiler.Json.Encode as Encode
import Compiler.Optimize.DecisionTree as DT
import Compiler.Reporting.Annotation as A
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Set as Set



-- EXPRESSIONS


generateJsExpr : Mode.Mode -> Opt.Expr -> JS.Expr
generateJsExpr mode expression =
  codeToExpr (generate mode expression)


generate : Mode.Mode -> Opt.Expr -> Code
generate = generateAsync False Nothing


{- NEW: generateAsync -}
generateAsync : Bool -> Maybe ( Name.Name, Name.Name ) -> Mode.Mode -> Opt.Expr -> Code
generateAsync isAsyncDef maybeBpNames mode expression =
  case expression of
    Opt.CBool bool ->
      JsExpr <| JS.CBool bool

    Opt.Chr char ->
      JsExpr <|
        case mode of
          Mode.Dev _ ->
            JS.Call toChar [ JS.CString char ]

          Mode.Prod _ ->
            JS.CString char

    Opt.Str string ->
      JsExpr <| JS.CString string

    Opt.CInt int ->
      JsExpr <| JS.CInt int

    Opt.CFloat float ->
      JsExpr <| JS.CFloat float

    Opt.VarLocal name ->
      JsExpr <| JS.Ref (JsName.fromLocal name)

    Opt.VarGlobal (Opt.Global home name) ->
      JsExpr <| JS.Ref (JsName.fromGlobal home name)

    Opt.VarEnum (Opt.Global home name) index ->
      case mode of
        Mode.Dev _ ->
          JsExpr <| JS.Ref (JsName.fromGlobal home name)

        Mode.Prod _ ->
          JsExpr <| JS.CInt (Index.toMachine index)

    Opt.VarBox (Opt.Global home name) ->
      JsExpr <| JS.Ref <|
        case mode of
          Mode.Dev _ -> JsName.fromGlobal home name
          Mode.Prod _ -> JsName.fromGlobal ModuleName.basics Name.identity_

    Opt.VarCycle home name ->
      JsExpr <| JS.Call (JS.Ref (JsName.fromCycle home name)) []

    Opt.VarDebug name home region unhandledValueName ->
      JsExpr <| generateDebug name home region unhandledValueName

    Opt.VarKernel home name ->
      JsExpr <| JS.Ref (JsName.fromKernel home name)

    Opt.CList entries ->
      case entries of
        [] ->
          JsExpr <| JS.Ref (JsName.fromKernel Name.list "Nil")

        _ ->
          JsExpr <|
            JS.Call
              (JS.Ref (JsName.fromKernel Name.list "fromArray"))
              [ JS.Array <| MList.map (generateJsExpr mode) entries
              ]

    Opt.Function args body ->
      generateFunctionAsync
        (isAsyncDef || isBreakpointFun args mode)
        (MList.map JsName.fromLocal args)
        (generate mode body)

    Opt.Call func args ->
      let ( newMode, newFunc, newArgs ) = breakpointDefCall func args mode maybeBpNames in
      JsExpr <| generateCall newMode newFunc newArgs

    Opt.TailCall name args ->
      JsBlock <| generateTailCall mode name args

    Opt.If branches final ->
      generateIf mode branches final

    Opt.Let def body ->
      JsBlock <|
        generateDef mode def :: codeToStmtList (generate mode body)

    Opt.Destruct (Opt.Destructor name path) body ->
      let
        pathDef = JS.Var (JsName.fromLocal name) (generatePath mode path)
      in
      JsBlock <| pathDef :: codeToStmtList (generate mode body)

    Opt.Case label root decider jumps ->
      JsBlock <| generateCase mode label root decider jumps

    Opt.Accessor field ->
      JsExpr <| JS.Function Nothing [JsName.dollar]
        [ JS.Return <|
            JS.Access (JS.Ref JsName.dollar) (generateField mode field)
        ]

    Opt.Access record field ->
      JsExpr <| JS.Access (generateJsExpr mode record) (generateField mode field)

    Opt.Update record fields ->
      JsExpr <|
        JS.Call (JS.Ref (JsName.fromKernel Name.utils "update"))
          [ generateJsExpr mode record
          , generateRecord mode fields
          ]

    Opt.Record fields ->
      JsExpr <| generateRecord mode fields

    Opt.Unit ->
      case mode of
        Mode.Dev _ ->
          JsExpr <| JS.Ref (JsName.fromKernel Name.utils "Tuple0")

        Mode.Prod _ ->
          JsExpr <| JS.CInt 0

    Opt.Tuple a b maybeC ->
      JsExpr <|
        case maybeC of
          Nothing ->
            JS.Call (JS.Ref (JsName.fromKernel Name.utils "Tuple2"))
              [ generateJsExpr mode a
              , generateJsExpr mode b
              ]

          Just c ->
            JS.Call (JS.Ref (JsName.fromKernel Name.utils "Tuple3"))
              [ generateJsExpr mode a
              , generateJsExpr mode b
              , generateJsExpr mode c
              ]

    Opt.Shader src attributes uniforms ->
      let
        toTranlation field =
          ( JsName.fromLocal field
          , JS.CString (JsName.toBuilder (generateField mode field))
          )

        toTranslationObject fields =
          JS.Object (MList.map toTranlation (Set.toList fields))
      in
      JsExpr <| JS.Object <|
        [ ( JsName.fromLocal "src", JS.CString (Shader.toJsStringBuilder src) )
        , ( JsName.fromLocal "attributes", toTranslationObject attributes )
        , ( JsName.fromLocal "uniforms", toTranslationObject uniforms )
        ]



-- CODE CHUNKS


type Code
  = JsExpr JS.Expr
  | JsBlock (TList JS.Stmt)


codeToExpr : Code -> JS.Expr
codeToExpr code =
  case code of
    JsExpr expr ->
      expr

    JsBlock [ JS.Return expr ] ->
      expr

    JsBlock stmts ->
      JS.Call (JS.Function Nothing [] stmts) []


codeToStmtList : Code -> TList JS.Stmt
codeToStmtList code =
  case code of
    JsExpr (JS.Call (JS.Function Nothing [] stmts) []) ->
      stmts

    JsExpr expr ->
      [ JS.Return expr ]

    JsBlock stmts ->
      stmts


codeToStmt : Code -> JS.Stmt
codeToStmt code =
  case code of
    JsExpr (JS.Call (JS.Function Nothing [] stmts) []) ->
      JS.Block stmts

    JsExpr expr ->
      JS.Return expr

    JsBlock [stmt] ->
      stmt

    JsBlock stmts ->
      JS.Block stmts



-- CHARS


toChar : JS.Expr
toChar =
  JS.Ref (JsName.fromKernel Name.utils "chr")



-- CTOR


generateCtor : Mode.Mode -> Opt.Global -> Index.ZeroBased -> Int -> Code
generateCtor mode (Opt.Global home name) index arity =
  let
    argNames =
      Index.indexedMap (\i _ -> JsName.fromIndex i) (MList.range 1 arity)

    ctorTag =
      case mode of
        Mode.Dev _ -> JS.CString (Name.toBuilder name)
        Mode.Prod _ -> JS.CInt (ctorToInt home name index)
  in
  generateFunction argNames <| JsExpr <| JS.Object <|
    (JsName.dollar, ctorTag) :: MList.map (\n -> (n, JS.Ref n)) argNames


ctorToInt : ModuleName.Canonical -> Name.Name -> Index.ZeroBased -> Int
ctorToInt home name index =
  if home == ModuleName.dict && name == "RBNode_elm_builtin" || name == "RBEmpty_elm_builtin" then
    0 - Index.toHuman index
  else
    Index.toMachine index



-- RECORDS


generateRecord : Mode.Mode -> Map.Map Name.Name Opt.Expr -> JS.Expr
generateRecord mode fields =
  let
    toPair (field, value) =
      (generateField mode field, generateJsExpr mode value)
  in
  JS.Object (MList.map toPair (Map.toList fields))


generateField : Mode.Mode -> Name.Name -> JsName.Name
generateField mode name =
  case mode of
    Mode.Dev _ ->
      JsName.fromLocal name

    Mode.Prod fields ->
      Map.ex fields name



-- DEBUG


generateDebug : Name.Name -> ModuleName.Canonical -> A.Region -> Maybe Name.Name -> JS.Expr
generateDebug name (ModuleName.Canonical _ home) region unhandledValueName =
  if name /= "todo" then
    JS.Ref (JsName.fromGlobal ModuleName.debug name)
  else
    case unhandledValueName of
      Nothing ->
        JS.Call (JS.Ref (JsName.fromKernel Name.debug "todo")) <|
          [ JS.CString (Name.toBuilder home)
          , regionToJsExpr region
          ]

      Just valueName ->
        JS.Call (JS.Ref (JsName.fromKernel Name.debug "todoCase")) <|
          [ JS.CString (Name.toBuilder home)
          , regionToJsExpr region
          , JS.Ref (JsName.fromLocal valueName)
          ]


regionToJsExpr : A.Region -> JS.Expr
regionToJsExpr (A.Region start end) =
  JS.Object
    [ ( JsName.fromLocal "start", positionToJsExpr start )
    , ( JsName.fromLocal "end", positionToJsExpr end )
    ]


positionToJsExpr : A.Position -> JS.Expr
positionToJsExpr (A.Position line column) =
  JS.Object
    [ ( JsName.fromLocal "line", JS.CInt line )
    , ( JsName.fromLocal "column", JS.CInt column )
    ]



-- FUNCTION


generateFunction : TList JsName.Name -> Code -> Code
generateFunction = generateFunctionAsync False


{- NEW: generateFunctionAsync -}
generateFunctionAsync : Bool -> TList JsName.Name -> Code -> Code
generateFunctionAsync isAsyncDef args body =
  let func = if isAsyncDef then JS.AsyncFunction else JS.Function in
  case Map.lookup (MList.length args) funcHelpers of
    Just helper ->
      JsExpr <|
        JS.Call helper
          [ func Nothing args <|
              codeToStmtList body
          ]

    Nothing ->
      let
        addArg arg code =
          JsExpr <| func Nothing [arg] <|
            codeToStmtList code
      in
      MList.foldr addArg body args


funcHelpers : Map.Map Int JS.Expr
funcHelpers =
  Map.fromList <|
    MList.map (\n -> (n, JS.Ref (JsName.makeF n))) (MList.range 2 9)



-- CALLS


generateCall : Mode.Mode -> Opt.Expr -> TList Opt.Expr -> JS.Expr
generateCall mode func args =
  case func of
    Opt.VarGlobal ((Opt.Global (ModuleName.Canonical pkg _) _) as global) ->
      if Mode.isAsyncActive mode && pkg == Pkg.dummyName then generateCallHelpAsync True mode func args
      else if pkg == Pkg.core then generateCoreCall mode global args
      else generateCallHelp mode (addAsyncGlobal mode "elm" "browser" "Browser" "sandbox" func) args

    Opt.VarBox _ ->
      case mode of
        Mode.Dev _ ->
          generateCallHelp mode func args

        Mode.Prod _ ->
          case args of
            [arg] ->
              generateJsExpr mode arg

            _ ->
              generateCallHelp mode func args

    _ ->
      generateCallHelpAsync (isSuspendCall func mode) mode func args


generateCallHelp : Mode.Mode -> Opt.Expr -> TList Opt.Expr -> JS.Expr
generateCallHelp = generateCallHelpAsync False


{- NEW: generateCallHelpAsync -}
generateCallHelpAsync : Bool -> Mode.Mode -> Opt.Expr -> TList Opt.Expr -> JS.Expr
generateCallHelpAsync isAsync mode func args =
  generateNormalCallAsync isAsync
    (generateJsExpr mode func)
    (MList.map (generateJsExpr mode) args)


generateGlobalCall : ModuleName.Canonical -> Name.Name -> TList JS.Expr -> JS.Expr
generateGlobalCall home name args =
  generateNormalCall (JS.Ref (JsName.fromGlobal home name)) args


generateNormalCall : JS.Expr -> TList JS.Expr -> JS.Expr
generateNormalCall = generateNormalCallAsync False


{- NEW: generateNormalCallAsync -}
generateNormalCallAsync : Bool -> JS.Expr -> TList JS.Expr -> JS.Expr
generateNormalCallAsync isAsync func args =
  let call = if isAsync then JS.AsyncCall else JS.Call in
  case Map.lookup (MList.length args) callHelpers of
    Just helper ->
      call helper (func::args)

    Nothing ->
      MList.foldl (\f a -> call f [a]) func args


callHelpers : Map.Map Int JS.Expr
callHelpers =
  Map.fromList <|
    MList.map (\n -> (n, JS.Ref (JsName.makeA n))) (MList.range 2 9)



-- CORE CALLS


generateCoreCall : Mode.Mode -> Opt.Global -> TList Opt.Expr -> JS.Expr
generateCoreCall mode (Opt.Global ((ModuleName.Canonical _ moduleName) as home) name) args =
  if moduleName == Name.basics then
    generateBasicsCall mode home name args

  else if moduleName == Name.bitwise then
    generateBitwiseCall home name (MList.map (generateJsExpr mode) args)

  else if moduleName == Name.tuple then
    generateTupleCall home name (MList.map (generateJsExpr mode) args)

  else if moduleName == Name.jsArray then
    generateJsArrayCall home name (MList.map (generateJsExpr mode) args)

  else
    generateGlobalCall home name (MList.map (generateJsExpr mode) args)


generateTupleCall : ModuleName.Canonical -> Name.Name -> TList JS.Expr -> JS.Expr
generateTupleCall home name args =
  case args of
    [value] ->
      case name of
        "first"  -> JS.Access value (JsName.fromLocal "a")
        "second" -> JS.Access value (JsName.fromLocal "b")
        _        -> generateGlobalCall home name args

    _ ->
      generateGlobalCall home name args


generateJsArrayCall : ModuleName.Canonical -> Name.Name -> TList JS.Expr -> JS.Expr
generateJsArrayCall home name args =
  let otherwise () = generateGlobalCall home name args in
  case args of
    [entry]        -> if name == "singleton" then JS.Array [entry]     else otherwise ()
    [index, array] -> if name == "unsafeGet" then JS.Index array index else otherwise ()
    _                                                                    -> otherwise ()


generateBitwiseCall : ModuleName.Canonical -> Name.Name -> TList JS.Expr -> JS.Expr
generateBitwiseCall home name args =
  case args of
    [arg] ->
      case name of
        "complement" -> JS.Prefix JS.PrefixComplement arg
        _            -> generateGlobalCall home name args

    [left,right] ->
      case name of
        "and"            -> JS.Infix JS.OpBitwiseAnd left right
        "or"             -> JS.Infix JS.OpBitwiseOr  left right
        "xor"            -> JS.Infix JS.OpBitwiseXor left right
        "shiftLeftBy"    -> JS.Infix JS.OpLShift     right left
        "shiftRightBy"   -> JS.Infix JS.OpSpRShift   right left
        "shiftRightZfBy" -> JS.Infix JS.OpZfRShift   right left
        _                -> generateGlobalCall home name args

    _ ->
      generateGlobalCall home name args


generateBasicsCall : Mode.Mode -> ModuleName.Canonical -> Name.Name -> TList Opt.Expr -> JS.Expr
generateBasicsCall mode home name args =
  case args of
    [elmArg] ->
      let arg = generateJsExpr mode elmArg in
      case name of
        "not"      -> JS.Prefix JS.PrefixNot arg
        "negate"   -> JS.Prefix JS.PrefixNegate arg
        "toFloat"  -> arg
        "truncate" -> JS.Infix JS.OpBitwiseOr arg (JS.CInt 0)
        _          -> generateGlobalCall home name [arg]

    [elmLeft, elmRight] ->
      case name of
        -- NOTE: removed "composeL" and "composeR" because of this issue:
        -- https://github.com/elm/compiler/issues/1722
        "append"   -> append mode elmLeft elmRight
        "apL"      -> generateJsExpr mode <| apply elmLeft elmRight
        "apR"      -> generateJsExpr mode <| apply elmRight elmLeft
        _ ->
          let
            left = generateJsExpr mode elmLeft
            right = generateJsExpr mode elmRight
          in
          case name of
            "add"  -> JS.Infix JS.OpAdd left right
            "sub"  -> JS.Infix JS.OpSub left right
            "mul"  -> JS.Infix JS.OpMul left right
            "fdiv" -> JS.Infix JS.OpDiv left right
            "idiv" -> JS.Infix JS.OpBitwiseOr (JS.Infix JS.OpDiv left right) (JS.CInt 0)
            "eq"   -> equal left right
            "neq"  -> notEqual left right
            "lt"   -> cmp JS.OpLt JS.OpLt   0  left right
            "gt"   -> cmp JS.OpGt JS.OpGt   0  left right
            "le"   -> cmp JS.OpLe JS.OpLt   1  left right
            "ge"   -> cmp JS.OpGe JS.OpGt (-1) left right
            "or"   -> JS.Infix JS.OpOr  left right
            "and"  -> JS.Infix JS.OpAnd left right
            "xor"  -> JS.Infix JS.OpNe  left right
            "remainderBy" -> JS.Infix JS.OpMod right left
            _      -> generateGlobalCall home name [left, right]

    _ ->
      generateGlobalCall home name (MList.map (generateJsExpr mode) args)


equal : JS.Expr -> JS.Expr -> JS.Expr
equal left right =
  if isLiteral left || isLiteral right then
    strictEq left right
  else
    JS.Call (JS.Ref (JsName.fromKernel Name.utils "eq")) [left, right]


notEqual : JS.Expr -> JS.Expr -> JS.Expr
notEqual left right =
  if isLiteral left || isLiteral right then
    strictNEq left right
  else
    JS.Prefix JS.PrefixNot <|
      JS.Call (JS.Ref (JsName.fromKernel Name.utils "eq")) [left, right]


cmp : JS.InfixOp -> JS.InfixOp -> Int -> JS.Expr -> JS.Expr -> JS.Expr
cmp idealOp backupOp backupInt left right =
  if isLiteral left || isLiteral right then
    JS.Infix idealOp left right
  else
    JS.Infix backupOp
      (JS.Call (JS.Ref (JsName.fromKernel Name.utils "cmp")) [left, right])
      (JS.CInt backupInt)


isLiteral : JS.Expr -> Bool
isLiteral expr =
  case expr of
    JS.CString _ ->
      True

    JS.CFloat _ ->
      True

    JS.CInt _ ->
      True

    JS.CBool _ ->
      True

    _ ->
      False


apply : Opt.Expr -> Opt.Expr -> Opt.Expr
apply func value =
  case func of
    Opt.Accessor field ->
      Opt.Access value field

    Opt.Call f args ->
      Opt.Call f (args ++ [value])

    _ ->
      Opt.Call func [value]


append : Mode.Mode -> Opt.Expr -> Opt.Expr -> JS.Expr
append mode left right =
  let seqs = generateJsExpr mode left :: toSeqs mode right in
  if MList.any isStringLiteral seqs then
    MList.foldr1 (JS.Infix JS.OpAdd) seqs
  else
    MList.foldr1 jsAppend seqs


jsAppend : JS.Expr -> JS.Expr -> JS.Expr
jsAppend a b =
  JS.Call (JS.Ref (JsName.fromKernel Name.utils "ap")) [a, b]


toSeqs : Mode.Mode -> Opt.Expr -> TList JS.Expr
toSeqs mode expr =
  let otherwise () = [generateJsExpr mode expr] in
  case expr of
    Opt.Call (Opt.VarGlobal (Opt.Global home "append")) [left, right] ->
      if home == ModuleName.basics then
        generateJsExpr mode left :: toSeqs mode right else otherwise ()

    _ ->
      otherwise ()


isStringLiteral : JS.Expr -> Bool
isStringLiteral expr =
  case expr of
    JS.CString _ ->
      True

    _ ->
      False



-- SIMPLIFY INFIX OPERATORS


strictEq : JS.Expr -> JS.Expr -> JS.Expr
strictEq left right =
  case left of
    JS.CInt 0 ->
      JS.Prefix JS.PrefixNot right

    JS.CBool bool ->
      if bool then right else JS.Prefix JS.PrefixNot right

    _ ->
      case right of
        JS.CInt 0 ->
          JS.Prefix JS.PrefixNot left

        JS.CBool bool ->
          if bool then left else JS.Prefix JS.PrefixNot left

        _ ->
          JS.Infix JS.OpEq left right


strictNEq : JS.Expr -> JS.Expr -> JS.Expr
strictNEq left right =
  case left of
    JS.CInt 0 ->
      JS.Prefix JS.PrefixNot (JS.Prefix JS.PrefixNot right)

    JS.CBool bool ->
      if bool then JS.Prefix JS.PrefixNot right else right

    _ ->
      case right of
        JS.CInt 0 ->
          JS.Prefix JS.PrefixNot (JS.Prefix JS.PrefixNot left)

        JS.CBool bool ->
          if bool then JS.Prefix JS.PrefixNot left else left

        _ ->
          JS.Infix JS.OpNe left right



-- TAIL CALL


-- TODO check if JS minifiers collapse unnecessary temporary variables
--
generateTailCall : Mode.Mode -> Name.Name -> TList (Name.Name, Opt.Expr) -> TList JS.Stmt
generateTailCall mode name args =
  let
    toTempVars (argName, arg) =
      ( JsName.makeTemp argName, generateJsExpr mode arg )

    toRealVars (argName, _) =
      JS.ExprStmt <|
        JS.Assign (JS.LRef (JsName.fromLocal argName)) (JS.Ref (JsName.makeTemp argName))
  in
  JS.Vars (MList.map toTempVars args)
  :: MList.map toRealVars args
  ++ [ JS.Continue (Just (JsName.fromLocal name)) ]



-- DEFINITIONS


generateDef : Mode.Mode -> Opt.Def -> JS.Stmt
generateDef mode def =
  case def of
    Opt.Def name body ->
      JS.Var (JsName.fromLocal name) (generateJsExpr mode body)

    Opt.TailDef name argNames body ->
      JS.Var (JsName.fromLocal name) (codeToExpr (generateTailDef mode name argNames body))


generateTailDef : Mode.Mode -> Name.Name -> TList Name.Name -> Opt.Expr -> Code
generateTailDef mode name argNames body =
  generateFunction (MList.map JsName.fromLocal argNames) <| JsBlock <|
    [ JS.Labelled (JsName.fromLocal name) <|
        JS.While (JS.CBool True) <|
          codeToStmt <| generate mode body
    ]



-- PATHS


generatePath : Mode.Mode -> Opt.Path -> JS.Expr
generatePath mode path =
  case path of
    Opt.Index index subPath ->
      JS.Access (generatePath mode subPath) (JsName.fromIndex index)

    Opt.Root name ->
      JS.Ref (JsName.fromLocal name)

    Opt.Field field subPath ->
      JS.Access (generatePath mode subPath) (generateField mode field)

    Opt.Unbox subPath ->
      case mode of
        Mode.Dev _ ->
          JS.Access (generatePath mode subPath) (JsName.fromIndex Index.first)

        Mode.Prod _ ->
          generatePath mode subPath



-- GENERATE IFS


generateIf : Mode.Mode -> TList (Opt.Expr, Opt.Expr) -> Opt.Expr -> Code
generateIf mode givenBranches givenFinal =
  let
    (branches, final) =
      crushIfs givenBranches givenFinal

    convertBranch (condition, expr) =
      ( generateJsExpr mode condition
      , generate mode expr
      )

    branchExprs = MList.map convertBranch branches
    finalCode = generate mode final
  in
  if isBlock finalCode || MList.any (isBlock << Tuple.second) branchExprs then
    JsBlock [ MList.foldr addStmtIf (codeToStmt finalCode) branchExprs ]
  else
    JsExpr <| MList.foldr addExprIf (codeToExpr finalCode) branchExprs


addExprIf : (JS.Expr, Code) -> JS.Expr -> JS.Expr
addExprIf (condition, branch) final =
  JS.If condition (codeToExpr branch) final


addStmtIf : (JS.Expr, Code) -> JS.Stmt -> JS.Stmt
addStmtIf (condition, branch) final =
  JS.IfStmt condition (codeToStmt branch) final


isBlock : Code -> Bool
isBlock code =
  case code of
    JsBlock _ -> True
    JsExpr _ -> False


crushIfs : TList (Opt.Expr, Opt.Expr) -> Opt.Expr -> (TList (Opt.Expr, Opt.Expr), Opt.Expr)
crushIfs branches final =
  crushIfsHelp [] branches final


crushIfsHelp
  : TList (Opt.Expr, Opt.Expr)
  -> TList (Opt.Expr, Opt.Expr)
  -> Opt.Expr
  -> (TList (Opt.Expr, Opt.Expr), Opt.Expr)
crushIfsHelp visitedBranches unvisitedBranches final =
  case unvisitedBranches of
    [] ->
      case final of
        Opt.If subBranches subFinal ->
          crushIfsHelp visitedBranches subBranches subFinal

        _ ->
          (MList.reverse visitedBranches, final)

    visiting :: unvisited ->
      crushIfsHelp (visiting :: visitedBranches) unvisited final



-- CASE EXPRESSIONS


generateCase : Mode.Mode -> Name.Name -> Name.Name -> Opt.Decider Opt.Choice -> TList (Int, Opt.Expr) -> TList JS.Stmt
generateCase mode label root decider jumps =
  MList.foldr (goto mode label) (generateDecider mode label root decider) jumps


goto : Mode.Mode -> Name.Name -> (Int, Opt.Expr) -> TList JS.Stmt -> TList JS.Stmt
goto mode label (index, branch) stmts =
  let
    labeledDeciderStmt =
      JS.Labelled
        (JsName.makeLabel label index)
        (JS.While (JS.CBool True) (JS.Block stmts))
  in
  labeledDeciderStmt :: codeToStmtList (generate mode branch)


generateDecider : Mode.Mode -> Name.Name -> Name.Name -> Opt.Decider Opt.Choice -> TList JS.Stmt
generateDecider mode label root decisionTree =
  case decisionTree of
    Opt.Leaf (Opt.Inline branch) ->
      codeToStmtList (generate mode branch)

    Opt.Leaf (Opt.Jump index) ->
      [ JS.Break (Just (JsName.makeLabel label index)) ]

    Opt.Chain testChain success failure ->
      [ JS.IfStmt
          (MList.foldl1 (JS.Infix JS.OpAnd) (MList.map (generateIfTest mode root) testChain))
          (JS.Block <| generateDecider mode label root success)
          (JS.Block <| generateDecider mode label root failure)
      ]

    Opt.FanOut path edges fallback ->
      [ JS.Switch
          (generateCaseTest mode root path (Tuple.first (MList.head edges)))
          ( MList.foldr
              (\edge cases -> generateCaseBranch mode label root edge :: cases)
              [ JS.Default (generateDecider mode label root fallback) ]
              edges
          )
      ]


generateIfTest : Mode.Mode -> Name.Name -> (DT.Path, DT.Test) -> JS.Expr
generateIfTest mode root (path, test) =
  let
    value = pathToJsExpr mode root path
  in
  case test of
    DT.IsCtor home name index _ opts ->
      let
        tag =
          case mode of
            Mode.Dev _ -> JS.Access value JsName.dollar
            Mode.Prod _ ->
              case opts of
                Can.Normal -> JS.Access value JsName.dollar
                Can.Enum   -> value
                Can.Unbox  -> value
      in
      strictEq tag <|
        case mode of
          Mode.Dev _ -> JS.CString (Name.toBuilder name)
          Mode.Prod _ -> JS.CInt (ctorToInt home name index)

    DT.IsBool True ->
      value

    DT.IsBool False ->
      JS.Prefix JS.PrefixNot value

    DT.IsInt int ->
      strictEq value (JS.CInt int)

    DT.IsChr char ->
      strictEq (JS.CString char) <|
        case mode of
          Mode.Dev _ -> JS.Call (JS.Access value (JsName.fromLocal "valueOf")) []
          Mode.Prod _ -> value

    DT.IsStr string ->
      strictEq value (JS.CString string)

    DT.IsCons ->
      JS.Access value (JsName.fromLocal "b")

    DT.IsNil ->
      JS.Prefix JS.PrefixNot <|
        JS.Access value (JsName.fromLocal "b")

    DT.IsTuple ->
      Debug.todo "COMPILER BUG - there should never be tests on a tuple"



generateCaseBranch : Mode.Mode -> Name.Name -> Name.Name -> (DT.Test, Opt.Decider Opt.Choice) -> JS.Case
generateCaseBranch mode label root (test, subTree) =
  JS.Case
    (generateCaseValue mode test)
    (generateDecider mode label root subTree)


generateCaseValue : Mode.Mode -> DT.Test -> JS.Expr
generateCaseValue mode test =
  case test of
    DT.IsCtor home name index _ _ ->
      case mode of
        Mode.Dev _ -> JS.CString (Name.toBuilder name)
        Mode.Prod _ -> JS.CInt (ctorToInt home name index)

    DT.IsInt int ->
      JS.CInt int

    DT.IsChr char ->
      JS.CString char

    DT.IsStr string ->
      JS.CString string

    DT.IsBool _ ->
      Debug.todo "COMPILER BUG - there should never be three tests on a boolean"

    DT.IsCons ->
      Debug.todo "COMPILER BUG - there should never be three tests on a list"

    DT.IsNil ->
      Debug.todo "COMPILER BUG - there should never be three tests on a list"

    DT.IsTuple ->
      Debug.todo "COMPILER BUG - there should never be three tests on a tuple"


generateCaseTest : Mode.Mode -> Name.Name -> DT.Path -> DT.Test -> JS.Expr
generateCaseTest mode root path exampleTest =
  let
    value = pathToJsExpr mode root path
  in
  case exampleTest of
    DT.IsCtor home name _ _ opts ->
      if name == Name.bool && home == ModuleName.basics then
        value
      else
        case mode of
          Mode.Dev _ ->
            JS.Access value JsName.dollar

          Mode.Prod _ ->
            case opts of
              Can.Normal ->
                JS.Access value JsName.dollar

              Can.Enum ->
                value

              Can.Unbox ->
                value

    DT.IsInt _ ->
      value

    DT.IsStr _ ->
      value

    DT.IsChr _ ->
      case mode of
        Mode.Dev _ ->
          JS.Call (JS.Access value (JsName.fromLocal "valueOf")) []

        Mode.Prod _ ->
          value

    DT.IsBool _ ->
      Debug.todo "COMPILER BUG - there should never be three tests on a list"

    DT.IsCons ->
      Debug.todo "COMPILER BUG - there should never be three tests on a list"

    DT.IsNil ->
      Debug.todo "COMPILER BUG - there should never be three tests on a list"

    DT.IsTuple ->
      Debug.todo "COMPILER BUG - there should never be three tests on a list"



-- PATTERN PATHS


pathToJsExpr : Mode.Mode -> Name.Name -> DT.Path -> JS.Expr
pathToJsExpr mode root path =
  case path of
    DT.Index index subPath ->
      JS.Access (pathToJsExpr mode root subPath) (JsName.fromIndex index)

    DT.Unbox subPath ->
      case mode of
        Mode.Dev _ ->
          JS.Access (pathToJsExpr mode root subPath) (JsName.fromIndex Index.first)

        Mode.Prod _ ->
          pathToJsExpr mode root subPath

    DT.Empty ->
      JS.Ref (JsName.fromLocal root)



-- GENERATE MAIN


generateMain : Mode.Mode -> ModuleName.Canonical -> Opt.Main -> JS.Expr
generateMain mode home main =
  case main of
    Opt.Static ->
      JS.Ref (JsName.fromKernel Name.virtualDom (addAsyncName mode "init"))
        |> hash (JS.Ref (JsName.fromGlobal home "main"))
        |> hash (JS.CInt 0)
        |> hash (JS.CInt 0)

    Opt.Dynamic msgType decoder ->
      JS.Ref (JsName.fromGlobal home "main")
        |> hash (generateJsExpr mode decoder)
        |> hash (toDebugMetadata mode msgType)


hash : JS.Expr -> JS.Expr -> JS.Expr
hash arg func =
  JS.Call func [arg]


toDebugMetadata : Mode.Mode -> Can.Type -> JS.Expr
toDebugMetadata mode msgType =
  case mode of
    Mode.Prod _ ->
      JS.CInt 0

    Mode.Dev (Mode.DevDebug interfaces) ->
      JS.Json <| Encode.object <|
        [ ( "versions", Encode.object [ ( "elm", V.encode V.compiler ) ] )
        , ( "types"   , Type.encodeMetadata (Extract.fromMsg interfaces msgType) )
        ]

    Mode.Dev _ ->
      JS.CInt 0



-- BREAKPOINTS


{- NEW: isBreakpointDef -}
isBreakpointDef : Opt.Expr -> TList Opt.Expr -> Bool
isBreakpointDef func args =
  getBreakpointDefSuspendFun func args /= Nothing


breakpointDefCall : Opt.Expr -> TList Opt.Expr -> Mode.Mode -> Maybe ( Name.Name, Name.Name ) -> ( Mode.Mode, Opt.Expr, TList Opt.Expr )
breakpointDefCall func args mode maybeBpNames =
  case ( mode, getBreakpointDefSuspendFun func args, maybeBpNames ) of
    ( Mode.Dev (Mode.DevAsync True suspendFuns), Just suspendFun, Just ( moduleName, bpName ) ) ->
      ( Mode.Dev (Mode.DevAsync True (Set.insert suspendFun suspendFuns))
      , Opt.VarKernel "Breakpoint" "named"
      , [ Opt.Str moduleName, Opt.Str bpName, Opt.Call func args ]
      )

    _ -> ( mode, func, args )


getBreakpointDefSuspendFun : Opt.Expr -> TList Opt.Expr -> Maybe String
getBreakpointDefSuspendFun func args =
  case ( func, args ) of
    ( Opt.VarGlobal (Opt.Global (ModuleName.Canonical pkg moduleName) "apL"), [ Opt.VarGlobal (Opt.Global (ModuleName.Canonical (Pkg.Name "elm" "breakpoint") "Breakpoint") "new"), Opt.Function (arg :: _) _ ] ) ->

      if pkg == Pkg.core && moduleName == Name.basics
      then Just arg
      else Nothing

    _ -> Nothing


isBreakpointFun : TList Name.Name -> Mode.Mode -> Bool
isBreakpointFun args mode =
  case ( args, mode ) of
    ( arg :: _, Mode.Dev (Mode.DevAsync True suspendFuns) ) -> Set.member arg suspendFuns
    _ -> False


isSuspendCall : Opt.Expr -> Mode.Mode -> Bool
isSuspendCall func mode =
  case ( func, mode ) of
    ( Opt.VarLocal name, Mode.Dev (Mode.DevAsync True suspendFuns) ) -> Set.member name suspendFuns
    _ -> False


addAsyncGlobal : Mode.Mode -> String -> String -> String -> String -> Opt.Expr -> Opt.Expr
addAsyncGlobal mode author pkg moduleName name expr =
  case expr of
    Opt.VarGlobal (Opt.Global (ModuleName.Canonical (Pkg.Name a p) m) n) ->
      if a == author && p == pkg && m == moduleName && n == name then
        Opt.VarGlobal (Opt.Global (ModuleName.Canonical (Pkg.Name a p) m) (addAsyncName mode n))
      else
        expr
    _ ->
      expr


addAsyncName : Mode.Mode -> Name.Name -> Name.Name
addAsyncName mode name =
  if Mode.isAsync mode then name ++ "_async" else name
