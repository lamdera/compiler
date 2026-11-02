{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Generate.JavaScript.Expression
  ( generate
  , genVariant
  , genEnum
  , genVector
  , genVoid
  , genField
  , genTailDef
  , genMain
  , tagToInt -- @LAMDERA exposed for Lamdera.Wire3.Direct
  , genCtorImplementation -- @LAMDERA exposed
  , genFunctionImplementation -- @LAMDERA exposed
  , genCurriedFunctionRef -- @LAMDERA exposed
  , genTailDefImplementation -- @LAMDERA exposed
  , FnArgLookup -- @LAMDERA
  , Code
  , codeToExpr
  , codeToStmtList
  )
  where


import Prelude hiding (and, or, not, negate, truncate)
import Data.ByteString.Builder.Prim ((>$<), (>*<))
import qualified Data.ByteString.Builder.Prim as P
import qualified Data.Char as Char
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Map.Utils as Map
import qualified Data.NonEmptyMap as NEM
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8

import qualified Array as A
import qualified Crash

import qualified AST.Optimized as Opt
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Shader as Shader
import qualified AST.Prim.Tag as Tag
import qualified AST.Prim.TypeVar as T
import qualified Data.Index as Index
import qualified Elm.Compiler.Type as Type
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.Name as JsName
import qualified Generate.Mode as Mode
import qualified Json.Encode as Encode
import Json.Encode ((==>))
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Annotation as A


import qualified Lamdera

-- EXPRESSIONS


-- @LAMDERA the arity of a top-level function or constructor, for direct calls
type FnArgLookup = ModuleName.Canonical -> N.Name -> Maybe Int


genJsExpr :: Mode.Mode -> FnArgLookup -> Opt.Expr -> JS.Expr
genJsExpr mode argLookup expression =
  codeToExpr (generate mode argLookup expression)


generate :: Mode.Mode -> FnArgLookup -> Opt.Expr -> Code
generate mode argLookup expression =
  case expression of
    Opt.Bool b -> JsExpr $ JS.Bool b
    Opt.Chr c ->
      JsExpr $
        case mode of
          Mode.Dev  _ -> JS.Call toChar [ JS.String (P.primBounded charUtf8 c) ]
          Mode.Prod _ -> JS.String (P.primBounded charUtf8 c)

    Opt.Str      s -> JsExpr $ JS.String (Utf8.toBuilder s)
    Opt.Int      i -> JsExpr $ JS.Int i
    Opt.Float    f -> JsExpr $ JS.Float (Utf8.toBuilder f)
    Opt.VarLocal x -> JsExpr $ JS.Ref (JsName.fromLocal x)

    Opt.VarGlobal (Opt.Global h n) ->
      JsExpr $ JS.Ref (JsName.fromGlobal h n)

    Opt.VarEnum (Opt.Global h n) t ->
      case mode of
        Mode.Dev  _ -> JsExpr $ JS.Ref (JsName.fromGlobal h n)
        Mode.Prod _ -> JsExpr $ JS.Int (fromIntegral (Tag.toWord32 t))

    Opt.VarBox (Opt.Global h n) ->
      JsExpr $ JS.Ref $
        case mode of
          Mode.Dev  _ -> JsName.fromGlobal h n
          Mode.Prod _ -> JsName.fromGlobal ModuleName.basics N.identity

    Opt.VarCycle h n     -> JsExpr $ JS.Call (JS.Ref (JsName.fromCycle h n)) []
    Opt.VarDebug n h r u -> JsExpr $ genDebug n h r u
    Opt.VarKernel  h n   -> JsExpr $ JS.Ref (JsName.fromKernel h n)

    Opt.List entries ->
      case entries of
        [] ->
          JsExpr $ JS.Ref (JsName.fromKernel Module.kernel_list [N.ascii|Nil|])

        _ ->
          JsExpr $
            JS.Call
              (JS.Ref (JsName.fromKernel Module.kernel_list N.fromArray))
              [ JS.Array $ map (genJsExpr mode argLookup) entries
              ]

    Opt.Function x xs body ->
      genFunction (map JsName.fromLocal (x:xs)) (generate mode argLookup body)

    Opt.Call f x xs     -> JsExpr $ genCall mode argLookup f x xs
    Opt.TailCall n x xs -> JsBlock $ genTailCall mode argLookup n (x:xs)
      Lamdera.& Lamdera.alternativeImplementation (
        let
          isNewValue :: Opt.TailAssign -> Bool
          isNewValue (Opt.TailAssign argName arg) =
            case arg of
              Opt.VarLocal name -> name /= argName
              _ -> True
        in
        genTailCall mode argLookup n (filter isNewValue (x:xs)))
    Opt.If bs f         -> genIf mode argLookup bs f

    Opt.Let def body ->
      JsBlock $
        genDef mode argLookup def : codeToStmtList (generate mode argLookup body)

    Opt.Destruct (Opt.Destructor name path) body ->
      let
        pathDef = JS.Var (JsName.fromLocal name) (genPath mode path)
      in
      JsBlock $ pathDef : codeToStmtList (generate mode argLookup body)

    Opt.Case label root decider jumps ->
      JsBlock $ genCase mode argLookup label root decider jumps

    Opt.Empty ->
      JsExpr $ JS.Object []

    Opt.Accessor field ->
      JsExpr $ JS.Function Nothing [JsName.dollar]
        [ JS.Return $
            JS.Access (JS.Ref JsName.dollar) (genField mode field)
        ]

    Opt.Access record field ->
      JsExpr $ JS.Access (genJsExpr mode argLookup record) (genField mode field)

    Opt.Update record fields ->
      JsExpr $
        JS.Call (JS.Ref (JsName.fromKernel Module.kernel_utils N.update))
          [ genJsExpr mode argLookup record
          , genRecord mode argLookup fields
          ]

    Opt.Record fields ->
      JsExpr $ genRecord mode argLookup fields

    Opt.Unit ->
      case mode of
        Mode.Dev  _ -> JsExpr $ JS.Ref (JsName.fromKernel Module.kernel_utils [N.ascii|Tuple0|])
        Mode.Prod _ -> JsExpr $ JS.Int 0

    Opt.Pair a b ->
      JsExpr $
        JS.Call (JS.Ref (JsName.fromKernel Module.kernel_utils [N.ascii|Tuple2|]))
          [ genJsExpr mode argLookup a
          , genJsExpr mode argLookup b
          ]

    Opt.Triple a b c ->
      JsExpr $
        JS.Call (JS.Ref (JsName.fromKernel Module.kernel_utils [N.ascii|Tuple3|]))
          [ genJsExpr mode argLookup a
          , genJsExpr mode argLookup b
          , genJsExpr mode argLookup c
          ]

    Opt.Shader src attributes uniforms ->
      let
        toTranlation field =
          ( JsName.fromLocal field
          , JS.String (JsName.toBuilder (genField mode field))
          )

        toTranslationObject fields =
          JS.Object (map toTranlation (Set.toList fields))
      in
      JsExpr $ JS.Object $
        [ ( JsName.fromLocal [N.ascii|src|], JS.String (Shader.toJsStringBuilder src) )
        , ( JsName.fromLocal [N.ascii|attributes|], toTranslationObject attributes )
        , ( JsName.fromLocal [N.ascii|uniforms|], toTranslationObject uniforms )
        ]



-- CODE CHUNKS


data Code
  = JsExpr JS.Expr
  | JsBlock [JS.Stmt]


codeToExpr :: Code -> JS.Expr
codeToExpr code =
  case code of
    JsExpr             expr  -> expr
    JsBlock [JS.Return expr] -> expr
    JsBlock stmts            -> JS.Call (JS.Function Nothing [] stmts) []


codeToStmtList :: Code -> [JS.Stmt]
codeToStmtList code =
  case code of
    JsExpr (JS.Call (JS.Function Nothing [] stmts) []) ->
        stmts

    JsExpr expr ->
        [ JS.Return expr ]

    JsBlock stmts ->
        stmts


codeToStmt :: Code -> JS.Stmt
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


{-# NOINLINE toChar #-}
toChar :: JS.Expr
toChar =
  JS.Ref (JsName.fromKernel Module.kernel_utils [N.ascii|chr|])



-- GENERATE VARIANTS


genVariant :: Mode.Mode -> Opt.Global -> Tag.Tag -> Int -> Code
genVariant mode (Opt.Global h n) tag arity =
  let
    argNames =
      Index.indexedMap (\i _ -> JsName.fromIndex i) [1 .. arity]

    ctorTag =
      case mode of
        Mode.Dev  _ -> JS.String (N.toBuilder n)
        Mode.Prod _ -> JS.Int (tagToInt h n tag)
  in
  genFunction argNames $ JsExpr $ JS.Object $
    (JsName.dollar, ctorTag) : map (\x -> (x, JS.Ref x)) argNames


genEnum :: Mode.Mode -> Opt.Global -> Tag.Tag -> Code
genEnum mode (Opt.Global h n) tag =
  JsExpr $
    case mode of
      Mode.Dev  _ -> JS.Object [(JsName.dollar, JS.String (N.toBuilder n))]
      Mode.Prod _ -> JS.Int (tagToInt h n tag)


genVector :: Mode.Mode -> Opt.Global -> Int -> Code
genVector mode global arity =
  genFunction argNames $ JsExpr $ JS.Object $ vectorFields mode global argNames
  where
    argNames = Index.indexedMap (\i _ -> JsName.fromIndex i) [1 .. arity]


-- @LAMDERA the object a Vector constructor builds, shared with genCtorImplementation
vectorFields :: Mode.Mode -> Opt.Global -> [JsName.Name] -> [(JsName.Name, JS.Expr)]
vectorFields mode (Opt.Global h name) argNames =
    case mode of
      Mode.Dev  _ -> toTag name : fields
      Mode.Prod _ ->
        -- @LAMDERA SeqDict_elm_builtin is the only constructor of SeqDict, so Elm 0.19.4
        -- leaves out its tag in prod. The equals override in Lamdera's elm/core finds a
        -- SeqDict by its tag (below -10, see tagToInt), so keep it.
        if h == ModuleName.seqDict && name == seqDictBuiltin
          then (JsName.dollar, JS.Int (tagToInt h name Tag.zero)) : fields
          else fields
  where
    toTag n  = (JsName.dollar, JS.String (N.toBuilder n))
    fields   = map (\x -> (x, JS.Ref x)) argNames


genVoid :: Mode.Mode -> Opt.Global -> Code
genVoid mode (Opt.Global _ n) =
  JsExpr $
    case mode of
      Mode.Dev  _ -> JS.Object [(JsName.dollar, JS.String (N.toBuilder n))]
      Mode.Prod _ -> JS.Int 0


tagToInt :: ModuleName.Canonical -> N.Name -> Tag.Tag -> Int
tagToInt h n tag =
  if h == ModuleName.dict && (n == rbNode || n == rbEmpty)
  then 0 - fromIntegral (Tag.toWord32 tag + 1)
  else fromIntegral (Tag.toWord32 tag)

  -- Elm uses this trick to detect `RBNode_elm_builtin` and `RBEmpty_elm_builtin` within equals
  -- and we're just doing the same trick for SeqDict but 10 integers lower. See equalsOverride
  -- for the other side of this
  Lamdera.& Lamdera.alternativeImplementation (
    if h == ModuleName.seqDict && n == seqDictBuiltin then
      -10 - fromIntegral (Tag.toWord32 tag + 1)
    else if h == ModuleName.dict && (n == rbNode || n == rbEmpty) then
      0 - fromIntegral (Tag.toWord32 tag + 1)
    else
      fromIntegral (Tag.toWord32 tag)
  )


-- @LAMDERA
{-# NOINLINE seqDictBuiltin #-}; seqDictBuiltin :: N.Name; seqDictBuiltin = [N.ascii|SeqDict_elm_builtin|]


{-# NOINLINE rbNode  #-}; rbNode  :: N.Name; rbNode  = [N.ascii|RBNode_elm_builtin|]
{-# NOINLINE rbEmpty #-}; rbEmpty :: N.Name; rbEmpty = [N.ascii|RBEmpty_elm_builtin|]



-- RECORDS


genRecord :: Mode.Mode -> FnArgLookup -> NEM.Map N.Name Opt.Expr -> JS.Expr
genRecord mode argLookup fields =
  let
    toPair (field, value) =
      (genField mode field, genJsExpr mode argLookup value)
  in
  JS.Object (map toPair (NEM.toList fields))


genField :: Mode.Mode -> N.Name -> JsName.Name
genField mode name =
  case mode of
    Mode.Dev _       -> JsName.fromLocal name
    Mode.Prod fields -> $(Map.require 'genField) name fields N.toChars




-- DEBUG


genDebug :: N.Name -> ModuleName.Canonical -> A.Region -> Maybe N.Name -> JS.Expr
genDebug name (ModuleName.Canonical _ home) region unhandledValueName =
  if name /= N.todo then
    JS.Ref (JsName.fromGlobal ModuleName.debug name)
  else
    case unhandledValueName of
      Nothing ->
        JS.Call (JS.Ref (JsName.fromKernel Module.kernel_debug N.todo)) $
          [ JS.String (Module.toBuilder home)
          , regionToJsExpr region
          ]

      Just valueName ->
        JS.Call (JS.Ref (JsName.fromKernel Module.kernel_debug [N.ascii|todoCase|])) $
          [ JS.String (Module.toBuilder home)
          , regionToJsExpr region
          , JS.Ref (JsName.fromLocal valueName)
          ]


regionToJsExpr :: A.Region -> JS.Expr
regionToJsExpr (A.Region start end) =
  JS.Object
    [ [N.ascii|start|] ===> JS.Object [ [N.ascii|line|] ===> JS.Int sr, [N.ascii|column|] ===> JS.Int sc ]
    , [N.ascii|end|]   ===> JS.Object [ [N.ascii|line|] ===> JS.Int er, [N.ascii|column|] ===> JS.Int ec ]
    ]
  where
    (===>) n v = (JsName.fromLocal n, v)
    (sr, sc) = A.toEditorRowCol start
    (er, ec) = A.toEditorRowCol end



-- FUNCTION


genFunction :: [JsName.Name] -> Code -> Code
genFunction args body =
  case IntMap.lookup (length args) funcHelpers of
    Just helper ->
      JsExpr $
        JS.Call helper
          [ JS.Function Nothing args $
              codeToStmtList body
          ]

    Nothing ->
      let
        addArg arg code =
          JsExpr $ JS.Function Nothing [arg] $
            codeToStmtList code
      in
      foldr addArg body args


{-# NOINLINE funcHelpers #-}
funcHelpers :: IntMap.IntMap JS.Expr
funcHelpers =
  IntMap.fromList $
    map (\n -> (n, JS.Ref (JsName.makeF n))) [2..9]



-- CALLS


genCall :: Mode.Mode -> FnArgLookup -> Opt.Expr -> Opt.Expr -> [Opt.Expr] -> JS.Expr
genCall mode argLookup func arg args =
  case func of
    Opt.VarGlobal global@(Opt.Global (ModuleName.Canonical pkg _) _) | pkg == Pkg.core ->
      genCoreCall mode argLookup global (arg:args)

    -- @LAMDERA
    Opt.VarGlobal (Opt.Global home name) ->
      genGlobalCall home name argLookup (map (genJsExpr mode argLookup) (arg:args))

    Opt.VarBox _ ->
      case mode of
        Mode.Dev  _ -> genCallHelp mode argLookup func (arg:args)
        Mode.Prod _ ->
          case args of
            [] -> genJsExpr mode argLookup arg
            _  -> genCallHelp mode argLookup func (arg:args)

    _ ->
      genCallHelp mode argLookup func (arg:args)


genCallHelp :: Mode.Mode -> FnArgLookup -> Opt.Expr -> [Opt.Expr] -> JS.Expr
genCallHelp mode argLookup func args =
  genNormalCall
    (genJsExpr mode argLookup func)
    (map (genJsExpr mode argLookup) args)


genGlobalCall :: ModuleName.Canonical -> N.Name -> FnArgLookup -> [JS.Expr] -> JS.Expr
genGlobalCall home name argLookup args =
  -- @LAMDERA
  case argLookup home name of
    Just n
      | n > 1 && n == length args ->
          JS.Call (JS.Ref (JsName.fromGlobalDirectFn home name)) args
    _ ->
      -- Original:
      genNormalCall (JS.Ref (JsName.fromGlobal home name)) args


genNormalCall :: JS.Expr -> [JS.Expr] -> JS.Expr
genNormalCall func args =
  case IntMap.lookup (length args) callHelpers of
    Just helper -> JS.Call helper (func:args)
    Nothing     -> List.foldl' (\f a -> JS.Call f [a]) func args


{-# NOINLINE callHelpers #-}
callHelpers :: IntMap.IntMap JS.Expr
callHelpers =
  IntMap.fromList $
    map (\n -> (n, JS.Ref (JsName.makeA n))) [2..9]



-- CORE CALLS


genCoreCall :: Mode.Mode -> FnArgLookup -> Opt.Global -> [Opt.Expr] -> JS.Expr
genCoreCall mode argLookup (Opt.Global home@(ModuleName.Canonical _ h) name) args
  | h == Module.basics  = genBasicsCall mode argLookup home name args
  | h == Module.bitwise = genBitwiseCall home name argLookup (map (genJsExpr mode argLookup) args)
  | h == Module.tuple   = genTupleCall   home name argLookup (map (genJsExpr mode argLookup) args)
  | h == Module.jsArray = genJsArrayCall home name argLookup (map (genJsExpr mode argLookup) args)
  | otherwise           = genGlobalCall  home name argLookup (map (genJsExpr mode argLookup) args)


genTupleCall :: ModuleName.Canonical -> N.Name -> FnArgLookup -> [JS.Expr] -> JS.Expr
genTupleCall home name argLookup args =
  case args of
    [value]
      | name == N.first  -> JS.Access value (JsName.fromLocal N.a)
      | name == N.second -> JS.Access value (JsName.fromLocal N.b)
      | otherwise        -> genGlobalCall home name argLookup args

    _ ->
      genGlobalCall home name argLookup args


genJsArrayCall :: ModuleName.Canonical -> N.Name -> FnArgLookup -> [JS.Expr] -> JS.Expr
genJsArrayCall home name argLookup args =
  case args of
    [entry]        | name == [N.ascii|singleton|] -> JS.Array [entry]
    [index, array] | name == [N.ascii|unsafeGet|] -> JS.Index array index
    _                                             -> genGlobalCall home name argLookup args


genBitwiseCall :: ModuleName.Canonical -> N.Name -> FnArgLookup -> [JS.Expr] -> JS.Expr
genBitwiseCall h n argLookup args =
  case args of
    [arg]
      | n == complement -> JS.Prefix JS.PrefixComplement arg
      | otherwise       -> genGlobalCall h n argLookup args

    [left,right]
      | n == and            -> JS.Infix JS.OpBitwiseAnd left right
      | n == or             -> JS.Infix JS.OpBitwiseOr  left right
      | n == xor            -> JS.Infix JS.OpBitwiseXor left right
      | n == shiftLeftBy    -> JS.Infix JS.OpLShift     right left
      | n == shiftRightBy   -> JS.Infix JS.OpSpRShift   right left
      | n == shiftRightZfBy -> JS.Infix JS.OpZfRShift   right left
      | otherwise           -> genGlobalCall h n argLookup args

    _ ->
      genGlobalCall h n argLookup args


{-# NOINLINE complement     #-}; complement     :: N.Name; complement     = [N.ascii|complement|]
{-# NOINLINE shiftLeftBy    #-}; shiftLeftBy    :: N.Name; shiftLeftBy    = [N.ascii|shiftLeftBy|]
{-# NOINLINE shiftRightBy   #-}; shiftRightBy   :: N.Name; shiftRightBy   = [N.ascii|shiftRightBy|]
{-# NOINLINE shiftRightZfBy #-}; shiftRightZfBy :: N.Name; shiftRightZfBy = [N.ascii|shiftRightZfBy|]


genBasicsCall :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> N.Name -> [Opt.Expr] -> JS.Expr
genBasicsCall mode argLookup h n args =
  case args of
    [elmArg] ->
      case genJsExpr mode argLookup elmArg of
        arg
          | n == not      -> JS.Prefix JS.PrefixNot arg
          | n == negate   -> JS.Prefix JS.PrefixNegate arg
          | n == toFloat  -> arg
          | n == truncate -> JS.Infix JS.OpBitwiseOr arg (JS.Int 0)
          | otherwise     -> genGlobalCall h n argLookup [arg]

    [elmLeft, elmRight]
      -- NOTE: removed "composeL" and "composeR" because of this issue:
      -- https://github.com/elm/compiler/issues/1722
      | n == append -> genAppend mode argLookup elmLeft elmRight
      | n == apL    -> genJsExpr mode argLookup $ apply elmLeft elmRight
      | n == apR    -> genJsExpr mode argLookup $ apply elmRight elmLeft
      | otherwise   ->
          let
            left  = genJsExpr mode argLookup elmLeft
            right = genJsExpr mode argLookup elmRight
          in
          case () of
            ()
              | n == add         -> JS.Infix JS.OpAdd left right
              | n == sub         -> JS.Infix JS.OpSub left right
              | n == mul         -> JS.Infix JS.OpMul left right
              | n == fdiv        -> JS.Infix JS.OpDiv left right
              | n == idiv        -> JS.Infix JS.OpBitwiseOr (JS.Infix JS.OpDiv left right) (JS.Int 0)
              | n == eq          -> equal left right
              | n == neq         -> notEqual left right
              | n == lt          -> comp JS.OpLt JS.OpLt   0  left right
              | n == gt          -> comp JS.OpGt JS.OpGt   0  left right
              | n == le          -> comp JS.OpLe JS.OpLt   1  left right
              | n == ge          -> comp JS.OpGe JS.OpGt (-1) left right
              | n == or          -> JS.Infix JS.OpOr  left right
              | n == and         -> JS.Infix JS.OpAnd left right
              | n == xor         -> JS.Infix JS.OpNe  left right
              | n == remainderBy -> JS.Infix JS.OpMod right left
              | otherwise        -> genGlobalCall h n argLookup [left, right]

    _ ->
      genGlobalCall h n argLookup (map (genJsExpr mode argLookup) args)


{-# NOINLINE not         #-}; not         :: N.Name; not         = [N.ascii|not|]
{-# NOINLINE negate      #-}; negate      :: N.Name; negate      = [N.ascii|negate|]
{-# NOINLINE toFloat     #-}; toFloat     :: N.Name; toFloat     = [N.ascii|toFloat|]
{-# NOINLINE truncate    #-}; truncate    :: N.Name; truncate    = [N.ascii|truncate|]
{-# NOINLINE append      #-}; append      :: N.Name; append      = [N.ascii|append|]
{-# NOINLINE apL         #-}; apL         :: N.Name; apL         = [N.ascii|apL|]
{-# NOINLINE apR         #-}; apR         :: N.Name; apR         = [N.ascii|apR|]
{-# NOINLINE add         #-}; add         :: N.Name; add         = [N.ascii|add|]
{-# NOINLINE sub         #-}; sub         :: N.Name; sub         = [N.ascii|sub|]
{-# NOINLINE mul         #-}; mul         :: N.Name; mul         = [N.ascii|mul|]
{-# NOINLINE fdiv        #-}; fdiv        :: N.Name; fdiv        = [N.ascii|fdiv|]
{-# NOINLINE idiv        #-}; idiv        :: N.Name; idiv        = [N.ascii|idiv|]
{-# NOINLINE eq          #-}; eq          :: N.Name; eq          = [N.ascii|eq|]
{-# NOINLINE neq         #-}; neq         :: N.Name; neq         = [N.ascii|neq|]
{-# NOINLINE lt          #-}; lt          :: N.Name; lt          = [N.ascii|lt|]
{-# NOINLINE gt          #-}; gt          :: N.Name; gt          = [N.ascii|gt|]
{-# NOINLINE le          #-}; le          :: N.Name; le          = [N.ascii|le|]
{-# NOINLINE ge          #-}; ge          :: N.Name; ge          = [N.ascii|ge|]
{-# NOINLINE or          #-}; or          :: N.Name; or          = [N.ascii|or|]
{-# NOINLINE and         #-}; and         :: N.Name; and         = [N.ascii|and|]
{-# NOINLINE xor         #-}; xor         :: N.Name; xor         = [N.ascii|xor|]
{-# NOINLINE remainderBy #-}; remainderBy :: N.Name; remainderBy = [N.ascii|remainderBy|]

{-# NOINLINE cmp      #-}; cmp      :: N.Name; cmp      = [N.ascii|cmp|]
{-# NOINLINE ap       #-}; ap       :: N.Name; ap       = [N.ascii|ap|]
{-# NOINLINE valueOf  #-}; valueOf  :: N.Name; valueOf  = [N.ascii|valueOf|]


equal :: JS.Expr -> JS.Expr -> JS.Expr
equal left right =
  if isLiteral left || isLiteral right then
    strictEq left right
  else
    JS.Call (JS.Ref (JsName.fromKernel Module.kernel_utils eq)) [left, right]


notEqual :: JS.Expr -> JS.Expr -> JS.Expr
notEqual left right =
  if isLiteral left || isLiteral right then
    strictNEq left right
  else
    JS.Prefix JS.PrefixNot $
      JS.Call (JS.Ref (JsName.fromKernel Module.kernel_utils eq)) [left, right]


comp :: JS.InfixOp -> JS.InfixOp -> Int -> JS.Expr -> JS.Expr -> JS.Expr
comp idealOp backupOp backupInt left right =
  if isLiteral left || isLiteral right then
    JS.Infix idealOp left right
  else
    JS.Infix backupOp
      (JS.Call (JS.Ref (JsName.fromKernel Module.kernel_utils cmp)) [left, right])
      (JS.Int backupInt)




isLiteral :: JS.Expr -> Bool
isLiteral expr =
  case expr of
    JS.String _ -> True
    JS.Float  _ -> True
    JS.Int    _ -> True
    JS.Bool   _ -> True
    _           -> False


apply :: Opt.Expr -> Opt.Expr -> Opt.Expr
apply func value =
  case func of
    Opt.Accessor field -> Opt.Access value field
    Opt.Call f x xs    -> Opt.Call f x (xs ++ [value])
    _                  -> Opt.Call func value []


genAppend :: Mode.Mode -> FnArgLookup -> Opt.Expr -> Opt.Expr -> JS.Expr
genAppend mode argLookup left right =
  if any isStringLiteral seqs
    then foldr1 (JS.Infix JS.OpAdd) seqs
    else foldr1 utils_ap seqs
  where
    seqs =
      genJsExpr mode argLookup left : toSeqs mode argLookup right

    utils_ap a b =
      JS.Call (JS.Ref (JsName.fromKernel Module.kernel_utils ap)) [a, b]


toSeqs :: Mode.Mode -> FnArgLookup -> Opt.Expr -> [JS.Expr]
toSeqs mode argLookup expr =
  case expr of
    Opt.Call (Opt.VarGlobal (Opt.Global h n)) left [right]
      | h == ModuleName.basics && n == append ->
          genJsExpr mode argLookup left : toSeqs mode argLookup right

    _ ->
      [genJsExpr mode argLookup expr]


isStringLiteral :: JS.Expr -> Bool
isStringLiteral expr =
  case expr of
    JS.String _ -> True
    _           -> False



-- SIMPLIFY INFIX OPERATORS


strictEq :: JS.Expr -> JS.Expr -> JS.Expr
strictEq left right =
  case left of
    JS.Int  0 -> JS.Prefix JS.PrefixNot right
    JS.Bool b -> if b then right else JS.Prefix JS.PrefixNot right
    _ ->
      case right of
        JS.Int  0 -> JS.Prefix JS.PrefixNot left
        JS.Bool b -> if b then left else JS.Prefix JS.PrefixNot left
        _         -> JS.Infix JS.OpEq left right


strictNEq :: JS.Expr -> JS.Expr -> JS.Expr
strictNEq left right =
  case left of
    JS.Int  0 -> JS.Prefix JS.PrefixNot (JS.Prefix JS.PrefixNot right)
    JS.Bool b -> if b then JS.Prefix JS.PrefixNot right else right
    _ ->
      case right of
        JS.Int  0 -> JS.Prefix JS.PrefixNot (JS.Prefix JS.PrefixNot left)
        JS.Bool b -> if b then JS.Prefix JS.PrefixNot left else left
        _         -> JS.Infix JS.OpNe left right



-- TAIL CALL


-- TODO check if JS minifiers collapse unnecessary temporary variables
--
-- @LAMDERA Note: we've removed unnecessary values in tail calls, see the `alternativeImplementation` above
-- (so this takes all the assignments as one list, possibly empty)
--
genTailCall :: Mode.Mode -> FnArgLookup -> N.Name -> [Opt.TailAssign] -> [JS.Stmt]
genTailCall mode argLookup name args =
  let
    toTempVars (Opt.TailAssign n e) =
      (JsName.makeTemp n, genJsExpr mode argLookup e)

    toRealVars (Opt.TailAssign n _) =
      JS.ExprStmt $ JS.Assign (JS.LRef (JsName.fromLocal n)) (JS.Ref (JsName.makeTemp n))
  in
  JS.Vars (map toTempVars args)
  : map toRealVars args
  ++ [ JS.Continue (Just (JsName.fromLocal name)) ]



-- DEFINITIONS


genDef :: Mode.Mode -> FnArgLookup -> Opt.Def -> JS.Stmt
genDef mode argLookup def =
  case def of
    Opt.Def     n      b -> JS.Var (JsName.fromLocal n) (genJsExpr mode argLookup b)
    Opt.TailDef n x xs b -> JS.Var (JsName.fromLocal n) (codeToExpr (genTailDef mode argLookup n x xs b))


genTailDef :: Mode.Mode -> FnArgLookup -> N.Name -> N.Name -> [N.Name] -> Opt.Expr -> Code
genTailDef mode argLookup name x xs body =
  genFunction (map JsName.fromLocal (x:xs)) $ JsBlock $
    [ JS.Labelled (JsName.fromLocal name) $
        JS.While (JS.Bool True) $
          codeToStmt $ generate mode argLookup body
    ]



-- PATHS


genPath :: Mode.Mode -> Opt.Path -> JS.Expr
genPath mode path =
  case path of
    Opt.Root  n   -> JS.Ref (JsName.fromLocal n)
    Opt.Index i p -> JS.Access (genPath mode p) (JsName.fromIndex i)
    Opt.Field f p -> JS.Access (genPath mode p) (genField mode f)
    Opt.Unbox p ->
      case mode of
        Mode.Dev  _ -> JS.Access (genPath mode p) (JsName.fromIndex Index.first)
        Mode.Prod _ -> genPath mode p



-- GENERATE IFS


genIf :: Mode.Mode -> FnArgLookup -> [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> Code
genIf mode argLookup givenBranches givenFinal =
  let
    (branches, final) =
      crushIfs givenBranches givenFinal

    convertBranch (condition, expr) =
      ( genJsExpr mode argLookup condition
      , generate mode argLookup expr
      )

    branchExprs = map convertBranch branches
    finalCode = generate mode argLookup final
  in
  if isBlock finalCode || any (isBlock . snd) branchExprs then
    JsBlock [ foldr addStmtIf (codeToStmt finalCode) branchExprs ]
  else
    JsExpr $ foldr addExprIf (codeToExpr finalCode) branchExprs


addExprIf :: (JS.Expr, Code) -> JS.Expr -> JS.Expr
addExprIf (condition, branch) final =
  JS.If condition (codeToExpr branch) final


addStmtIf :: (JS.Expr, Code) -> JS.Stmt -> JS.Stmt
addStmtIf (condition, branch) final =
  JS.IfStmt condition (codeToStmt branch) final


isBlock :: Code -> Bool
isBlock code =
  case code of
    JsBlock _ -> True
    JsExpr  _ -> False


crushIfs :: [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> ([(Opt.Expr, Opt.Expr)], Opt.Expr)
crushIfs branches final =
  crushIfsHelp [] branches final


crushIfsHelp
    :: [(Opt.Expr, Opt.Expr)]
    -> [(Opt.Expr, Opt.Expr)]
    -> Opt.Expr
    -> ([(Opt.Expr, Opt.Expr)], Opt.Expr)
crushIfsHelp visitedBranches unvisitedBranches final =
  case unvisitedBranches of
    [] ->
        case final of
          Opt.If subBranches subFinal ->
              crushIfsHelp visitedBranches subBranches subFinal

          _ ->
              (reverse visitedBranches, final)

    visiting : unvisited ->
        crushIfsHelp (visiting : visitedBranches) unvisited final



-- CASE EXPRESSIONS


genCase :: Mode.Mode -> FnArgLookup -> N.Name -> N.Name -> Opt.Decider Opt.Choice -> [(Int, Opt.Expr)] -> [JS.Stmt]
genCase mode argLookup label root decider jumps =
  foldr (goto mode argLookup label) (genDecider mode argLookup label root decider) jumps


goto :: Mode.Mode -> FnArgLookup -> N.Name -> (Int, Opt.Expr) -> [JS.Stmt] -> [JS.Stmt]
goto mode argLookup label (index, branch) stmts =
  let
    labeledDeciderStmt =
      JS.Labelled
        (JsName.makeLabel label index)
        (JS.While (JS.Bool True) (JS.Block stmts))
  in
  labeledDeciderStmt : codeToStmtList (generate mode argLookup branch)


genDecider :: Mode.Mode -> FnArgLookup -> N.Name -> N.Name -> Opt.Decider Opt.Choice -> [JS.Stmt]
genDecider mode argLookup label root decisionTree =
  case decisionTree of
    Opt.Leaf (Opt.Inline branch) ->
      codeToStmtList (generate mode argLookup branch)

    Opt.Leaf (Opt.Jump index) ->
      [ JS.Break (Just (JsName.makeLabel label index)) ]

    Opt.Chain testChain success failure ->
      [ JS.IfStmt
          (List.foldl1' (JS.Infix JS.OpAnd) (map (genIfTest mode root) testChain))
          (JS.Block $ genDecider mode argLookup label root success)
          (JS.Block $ genDecider mode argLookup label root failure)
      ]

    Opt.FanOut path edges fallback ->
      [ JS.Switch
          (genCaseTest mode root path (fst (head edges)))
          ( foldr
              (\edge cases -> genCaseBranch mode argLookup label root edge : cases)
              [ JS.Default (genDecider mode argLookup label root fallback) ]
              edges
          )
      ]


genIfTest :: Mode.Mode -> N.Name -> (DT.Path, DT.Test) -> JS.Expr
genIfTest mode root (path, test) =
  let
    value = pathToJsExpr mode root path
  in
  case test of
    DT.IsVariant h n t _ ->
      strictEq (JS.Access value JsName.dollar) $
        case mode of
          Mode.Dev  _ -> JS.String (N.toBuilder n)
          Mode.Prod _ -> JS.Int (tagToInt h n t)

    DT.IsEnum h n t _ ->
      case mode of
        Mode.Dev  _ -> strictEq (JS.Access value JsName.dollar) (JS.String (N.toBuilder n))
        Mode.Prod _ -> strictEq value (JS.Int (tagToInt h n t))

    DT.IsBool True  -> value
    DT.IsBool False -> JS.Prefix JS.PrefixNot value
    DT.IsInt i      -> strictEq value (JS.Int i)

    DT.IsChr char ->
      strictEq (JS.String (P.primBounded charUtf8 char)) $
        case mode of
          Mode.Dev _ -> JS.Call (JS.Access value (JsName.fromLocal valueOf)) []
          Mode.Prod _ -> value

    DT.IsStr string ->
      strictEq value (JS.String (Utf8.toBuilder string))

    DT.IsCons ->
      JS.Access value (JsName.fromLocal N.b)

    DT.IsNil ->
      JS.Prefix JS.PrefixNot $
        JS.Access value (JsName.fromLocal N.b)


genCaseBranch :: Mode.Mode -> FnArgLookup -> N.Name -> N.Name -> (DT.Test, Opt.Decider Opt.Choice) -> JS.Case
genCaseBranch mode argLookup label root (test, subTree) =
  JS.Case
    (genCaseValue mode test)
    (genDecider mode argLookup label root subTree)


genCaseValue :: Mode.Mode -> DT.Test -> JS.Expr
genCaseValue mode test =
  case test of
    DT.IsVariant h n t _ ->
      case mode of
        Mode.Dev  _ -> JS.String (N.toBuilder n)
        Mode.Prod _ -> JS.Int (tagToInt h n t)

    DT.IsEnum h n t _ ->
      case mode of
        Mode.Dev  _ -> JS.String (N.toBuilder n)
        Mode.Prod _ -> JS.Int (tagToInt h n t)

    DT.IsInt  i -> JS.Int i
    DT.IsChr  c -> JS.String (P.primBounded charUtf8 c)
    DT.IsStr  s -> JS.String (Utf8.toBuilder s)
    DT.IsBool _ -> $(Crash.crash 'genCaseValue) "COMPILER BUG - there should never be three tests on a boolean"
    DT.IsCons   -> $(Crash.crash 'genCaseValue) "COMPILER BUG - there should never be three tests on a list"
    DT.IsNil    -> $(Crash.crash 'genCaseValue) "COMPILER BUG - there should never be three tests on a list"


genCaseTest :: Mode.Mode -> N.Name -> DT.Path -> DT.Test -> JS.Expr
genCaseTest mode root path exampleTest =
  let
    value = pathToJsExpr mode root path
  in
  case exampleTest of
    DT.IsVariant _ _ _ _ ->
      JS.Access value JsName.dollar

    DT.IsEnum _ _ _ _ ->
      case mode of
        Mode.Dev  _ -> JS.Access value JsName.dollar
        Mode.Prod _ -> value

    DT.IsInt _ -> value
    DT.IsStr _ -> value
    DT.IsChr _ ->
      case mode of
        Mode.Dev  _ -> JS.Call (JS.Access value (JsName.fromLocal valueOf)) []
        Mode.Prod _ -> value

    DT.IsBool _ -> $(Crash.crash 'genCaseTest) "COMPILER BUG - there should never be three tests on a list"
    DT.IsCons   -> $(Crash.crash 'genCaseTest) "COMPILER BUG - there should never be three tests on a list"
    DT.IsNil    -> $(Crash.crash 'genCaseTest) "COMPILER BUG - there should never be three tests on a list"



-- PATTERN PATHS


pathToJsExpr :: Mode.Mode -> N.Name -> DT.Path -> JS.Expr
pathToJsExpr mode root path =
  case path of
    DT.Index i p ->
      JS.Access (pathToJsExpr mode root p) (JsName.fromIndex i)

    DT.Unbox p ->
      case mode of
        Mode.Dev  _ -> JS.Access (pathToJsExpr mode root p) (JsName.fromIndex Index.first)
        Mode.Prod _ -> pathToJsExpr mode root p

    DT.Empty ->
      JS.Ref (JsName.fromLocal root)



-- GENERATE CHAR


charUtf8 :: P.BoundedPrim Char
charUtf8 =
    P.condB (== '\\') (esc 0x5C) $
    P.condB (== '\'') (esc 0x27) $
    P.condB (>= ' ' ) P.charUtf8 $
    P.condB (== '\b') (esc 0x62) $
    P.condB (== '\f') (esc 0x66) $
    P.condB (== '\n') (esc 0x6E) $
    P.condB (== '\r') (esc 0x72) $
    P.condB (== '\t') (esc 0x74) $ fallback
  where
    {-# INLINE esc #-}
    esc w =
      P.liftFixedToBounded $ const (0x5C,w) >$< P.word8 >*< P.word8

    {-# INLINE fallback #-}
    fallback =
      P.liftFixedToBounded $ (\c -> (0x5C,(0x75,(0x30,(0x30,toFallback c))))) >$<
        P.word8 >*< P.word8 >*< P.word8 >*< P.word8 >*< P.word8 >*< P.word8

    toFallback char =
        if w < 0x10
        then (0x30, if w < 0x0a then w + 0x30 else w + 0x57)
        else (0x31, if w < 0x1a then w + 0x20 else w + 0x47)
      where
        w = fromIntegral (Char.ord char)



-- GENERATE MAIN


genMain :: Mode.Mode -> FnArgLookup -> ModuleName.Canonical -> Opt.Main -> JS.Expr
genMain mode argLookup home main =
  case main of
    Opt.Static ->
      JS.Ref (JsName.fromKernel Module.kernel_vdom [N.ascii|init|])
        # JS.Ref (JsName.fromGlobal home N.main)
        # JS.Int 0
        # JS.Int 0

    Opt.Dynamic msgArgs msgType decoder ->
      JS.Ref (JsName.fromGlobal home N.main)
        # genJsExpr mode argLookup decoder
        # toDebugMetadata mode msgArgs msgType


(#) :: JS.Expr -> JS.Expr -> JS.Expr
(#) func arg =
  JS.Call func [arg]


toDebugMetadata :: Mode.Mode -> A.Array T.Var -> I.Type -> JS.Expr
toDebugMetadata mode msgArgs msgType =
  case mode of
    Mode.Prod _ ->
      JS.Int 0

    Mode.Dev Nothing ->
      JS.Int 0

    Mode.Dev (Just interfaces) ->
      JS.Json $ Encode.object $
        [ "versions" ==> Encode.object [ "elm" ==> V.encode V.compiler ]
        , "types"    ==> Type.encodeMetadata (Extract.fromMsg interfaces msgArgs msgType)
        ]



-- @LAMDERA


-- A constructor with more than one argument as a plain JS function of all of them,
-- for direct calls (see Generate.JavaScript.ctor). Builds the same object as
-- genVariant / genVector.
genCtorImplementation :: Mode.Mode -> Opt.Global -> Maybe Tag.Tag -> Int -> Code
genCtorImplementation mode global@(Opt.Global home name) maybeTag arity =
  let
    argNames =
      Index.indexedMap (\i _ -> JsName.fromIndex i) [1 .. arity]

    fields =
      case maybeTag of
        Just tag ->
          let
            ctorTag =
              case mode of
                Mode.Dev _ -> JS.String (N.toBuilder name)
                Mode.Prod _ -> JS.Int (tagToInt home name tag)
          in
          (JsName.dollar, ctorTag) : map (\n -> (n, JS.Ref n)) argNames

        Nothing ->
          vectorFields mode global argNames
  in
  JsExpr $
    JS.Function Nothing argNames $
      codeToStmtList $ JsExpr $ JS.Object fields


genCurriedFunctionRef :: [JsName.Name] -> JsName.Name -> Code
genCurriedFunctionRef args ref =
  case IntMap.lookup (length args) funcHelpers of
    Just helper -> JsExpr $ JS.Call helper [ JS.Ref ref ]
    Nothing ->
      let
        addArg arg code =
          JsExpr $ JS.Function Nothing [arg] $ codeToStmtList code
      in
      foldr addArg (JsExpr $ JS.Call (JS.Ref ref) (map JS.Ref args)) args


genFunctionImplementation :: Mode.Mode -> FnArgLookup -> [N.Name] -> Opt.Expr -> Code
genFunctionImplementation mode argLookup argNames body =
  JsExpr $
    JS.Function Nothing (map JsName.fromLocal argNames) $
      codeToStmtList $
        generate mode argLookup body


genTailDefImplementation :: Mode.Mode -> FnArgLookup -> N.Name -> [N.Name] -> Opt.Expr -> Code
genTailDefImplementation mode argLookup name argNames body =
  JsExpr $ JS.Function Nothing (map JsName.fromLocal argNames) $
    codeToStmtList $ JsBlock $
      [ JS.Labelled (JsName.fromLocal name) $
          JS.While (JS.Bool True) $
            codeToStmt $ generate mode argLookup body
      ]
