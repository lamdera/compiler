{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Lamdera.Evaluate.Optimized where

import qualified Data.List as List
import Data.Int (Int64)
import Text.Read (readMaybe)
import Data.Ord

import Data.STRef
import Control.Monad.ST

import AST.Optimized
import qualified Elm.ModuleName as Module
import qualified Data.Name
import qualified Data.Utf8 as Utf8

import Elm.Package
import qualified Reporting.Annotation as A
import qualified Elm.ModuleName as Module
import Lamdera
import Lamdera.Wire3.Helpers
import StandaloneInstances

import Lamdera
import Data.Map ((!))
import Data.Set (fromList)
import qualified Data.Map as Map
import qualified Lamdera.Compile
import Ext.Common hiding ((&), atomicPutStrLn)

import qualified Elm.ModuleName as Module

import qualified Elm.Package as Pkg
import qualified Data.Name
import qualified Data.Text as T
import qualified AST.Optimized as Opt

import qualified Elm.Float

{-

  addOne x =
    x + 1

-}
exampleDef =
  -- Def (a ("addOne"))
  --   [(a (PVar "x"))]
  --   (a (Binop
  --         "+"
  --         (Module.Canonical (Name "elm" "core") "Basics")
  --         "add"
  --         (Forall (Map.fromList [("number", ())]) (TLambda (TVar "number") (TLambda (TVar "number") (TVar "number"))))
  --         (a (VarLocal "x"))
  --         (a (Int 1))))
  Define
      (Function ["x"] (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "Basics") "add")) [VarLocal "x", Int 1]))
      (fromList [Global (Module.Canonical (Name "elm" "core") "Basics") "add"])


-- test x =
--   run exampleDef (Map.fromList [("x", Int x)])


run def args nodes =
  case def of
    Define expr funcDeps ->
      evalExpr expr args nodes


type Locals = Map.Map Data.Name.Name Expr
type Globals = Map.Map Global Node

-- call def args =
--   evalExpr (Call def args)

pFloat :: Elm.Float.Float -> Maybe Double
pFloat = readMaybe . Utf8.toChars


evalExpr :: Expr -> Locals -> Globals -> Expr
evalExpr expr locals globals =
  -- case debugPass "expr" (expr, locals) expr of
  -- debugHaskell (show_ expr) $
  -- debugEval expr $
   case expr of
    -- (Call (VarGlobal (Global (Module.Canonical (Name "author" "project") "Test.Basic") "addOne")) args_) ->
    --     let
    --       inlined = (Function ["x"] (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "Basics") "add")) [VarLocal "x", Int 1]))
    --     in
    --     evalExpr (Call inlined args_) (locals) globals

    (VarGlobal (Global (Module.Canonical (Name "author" "project") "Eval") "timed")) ->
      -- @TODO Can we shortcut unecessary redirects/inlinings?
      VarKernel "Eval" "timed"

    (Call (VarKernel "Eval" "timed") [arg1]) ->
      let (s, Str v) = track_ "timed" arg1
      in
      Str $ Utf8.join 0x20 [v, Utf8.fromChars s]

    (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "Basics") "add")) [arg1, arg2]) ->
      case [evalExpr arg1 locals globals, evalExpr arg2 locals globals] of
        [Int a, Int b] -> Int $ a + b
        [Float a, Float b] -> do
          case (pFloat a, pFloat b) of
            (Just a_, Just b_) -> Float $ Utf8.fromChars $ show $ a_ + b_
            _ -> error $ "impossible failure to parse floats: " <> show a <> ", " <> show b
        [Int a, Float b] -> do
          case pFloat b of
            Just b_ -> Float $ Utf8.fromChars $ show $ (fromIntegral a) + b_
            _ -> error $ "impossible failure to parse float: " <> show b
        [Float a, Int b] -> do
          case pFloat a of
            Just a_ -> Float $ Utf8.fromChars $ show $ a_ + (fromIntegral b)
            _ -> error $ "impossible failure to parse float: " <> show a

        evaled -> error $ "unhandled Basics.add: " <> show evaled

    (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "Basics") "sub")) [arg1, arg2]) ->
      case [evalExpr arg1 locals globals, evalExpr arg2 locals globals] of
        [Int a, Int b] -> Int $ a - b
        [Float a, Float b] -> do
          case (pFloat a, pFloat b) of
            (Just a_, Just b_) -> Float $ Utf8.fromChars $ show $ a_ - b_
            _ -> error $ "impossible failure to parse floats: " <> show a <> ", " <> show b
        [Int a, Float b] -> do
          case pFloat b of
            Just b_ -> Float $ Utf8.fromChars $ show $ (fromIntegral a) - b_
            _ -> error $ "impossible failure to parse float: " <> show b
        [Float a, Int b] -> do
          case pFloat a of
            Just a_ -> Float $ Utf8.fromChars $ show $ a_ - (fromIntegral b)
            _ -> error $ "impossible failure to parse float: " <> show a

        evaled -> error $ "unhandled Basics.sub: " <> show evaled


    (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "Basics") "eq")) [arg1, arg2]) ->
      case [evalExpr arg1 locals globals, evalExpr arg2 locals globals] of
        [a, b] -> Bool $ a == b

    (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "Basics") "neq")) [arg1, arg2]) ->
      case [evalExpr arg1 locals globals, evalExpr arg2 locals globals] of
        [a, b] -> Bool $ a /= b

    (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "Basics") "append")) [arg1, arg2]) ->
      case [evalExpr arg1 locals globals, evalExpr arg2 locals globals] of
        [Str a, Str b] -> Str $ Utf8.fromChars $ Utf8.toChars a <> Utf8.toChars b

    (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "Basics") "apL")) [arg1, arg2]) ->
      let
        left = evalExpr arg1 locals globals
        right = evalExpr arg2 locals globals
      in
      -- debugHaskellPass "apL" ([arg1, arg2], left, right) $
        evalExpr (Call left [right]) locals globals


    (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "List") "repeat")) [arg1, arg2]) ->
      case [evalExpr arg1 locals globals, evalExpr arg2 locals globals] of
        [Int a, b] -> List $ take a $ repeat b


    -- (Call (VarGlobal (Global (Module.Canonical (Name "elm" "core") "List") "foldl")) [arg1, arg2, arg3]) ->
    --   error "got it!"
    --
    --   foldl


    (Call (VarGlobal global) args_) ->

      let args = args_ & fmap (applyVarLocals locals)
      -- let inlined = getNodeExpr global globals
      in
      -- debugHaskell (show_ (inlined, args_)) $
      -- evalExpr (Call inlined args_) locals globals
      case Map.lookup global globals of
        Just node ->
          case node of
            Define expr funcDeps ->
              evalExpr (Call (applyVarLocals locals expr) args_) locals globals

            Link global_ ->
              -- Recurse again to find the linked global
              evalExpr (Call (VarGlobal global_) args_) locals globals

            -- Cycle [Name] [(Name, Expr)] [Def] (Set.Set Global)
            Cycle [name] _unknown [TailDef name_ pnames expr_] _funcDeps ->
              handleCyclePlain node args globals

            Ctor index numParams ->
              -- We've got a custom type constructor with args, so just apply the args and return
              -- as there's no other way to represent an applied constructor...
              -- @TODO this seems bad.
              (Call (VarGlobal global) args)

            _ ->
              error $ "unimplemented VarGlobal lookup:\n" <> T.unpack (hindentFormatValue (node, locals, args_)) <>
                      "\nin the expression:\n" <> T.unpack (hindentFormatValue expr)


        Nothing ->
          error $ "Global not found in globals:\n"<> show global <> "\n\n "-- <> show globals


    (Call expr args_) ->

      let args = args_ & fmap (applyVarLocals locals)
      in
      case expr of
        Function pnames expr_ ->
          if length pnames /= length args then
            -- This means we're currying
            let (locals_, pnames_) = zipRem pnames args
            in
            (Function pnames_ (applyVarLocals (locals_ & Map.fromList) expr_))
            -- error $ "lengths do not match! " <> (T.unpack $ hindentFormatValue (pnames, args))
          else
            evalExpr expr_ (zip pnames args & Map.fromList) globals

        Call expr_ args_ ->
          -- We have all the args we need to reduce the full expression
          evalExpr (Call (expr_ & applyVarLocals locals) args_) locals globals

        If conditionBranches elseBranch ->
          evalExpr (If conditionBranches elseBranch) locals globals


        -- @TODO if we lift these to the wrapper basics invocations, we can skip some fn calls:
        -- https://github.com/elm/core/blob/1.0.5/src/Basics.elm#L79
        VarKernel "Utils" "le" ->
          case args of
            [a1, a2] -> Bool $ a1 <= a2

        VarKernel "List" "cons" ->
          case args of
            [a1, List a2] ->
              List (a1 : a2)
              -- case evalExpr a2 locals globals of
              --   List v2 ->
              --     List (evalExpr a1 locals globals : v2)
              --
              --   r -> error $ "VarKernel List.cons unexpected 2nd arg reduction: \n" <> (T.unpack $ hindentFormatValue (expr, a1, a2, locals))

            _ -> error $ "VarKernel List.cons unexpected args: \n" <> (T.unpack $ hindentFormatValue (expr, args))


        _ ->
          error $ "call(expr) not handled: \n" <> (T.unpack $ hindentFormatValue (expr, args))

    VarLocal n ->
      case Map.lookup n locals of
        Just v -> evalExpr v Map.empty globals
        Nothing ->
          -- Could be a curried function!
          expr
          -- error $ "'VarLocal "<> show n <> "' not found in locals: " <> show locals

    VarGlobal global ->
      expr

    Int n -> expr
    Float n -> expr
    Str s -> expr
    Bool b -> expr

    List exprs -> exprs & fmap (\expr -> evalExpr expr locals globals) & List

    If conditionBranches elseBranch ->
      conditionBranches
        & List.find (\(condition, def) ->
          -- debugPass "evaluating on" (condition, locals) $
            evalExpr condition locals globals == Bool True
        )
        & (\defM ->
          case defM of
            Just (_, def) ->
              -- debugPass "evaluating on" def $
                evalExpr def locals globals
            Nothing ->
              -- debugPass "passing to else" () $
                evalExpr elseBranch locals globals
        )

    TailCall name args ->
      -- TailCalls are handled specially by caller, so just reduce the args expressions and return
      args
        & fmap (\(name, expr) -> (name, evalExpr expr locals globals))
        & TailCall name

    VarEnum global index ->
      expr

    -- Case Name Name (Decider Choice) [(Int, Expr)]
    -- data Choice
    --   = Inline Expr
    --   | Jump Int
    -- data Decider a
    --   = Leaf a
    --   | Chain
    --       { _testChain :: [(DT.Path, DT.Test)]
    --       , _success :: Decider a
    --       , _failure :: Decider a
    --       }
    --   | FanOut
    --       { _path :: DT.Path
    --       , _tests :: [(DT.Test, Decider a)]
    --       , _fallback :: Decider a
    --       }
    --   deriving (Eq)
    Case var name decider jumps ->
      error $ "case statement:\n" <> (T.unpack $ hindentFormatValue expr)


    -- case blah of
    --   0 ->
      -- (x,y) ->
      -- (x, x:xs) ->
      -- (x, (y,z,k)) ->
      -- SomeType x y z p "x" f k y (",") n a b ->
      -- (x, (y,z, (y,z,""), (y,z,""), (y,z,""), (y,z,""), (y,z,""), (y,z,""), (y,z,""), (y,z,""))) ->
      -- x:xs ->
      -- x:xs:[] ->

    _ ->
      error $ "evaluate unimplemented:\n" <> (T.unpack $ hindentFormatValue expr)


applyVarLocals :: Locals -> Expr -> Expr
applyVarLocals locals expr =
  case expr of
    VarLocal n ->
      case Map.lookup n locals of
        Just v -> v
        Nothing ->
          -- Could be a curried function!
          expr

    VarGlobal _ -> expr
    Str _ -> expr
    Int _ -> expr
    Float _ -> expr
    Bool _ -> expr

    List vals -> List (vals & fmap (applyVarLocals locals))

    (Call expr args) ->
      Call (applyVarLocals locals expr) (fmap (applyVarLocals locals) args)

    If conditionBranches elseBranch ->
      If
        (conditionBranches & fmap (\(condition,branch) -> (applyVarLocals locals condition, applyVarLocals locals branch)))
        (elseBranch & applyVarLocals locals )

    Function pnames expr_ ->
      Function pnames (expr_ & applyVarLocals locals)

    VarKernel m n -> expr

    TailCall name args ->
      TailCall name (args & fmap (\(n,v) -> (n, applyVarLocals locals v)))

    -- | VarEnum Global Index.ZeroBased
    -- VarEnum global index ->
    --   expr

    _ ->
      error $ "applyVarLocals missing coverage: " <> show expr


getNodeExpr :: Global -> Globals -> Expr
getNodeExpr global globals =
  case Map.lookup global globals of
    Just node ->
      case node of
        Define expr funcDeps ->
          expr

        Link global_ ->
          getNodeExpr global_ globals

        -- Cycle [Name] [(Name, Expr)] [Def] (Set.Set Global)
        -- Cycle [name] unknown [def] funcDeps ->
        --   getDefExpr def

        _ ->
          error $ "unimplemented getDef node: " <> T.unpack (hindentFormatValue node)

    Nothing ->
      error $ "Global not found in globals:\n"<> show global <> "\n\n "-- <> show globals


getDefExpr :: Def -> Expr
getDefExpr def =
  case def of
    Def name expr -> expr
    -- @TODO almost certainly need to optimise this
    TailDef name tailDefArgs expr -> expr


-- Zip first list with the second, returning all non-zipped items
-- from the first (assumed always longer) list
zipRem :: [a] -> [b] -> ([(a, b)], [a])
zipRem l1 l2 =
  let zipped = zip l1 l2
  in
  (zipped, drop (length zipped) l1)


-- https://package.elm-lang.org/packages/elm/core/latest/Basics#comparison
instance Ord Expr where
  compare v1 v2 =
    case [v1,v2] of
      -- | Chr ES.String
      [Chr v1, Chr v2] ->
        error "todo: instance Ord Expr for Chr"
      -- | Str ES.String
      [Str v1, Str v2] ->
        error "todo: instance Ord Expr for Str"
      -- | Int Int
      [Int v1, Int v2] ->
        compare v1 v2
      -- | Float EF.Float
      [Float v1, Float v2] ->
        error "todo: instance Ord Expr for Float"
      -- | List [Expr]
      [List v1, List v2] ->
        error "todo: instance Ord Expr for List"

      _ -> EQ

      -- @TODO missing Time.Posix?



handleCyclePlain :: Node -> [Expr] -> Globals -> Expr
handleCyclePlain node args globals =
  case node of
    Cycle [name] _unknown [TailDef name_ pnames expr_] _funcDeps ->
      let
        tailDefResult args__ =
          let
            res =
              -- @TODO is it ever possible to have curried TailDef calls?
              if length pnames /= length args__ then
                error "TODO: Curried Cycle!"
              else
                let locals_ = (args__ & zip pnames & Map.fromList)
                in
                evalExpr expr_ locals_ globals
          in
          case res of
            TailCall _ newArgs ->
              tailDefResult (newArgs & fmap snd)
            _ -> res
      in
      tailDefResult args


{- A naive implementation of tail recursion using stRefs, seems to have no
   benefit over the handleCyclePlain, probably because we still recurse and
   still allocate for all the new args values.

   Needs more research as to whether a tail-recursive function has some natural
   transformation into a performant version we can do at this level.
-}
handleCycleStRef :: Node -> [Expr] -> Globals -> Expr
handleCycleStRef node args globals =
  case node of
    Cycle [name] _unknown [TailDef name_ pnames expr_] _funcDeps ->
      runST $ do
        args_ <- newSTRef args
        let
          tailDefResult stArgs = do
            args__ <- readSTRef stArgs
            let
              res =
                -- @TODO is it ever possible to have curried TailDef calls?
                if length pnames /= length args__ then
                  error "TODO: Curried Cycle!"
                else
                  let locals_ = (args__ & zip pnames & Map.fromList)
                  in
                  evalExpr expr_ locals_ globals
            case res of
              TailCall _ newArgs -> do
                writeSTRef stArgs (newArgs & fmap snd)
                tailDefResult stArgs
              _ ->
                pure res

        tailDefResult args_

    _ -> error $ "handleCyclePlain: unexpected node: " <> T.unpack (hindentFormatValue node)


{- A nice-ish debug display format for from -> to transformations
-}
debugEval :: (Show a, Show b) => a -> b -> b
debugEval from to =
  unsafePerformIO $ do
    f <- hindent from
    t <- hindent to
    atomicPutStrLn $ T.unpack $
      "\n🔶\n"
        <> f
        <> "➡️\n"
        <> t
    pure to
