module Simple exposing (..)

import Bitwise
import Browser
import Dict
import Html



-- ### changes in 0.19 ###
-- cannot expose specific constructors anymore; it's all `T(..)` or nothing `T`
-- no more variable shadowing, anywhere
-- top-level record destructoring removed; intentional or not?


unitconstant =
  -- Define Unit (fromList [elm/kernel~Utils.$]
  ()


floatconstant =
  -- Define (Float 0.19) (fromList []))
  0.19


funcWithArgs arg1 arg2 =
  -- Define (Function [arg1,arg2] (VarLocal arg1)) (fromList []))
  arg1 floatconstant


otherFuncWithArgs arg1 =
  funcWithArgs arg1 ()


recSum x =
  if x == 0 then
    0

  else
    x + recSum (x - 1)



{-
   main =
     Browser.sandbox
       { init = ()
       , view = \() -> Html.text ("hi")
       , update =
           \m mod ->
             let
             {-
               a1 =
                 unitconstant

               a2 =
                 floatconstant

               a3 =
                 funcWithArgs

               a4 =
                 otherFuncWithArgs

               a5 =
                 recSum

               a7 =
                 trec

               a8 =
                 tailCallFn

               a9 =
                 tailCallFnNested

               a10 =
                 varOp

               a11 =
                 both

               -- a12 = debugVar
               a13 =
                 letexpr

               -- a14 = letrec
               a15 =
                 boolPattern

               -- a16 = recDef1
               -- a17 = recDef2
               -- a18 = letdestruct
               -}
               a19 =
                 recordPatternArg
               a20 =
                 complexRecordPatternArg
             in
             mod
       }
-}


trec n accumulator =
  if n == 0 then
    accumulator

  else
    trec (n - 1) (n * accumulator)


tailCallFn lst =
  -- Cycle -- keeps track of
  --   [tailCallFn] -- the function we're calling in a circle
  --   [] -- ???
  --   [TailDef -- this is a tail-call optimized function definition
  --     tailCallFn
  --     [lst]
  --     (Case _n0 lst
  --       (Chain { -- single if/else branch, e.g. List.empty.
  --         _testChain = [(Empty,IsCons)], -- Uses _testChain to check if it should take the _success branch or not
  --                                        -- Empty is the path down the arg to the thing we should check, e.g. when destructuring args
  --                                        -- IsCons, IsNil etc. is the check to perform
  --         _success = Leaf (Inline (Destruct -- if _testChain check is true, do this
  --           (Destructor x (Index (ZeroBased 0) (Root lst)))
  --           (Destruct (Destructor xs (Index (ZeroBased 1) (Root lst)))
  --             (TailCall tailCallFn [(lst,VarLocal xs)]))
  --         )),
  --         _failure = Leaf (Inline (Int 3))}) [])] -- else do this
  --   (fromList []))
  case lst of
    x :: xs ->
      tailCallFn xs

    [] ->
      3



-- type ADT a = Leaf a | Branch (ADT a) (ADT a) | Empty
-- Link does what?
-- (author/project~Simple.tailCallFn,Link author/project~Simple.$tailCallFn) when a tail-call optimized fn references itself?


type T
  = TC (List T)


tailCallFnNested lst =
  -- Cycle
  --   [tailCallFnNested]
  --   []
  --   [TailDef
  --     tailCallFnNested
  --     [lst]
  --     (Case _n0 lst
  --       (Chain
  --         { _testChain = [(Unbox Empty,IsCons)] -- unbox argument before checking it, and check that it IsCons
  --         , _success = Leaf (Inline (Destruct  -- if check succeeds, return this
  --             (Destructor _n1 (Unbox (Root lst))) -- let (T _n1) = lst -- a.k.a. -- let _n1 = unbox (lst)
  --             (Destruct
  --               (Destructor x (Index (ZeroBased 0) (Root _n1))) -- let x = fetch idx 0 of unboxed `lst`
  --               (Destruct
  --                 (Destructor xs (Index (ZeroBased 1) (Root _n1))) -- let x = fetch idx 1 of unboxed `lst`
  --                 (TailCall tailCallFnNested [
  --                   (lst -- let this variable
  --                   , Call (VarBox author/project~Simple.T) [VarLocal xs] -- equal the value of this expr
  --                   )
  --                 ])
  --               )
  --             )
  --           ))
  --         , _failure = Leaf (Inline (Int 3)) -- if check fails, return this
  --         }
  --       )
  --       []
  --     )
  --   ]
  --   (fromList [author/project~Simple.T,elm/core~Basics.identity]))
  case lst of
    TC (x :: xs) ->
      tailCallFnNested (TC xs)

    TC [] ->
      3


varOp =
  1 + 2


both =
  Bitwise.and 3 4



-- debugVar =
--   Debug.log "log something" 42


letexpr =
  let
    a =
      3

    fn x =
      x
  in
  fn a


letrec =
  let
    sum a =
      if a < 0 then
        0

      else
        a + sum (a - 1)
  in
  sum 5


recDef1 a =
  recDef2 a


recDef2 a =
  if a < 3 then
    3

  else
    recDef1 (a - 1)


boolPattern e =
  case e of
    True ->
      1

    False ->
      3


complexRecordPatternArg ( _, ( { x, y, z }, { a, b } ) ) =
  x + y + z


letdestruct =
  let
    ( x, y, z ) =
      ( 1, 2, 3 )

    { a, b } =
      { a = 3, b = 5 }

    { c } =
      { a = 3, b = 5, c = 7 }
  in
  a + b


recordPatternArg { a, b } ( x, { c, d }, ( y, z ) ) =
  -- nested record destructurings are not allowed
  a + c


smallLetDestr =
  let
    { a, b } =
      { a = 1, b = 2 }
  in
  a + b


letDestr =
  let
    { a, b, c } =
      { a = 1, b = 2, c = 3 }

    { d, e, f } =
      { d = 4, e = 5, f = 6 }

    { x, y, z } =
      { x = 7, y = 8, z = 9 }
  in
  c + e + x


record =
  { a = 1, b = 3 }


letWithcomplexRecordPatternArg rec =
  let
    ( _, ( { x, y, z }, { a, b } ) ) =
      rec
  in
  x + y + z


reservedWords data class hiding foreign qualified family =
  123
