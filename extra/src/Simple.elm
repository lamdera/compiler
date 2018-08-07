module Simple exposing (..)

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


sum x =
  Cycle
    [sum]
    []
  [Def
    sum
    (Function
      [x]
      (If
        [(Call (VarGlobal elm/core~Basics.eq) [VarLocal x,Int 0],Int 0)]
        (Call (VarGlobal elm/core~Basics.add)
          [VarLocal x,Call (VarGlobal author/project~Simple.sum) [Call (VarGlobal elm/core~Basics.sub) [VarLocal x,Int 1]]]
        )
      )
    )
  ]
  (fromList
    [author/project~Simple.sum
    ,elm/core~Basics.add
    ,elm/core~Basics.eq
    ,elm/core~Basics.sub
    ]
  ))

  if x == 0 then
    0

  else
    x + sum (x - 1)



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


-- Link does what?
-- (author/project~Simple.tailCallFn,Link author/project~Simple.$tailCallFn) when a tail-call optimized fn references itself?

type T = T (List T)

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
    T (x :: xs) ->
      tailCallFnNested (T xs)

    T [] ->
      3

