module AllTypes_Gen exposing (..)

import AllTypes exposing (..)
import Evergreen as EG
import Json.Decode as D
import Json.Encode as E
import Set


{- WARNING WARNING WARNING WARNING  WARNING  WARNING  WARNING  WARNING  WARNING  WARNING  WARNING  WARNING

   This file is special, if you're using elmx anything you write here will be overwritten on the fly
   during compilation!

-}
-- Causes Elm to complain about recursive values...
-- evg_e_RecursiveUnion : RecursiveUnion -> E.Value
-- evg_e_RecursiveUnion evg_p0 =
--     case evg_p0 of
--         Node evg_v0 ->
--             E.list [ E.string "Node", evg_e_RecursiveUnion evg_v0 ]
--
--         Leaf ->
--             E.list [ E.string "Leaf" ]
--
--
-- evg_d_RecursiveUnion : D.Decoder RecursiveUnion
-- evg_d_RecursiveUnion =
--     D.oneOf
--         [ eqPosStr 0 "Node" Node
--         , eqPosStr1 0 "Leaf" evg_d_RecursiveUnion Leaf
--         ]
-- These functions are here for reference, as they're what's implemented in the AST but they
-- can be deleted and everything should still compile as they're being replaced by the compiler


evg_e_AllTypes : AllTypes -> E.Value
evg_e_AllTypes evg_p0 =
    E.list identity
        [ E.int evg_p0.int
        , E.float evg_p0.float
        , E.bool evg_p0.bool
        , EG.e_Char evg_p0.char
        , E.string evg_p0.string
        , E.list E.int evg_p0.listInt
        , E.set E.float evg_p0.setFloat
        , E.array E.string evg_p0.arrayString
        , EG.e_Order evg_p0.order
        ]


evg_d_AllTypes : D.Decoder AllTypes
evg_d_AllTypes =
    D.succeed AllTypes
        |> EG.atIndex 0 D.int
        |> EG.atIndex 1 D.float
        |> EG.atIndex 2 D.bool
        |> EG.atIndex 3 EG.d_Char
        |> EG.atIndex 4 D.string
        |> EG.atIndex 5 (D.list D.int)
        |> EG.atIndex 6 (EG.d_set D.float)
        |> EG.atIndex 7 (D.array D.string)
        |> EG.atIndex 8 EG.d_Order
