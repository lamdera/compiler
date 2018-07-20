module AllTypes_Gen exposing (..)

import AllTypes exposing (..)
import Json.Decode as D
import Json.Encode as E
import Set


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


evg_e_AllTypes : AllTypes -> E.Value
evg_e_AllTypes evg_p0 =
    E.list identity
        [ E.string ""
        , E.int evg_p0.int
        , E.float evg_p0.float
        , E.bool evg_p0.bool
        , evg_e_Char evg_p0.char
        , E.string evg_p0.string
        , E.list E.int evg_p0.listInt
        , E.set E.float evg_p0.setFloat
        , E.array E.string evg_p0.arrayString

        -- , E.dict String.fromInt E.string evg_p0.dictIntString
        , evg_e_Order evg_p0.order

        -- , evg_e_RecursiveUnion evg_p0.union
        ]


evg_d_AllTypes : D.Decoder AllTypes
evg_d_AllTypes =
    D.list D.value
        |> D.andThen
            (\v ->
                D.succeed AllTypes
                    |> evg_atIndex 1 D.int
                    |> evg_atIndex 2 D.float
                    |> evg_atIndex 3 D.bool
                    |> evg_atIndex 4 evg_d_Char
                    |> evg_atIndex 5 D.string
                    |> evg_atIndex 6 (D.list D.int)
                    |> evg_atIndex 7 (D.list D.float |> D.andThen (\evg_v0 -> D.succeed <| Set.fromList evg_v0))
                    |> evg_atIndex 8 (D.array D.string)
                    -- Dicts are kinda tricky... how do we rep non-string keys? In 0.19 any type will be Dict Keyable...
                    -- |> evg_atIndex 9 (D.dict D.string |> D.andThen (\evg_v0 ->  ))
                    |> evg_atIndex 10 evg_d_Order
            )



-- Move elsewhere later


evg_e_Char : Char -> E.Value
evg_e_Char evg_p0 =
    E.int (Char.toCode evg_p0)


evg_d_Char : D.Decoder Char
evg_d_Char =
    D.int |> D.map (\evg_v0 -> Char.fromCode evg_v0)


evg_e_Order : Order -> E.Value
evg_e_Order evg_p0 =
    case evg_p0 of
        LT ->
            E.string "LT"

        EQ ->
            E.string "EQ"

        GT ->
            E.string "GT"


evg_d_Order : D.Decoder Order
evg_d_Order =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "LT" ->
                        D.succeed LT

                    "EQ" ->
                        D.succeed EQ

                    "GT" ->
                        D.succeed GT

                    _ ->
                        D.fail <| "unexpected Order value: " ++ s
            )


eqPosStr : Int -> String -> a -> D.Decoder a
eqPosStr idx str final =
    D.index idx D.string
        |> D.andThen
            (\s ->
                if s == str then
                    D.succeed final
                else
                    D.fail "evergreen wire decode failed for union type"
            )


eqPosStr1 : Int -> String -> D.Decoder a -> (a -> b) -> D.Decoder b
eqPosStr1 idx str subDecoder constructor =
    D.index idx D.string
        |> D.andThen
            (\s ->
                if s == str then
                    subDecoder |> D.andThen (\v1 -> D.succeed <| constructor v1)
                else
                    D.fail "evergreen wire decode failed for union type"
            )


evg_atIndex idx decoder =
    evg_custom (D.index idx decoder)



-- Trick from json-decode-pipeline


evg_custom : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
evg_custom =
    D.map2 (|>)
