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
        , E.dict String.fromInt E.string evg_p0.dictIntString
        , evg_e_Order evg_p0.order

        -- , evg_e_RecursiveUnion evg_p0.union
        ]



--
-- haskelm_wire_decoder_AllTypes : D.Decoder AllTypes
-- haskelm_wire_decoder_AllTypes =
--     D.list D.value
--         |> D.andThen
--             (\v ->
--                 D.oneOf
--                     [ let
--                         d_c =
--                             D.decodeValue (D.index 0 D.string) v
--
--                         d_a0 =
--                             D.decodeValue (D.index 1 D.int) v
--
--                         d_a1 =
--                             D.decodeValue (D.index 2 D.float) v
--
--                         d_a2 =
--                             D.decodeValue (D.index 3 D.bool) v
--
--                         d_a3 =
--                             D.decodeValue (D.index 4 evg_d_Char) v
--
--                         d_a4 =
--                             D.decodeValue (D.index 5 D.string) v
--
--                         d_a5 =
--                             D.decodeValue (D.index 6 (D.list D.int)) v
--
--                         d_a6 =
--                             D.decodeValue (D.index 7 (D.list D.float |> D.andThen (\evg_v0 -> Set.fromList evg_v0))) v
--
--                         d_a7 =
--                             D.decodeValue (D.index 8 (D.array D.string)) v
--
--                         d_a8 =
--                             D.decodeValue (D.index 9 (D.dict D.string)) v
--
--                         d_a9 =
--                             D.decodeValue (D.index 10 evg_d_Order) v
--
--                         -- d_a10 =
--                         --     D.decodeValue (D.index 11 haskelm_wire_decoder_RecursiveUnion) v
--                       in
--                       case ( evg_c, evg_a0, evg_a1, evg_a2, evg_a3, evg_a4, evg_a5, evg_a6, evg_a7, evg_a8, evg_a9 ) of
--                         ( Ok "", Ok evg_x0, Ok evg_x1, Ok evg_x2, Ok evg_x3, Ok evg_x4, Ok evg_x5, Ok evg_x6, Ok evg_x7, Ok evg_x8, Ok evg_x9 ) ->
--                             D.succeed
--                                 { int = evg_x0, float = evg_x1, bool = evg_x2, char = evg_x3, string = evg_x4, listInt = evg_x5, setFloat = evg_x6, arrayString = evg_x7, dictIntStirng = evg_x8, order = evg_x9 }
--
--                         partialDecodes ->
--                             D.fail "evergreen wire decode failed on partialDecodes"
--                     ]
--             )
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
