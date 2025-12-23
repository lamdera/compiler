module Extra.Data.Binary.Put exposing
    ( Put
    , join
    , mappend
    , mempty
    , put2
    , put3
    , put4
    , put5
    , put6
    , putSequence
    , putString
    , putWord16
    , putWord32
    , putWord8
    , runPut
    )

import Bytes
import Bytes.Encode
import Extra.Type.List as MList exposing (TList)


type alias Put =
    Bytes.Encode.Encoder


mempty : Put
mempty =
    join []


mappend : Put -> Put -> Put
mappend p1 p2 =
    join [ p1, p2 ]


join : TList Put -> Put
join =
    Bytes.Encode.sequence


runPut : Put -> Bytes.Bytes
runPut =
    Bytes.Encode.encode



-- PRIMITIVES


putWord8 : Int -> Put
putWord8 =
    Bytes.Encode.unsignedInt8


putWord16 : Int -> Put
putWord16 word =
    Bytes.Encode.unsignedInt16 Bytes.BE word


putWord32 : Int -> Put
putWord32 word =
    Bytes.Encode.unsignedInt32 Bytes.BE word


putString : String -> Put
putString =
    Bytes.Encode.string



-- COMBINATORS


put2 :
    (a -> Put)
    -> (b -> Put)
    -> (a -> b -> Put)
put2 fa fb a b =
    join [ fa a, fb b ]


put3 :
    (a -> Put)
    -> (b -> Put)
    -> (c -> Put)
    -> (a -> b -> c -> Put)
put3 fa fb fc a b c =
    join [ fa a, fb b, fc c ]


put4 :
    (a -> Put)
    -> (b -> Put)
    -> (c -> Put)
    -> (d -> Put)
    -> (a -> b -> c -> d -> Put)
put4 fa fb fc fd a b c d =
    join [ fa a, fb b, fc c, fd d ]


put5 :
    (a -> Put)
    -> (b -> Put)
    -> (c -> Put)
    -> (d -> Put)
    -> (e -> Put)
    -> (a -> b -> c -> d -> e -> Put)
put5 fa fb fc fd fe a b c d e =
    join [ fa a, fb b, fc c, fd d, fe e ]


put6 :
    (a -> Put)
    -> (b -> Put)
    -> (c -> Put)
    -> (d -> Put)
    -> (e -> Put)
    -> (f -> Put)
    -> (a -> b -> c -> d -> e -> f -> Put)
put6 fa fb fc fd fe ff a b c d e f =
    join [ fa a, fb b, fc c, fd d, fe e, ff f ]


putSequence : (a -> Put) -> List a -> Put
putSequence fa list =
    join (MList.map fa list)
