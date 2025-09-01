module Extra.Data.Binary exposing
    ( Binary
    , ByteOffset
    , T2(..)
    , T3(..)
    , T4(..)
    , T5(..)
    , T6(..)
    , bBigInt
    , bBool
    , bMap
    , bMaybe
    , bPath
    , bSequence
    , bSet
    , bString
    , bStringWith
    , bTList
    , bTuple
    , bUnit
    , bWord16
    , bWord64
    , bWord8
    , bigToInt
    , bin1
    , bin2
    , bin3
    , bin4
    , bin5
    , bin6
    , custom
    , customVar3
    , decodeFileOrFail
    , encodeFile
    , enum
    , finish
    , iso
    , lazy
    , var0
    , var1
    , var2
    , var3
    , var4
    , var5
    )

import BigInt exposing (BigInt)
import Bytes.Encode
import Extra.Data.Binary.Get as Get exposing (Get)
import Extra.Data.Binary.Put as Put exposing (Put)
import Extra.System.Dir as Dir exposing (FilePath)
import Extra.System.IO as IO
import Extra.Type.Either exposing (Either(..))
import Extra.Type.List as MList exposing (TList)
import Extra.Type.Map as Map
import Extra.Type.Set as Set



-- PRIVATE IO


type alias IO c d e f g h v =
    IO.IO (Dir.GlobalState c d e f g h) v



-- BINARY


type alias Binary a =
    { put : a -> Put
    , get : Get a
    }


type alias ByteOffset =
    Get.ByteOffset


encodeFile : Binary v -> FilePath -> v -> IO c d e f g h ()
encodeFile binV path v =
    Dir.writeFile path (Put.runPut (binV.put v))


decodeFileOrFail : Binary v -> FilePath -> IO c d e f g h (Either ( ByteOffset, String ) v)
decodeFileOrFail binV path =
    Dir.readFile path
        |> IO.fmap
            (\maybeBytes ->
                case maybeBytes of
                    Just bytes ->
                        Get.runGetOrFail binV.get bytes

                    Nothing ->
                        Left ( 0, "File not found: " ++ Dir.toString path )
            )



-- MISC


lazy : (() -> Binary a) -> Binary a
lazy thunk =
    { put = \a -> (thunk ()).put a
    , get = \o -> (thunk ()).get o
    }


iso : (a -> b) -> (b -> a) -> Binary a -> Binary b
iso to from binA =
    { put = binA.put << from
    , get = Get.fmap to binA.get
    }



-- ENUMS


enum : String -> TList a -> Binary a
enum error items =
    MList.foldl
        (\( id, put, get ) item ->
            ( id + 1
            , \value ->
                if value == item then
                    Put.putWord8 id

                else
                    put value
            , \tag ->
                if tag == id then
                    Get.return item

                else
                    get tag
            )
        )
        ( 0, \_ -> Put.mempty, \_ -> Get.fail error )
        items
        |> (\( _, put, get ) -> { put = put, get = Get.bind Get.getWord8 get })



-- TAGGED TYPES


bin1 :
    (a -> b)
    -> (b -> a)
    -> (Binary a -> Binary b)
bin1 ctor dtor binA =
    { put = \b -> binA.put (dtor b)
    , get = Get.fmap ctor binA.get
    }


type T2 a b
    = T2 a b


bin2 :
    (a -> b -> c)
    -> (c -> T2 a b)
    -> (Binary a -> Binary b -> Binary c)
bin2 ctor dtor binA binB =
    { put =
        \c ->
            case dtor c of
                T2 a b ->
                    Put.put2 binA.put binB.put a b
    , get = Get.liftM2 ctor binA.get binB.get
    }


type T3 a b c
    = T3 a b c


bin3 :
    (a -> b -> c -> d)
    -> (d -> T3 a b c)
    -> (Binary a -> Binary b -> Binary c -> Binary d)
bin3 ctor dtor binA binB binC =
    { put =
        \d ->
            case dtor d of
                T3 a b c ->
                    Put.put3 binA.put binB.put binC.put a b c
    , get = Get.liftM3 ctor binA.get binB.get binC.get
    }


type T4 a b c d
    = T4 a b c d


bin4 :
    (a -> b -> c -> d -> e)
    -> (e -> T4 a b c d)
    -> (Binary a -> Binary b -> Binary c -> Binary d -> Binary e)
bin4 ctor dtor binA binB binC binD =
    { put =
        \e ->
            case dtor e of
                T4 a b c d ->
                    Put.put4 binA.put binB.put binC.put binD.put a b c d
    , get = Get.liftM4 ctor binA.get binB.get binC.get binD.get
    }


type T5 a b c d e
    = T5 a b c d e


bin5 :
    (a -> b -> c -> d -> e -> f)
    -> (f -> T5 a b c d e)
    -> (Binary a -> Binary b -> Binary c -> Binary d -> Binary e -> Binary f)
bin5 ctor dtor binA binB binC binD binE =
    { put =
        \f ->
            case dtor f of
                T5 a b c d e ->
                    Put.put5 binA.put binB.put binC.put binD.put binE.put a b c d e
    , get = Get.liftM5 ctor binA.get binB.get binC.get binD.get binE.get
    }


type T6 a b c d e f
    = T6 a b c d e f


bin6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> (g -> T6 a b c d e f)
    -> (Binary a -> Binary b -> Binary c -> Binary d -> Binary e -> Binary f -> Binary g)
bin6 ctor dtor binA binB binC binD binE binF =
    { put =
        \g ->
            case dtor g of
                T6 a b c d e f ->
                    Put.put6 binA.put binB.put binC.put binD.put binE.put binF.put a b c d e f
    , get = Get.liftM6 ctor binA.get binB.get binC.get binD.get binE.get binF.get
    }



-- CUSTOM TYPES


type alias CustomType toPut v =
    -- toPut = putN -> putN -> ... -> custom -> Put
    { toPut : toPut
    , toGet : Int -> Get v
    }


custom : String -> toPut -> CustomType toPut v
custom error toPut =
    { toPut = toPut
    , toGet = \_ -> Get.fail error
    }


variant :
    (Int -> Bool)
    -> putN
    -> Get v
    -> CustomType (putN -> z) v
    -> CustomType z v
variant pred putN get prev =
    customVariant pred putN (\_ -> get) prev


customVariant :
    (Int -> Bool)
    -> putN
    -> (Int -> Get v)
    -> CustomType (putN -> z) v
    -> CustomType z v
customVariant pred putN get prev =
    { toPut = prev.toPut putN
    , toGet =
        \tag ->
            if pred tag then
                get tag

            else
                prev.toGet tag
    }


var0 :
    Int
    -> v
    -> CustomType (Put -> z) v
    -> CustomType z v
var0 id v =
    variant ((==) id)
        (Put.putWord8 id)
        (Get.return v)


var1 :
    Int
    -> (a -> v)
    -> Binary a
    -> CustomType ((a -> Put) -> z) v
    -> CustomType z v
var1 id ctor m1 =
    variant ((==) id)
        (\a ->
            Put.join
                [ Put.putWord8 id
                , m1.put a
                ]
        )
        (Get.fmap ctor m1.get)


customVar1 :
    (Int -> Bool)
    -> (a -> v)
    -> (Int -> Binary a)
    -> CustomType ((Int -> a -> Put) -> z) v
    -> CustomType z v
customVar1 pred ctor m1 =
    customVariant pred
        (\id a ->
            Put.join
                [ Put.putWord8 id
                , (m1 id).put a
                ]
        )
        (\id -> Get.fmap ctor (m1 id).get)


var2 :
    Int
    -> (a -> b -> v)
    -> Binary a
    -> Binary b
    -> CustomType ((a -> b -> Put) -> z) v
    -> CustomType z v
var2 id ctor m1 m2 =
    variant ((==) id)
        (\a b ->
            Put.join
                [ Put.putWord8 id
                , m1.put a
                , m2.put b
                ]
        )
        (Get.liftM2 ctor m1.get m2.get)


var3 :
    Int
    -> (a -> b -> c -> v)
    -> Binary a
    -> Binary b
    -> Binary c
    -> CustomType ((a -> b -> c -> Put) -> z) v
    -> CustomType z v
var3 id ctor m1 m2 m3 =
    variant ((==) id)
        (\a b c ->
            Put.join
                [ Put.putWord8 id
                , m1.put a
                , m2.put b
                , m3.put c
                ]
        )
        (Get.liftM3 ctor m1.get m2.get m3.get)


customVar3 :
    (Int -> Bool)
    -> (a -> b -> c -> v)
    -> (Int -> Binary a)
    -> (Int -> Binary b)
    -> (Int -> Binary c)
    -> CustomType ((Int -> a -> b -> c -> Put) -> z) v
    -> CustomType z v
customVar3 pred ctor m1 m2 m3 =
    customVariant pred
        (\id a b c ->
            Put.join
                [ Put.putWord8 id
                , (m1 id).put a
                , (m2 id).put b
                , (m3 id).put c
                ]
        )
        (\id -> Get.liftM3 ctor (m1 id).get (m2 id).get (m3 id).get)


var4 :
    Int
    -> (a -> b -> c -> d -> v)
    -> Binary a
    -> Binary b
    -> Binary c
    -> Binary d
    -> CustomType ((a -> b -> c -> d -> Put) -> z) v
    -> CustomType z v
var4 id ctor m1 m2 m3 m4 =
    variant ((==) id)
        (\a b c d ->
            Put.join
                [ Put.putWord8 id
                , m1.put a
                , m2.put b
                , m3.put c
                , m4.put d
                ]
        )
        (Get.liftM4 ctor m1.get m2.get m3.get m4.get)


var5 :
    Int
    -> (a -> b -> c -> d -> e -> v)
    -> Binary a
    -> Binary b
    -> Binary c
    -> Binary d
    -> Binary e
    -> CustomType ((a -> b -> c -> d -> e -> Put) -> z) v
    -> CustomType z v
var5 id ctor m1 m2 m3 m4 m5 =
    variant ((==) id)
        (\a b c d e ->
            Put.join
                [ Put.putWord8 id
                , m1.put a
                , m2.put b
                , m3.put c
                , m4.put d
                , m5.put e
                ]
        )
        (Get.liftM5 ctor m1.get m2.get m3.get m4.get m5.get)


finish : CustomType (a -> Put) a -> Binary a
finish customType =
    { put = customType.toPut
    , get = Get.bind Get.getWord8 customType.toGet
    }



-- STANDARD TYPES


bBool : Binary Bool
bBool =
    enum "Could not map value to Bool" [ False, True ]


bMap : Binary comparable -> Binary a -> Binary (Map.Map comparable a)
bMap binK binA =
    bin1 Map.fromList Map.toList <|
        bTList (bTuple binK binA)


bMaybe : Binary a -> Binary (Maybe a)
bMaybe binA =
    custom "can't happen"
        (\p0 p1 maybe ->
            case maybe of
                Nothing ->
                    p0

                Just a ->
                    p1 1 a
        )
        |> var0 0 Nothing
        |> customVar1 (\id -> id > 0) Just (\_ -> binA)
        |> finish


bPath : Binary Dir.FilePath
bPath =
    bin1 Dir.fromString Dir.toString bString


bSequence : Binary a -> Int -> Binary (List a)
bSequence binA n =
    { put = Put.putSequence binA.put
    , get = Get.getSequence binA.get n
    }


bSet : Binary comparable -> Binary (Set.Set comparable)
bSet binC =
    bin1 Set.fromList Set.toList <|
        bTList binC


bString : Binary String
bString =
    bStringWith bWord64


bStringWith : Binary Int -> Binary String
bStringWith bWidth =
    { put =
        \string ->
            Put.mappend
                (bWidth.put (Bytes.Encode.getStringWidth string))
                (Put.putString string)
    , get =
        Get.bind
            bWidth.get
            Get.getString
    }


bTList : Binary a -> Binary (TList a)
bTList binA =
    { put =
        \list ->
            Put.mappend
                (bWord64.put (MList.length list))
                (Put.putSequence binA.put list)
    , get =
        Get.bind
            bWord64.get
            (Get.getSequence binA.get)
    }


bTuple : Binary a -> Binary b -> Binary ( a, b )
bTuple binA binB =
    bin2 Tuple.pair (\( a, b ) -> T2 a b) binA binB


bUnit : Binary ()
bUnit =
    { put = \() -> Put.mempty
    , get = Get.return ()
    }


bWord8 : Binary Int
bWord8 =
    { put = Put.putWord8
    , get = Get.getWord8
    }


bWord16 : Binary Int
bWord16 =
    { put = Put.putWord16
    , get = Get.getWord16
    }


bWord32 : Binary Int
bWord32 =
    { put = Put.putWord32
    , get = Get.getWord32
    }


bWord64 : Binary Int
bWord64 =
    bin2
        (\hi lo -> hi * 0x0000000100000000 + lo)
        (\word -> T2 (word // 0x0000000100000000) (modBy 0x0000000100000000 word))
        bWord32
        bWord32



-- BIG INT


bBigInt : Binary BigInt
bBigInt =
    bin3
        (\_ _ bytes -> bytesToBig bytes)
        (\big -> T3 1 1 (bigToBytes big []))
        bWord8
        bWord8
        (bTList bWord8)


bytesToBig : TList Int -> BigInt
bytesToBig bytes =
    MList.foldr
        (\byte big -> BigInt.add (BigInt.mul big bigFactor) (BigInt.fromInt byte))
        bigZero
        bytes


bigToBytes : BigInt -> TList Int -> TList Int
bigToBytes big bytes =
    if BigInt.gt big bigZero then
        bigToBytes
            (BigInt.div big bigFactor)
            (maybeBigToInt (BigInt.modBy bigFactor big) :: bytes)

    else
        MList.reverse bytes


maybeBigToInt : Maybe BigInt -> Int
maybeBigToInt maybeBig =
    maybeBig |> Maybe.withDefault bigZero |> bigToInt


bigToInt : BigInt -> Int
bigToInt big =
    String.toInt (BigInt.toString big) |> Maybe.withDefault 0


bigZero : BigInt
bigZero =
    BigInt.fromInt 0


bigFactor : BigInt
bigFactor =
    BigInt.fromInt 0x0100
