port module Main exposing (..)

import Lamdera.Wire3 as W
import Bytes exposing (Bytes)
import Bytes.Encode
import Bytes.Decode
import Array
import Dict
import Set
import SeqDict
import Other exposing (..)
import Types exposing (..)
import Params exposing (..)
import Validated exposing (..)


port out : Int -> Cmd msg

readBytesPast : Int -> Bytes -> Maybe Int
readBytesPast n b =
    Bytes.Decode.decode (Bytes.Decode.bytes n) b |> Maybe.map Bytes.width



s_TAll : TAll
s_TAll =
    { i = -5, f = 0 / 0, s = "héllo wörld, long enough", c = 'x', b = True, o = GT
    , l = [ ( 1, Just "a" ), ( 2, Nothing ) ], d = Dict.fromList [ ( "k", [ 1.5, -0.0 ] ), ( "a", [] ) ], st = Set.fromList [ ( 3, 'z' ), ( -1, 'a' ) ]
    , m = Beta (Just (Mid "m" 2.5)), t = Node Leaf 3 (Node Leaf 4 Leaf), big = BigK000, e300 = Enum300K299, e256 = Enum256K255
    , bs = Bytes.Encode.encode (Bytes.Encode.sequence [ Bytes.Encode.unsignedInt8 1, Bytes.Encode.unsignedInt8 2 ])
    , r = Err ( 1, 2 ), w = Wrapped (Id "w"), sd = SeqDict.fromList [ ( 5, "five" ), ( 1, "one" ) ]
    , fo = Forest [ TreeB 1 NoForest, TreeB 2 (Forest []) ], th = Thing (Rose NoThing [ Rose (Thing (Rose NoThing [])) [] ])
    , two = TwoAB "s" (TwoB True), n = { outer = { inner = { deep = [ 1, 2 ] }, flag = False }, z = 9 }
    }



s_TVModel : TVModel
s_TVModel =
    { positives = [ Positive 1, Positive 0, Positive 300 ]
    , tree = VNode (VNode (VLeaf 3) 12 (VLeaf 900)) 14 (VLeaf 0)
    , roses = Dict.fromList [ ( "a", VRose 1 [ VRose 2 [], VRose 3 [ VRose 4 [] ] ] ), ( "b", VRose 5 [] ) ]
    , rosePair = VRose ( Positive 2, "p" ) [ VRose ( Positive 3, "q" ) [] ]
    , pair = Just (VPair "a" (Positive 4) (Just VPairEnd))
    , enums = [ VA, VB, VA ]
    , box = VBox [ Positive 10, Positive 20 ]
    , ping = VPing 1 (Just (VPong "x" [ VPing 2 Nothing, VPing 3 (Just (VPong "y" [])) ]))
    , phantom = VPhantom 5
    , rec = VRec { name = "nm", scores = Dict.fromList [ ( "s", Positive 6 ) ], child = Just VRecNone }
    , unvalidated = Unvalidated [ Positive 7 ] (Just VB)
    , keyed = Dict.fromList [ ( -1, [ Nothing, Just (VLeaf 8) ] ), ( 2, [] ) ]
    }



e_TInt : TInt -> Bytes
e_TInt v =
    W.bytesEncode (w3_encode_TInt v)


d_TInt : W.Decoder TInt
d_TInt =
    w3_decode_TInt


u_TInt : W.Decoder TInt
u_TInt =
    w3_unsafe_decode_TInt


e_TFloat : TFloat -> Bytes
e_TFloat v =
    W.bytesEncode (w3_encode_TFloat v)


d_TFloat : W.Decoder TFloat
d_TFloat =
    w3_decode_TFloat


u_TFloat : W.Decoder TFloat
u_TFloat =
    w3_unsafe_decode_TFloat


e_TString : TString -> Bytes
e_TString v =
    W.bytesEncode (w3_encode_TString v)


d_TString : W.Decoder TString
d_TString =
    w3_decode_TString


u_TString : W.Decoder TString
u_TString =
    w3_unsafe_decode_TString


e_TChar : TChar -> Bytes
e_TChar v =
    W.bytesEncode (w3_encode_TChar v)


d_TChar : W.Decoder TChar
d_TChar =
    w3_decode_TChar


u_TChar : W.Decoder TChar
u_TChar =
    w3_unsafe_decode_TChar


e_TBool : TBool -> Bytes
e_TBool v =
    W.bytesEncode (w3_encode_TBool v)


d_TBool : W.Decoder TBool
d_TBool =
    w3_decode_TBool


u_TBool : W.Decoder TBool
u_TBool =
    w3_unsafe_decode_TBool


e_TOrder : TOrder -> Bytes
e_TOrder v =
    W.bytesEncode (w3_encode_TOrder v)


d_TOrder : W.Decoder TOrder
d_TOrder =
    w3_decode_TOrder


u_TOrder : W.Decoder TOrder
u_TOrder =
    w3_unsafe_decode_TOrder


e_TUnit : TUnit -> Bytes
e_TUnit v =
    W.bytesEncode (w3_encode_TUnit v)


d_TUnit : W.Decoder TUnit
d_TUnit =
    w3_decode_TUnit


u_TUnit : W.Decoder TUnit
u_TUnit =
    w3_unsafe_decode_TUnit


e_TBytes : TBytes -> Bytes
e_TBytes v =
    W.bytesEncode (w3_encode_TBytes v)


d_TBytes : W.Decoder TBytes
d_TBytes =
    w3_decode_TBytes


u_TBytes : W.Decoder TBytes
u_TBytes =
    w3_unsafe_decode_TBytes


e_TPosix : TPosix -> Bytes
e_TPosix v =
    W.bytesEncode (w3_encode_TPosix v)


d_TPosix : W.Decoder TPosix
d_TPosix =
    w3_decode_TPosix


u_TPosix : W.Decoder TPosix
u_TPosix =
    w3_unsafe_decode_TPosix


e_TMaybe : TMaybe -> Bytes
e_TMaybe v =
    W.bytesEncode (w3_encode_TMaybe v)


d_TMaybe : W.Decoder TMaybe
d_TMaybe =
    w3_decode_TMaybe


u_TMaybe : W.Decoder TMaybe
u_TMaybe =
    w3_unsafe_decode_TMaybe


e_TMaybeMaybe : TMaybeMaybe -> Bytes
e_TMaybeMaybe v =
    W.bytesEncode (w3_encode_TMaybeMaybe v)


d_TMaybeMaybe : W.Decoder TMaybeMaybe
d_TMaybeMaybe =
    w3_decode_TMaybeMaybe


u_TMaybeMaybe : W.Decoder TMaybeMaybe
u_TMaybeMaybe =
    w3_unsafe_decode_TMaybeMaybe


e_TResult : TResult -> Bytes
e_TResult v =
    W.bytesEncode (w3_encode_TResult v)


d_TResult : W.Decoder TResult
d_TResult =
    w3_decode_TResult


u_TResult : W.Decoder TResult
u_TResult =
    w3_unsafe_decode_TResult


e_TList : TList -> Bytes
e_TList v =
    W.bytesEncode (w3_encode_TList v)


d_TList : W.Decoder TList
d_TList =
    w3_decode_TList


u_TList : W.Decoder TList
u_TList =
    w3_unsafe_decode_TList


e_TListUnit : TListUnit -> Bytes
e_TListUnit v =
    W.bytesEncode (w3_encode_TListUnit v)


d_TListUnit : W.Decoder TListUnit
d_TListUnit =
    w3_decode_TListUnit


u_TListUnit : W.Decoder TListUnit
u_TListUnit =
    w3_unsafe_decode_TListUnit


e_TListStr : TListStr -> Bytes
e_TListStr v =
    W.bytesEncode (w3_encode_TListStr v)


d_TListStr : W.Decoder TListStr
d_TListStr =
    w3_decode_TListStr


u_TListStr : W.Decoder TListStr
u_TListStr =
    w3_unsafe_decode_TListStr


e_TArray : TArray -> Bytes
e_TArray v =
    W.bytesEncode (w3_encode_TArray v)


d_TArray : W.Decoder TArray
d_TArray =
    w3_decode_TArray


u_TArray : W.Decoder TArray
u_TArray =
    w3_unsafe_decode_TArray


e_TSetInt : TSetInt -> Bytes
e_TSetInt v =
    W.bytesEncode (w3_encode_TSetInt v)


d_TSetInt : W.Decoder TSetInt
d_TSetInt =
    w3_decode_TSetInt


u_TSetInt : W.Decoder TSetInt
u_TSetInt =
    w3_unsafe_decode_TSetInt


e_TDictIntFloat : TDictIntFloat -> Bytes
e_TDictIntFloat v =
    W.bytesEncode (w3_encode_TDictIntFloat v)


d_TDictIntFloat : W.Decoder TDictIntFloat
d_TDictIntFloat =
    w3_decode_TDictIntFloat


u_TDictIntFloat : W.Decoder TDictIntFloat
u_TDictIntFloat =
    w3_unsafe_decode_TDictIntFloat


e_TArrayFloat : TArrayFloat -> Bytes
e_TArrayFloat v =
    W.bytesEncode (w3_encode_TArrayFloat v)


d_TArrayFloat : W.Decoder TArrayFloat
d_TArrayFloat =
    w3_decode_TArrayFloat


u_TArrayFloat : W.Decoder TArrayFloat
u_TArrayFloat =
    w3_unsafe_decode_TArrayFloat


e_TListFloat : TListFloat -> Bytes
e_TListFloat v =
    W.bytesEncode (w3_encode_TListFloat v)


d_TListFloat : W.Decoder TListFloat
d_TListFloat =
    w3_decode_TListFloat


u_TListFloat : W.Decoder TListFloat
u_TListFloat =
    w3_unsafe_decode_TListFloat


e_TRecFloat : TRecFloat -> Bytes
e_TRecFloat v =
    W.bytesEncode (w3_encode_TRecFloat v)


d_TRecFloat : W.Decoder TRecFloat
d_TRecFloat =
    w3_decode_TRecFloat


u_TRecFloat : W.Decoder TRecFloat
u_TRecFloat =
    w3_unsafe_decode_TRecFloat


e_TSeqDictFloat : TSeqDictFloat -> Bytes
e_TSeqDictFloat v =
    W.bytesEncode (w3_encode_TSeqDictFloat v)


d_TSeqDictFloat : W.Decoder TSeqDictFloat
d_TSeqDictFloat =
    w3_decode_TSeqDictFloat


u_TSeqDictFloat : W.Decoder TSeqDictFloat
u_TSeqDictFloat =
    w3_unsafe_decode_TSeqDictFloat


e_TSetFloat : TSetFloat -> Bytes
e_TSetFloat v =
    W.bytesEncode (w3_encode_TSetFloat v)


d_TSetFloat : W.Decoder TSetFloat
d_TSetFloat =
    w3_decode_TSetFloat


u_TSetFloat : W.Decoder TSetFloat
u_TSetFloat =
    w3_unsafe_decode_TSetFloat


e_TSetString : TSetString -> Bytes
e_TSetString v =
    W.bytesEncode (w3_encode_TSetString v)


d_TSetString : W.Decoder TSetString
d_TSetString =
    w3_decode_TSetString


u_TSetString : W.Decoder TSetString
u_TSetString =
    w3_unsafe_decode_TSetString


e_TSetChar : TSetChar -> Bytes
e_TSetChar v =
    W.bytesEncode (w3_encode_TSetChar v)


d_TSetChar : W.Decoder TSetChar
d_TSetChar =
    w3_decode_TSetChar


u_TSetChar : W.Decoder TSetChar
u_TSetChar =
    w3_unsafe_decode_TSetChar


e_TSetTuple : TSetTuple -> Bytes
e_TSetTuple v =
    W.bytesEncode (w3_encode_TSetTuple v)


d_TSetTuple : W.Decoder TSetTuple
d_TSetTuple =
    w3_decode_TSetTuple


u_TSetTuple : W.Decoder TSetTuple
u_TSetTuple =
    w3_unsafe_decode_TSetTuple


e_TSetList : TSetList -> Bytes
e_TSetList v =
    W.bytesEncode (w3_encode_TSetList v)


d_TSetList : W.Decoder TSetList
d_TSetList =
    w3_decode_TSetList


u_TSetList : W.Decoder TSetList
u_TSetList =
    w3_unsafe_decode_TSetList


e_TDictInt : TDictInt -> Bytes
e_TDictInt v =
    W.bytesEncode (w3_encode_TDictInt v)


d_TDictInt : W.Decoder TDictInt
d_TDictInt =
    w3_decode_TDictInt


u_TDictInt : W.Decoder TDictInt
u_TDictInt =
    w3_unsafe_decode_TDictInt


e_TDictFloat : TDictFloat -> Bytes
e_TDictFloat v =
    W.bytesEncode (w3_encode_TDictFloat v)


d_TDictFloat : W.Decoder TDictFloat
d_TDictFloat =
    w3_decode_TDictFloat


u_TDictFloat : W.Decoder TDictFloat
u_TDictFloat =
    w3_unsafe_decode_TDictFloat


e_TDictString : TDictString -> Bytes
e_TDictString v =
    W.bytesEncode (w3_encode_TDictString v)


d_TDictString : W.Decoder TDictString
d_TDictString =
    w3_decode_TDictString


u_TDictString : W.Decoder TDictString
u_TDictString =
    w3_unsafe_decode_TDictString


e_TDictChar : TDictChar -> Bytes
e_TDictChar v =
    W.bytesEncode (w3_encode_TDictChar v)


d_TDictChar : W.Decoder TDictChar
d_TDictChar =
    w3_decode_TDictChar


u_TDictChar : W.Decoder TDictChar
u_TDictChar =
    w3_unsafe_decode_TDictChar


e_TDictTuple : TDictTuple -> Bytes
e_TDictTuple v =
    W.bytesEncode (w3_encode_TDictTuple v)


d_TDictTuple : W.Decoder TDictTuple
d_TDictTuple =
    w3_decode_TDictTuple


u_TDictTuple : W.Decoder TDictTuple
u_TDictTuple =
    w3_unsafe_decode_TDictTuple


e_TDictTriple : TDictTriple -> Bytes
e_TDictTriple v =
    W.bytesEncode (w3_encode_TDictTriple v)


d_TDictTriple : W.Decoder TDictTriple
d_TDictTriple =
    w3_decode_TDictTriple


u_TDictTriple : W.Decoder TDictTriple
u_TDictTriple =
    w3_unsafe_decode_TDictTriple


e_TDictList : TDictList -> Bytes
e_TDictList v =
    W.bytesEncode (w3_encode_TDictList v)


d_TDictList : W.Decoder TDictList
d_TDictList =
    w3_decode_TDictList


u_TDictList : W.Decoder TDictList
u_TDictList =
    w3_unsafe_decode_TDictList


e_TSeqDict : TSeqDict -> Bytes
e_TSeqDict v =
    W.bytesEncode (w3_encode_TSeqDict v)


d_TSeqDict : W.Decoder TSeqDict
d_TSeqDict =
    w3_decode_TSeqDict


u_TSeqDict : W.Decoder TSeqDict
u_TSeqDict =
    w3_unsafe_decode_TSeqDict


e_TSeqDictRec : TSeqDictRec -> Bytes
e_TSeqDictRec v =
    W.bytesEncode (w3_encode_TSeqDictRec v)


d_TSeqDictRec : W.Decoder TSeqDictRec
d_TSeqDictRec =
    w3_decode_TSeqDictRec


u_TSeqDictRec : W.Decoder TSeqDictRec
u_TSeqDictRec =
    w3_unsafe_decode_TSeqDictRec


e_TSeqDictDict : TSeqDictDict -> Bytes
e_TSeqDictDict v =
    W.bytesEncode (w3_encode_TSeqDictDict v)


d_TSeqDictDict : W.Decoder TSeqDictDict
d_TSeqDictDict =
    w3_decode_TSeqDictDict


u_TSeqDictDict : W.Decoder TSeqDictDict
u_TSeqDictDict =
    w3_unsafe_decode_TSeqDictDict


e_TSeqSet : TSeqSet -> Bytes
e_TSeqSet v =
    W.bytesEncode (w3_encode_TSeqSet v)


d_TSeqSet : W.Decoder TSeqSet
d_TSeqSet =
    w3_decode_TSeqSet


u_TSeqSet : W.Decoder TSeqSet
u_TSeqSet =
    w3_unsafe_decode_TSeqSet


e_TPair : TPair -> Bytes
e_TPair v =
    W.bytesEncode (w3_encode_TPair v)


d_TPair : W.Decoder TPair
d_TPair =
    w3_decode_TPair


u_TPair : W.Decoder TPair
u_TPair =
    w3_unsafe_decode_TPair


e_TTriple : TTriple -> Bytes
e_TTriple v =
    W.bytesEncode (w3_encode_TTriple v)


d_TTriple : W.Decoder TTriple
d_TTriple =
    w3_decode_TTriple


u_TTriple : W.Decoder TTriple
u_TTriple =
    w3_unsafe_decode_TTriple


e_TColor : TColor -> Bytes
e_TColor v =
    W.bytesEncode (w3_encode_TColor v)


d_TColor : W.Decoder TColor
d_TColor =
    w3_decode_TColor


u_TColor : W.Decoder TColor
u_TColor =
    w3_unsafe_decode_TColor


e_TBoxes : TBoxes -> Bytes
e_TBoxes v =
    W.bytesEncode (w3_encode_TBoxes v)


d_TBoxes : W.Decoder TBoxes
d_TBoxes =
    w3_decode_TBoxes


u_TBoxes : W.Decoder TBoxes
u_TBoxes =
    w3_unsafe_decode_TBoxes


e_TTwo : TTwo -> Bytes
e_TTwo v =
    W.bytesEncode (w3_encode_TTwo v)


d_TTwo : W.Decoder TTwo
d_TTwo =
    w3_decode_TTwo


u_TTwo : W.Decoder TTwo
u_TTwo =
    w3_unsafe_decode_TTwo


e_TWrap : TWrap -> Bytes
e_TWrap v =
    W.bytesEncode (w3_encode_TWrap v)


d_TWrap : W.Decoder TWrap
d_TWrap =
    w3_decode_TWrap


u_TWrap : W.Decoder TWrap
u_TWrap =
    w3_unsafe_decode_TWrap


e_TRose : TRose -> Bytes
e_TRose v =
    W.bytesEncode (w3_encode_TRose v)


d_TRose : W.Decoder TRose
d_TRose =
    w3_decode_TRose


u_TRose : W.Decoder TRose
u_TRose =
    w3_unsafe_decode_TRose


e_Reserved : Reserved -> Bytes
e_Reserved v =
    W.bytesEncode (w3_encode_Reserved v)


d_Reserved : W.Decoder Reserved
d_Reserved =
    w3_decode_Reserved


u_Reserved : W.Decoder Reserved
u_Reserved =
    w3_unsafe_decode_Reserved


e_Person : Person -> Bytes
e_Person v =
    W.bytesEncode (w3_encode_Person v)


d_Person : W.Decoder Person
d_Person =
    w3_decode_Person


u_Person : W.Decoder Person
u_Person =
    w3_unsafe_decode_Person


e_TNamedField : TNamedField -> Bytes
e_TNamedField v =
    W.bytesEncode (w3_encode_TNamedField v)


d_TNamedField : W.Decoder TNamedField
d_TNamedField =
    w3_decode_TNamedField


u_TNamedField : W.Decoder TNamedField
u_TNamedField =
    w3_unsafe_decode_TNamedField


e_Nested : Nested -> Bytes
e_Nested v =
    W.bytesEncode (w3_encode_Nested v)


d_Nested : W.Decoder Nested
d_Nested =
    w3_decode_Nested


u_Nested : W.Decoder Nested
u_Nested =
    w3_unsafe_decode_Nested


e_TNever : TNever -> Bytes
e_TNever v =
    W.bytesEncode (w3_encode_TNever v)


d_TNever : W.Decoder TNever
d_TNever =
    w3_decode_TNever


u_TNever : W.Decoder TNever
u_TNever =
    w3_unsafe_decode_TNever


e_TResultNever : TResultNever -> Bytes
e_TResultNever v =
    W.bytesEncode (w3_encode_TResultNever v)


d_TResultNever : W.Decoder TResultNever
d_TResultNever =
    w3_decode_TResultNever


u_TResultNever : W.Decoder TResultNever
u_TResultNever =
    w3_unsafe_decode_TResultNever


e_TListNever : TListNever -> Bytes
e_TListNever v =
    W.bytesEncode (w3_encode_TListNever v)


d_TListNever : W.Decoder TListNever
d_TListNever =
    w3_decode_TListNever


u_TListNever : W.Decoder TListNever
u_TListNever =
    w3_unsafe_decode_TListNever


e_TPhantom : TPhantom -> Bytes
e_TPhantom v =
    W.bytesEncode (w3_encode_TPhantom v)


d_TPhantom : W.Decoder TPhantom
d_TPhantom =
    w3_decode_TPhantom


u_TPhantom : W.Decoder TPhantom
u_TPhantom =
    w3_unsafe_decode_TPhantom


e_TSeed : TSeed -> Bytes
e_TSeed v =
    W.bytesEncode (w3_encode_TSeed v)


d_TSeed : W.Decoder TSeed
d_TSeed =
    w3_decode_TSeed


u_TSeed : W.Decoder TSeed
u_TSeed =
    w3_unsafe_decode_TSeed


e_TMX : TMX -> Bytes
e_TMX v =
    W.bytesEncode (w3_encode_TMX v)


d_TMX : W.Decoder TMX
d_TMX =
    w3_decode_TMX


u_TMX : W.Decoder TMX
u_TMX =
    w3_unsafe_decode_TMX


e_TMP : TMP -> Bytes
e_TMP v =
    W.bytesEncode (w3_encode_TMP v)


d_TMP : W.Decoder TMP
d_TMP =
    w3_decode_TMP


u_TMP : W.Decoder TMP
u_TMP =
    w3_unsafe_decode_TMP


e_THolder : THolder -> Bytes
e_THolder v =
    W.bytesEncode (w3_encode_THolder v)


d_THolder : W.Decoder THolder
d_THolder =
    w3_decode_THolder


u_THolder : W.Decoder THolder
u_THolder =
    w3_unsafe_decode_THolder


e_TPRec : TPRec -> Bytes
e_TPRec v =
    W.bytesEncode (w3_encode_TPRec v)


d_TPRec : W.Decoder TPRec
d_TPRec =
    w3_decode_TPRec


u_TPRec : W.Decoder TPRec
u_TPRec =
    w3_unsafe_decode_TPRec


e_Order1 : Order1 -> Bytes
e_Order1 v =
    W.bytesEncode (w3_encode_Order1 v)


d_Order1 : W.Decoder Order1
d_Order1 =
    w3_decode_Order1


u_Order1 : W.Decoder Order1
u_Order1 =
    w3_unsafe_decode_Order1


e_TOrderedTuple : TOrderedTuple -> Bytes
e_TOrderedTuple v =
    W.bytesEncode (w3_encode_TOrderedTuple v)


d_TOrderedTuple : W.Decoder TOrderedTuple
d_TOrderedTuple =
    w3_decode_TOrderedTuple


u_TOrderedTuple : W.Decoder TOrderedTuple
u_TOrderedTuple =
    w3_unsafe_decode_TOrderedTuple


e_TCtorOrder : TCtorOrder -> Bytes
e_TCtorOrder v =
    W.bytesEncode (w3_encode_TCtorOrder v)


d_TCtorOrder : W.Decoder TCtorOrder
d_TCtorOrder =
    w3_decode_TCtorOrder


u_TCtorOrder : W.Decoder TCtorOrder
u_TCtorOrder =
    w3_unsafe_decode_TCtorOrder


e_TExtOrder : TExtOrder -> Bytes
e_TExtOrder v =
    W.bytesEncode (w3_encode_TExtOrder v)


d_TExtOrder : W.Decoder TExtOrder
d_TExtOrder =
    w3_decode_TExtOrder


u_TExtOrder : W.Decoder TExtOrder
u_TExtOrder =
    w3_unsafe_decode_TExtOrder


e_TExt : TExt -> Bytes
e_TExt v =
    W.bytesEncode (w3_encode_TExt v)


d_TExt : W.Decoder TExt
d_TExt =
    w3_decode_TExt


u_TExt : W.Decoder TExt
u_TExt =
    w3_unsafe_decode_TExt


e_TSwap : TSwap -> Bytes
e_TSwap v =
    W.bytesEncode (w3_encode_TSwap v)


d_TSwap : W.Decoder TSwap
d_TSwap =
    w3_decode_TSwap


u_TSwap : W.Decoder TSwap
u_TSwap =
    w3_unsafe_decode_TSwap


e_TDeep3 : TDeep3 -> Bytes
e_TDeep3 v =
    W.bytesEncode (w3_encode_TDeep3 v)


d_TDeep3 : W.Decoder TDeep3
d_TDeep3 =
    w3_decode_TDeep3


u_TDeep3 : W.Decoder TDeep3
u_TDeep3 =
    w3_unsafe_decode_TDeep3


e_TDB : TDB -> Bytes
e_TDB v =
    W.bytesEncode (w3_encode_TDB v)


d_TDB : W.Decoder TDB
d_TDB =
    w3_decode_TDB


u_TDB : W.Decoder TDB
u_TDB =
    w3_unsafe_decode_TDB


e_TVPositive : TVPositive -> Bytes
e_TVPositive v =
    W.bytesEncode (w3_encode_TVPositive v)


d_TVPositive : W.Decoder TVPositive
d_TVPositive =
    w3_decode_TVPositive


u_TVPositive : W.Decoder TVPositive
u_TVPositive =
    w3_unsafe_decode_TVPositive


e_TVTree : TVTree -> Bytes
e_TVTree v =
    W.bytesEncode (w3_encode_TVTree v)


d_TVTree : W.Decoder TVTree
d_TVTree =
    w3_decode_TVTree


u_TVTree : W.Decoder TVTree
u_TVTree =
    w3_unsafe_decode_TVTree


e_TVRose : TVRose -> Bytes
e_TVRose v =
    W.bytesEncode (w3_encode_TVRose v)


d_TVRose : W.Decoder TVRose
d_TVRose =
    w3_decode_TVRose


u_TVRose : W.Decoder TVRose
u_TVRose =
    w3_unsafe_decode_TVRose


e_TVRosePos : TVRosePos -> Bytes
e_TVRosePos v =
    W.bytesEncode (w3_encode_TVRosePos v)


d_TVRosePos : W.Decoder TVRosePos
d_TVRosePos =
    w3_decode_TVRosePos


u_TVRosePos : W.Decoder TVRosePos
u_TVRosePos =
    w3_unsafe_decode_TVRosePos


e_TVPair : TVPair -> Bytes
e_TVPair v =
    W.bytesEncode (w3_encode_TVPair v)


d_TVPair : W.Decoder TVPair
d_TVPair =
    w3_decode_TVPair


u_TVPair : W.Decoder TVPair
u_TVPair =
    w3_unsafe_decode_TVPair


e_TVEnum : TVEnum -> Bytes
e_TVEnum v =
    W.bytesEncode (w3_encode_TVEnum v)


d_TVEnum : W.Decoder TVEnum
d_TVEnum =
    w3_decode_TVEnum


u_TVEnum : W.Decoder TVEnum
u_TVEnum =
    w3_unsafe_decode_TVEnum


e_TVEnums : TVEnums -> Bytes
e_TVEnums v =
    W.bytesEncode (w3_encode_TVEnums v)


d_TVEnums : W.Decoder TVEnums
d_TVEnums =
    w3_decode_TVEnums


u_TVEnums : W.Decoder TVEnums
u_TVEnums =
    w3_unsafe_decode_TVEnums


e_TVBox : TVBox -> Bytes
e_TVBox v =
    W.bytesEncode (w3_encode_TVBox v)


d_TVBox : W.Decoder TVBox
d_TVBox =
    w3_decode_TVBox


u_TVBox : W.Decoder TVBox
u_TVBox =
    w3_unsafe_decode_TVBox


e_TVPing : TVPing -> Bytes
e_TVPing v =
    W.bytesEncode (w3_encode_TVPing v)


d_TVPing : W.Decoder TVPing
d_TVPing =
    w3_decode_TVPing


u_TVPing : W.Decoder TVPing
u_TVPing =
    w3_unsafe_decode_TVPing


e_TVPong : TVPong -> Bytes
e_TVPong v =
    W.bytesEncode (w3_encode_TVPong v)


d_TVPong : W.Decoder TVPong
d_TVPong =
    w3_decode_TVPong


u_TVPong : W.Decoder TVPong
u_TVPong =
    w3_unsafe_decode_TVPong


e_TVPhantom : TVPhantom -> Bytes
e_TVPhantom v =
    W.bytesEncode (w3_encode_TVPhantom v)


d_TVPhantom : W.Decoder TVPhantom
d_TVPhantom =
    w3_decode_TVPhantom


u_TVPhantom : W.Decoder TVPhantom
u_TVPhantom =
    w3_unsafe_decode_TVPhantom


e_TVRec : TVRec -> Bytes
e_TVRec v =
    W.bytesEncode (w3_encode_TVRec v)


d_TVRec : W.Decoder TVRec
d_TVRec =
    w3_decode_TVRec


u_TVRec : W.Decoder TVRec
u_TVRec =
    w3_unsafe_decode_TVRec


e_TVUnvalidated : TVUnvalidated -> Bytes
e_TVUnvalidated v =
    W.bytesEncode (w3_encode_TVUnvalidated v)


d_TVUnvalidated : W.Decoder TVUnvalidated
d_TVUnvalidated =
    w3_decode_TVUnvalidated


u_TVUnvalidated : W.Decoder TVUnvalidated
u_TVUnvalidated =
    w3_unsafe_decode_TVUnvalidated


e_TVModel : TVModel -> Bytes
e_TVModel v =
    W.bytesEncode (w3_encode_TVModel v)


d_TVModel : W.Decoder TVModel
d_TVModel =
    w3_decode_TVModel


u_TVModel : W.Decoder TVModel
u_TVModel =
    w3_unsafe_decode_TVModel


e_TVDict : TVDict -> Bytes
e_TVDict v =
    W.bytesEncode (w3_encode_TVDict v)


d_TVDict : W.Decoder TVDict
d_TVDict =
    w3_decode_TVDict


u_TVDict : W.Decoder TVDict
u_TVDict =
    w3_unsafe_decode_TVDict


e_TAll : TAll -> Bytes
e_TAll v =
    W.bytesEncode (w3_encode_TAll v)


d_TAll : W.Decoder TAll
d_TAll =
    w3_decode_TAll


u_TAll : W.Decoder TAll
u_TAll =
    w3_unsafe_decode_TAll


e_Fat : Fat -> Bytes
e_Fat v =
    W.bytesEncode (w3_encode_Fat v)


d_Fat : W.Decoder Fat
d_Fat =
    w3_decode_Fat


u_Fat : W.Decoder Fat
u_Fat =
    w3_unsafe_decode_Fat


e_Fat2 : Fat2 -> Bytes
e_Fat2 v =
    W.bytesEncode (w3_encode_Fat2 v)


d_Fat2 : W.Decoder Fat2
d_Fat2 =
    w3_decode_Fat2


u_Fat2 : W.Decoder Fat2
u_Fat2 =
    w3_unsafe_decode_Fat2


e_Nest : Nest -> Bytes
e_Nest v =
    W.bytesEncode (w3_encode_Nest v)


d_Nest : W.Decoder Nest
d_Nest =
    w3_decode_Nest


u_Nest : W.Decoder Nest
u_Nest =
    w3_unsafe_decode_Nest


e_Fat3 : Fat3 -> Bytes
e_Fat3 v =
    W.bytesEncode (w3_encode_Fat3 v)


d_Fat3 : W.Decoder Fat3
d_Fat3 =
    w3_decode_Fat3


u_Fat3 : W.Decoder Fat3
u_Fat3 =
    w3_unsafe_decode_Fat3


e_Hist : Hist -> Bytes
e_Hist v =
    W.bytesEncode (w3_encode_Hist v)


d_Hist : W.Decoder Hist
d_Hist =
    w3_decode_Hist


u_Hist : W.Decoder Hist
u_Hist =
    w3_unsafe_decode_Hist


e_Color : Color -> Bytes
e_Color v =
    W.bytesEncode (w3_encode_Color v)


d_Color : W.Decoder Color
d_Color =
    w3_decode_Color


u_Color : W.Decoder Color
u_Color =
    w3_unsafe_decode_Color


e_Id : Id -> Bytes
e_Id v =
    W.bytesEncode (w3_encode_Id v)


d_Id : W.Decoder Id
d_Id =
    w3_decode_Id


u_Id : W.Decoder Id
u_Id =
    w3_unsafe_decode_Id


e_Wrapped : Wrapped -> Bytes
e_Wrapped v =
    W.bytesEncode (w3_encode_Wrapped v)


d_Wrapped : W.Decoder Wrapped
d_Wrapped =
    w3_decode_Wrapped


u_Wrapped : W.Decoder Wrapped
u_Wrapped =
    w3_unsafe_decode_Wrapped


e_Multi : Multi -> Bytes
e_Multi v =
    W.bytesEncode (w3_encode_Multi v)


d_Multi : W.Decoder Multi
d_Multi =
    w3_decode_Multi


u_Multi : W.Decoder Multi
u_Multi =
    w3_unsafe_decode_Multi


e_Mixed : Mixed -> Bytes
e_Mixed v =
    W.bytesEncode (w3_encode_Mixed v)


d_Mixed : W.Decoder Mixed
d_Mixed =
    w3_decode_Mixed


u_Mixed : W.Decoder Mixed
u_Mixed =
    w3_unsafe_decode_Mixed


e_Tree : Tree -> Bytes
e_Tree v =
    W.bytesEncode (w3_encode_Tree v)


d_Tree : W.Decoder Tree
d_Tree =
    w3_decode_Tree


u_Tree : W.Decoder Tree
u_Tree =
    w3_unsafe_decode_Tree


e_Forest : Forest -> Bytes
e_Forest v =
    W.bytesEncode (w3_encode_Forest v)


d_Forest : W.Decoder Forest
d_Forest =
    w3_decode_Forest


u_Forest : W.Decoder Forest
u_Forest =
    w3_unsafe_decode_Forest


e_TreeB : TreeB -> Bytes
e_TreeB v =
    W.bytesEncode (w3_encode_TreeB v)


d_TreeB : W.Decoder TreeB
d_TreeB =
    w3_decode_TreeB


u_TreeB : W.Decoder TreeB
u_TreeB =
    w3_unsafe_decode_TreeB


e_Thing : Thing -> Bytes
e_Thing v =
    W.bytesEncode (w3_encode_Thing v)


d_Thing : W.Decoder Thing
d_Thing =
    w3_decode_Thing


u_Thing : W.Decoder Thing
u_Thing =
    w3_unsafe_decode_Thing


e_HasFn : HasFn -> Bytes
e_HasFn v =
    W.bytesEncode (w3_encode_HasFn v)


d_HasFn : W.Decoder HasFn
d_HasFn =
    w3_decode_HasFn


u_HasFn : W.Decoder HasFn
u_HasFn =
    w3_unsafe_decode_HasFn


e_CtorOrder : CtorOrder -> Bytes
e_CtorOrder v =
    W.bytesEncode (w3_encode_CtorOrder v)


d_CtorOrder : W.Decoder CtorOrder
d_CtorOrder =
    w3_decode_CtorOrder


u_CtorOrder : W.Decoder CtorOrder
u_CtorOrder =
    w3_unsafe_decode_CtorOrder


e_Big : Big -> Bytes
e_Big v =
    W.bytesEncode (w3_encode_Big v)


d_Big : W.Decoder Big
d_Big =
    w3_decode_Big


u_Big : W.Decoder Big
u_Big =
    w3_unsafe_decode_Big


e_Enum300 : Enum300 -> Bytes
e_Enum300 v =
    W.bytesEncode (w3_encode_Enum300 v)


d_Enum300 : W.Decoder Enum300
d_Enum300 =
    w3_decode_Enum300


u_Enum300 : W.Decoder Enum300
u_Enum300 =
    w3_unsafe_decode_Enum300


e_Enum256 : Enum256 -> Bytes
e_Enum256 v =
    W.bytesEncode (w3_encode_Enum256 v)


d_Enum256 : W.Decoder Enum256
d_Enum256 =
    w3_decode_Enum256


u_Enum256 : W.Decoder Enum256
u_Enum256 =
    w3_unsafe_decode_Enum256


e_Enum255 : Enum255 -> Bytes
e_Enum255 v =
    W.bytesEncode (w3_encode_Enum255 v)


d_Enum255 : W.Decoder Enum255
d_Enum255 =
    w3_decode_Enum255


u_Enum255 : W.Decoder Enum255
u_Enum255 =
    w3_unsafe_decode_Enum255


e_IP0_0 : IP0_0 -> Bytes
e_IP0_0 v =
    W.bytesEncode (w3_encode_IP0_0 v)


d_IP0_0 : W.Decoder IP0_0
d_IP0_0 =
    w3_decode_IP0_0


u_IP0_0 : W.Decoder IP0_0
u_IP0_0 =
    w3_unsafe_decode_IP0_0


e_IP0_1 : IP0_1 -> Bytes
e_IP0_1 v =
    W.bytesEncode (w3_encode_IP0_1 v)


d_IP0_1 : W.Decoder IP0_1
d_IP0_1 =
    w3_decode_IP0_1


u_IP0_1 : W.Decoder IP0_1
u_IP0_1 =
    w3_unsafe_decode_IP0_1


e_IP0_2 : IP0_2 -> Bytes
e_IP0_2 v =
    W.bytesEncode (w3_encode_IP0_2 v)


d_IP0_2 : W.Decoder IP0_2
d_IP0_2 =
    w3_decode_IP0_2


u_IP0_2 : W.Decoder IP0_2
u_IP0_2 =
    w3_unsafe_decode_IP0_2


e_IP1_0 : IP1_0 -> Bytes
e_IP1_0 v =
    W.bytesEncode (w3_encode_IP1_0 v)


d_IP1_0 : W.Decoder IP1_0
d_IP1_0 =
    w3_decode_IP1_0


u_IP1_0 : W.Decoder IP1_0
u_IP1_0 =
    w3_unsafe_decode_IP1_0


e_IP1_1 : IP1_1 -> Bytes
e_IP1_1 v =
    W.bytesEncode (w3_encode_IP1_1 v)


d_IP1_1 : W.Decoder IP1_1
d_IP1_1 =
    w3_decode_IP1_1


u_IP1_1 : W.Decoder IP1_1
u_IP1_1 =
    w3_unsafe_decode_IP1_1


e_IP1_2 : IP1_2 -> Bytes
e_IP1_2 v =
    W.bytesEncode (w3_encode_IP1_2 v)


d_IP1_2 : W.Decoder IP1_2
d_IP1_2 =
    w3_decode_IP1_2


u_IP1_2 : W.Decoder IP1_2
u_IP1_2 =
    w3_unsafe_decode_IP1_2


e_IP2_0 : IP2_0 -> Bytes
e_IP2_0 v =
    W.bytesEncode (w3_encode_IP2_0 v)


d_IP2_0 : W.Decoder IP2_0
d_IP2_0 =
    w3_decode_IP2_0


u_IP2_0 : W.Decoder IP2_0
u_IP2_0 =
    w3_unsafe_decode_IP2_0


e_IP2_1 : IP2_1 -> Bytes
e_IP2_1 v =
    W.bytesEncode (w3_encode_IP2_1 v)


d_IP2_1 : W.Decoder IP2_1
d_IP2_1 =
    w3_decode_IP2_1


u_IP2_1 : W.Decoder IP2_1
u_IP2_1 =
    w3_unsafe_decode_IP2_1


e_IP2_2 : IP2_2 -> Bytes
e_IP2_2 v =
    W.bytesEncode (w3_encode_IP2_2 v)


d_IP2_2 : W.Decoder IP2_2
d_IP2_2 =
    w3_decode_IP2_2


u_IP2_2 : W.Decoder IP2_2
u_IP2_2 =
    w3_unsafe_decode_IP2_2


e_IP3_0 : IP3_0 -> Bytes
e_IP3_0 v =
    W.bytesEncode (w3_encode_IP3_0 v)


d_IP3_0 : W.Decoder IP3_0
d_IP3_0 =
    w3_decode_IP3_0


u_IP3_0 : W.Decoder IP3_0
u_IP3_0 =
    w3_unsafe_decode_IP3_0


e_IP4_0 : IP4_0 -> Bytes
e_IP4_0 v =
    W.bytesEncode (w3_encode_IP4_0 v)


d_IP4_0 : W.Decoder IP4_0
d_IP4_0 =
    w3_decode_IP4_0


u_IP4_0 : W.Decoder IP4_0
u_IP4_0 =
    w3_unsafe_decode_IP4_0


e_IP4_1 : IP4_1 -> Bytes
e_IP4_1 v =
    W.bytesEncode (w3_encode_IP4_1 v)


d_IP4_1 : W.Decoder IP4_1
d_IP4_1 =
    w3_decode_IP4_1


u_IP4_1 : W.Decoder IP4_1
u_IP4_1 =
    w3_unsafe_decode_IP4_1


e_IP4_2 : IP4_2 -> Bytes
e_IP4_2 v =
    W.bytesEncode (w3_encode_IP4_2 v)


d_IP4_2 : W.Decoder IP4_2
d_IP4_2 =
    w3_decode_IP4_2


u_IP4_2 : W.Decoder IP4_2
u_IP4_2 =
    w3_unsafe_decode_IP4_2


e_IP5_0 : IP5_0 -> Bytes
e_IP5_0 v =
    W.bytesEncode (w3_encode_IP5_0 v)


d_IP5_0 : W.Decoder IP5_0
d_IP5_0 =
    w3_decode_IP5_0


u_IP5_0 : W.Decoder IP5_0
u_IP5_0 =
    w3_unsafe_decode_IP5_0


e_IP6_0 : IP6_0 -> Bytes
e_IP6_0 v =
    W.bytesEncode (w3_encode_IP6_0 v)


d_IP6_0 : W.Decoder IP6_0
d_IP6_0 =
    w3_decode_IP6_0


u_IP6_0 : W.Decoder IP6_0
u_IP6_0 =
    w3_unsafe_decode_IP6_0


e_IP6_1 : IP6_1 -> Bytes
e_IP6_1 v =
    W.bytesEncode (w3_encode_IP6_1 v)


d_IP6_1 : W.Decoder IP6_1
d_IP6_1 =
    w3_decode_IP6_1


u_IP6_1 : W.Decoder IP6_1
u_IP6_1 =
    w3_unsafe_decode_IP6_1


e_IP6_2 : IP6_2 -> Bytes
e_IP6_2 v =
    W.bytesEncode (w3_encode_IP6_2 v)


d_IP6_2 : W.Decoder IP6_2
d_IP6_2 =
    w3_decode_IP6_2


u_IP6_2 : W.Decoder IP6_2
u_IP6_2 =
    w3_unsafe_decode_IP6_2


e_IP7_0 : IP7_0 -> Bytes
e_IP7_0 v =
    W.bytesEncode (w3_encode_IP7_0 v)


d_IP7_0 : W.Decoder IP7_0
d_IP7_0 =
    w3_decode_IP7_0


u_IP7_0 : W.Decoder IP7_0
u_IP7_0 =
    w3_unsafe_decode_IP7_0


e_IP7_1 : IP7_1 -> Bytes
e_IP7_1 v =
    W.bytesEncode (w3_encode_IP7_1 v)


d_IP7_1 : W.Decoder IP7_1
d_IP7_1 =
    w3_decode_IP7_1


u_IP7_1 : W.Decoder IP7_1
u_IP7_1 =
    w3_unsafe_decode_IP7_1


e_IP7_2 : IP7_2 -> Bytes
e_IP7_2 v =
    W.bytesEncode (w3_encode_IP7_2 v)


d_IP7_2 : W.Decoder IP7_2
d_IP7_2 =
    w3_decode_IP7_2


u_IP7_2 : W.Decoder IP7_2
u_IP7_2 =
    w3_unsafe_decode_IP7_2


e_IP8_0 : IP8_0 -> Bytes
e_IP8_0 v =
    W.bytesEncode (w3_encode_IP8_0 v)


d_IP8_0 : W.Decoder IP8_0
d_IP8_0 =
    w3_decode_IP8_0


u_IP8_0 : W.Decoder IP8_0
u_IP8_0 =
    w3_unsafe_decode_IP8_0


e_IP8_1 : IP8_1 -> Bytes
e_IP8_1 v =
    W.bytesEncode (w3_encode_IP8_1 v)


d_IP8_1 : W.Decoder IP8_1
d_IP8_1 =
    w3_decode_IP8_1


u_IP8_1 : W.Decoder IP8_1
u_IP8_1 =
    w3_unsafe_decode_IP8_1


e_IP8_2 : IP8_2 -> Bytes
e_IP8_2 v =
    W.bytesEncode (w3_encode_IP8_2 v)


d_IP8_2 : W.Decoder IP8_2
d_IP8_2 =
    w3_decode_IP8_2


u_IP8_2 : W.Decoder IP8_2
u_IP8_2 =
    w3_unsafe_decode_IP8_2


e_IP9_0 : IP9_0 -> Bytes
e_IP9_0 v =
    W.bytesEncode (w3_encode_IP9_0 v)


d_IP9_0 : W.Decoder IP9_0
d_IP9_0 =
    w3_decode_IP9_0


u_IP9_0 : W.Decoder IP9_0
u_IP9_0 =
    w3_unsafe_decode_IP9_0


e_IP9_1 : IP9_1 -> Bytes
e_IP9_1 v =
    W.bytesEncode (w3_encode_IP9_1 v)


d_IP9_1 : W.Decoder IP9_1
d_IP9_1 =
    w3_decode_IP9_1


u_IP9_1 : W.Decoder IP9_1
u_IP9_1 =
    w3_unsafe_decode_IP9_1


e_IP9_2 : IP9_2 -> Bytes
e_IP9_2 v =
    W.bytesEncode (w3_encode_IP9_2 v)


d_IP9_2 : W.Decoder IP9_2
d_IP9_2 =
    w3_decode_IP9_2


u_IP9_2 : W.Decoder IP9_2
u_IP9_2 =
    w3_unsafe_decode_IP9_2


e_IP10_0 : IP10_0 -> Bytes
e_IP10_0 v =
    W.bytesEncode (w3_encode_IP10_0 v)


d_IP10_0 : W.Decoder IP10_0
d_IP10_0 =
    w3_decode_IP10_0


u_IP10_0 : W.Decoder IP10_0
u_IP10_0 =
    w3_unsafe_decode_IP10_0


e_IP10_1 : IP10_1 -> Bytes
e_IP10_1 v =
    W.bytesEncode (w3_encode_IP10_1 v)


d_IP10_1 : W.Decoder IP10_1
d_IP10_1 =
    w3_decode_IP10_1


u_IP10_1 : W.Decoder IP10_1
u_IP10_1 =
    w3_unsafe_decode_IP10_1


e_IP10_2 : IP10_2 -> Bytes
e_IP10_2 v =
    W.bytesEncode (w3_encode_IP10_2 v)


d_IP10_2 : W.Decoder IP10_2
d_IP10_2 =
    w3_decode_IP10_2


u_IP10_2 : W.Decoder IP10_2
u_IP10_2 =
    w3_unsafe_decode_IP10_2


e_IP11_0 : IP11_0 -> Bytes
e_IP11_0 v =
    W.bytesEncode (w3_encode_IP11_0 v)


d_IP11_0 : W.Decoder IP11_0
d_IP11_0 =
    w3_decode_IP11_0


u_IP11_0 : W.Decoder IP11_0
u_IP11_0 =
    w3_unsafe_decode_IP11_0


e_IP11_1 : IP11_1 -> Bytes
e_IP11_1 v =
    W.bytesEncode (w3_encode_IP11_1 v)


d_IP11_1 : W.Decoder IP11_1
d_IP11_1 =
    w3_decode_IP11_1


u_IP11_1 : W.Decoder IP11_1
u_IP11_1 =
    w3_unsafe_decode_IP11_1


e_IP11_2 : IP11_2 -> Bytes
e_IP11_2 v =
    W.bytesEncode (w3_encode_IP11_2 v)


d_IP11_2 : W.Decoder IP11_2
d_IP11_2 =
    w3_decode_IP11_2


u_IP11_2 : W.Decoder IP11_2
u_IP11_2 =
    w3_unsafe_decode_IP11_2


e_IP12_0 : IP12_0 -> Bytes
e_IP12_0 v =
    W.bytesEncode (w3_encode_IP12_0 v)


d_IP12_0 : W.Decoder IP12_0
d_IP12_0 =
    w3_decode_IP12_0


u_IP12_0 : W.Decoder IP12_0
u_IP12_0 =
    w3_unsafe_decode_IP12_0


e_IP12_1 : IP12_1 -> Bytes
e_IP12_1 v =
    W.bytesEncode (w3_encode_IP12_1 v)


d_IP12_1 : W.Decoder IP12_1
d_IP12_1 =
    w3_decode_IP12_1


u_IP12_1 : W.Decoder IP12_1
u_IP12_1 =
    w3_unsafe_decode_IP12_1


e_IP12_2 : IP12_2 -> Bytes
e_IP12_2 v =
    W.bytesEncode (w3_encode_IP12_2 v)


d_IP12_2 : W.Decoder IP12_2
d_IP12_2 =
    w3_decode_IP12_2


u_IP12_2 : W.Decoder IP12_2
u_IP12_2 =
    w3_unsafe_decode_IP12_2


e_IP13_0 : IP13_0 -> Bytes
e_IP13_0 v =
    W.bytesEncode (w3_encode_IP13_0 v)


d_IP13_0 : W.Decoder IP13_0
d_IP13_0 =
    w3_decode_IP13_0


u_IP13_0 : W.Decoder IP13_0
u_IP13_0 =
    w3_unsafe_decode_IP13_0


e_IP13_1 : IP13_1 -> Bytes
e_IP13_1 v =
    W.bytesEncode (w3_encode_IP13_1 v)


d_IP13_1 : W.Decoder IP13_1
d_IP13_1 =
    w3_decode_IP13_1


u_IP13_1 : W.Decoder IP13_1
u_IP13_1 =
    w3_unsafe_decode_IP13_1


e_IP13_2 : IP13_2 -> Bytes
e_IP13_2 v =
    W.bytesEncode (w3_encode_IP13_2 v)


d_IP13_2 : W.Decoder IP13_2
d_IP13_2 =
    w3_decode_IP13_2


u_IP13_2 : W.Decoder IP13_2
u_IP13_2 =
    w3_unsafe_decode_IP13_2


e_PModel : PModel -> Bytes
e_PModel v =
    W.bytesEncode (w3_encode_PModel v)


d_PModel : W.Decoder PModel
d_PModel =
    w3_decode_PModel


u_PModel : W.Decoder PModel
u_PModel =
    w3_unsafe_decode_PModel


type alias Api =
    { s_TAll : TAll
    , s_TVModel : TVModel
    , readBytesPast : Int -> Bytes -> Maybe Int
    , e_TInt : TInt -> Bytes, d_TInt : W.Decoder TInt, u_TInt : W.Decoder TInt
    , e_TFloat : TFloat -> Bytes, d_TFloat : W.Decoder TFloat, u_TFloat : W.Decoder TFloat
    , e_TString : TString -> Bytes, d_TString : W.Decoder TString, u_TString : W.Decoder TString
    , e_TChar : TChar -> Bytes, d_TChar : W.Decoder TChar, u_TChar : W.Decoder TChar
    , e_TBool : TBool -> Bytes, d_TBool : W.Decoder TBool, u_TBool : W.Decoder TBool
    , e_TOrder : TOrder -> Bytes, d_TOrder : W.Decoder TOrder, u_TOrder : W.Decoder TOrder
    , e_TUnit : TUnit -> Bytes, d_TUnit : W.Decoder TUnit, u_TUnit : W.Decoder TUnit
    , e_TBytes : TBytes -> Bytes, d_TBytes : W.Decoder TBytes, u_TBytes : W.Decoder TBytes
    , e_TPosix : TPosix -> Bytes, d_TPosix : W.Decoder TPosix, u_TPosix : W.Decoder TPosix
    , e_TMaybe : TMaybe -> Bytes, d_TMaybe : W.Decoder TMaybe, u_TMaybe : W.Decoder TMaybe
    , e_TMaybeMaybe : TMaybeMaybe -> Bytes, d_TMaybeMaybe : W.Decoder TMaybeMaybe, u_TMaybeMaybe : W.Decoder TMaybeMaybe
    , e_TResult : TResult -> Bytes, d_TResult : W.Decoder TResult, u_TResult : W.Decoder TResult
    , e_TList : TList -> Bytes, d_TList : W.Decoder TList, u_TList : W.Decoder TList
    , e_TListUnit : TListUnit -> Bytes, d_TListUnit : W.Decoder TListUnit, u_TListUnit : W.Decoder TListUnit
    , e_TListStr : TListStr -> Bytes, d_TListStr : W.Decoder TListStr, u_TListStr : W.Decoder TListStr
    , e_TArray : TArray -> Bytes, d_TArray : W.Decoder TArray, u_TArray : W.Decoder TArray
    , e_TSetInt : TSetInt -> Bytes, d_TSetInt : W.Decoder TSetInt, u_TSetInt : W.Decoder TSetInt
    , e_TDictIntFloat : TDictIntFloat -> Bytes, d_TDictIntFloat : W.Decoder TDictIntFloat, u_TDictIntFloat : W.Decoder TDictIntFloat
    , e_TArrayFloat : TArrayFloat -> Bytes, d_TArrayFloat : W.Decoder TArrayFloat, u_TArrayFloat : W.Decoder TArrayFloat
    , e_TListFloat : TListFloat -> Bytes, d_TListFloat : W.Decoder TListFloat, u_TListFloat : W.Decoder TListFloat
    , e_TRecFloat : TRecFloat -> Bytes, d_TRecFloat : W.Decoder TRecFloat, u_TRecFloat : W.Decoder TRecFloat
    , e_TSeqDictFloat : TSeqDictFloat -> Bytes, d_TSeqDictFloat : W.Decoder TSeqDictFloat, u_TSeqDictFloat : W.Decoder TSeqDictFloat
    , e_TSetFloat : TSetFloat -> Bytes, d_TSetFloat : W.Decoder TSetFloat, u_TSetFloat : W.Decoder TSetFloat
    , e_TSetString : TSetString -> Bytes, d_TSetString : W.Decoder TSetString, u_TSetString : W.Decoder TSetString
    , e_TSetChar : TSetChar -> Bytes, d_TSetChar : W.Decoder TSetChar, u_TSetChar : W.Decoder TSetChar
    , e_TSetTuple : TSetTuple -> Bytes, d_TSetTuple : W.Decoder TSetTuple, u_TSetTuple : W.Decoder TSetTuple
    , e_TSetList : TSetList -> Bytes, d_TSetList : W.Decoder TSetList, u_TSetList : W.Decoder TSetList
    , e_TDictInt : TDictInt -> Bytes, d_TDictInt : W.Decoder TDictInt, u_TDictInt : W.Decoder TDictInt
    , e_TDictFloat : TDictFloat -> Bytes, d_TDictFloat : W.Decoder TDictFloat, u_TDictFloat : W.Decoder TDictFloat
    , e_TDictString : TDictString -> Bytes, d_TDictString : W.Decoder TDictString, u_TDictString : W.Decoder TDictString
    , e_TDictChar : TDictChar -> Bytes, d_TDictChar : W.Decoder TDictChar, u_TDictChar : W.Decoder TDictChar
    , e_TDictTuple : TDictTuple -> Bytes, d_TDictTuple : W.Decoder TDictTuple, u_TDictTuple : W.Decoder TDictTuple
    , e_TDictTriple : TDictTriple -> Bytes, d_TDictTriple : W.Decoder TDictTriple, u_TDictTriple : W.Decoder TDictTriple
    , e_TDictList : TDictList -> Bytes, d_TDictList : W.Decoder TDictList, u_TDictList : W.Decoder TDictList
    , e_TSeqDict : TSeqDict -> Bytes, d_TSeqDict : W.Decoder TSeqDict, u_TSeqDict : W.Decoder TSeqDict
    , e_TSeqDictRec : TSeqDictRec -> Bytes, d_TSeqDictRec : W.Decoder TSeqDictRec, u_TSeqDictRec : W.Decoder TSeqDictRec
    , e_TSeqDictDict : TSeqDictDict -> Bytes, d_TSeqDictDict : W.Decoder TSeqDictDict, u_TSeqDictDict : W.Decoder TSeqDictDict
    , e_TSeqSet : TSeqSet -> Bytes, d_TSeqSet : W.Decoder TSeqSet, u_TSeqSet : W.Decoder TSeqSet
    , e_TPair : TPair -> Bytes, d_TPair : W.Decoder TPair, u_TPair : W.Decoder TPair
    , e_TTriple : TTriple -> Bytes, d_TTriple : W.Decoder TTriple, u_TTriple : W.Decoder TTriple
    , e_TColor : TColor -> Bytes, d_TColor : W.Decoder TColor, u_TColor : W.Decoder TColor
    , e_TBoxes : TBoxes -> Bytes, d_TBoxes : W.Decoder TBoxes, u_TBoxes : W.Decoder TBoxes
    , e_TTwo : TTwo -> Bytes, d_TTwo : W.Decoder TTwo, u_TTwo : W.Decoder TTwo
    , e_TWrap : TWrap -> Bytes, d_TWrap : W.Decoder TWrap, u_TWrap : W.Decoder TWrap
    , e_TRose : TRose -> Bytes, d_TRose : W.Decoder TRose, u_TRose : W.Decoder TRose
    , e_Reserved : Reserved -> Bytes, d_Reserved : W.Decoder Reserved, u_Reserved : W.Decoder Reserved
    , e_Person : Person -> Bytes, d_Person : W.Decoder Person, u_Person : W.Decoder Person
    , e_TNamedField : TNamedField -> Bytes, d_TNamedField : W.Decoder TNamedField, u_TNamedField : W.Decoder TNamedField
    , e_Nested : Nested -> Bytes, d_Nested : W.Decoder Nested, u_Nested : W.Decoder Nested
    , e_TNever : TNever -> Bytes, d_TNever : W.Decoder TNever, u_TNever : W.Decoder TNever
    , e_TResultNever : TResultNever -> Bytes, d_TResultNever : W.Decoder TResultNever, u_TResultNever : W.Decoder TResultNever
    , e_TListNever : TListNever -> Bytes, d_TListNever : W.Decoder TListNever, u_TListNever : W.Decoder TListNever
    , e_TPhantom : TPhantom -> Bytes, d_TPhantom : W.Decoder TPhantom, u_TPhantom : W.Decoder TPhantom
    , e_TSeed : TSeed -> Bytes, d_TSeed : W.Decoder TSeed, u_TSeed : W.Decoder TSeed
    , e_TMX : TMX -> Bytes, d_TMX : W.Decoder TMX, u_TMX : W.Decoder TMX
    , e_TMP : TMP -> Bytes, d_TMP : W.Decoder TMP, u_TMP : W.Decoder TMP
    , e_THolder : THolder -> Bytes, d_THolder : W.Decoder THolder, u_THolder : W.Decoder THolder
    , e_TPRec : TPRec -> Bytes, d_TPRec : W.Decoder TPRec, u_TPRec : W.Decoder TPRec
    , e_Order1 : Order1 -> Bytes, d_Order1 : W.Decoder Order1, u_Order1 : W.Decoder Order1
    , e_TOrderedTuple : TOrderedTuple -> Bytes, d_TOrderedTuple : W.Decoder TOrderedTuple, u_TOrderedTuple : W.Decoder TOrderedTuple
    , e_TCtorOrder : TCtorOrder -> Bytes, d_TCtorOrder : W.Decoder TCtorOrder, u_TCtorOrder : W.Decoder TCtorOrder
    , e_TExtOrder : TExtOrder -> Bytes, d_TExtOrder : W.Decoder TExtOrder, u_TExtOrder : W.Decoder TExtOrder
    , e_TExt : TExt -> Bytes, d_TExt : W.Decoder TExt, u_TExt : W.Decoder TExt
    , e_TSwap : TSwap -> Bytes, d_TSwap : W.Decoder TSwap, u_TSwap : W.Decoder TSwap
    , e_TDeep3 : TDeep3 -> Bytes, d_TDeep3 : W.Decoder TDeep3, u_TDeep3 : W.Decoder TDeep3
    , e_TDB : TDB -> Bytes, d_TDB : W.Decoder TDB, u_TDB : W.Decoder TDB
    , e_TVPositive : TVPositive -> Bytes, d_TVPositive : W.Decoder TVPositive, u_TVPositive : W.Decoder TVPositive
    , e_TVTree : TVTree -> Bytes, d_TVTree : W.Decoder TVTree, u_TVTree : W.Decoder TVTree
    , e_TVRose : TVRose -> Bytes, d_TVRose : W.Decoder TVRose, u_TVRose : W.Decoder TVRose
    , e_TVRosePos : TVRosePos -> Bytes, d_TVRosePos : W.Decoder TVRosePos, u_TVRosePos : W.Decoder TVRosePos
    , e_TVPair : TVPair -> Bytes, d_TVPair : W.Decoder TVPair, u_TVPair : W.Decoder TVPair
    , e_TVEnum : TVEnum -> Bytes, d_TVEnum : W.Decoder TVEnum, u_TVEnum : W.Decoder TVEnum
    , e_TVEnums : TVEnums -> Bytes, d_TVEnums : W.Decoder TVEnums, u_TVEnums : W.Decoder TVEnums
    , e_TVBox : TVBox -> Bytes, d_TVBox : W.Decoder TVBox, u_TVBox : W.Decoder TVBox
    , e_TVPing : TVPing -> Bytes, d_TVPing : W.Decoder TVPing, u_TVPing : W.Decoder TVPing
    , e_TVPong : TVPong -> Bytes, d_TVPong : W.Decoder TVPong, u_TVPong : W.Decoder TVPong
    , e_TVPhantom : TVPhantom -> Bytes, d_TVPhantom : W.Decoder TVPhantom, u_TVPhantom : W.Decoder TVPhantom
    , e_TVRec : TVRec -> Bytes, d_TVRec : W.Decoder TVRec, u_TVRec : W.Decoder TVRec
    , e_TVUnvalidated : TVUnvalidated -> Bytes, d_TVUnvalidated : W.Decoder TVUnvalidated, u_TVUnvalidated : W.Decoder TVUnvalidated
    , e_TVModel : TVModel -> Bytes, d_TVModel : W.Decoder TVModel, u_TVModel : W.Decoder TVModel
    , e_TVDict : TVDict -> Bytes, d_TVDict : W.Decoder TVDict, u_TVDict : W.Decoder TVDict
    , e_TAll : TAll -> Bytes, d_TAll : W.Decoder TAll, u_TAll : W.Decoder TAll
    , e_Fat : Fat -> Bytes, d_Fat : W.Decoder Fat, u_Fat : W.Decoder Fat
    , e_Fat2 : Fat2 -> Bytes, d_Fat2 : W.Decoder Fat2, u_Fat2 : W.Decoder Fat2
    , e_Nest : Nest -> Bytes, d_Nest : W.Decoder Nest, u_Nest : W.Decoder Nest
    , e_Fat3 : Fat3 -> Bytes, d_Fat3 : W.Decoder Fat3, u_Fat3 : W.Decoder Fat3
    , e_Hist : Hist -> Bytes, d_Hist : W.Decoder Hist, u_Hist : W.Decoder Hist
    , e_Color : Color -> Bytes, d_Color : W.Decoder Color, u_Color : W.Decoder Color
    , e_Id : Id -> Bytes, d_Id : W.Decoder Id, u_Id : W.Decoder Id
    , e_Wrapped : Wrapped -> Bytes, d_Wrapped : W.Decoder Wrapped, u_Wrapped : W.Decoder Wrapped
    , e_Multi : Multi -> Bytes, d_Multi : W.Decoder Multi, u_Multi : W.Decoder Multi
    , e_Mixed : Mixed -> Bytes, d_Mixed : W.Decoder Mixed, u_Mixed : W.Decoder Mixed
    , e_Tree : Tree -> Bytes, d_Tree : W.Decoder Tree, u_Tree : W.Decoder Tree
    , e_Forest : Forest -> Bytes, d_Forest : W.Decoder Forest, u_Forest : W.Decoder Forest
    , e_TreeB : TreeB -> Bytes, d_TreeB : W.Decoder TreeB, u_TreeB : W.Decoder TreeB
    , e_Thing : Thing -> Bytes, d_Thing : W.Decoder Thing, u_Thing : W.Decoder Thing
    , e_HasFn : HasFn -> Bytes, d_HasFn : W.Decoder HasFn, u_HasFn : W.Decoder HasFn
    , e_CtorOrder : CtorOrder -> Bytes, d_CtorOrder : W.Decoder CtorOrder, u_CtorOrder : W.Decoder CtorOrder
    , e_Big : Big -> Bytes, d_Big : W.Decoder Big, u_Big : W.Decoder Big
    , e_Enum300 : Enum300 -> Bytes, d_Enum300 : W.Decoder Enum300, u_Enum300 : W.Decoder Enum300
    , e_Enum256 : Enum256 -> Bytes, d_Enum256 : W.Decoder Enum256, u_Enum256 : W.Decoder Enum256
    , e_Enum255 : Enum255 -> Bytes, d_Enum255 : W.Decoder Enum255, u_Enum255 : W.Decoder Enum255
    , e_IP0_0 : IP0_0 -> Bytes, d_IP0_0 : W.Decoder IP0_0, u_IP0_0 : W.Decoder IP0_0
    , e_IP0_1 : IP0_1 -> Bytes, d_IP0_1 : W.Decoder IP0_1, u_IP0_1 : W.Decoder IP0_1
    , e_IP0_2 : IP0_2 -> Bytes, d_IP0_2 : W.Decoder IP0_2, u_IP0_2 : W.Decoder IP0_2
    , e_IP1_0 : IP1_0 -> Bytes, d_IP1_0 : W.Decoder IP1_0, u_IP1_0 : W.Decoder IP1_0
    , e_IP1_1 : IP1_1 -> Bytes, d_IP1_1 : W.Decoder IP1_1, u_IP1_1 : W.Decoder IP1_1
    , e_IP1_2 : IP1_2 -> Bytes, d_IP1_2 : W.Decoder IP1_2, u_IP1_2 : W.Decoder IP1_2
    , e_IP2_0 : IP2_0 -> Bytes, d_IP2_0 : W.Decoder IP2_0, u_IP2_0 : W.Decoder IP2_0
    , e_IP2_1 : IP2_1 -> Bytes, d_IP2_1 : W.Decoder IP2_1, u_IP2_1 : W.Decoder IP2_1
    , e_IP2_2 : IP2_2 -> Bytes, d_IP2_2 : W.Decoder IP2_2, u_IP2_2 : W.Decoder IP2_2
    , e_IP3_0 : IP3_0 -> Bytes, d_IP3_0 : W.Decoder IP3_0, u_IP3_0 : W.Decoder IP3_0
    , e_IP4_0 : IP4_0 -> Bytes, d_IP4_0 : W.Decoder IP4_0, u_IP4_0 : W.Decoder IP4_0
    , e_IP4_1 : IP4_1 -> Bytes, d_IP4_1 : W.Decoder IP4_1, u_IP4_1 : W.Decoder IP4_1
    , e_IP4_2 : IP4_2 -> Bytes, d_IP4_2 : W.Decoder IP4_2, u_IP4_2 : W.Decoder IP4_2
    , e_IP5_0 : IP5_0 -> Bytes, d_IP5_0 : W.Decoder IP5_0, u_IP5_0 : W.Decoder IP5_0
    , e_IP6_0 : IP6_0 -> Bytes, d_IP6_0 : W.Decoder IP6_0, u_IP6_0 : W.Decoder IP6_0
    , e_IP6_1 : IP6_1 -> Bytes, d_IP6_1 : W.Decoder IP6_1, u_IP6_1 : W.Decoder IP6_1
    , e_IP6_2 : IP6_2 -> Bytes, d_IP6_2 : W.Decoder IP6_2, u_IP6_2 : W.Decoder IP6_2
    , e_IP7_0 : IP7_0 -> Bytes, d_IP7_0 : W.Decoder IP7_0, u_IP7_0 : W.Decoder IP7_0
    , e_IP7_1 : IP7_1 -> Bytes, d_IP7_1 : W.Decoder IP7_1, u_IP7_1 : W.Decoder IP7_1
    , e_IP7_2 : IP7_2 -> Bytes, d_IP7_2 : W.Decoder IP7_2, u_IP7_2 : W.Decoder IP7_2
    , e_IP8_0 : IP8_0 -> Bytes, d_IP8_0 : W.Decoder IP8_0, u_IP8_0 : W.Decoder IP8_0
    , e_IP8_1 : IP8_1 -> Bytes, d_IP8_1 : W.Decoder IP8_1, u_IP8_1 : W.Decoder IP8_1
    , e_IP8_2 : IP8_2 -> Bytes, d_IP8_2 : W.Decoder IP8_2, u_IP8_2 : W.Decoder IP8_2
    , e_IP9_0 : IP9_0 -> Bytes, d_IP9_0 : W.Decoder IP9_0, u_IP9_0 : W.Decoder IP9_0
    , e_IP9_1 : IP9_1 -> Bytes, d_IP9_1 : W.Decoder IP9_1, u_IP9_1 : W.Decoder IP9_1
    , e_IP9_2 : IP9_2 -> Bytes, d_IP9_2 : W.Decoder IP9_2, u_IP9_2 : W.Decoder IP9_2
    , e_IP10_0 : IP10_0 -> Bytes, d_IP10_0 : W.Decoder IP10_0, u_IP10_0 : W.Decoder IP10_0
    , e_IP10_1 : IP10_1 -> Bytes, d_IP10_1 : W.Decoder IP10_1, u_IP10_1 : W.Decoder IP10_1
    , e_IP10_2 : IP10_2 -> Bytes, d_IP10_2 : W.Decoder IP10_2, u_IP10_2 : W.Decoder IP10_2
    , e_IP11_0 : IP11_0 -> Bytes, d_IP11_0 : W.Decoder IP11_0, u_IP11_0 : W.Decoder IP11_0
    , e_IP11_1 : IP11_1 -> Bytes, d_IP11_1 : W.Decoder IP11_1, u_IP11_1 : W.Decoder IP11_1
    , e_IP11_2 : IP11_2 -> Bytes, d_IP11_2 : W.Decoder IP11_2, u_IP11_2 : W.Decoder IP11_2
    , e_IP12_0 : IP12_0 -> Bytes, d_IP12_0 : W.Decoder IP12_0, u_IP12_0 : W.Decoder IP12_0
    , e_IP12_1 : IP12_1 -> Bytes, d_IP12_1 : W.Decoder IP12_1, u_IP12_1 : W.Decoder IP12_1
    , e_IP12_2 : IP12_2 -> Bytes, d_IP12_2 : W.Decoder IP12_2, u_IP12_2 : W.Decoder IP12_2
    , e_IP13_0 : IP13_0 -> Bytes, d_IP13_0 : W.Decoder IP13_0, u_IP13_0 : W.Decoder IP13_0
    , e_IP13_1 : IP13_1 -> Bytes, d_IP13_1 : W.Decoder IP13_1, u_IP13_1 : W.Decoder IP13_1
    , e_IP13_2 : IP13_2 -> Bytes, d_IP13_2 : W.Decoder IP13_2, u_IP13_2 : W.Decoder IP13_2
    , e_PModel : PModel -> Bytes, d_PModel : W.Decoder PModel, u_PModel : W.Decoder PModel
    }


api : Api

api =
    { s_TAll = s_TAll
    , s_TVModel = s_TVModel
    , readBytesPast = readBytesPast
    , e_TInt = e_TInt, d_TInt = d_TInt, u_TInt = u_TInt
    , e_TFloat = e_TFloat, d_TFloat = d_TFloat, u_TFloat = u_TFloat
    , e_TString = e_TString, d_TString = d_TString, u_TString = u_TString
    , e_TChar = e_TChar, d_TChar = d_TChar, u_TChar = u_TChar
    , e_TBool = e_TBool, d_TBool = d_TBool, u_TBool = u_TBool
    , e_TOrder = e_TOrder, d_TOrder = d_TOrder, u_TOrder = u_TOrder
    , e_TUnit = e_TUnit, d_TUnit = d_TUnit, u_TUnit = u_TUnit
    , e_TBytes = e_TBytes, d_TBytes = d_TBytes, u_TBytes = u_TBytes
    , e_TPosix = e_TPosix, d_TPosix = d_TPosix, u_TPosix = u_TPosix
    , e_TMaybe = e_TMaybe, d_TMaybe = d_TMaybe, u_TMaybe = u_TMaybe
    , e_TMaybeMaybe = e_TMaybeMaybe, d_TMaybeMaybe = d_TMaybeMaybe, u_TMaybeMaybe = u_TMaybeMaybe
    , e_TResult = e_TResult, d_TResult = d_TResult, u_TResult = u_TResult
    , e_TList = e_TList, d_TList = d_TList, u_TList = u_TList
    , e_TListUnit = e_TListUnit, d_TListUnit = d_TListUnit, u_TListUnit = u_TListUnit
    , e_TListStr = e_TListStr, d_TListStr = d_TListStr, u_TListStr = u_TListStr
    , e_TArray = e_TArray, d_TArray = d_TArray, u_TArray = u_TArray
    , e_TSetInt = e_TSetInt, d_TSetInt = d_TSetInt, u_TSetInt = u_TSetInt
    , e_TDictIntFloat = e_TDictIntFloat, d_TDictIntFloat = d_TDictIntFloat, u_TDictIntFloat = u_TDictIntFloat
    , e_TArrayFloat = e_TArrayFloat, d_TArrayFloat = d_TArrayFloat, u_TArrayFloat = u_TArrayFloat
    , e_TListFloat = e_TListFloat, d_TListFloat = d_TListFloat, u_TListFloat = u_TListFloat
    , e_TRecFloat = e_TRecFloat, d_TRecFloat = d_TRecFloat, u_TRecFloat = u_TRecFloat
    , e_TSeqDictFloat = e_TSeqDictFloat, d_TSeqDictFloat = d_TSeqDictFloat, u_TSeqDictFloat = u_TSeqDictFloat
    , e_TSetFloat = e_TSetFloat, d_TSetFloat = d_TSetFloat, u_TSetFloat = u_TSetFloat
    , e_TSetString = e_TSetString, d_TSetString = d_TSetString, u_TSetString = u_TSetString
    , e_TSetChar = e_TSetChar, d_TSetChar = d_TSetChar, u_TSetChar = u_TSetChar
    , e_TSetTuple = e_TSetTuple, d_TSetTuple = d_TSetTuple, u_TSetTuple = u_TSetTuple
    , e_TSetList = e_TSetList, d_TSetList = d_TSetList, u_TSetList = u_TSetList
    , e_TDictInt = e_TDictInt, d_TDictInt = d_TDictInt, u_TDictInt = u_TDictInt
    , e_TDictFloat = e_TDictFloat, d_TDictFloat = d_TDictFloat, u_TDictFloat = u_TDictFloat
    , e_TDictString = e_TDictString, d_TDictString = d_TDictString, u_TDictString = u_TDictString
    , e_TDictChar = e_TDictChar, d_TDictChar = d_TDictChar, u_TDictChar = u_TDictChar
    , e_TDictTuple = e_TDictTuple, d_TDictTuple = d_TDictTuple, u_TDictTuple = u_TDictTuple
    , e_TDictTriple = e_TDictTriple, d_TDictTriple = d_TDictTriple, u_TDictTriple = u_TDictTriple
    , e_TDictList = e_TDictList, d_TDictList = d_TDictList, u_TDictList = u_TDictList
    , e_TSeqDict = e_TSeqDict, d_TSeqDict = d_TSeqDict, u_TSeqDict = u_TSeqDict
    , e_TSeqDictRec = e_TSeqDictRec, d_TSeqDictRec = d_TSeqDictRec, u_TSeqDictRec = u_TSeqDictRec
    , e_TSeqDictDict = e_TSeqDictDict, d_TSeqDictDict = d_TSeqDictDict, u_TSeqDictDict = u_TSeqDictDict
    , e_TSeqSet = e_TSeqSet, d_TSeqSet = d_TSeqSet, u_TSeqSet = u_TSeqSet
    , e_TPair = e_TPair, d_TPair = d_TPair, u_TPair = u_TPair
    , e_TTriple = e_TTriple, d_TTriple = d_TTriple, u_TTriple = u_TTriple
    , e_TColor = e_TColor, d_TColor = d_TColor, u_TColor = u_TColor
    , e_TBoxes = e_TBoxes, d_TBoxes = d_TBoxes, u_TBoxes = u_TBoxes
    , e_TTwo = e_TTwo, d_TTwo = d_TTwo, u_TTwo = u_TTwo
    , e_TWrap = e_TWrap, d_TWrap = d_TWrap, u_TWrap = u_TWrap
    , e_TRose = e_TRose, d_TRose = d_TRose, u_TRose = u_TRose
    , e_Reserved = e_Reserved, d_Reserved = d_Reserved, u_Reserved = u_Reserved
    , e_Person = e_Person, d_Person = d_Person, u_Person = u_Person
    , e_TNamedField = e_TNamedField, d_TNamedField = d_TNamedField, u_TNamedField = u_TNamedField
    , e_Nested = e_Nested, d_Nested = d_Nested, u_Nested = u_Nested
    , e_TNever = e_TNever, d_TNever = d_TNever, u_TNever = u_TNever
    , e_TResultNever = e_TResultNever, d_TResultNever = d_TResultNever, u_TResultNever = u_TResultNever
    , e_TListNever = e_TListNever, d_TListNever = d_TListNever, u_TListNever = u_TListNever
    , e_TPhantom = e_TPhantom, d_TPhantom = d_TPhantom, u_TPhantom = u_TPhantom
    , e_TSeed = e_TSeed, d_TSeed = d_TSeed, u_TSeed = u_TSeed
    , e_TMX = e_TMX, d_TMX = d_TMX, u_TMX = u_TMX
    , e_TMP = e_TMP, d_TMP = d_TMP, u_TMP = u_TMP
    , e_THolder = e_THolder, d_THolder = d_THolder, u_THolder = u_THolder
    , e_TPRec = e_TPRec, d_TPRec = d_TPRec, u_TPRec = u_TPRec
    , e_Order1 = e_Order1, d_Order1 = d_Order1, u_Order1 = u_Order1
    , e_TOrderedTuple = e_TOrderedTuple, d_TOrderedTuple = d_TOrderedTuple, u_TOrderedTuple = u_TOrderedTuple
    , e_TCtorOrder = e_TCtorOrder, d_TCtorOrder = d_TCtorOrder, u_TCtorOrder = u_TCtorOrder
    , e_TExtOrder = e_TExtOrder, d_TExtOrder = d_TExtOrder, u_TExtOrder = u_TExtOrder
    , e_TExt = e_TExt, d_TExt = d_TExt, u_TExt = u_TExt
    , e_TSwap = e_TSwap, d_TSwap = d_TSwap, u_TSwap = u_TSwap
    , e_TDeep3 = e_TDeep3, d_TDeep3 = d_TDeep3, u_TDeep3 = u_TDeep3
    , e_TDB = e_TDB, d_TDB = d_TDB, u_TDB = u_TDB
    , e_TVPositive = e_TVPositive, d_TVPositive = d_TVPositive, u_TVPositive = u_TVPositive
    , e_TVTree = e_TVTree, d_TVTree = d_TVTree, u_TVTree = u_TVTree
    , e_TVRose = e_TVRose, d_TVRose = d_TVRose, u_TVRose = u_TVRose
    , e_TVRosePos = e_TVRosePos, d_TVRosePos = d_TVRosePos, u_TVRosePos = u_TVRosePos
    , e_TVPair = e_TVPair, d_TVPair = d_TVPair, u_TVPair = u_TVPair
    , e_TVEnum = e_TVEnum, d_TVEnum = d_TVEnum, u_TVEnum = u_TVEnum
    , e_TVEnums = e_TVEnums, d_TVEnums = d_TVEnums, u_TVEnums = u_TVEnums
    , e_TVBox = e_TVBox, d_TVBox = d_TVBox, u_TVBox = u_TVBox
    , e_TVPing = e_TVPing, d_TVPing = d_TVPing, u_TVPing = u_TVPing
    , e_TVPong = e_TVPong, d_TVPong = d_TVPong, u_TVPong = u_TVPong
    , e_TVPhantom = e_TVPhantom, d_TVPhantom = d_TVPhantom, u_TVPhantom = u_TVPhantom
    , e_TVRec = e_TVRec, d_TVRec = d_TVRec, u_TVRec = u_TVRec
    , e_TVUnvalidated = e_TVUnvalidated, d_TVUnvalidated = d_TVUnvalidated, u_TVUnvalidated = u_TVUnvalidated
    , e_TVModel = e_TVModel, d_TVModel = d_TVModel, u_TVModel = u_TVModel
    , e_TVDict = e_TVDict, d_TVDict = d_TVDict, u_TVDict = u_TVDict
    , e_TAll = e_TAll, d_TAll = d_TAll, u_TAll = u_TAll
    , e_Fat = e_Fat, d_Fat = d_Fat, u_Fat = u_Fat
    , e_Fat2 = e_Fat2, d_Fat2 = d_Fat2, u_Fat2 = u_Fat2
    , e_Nest = e_Nest, d_Nest = d_Nest, u_Nest = u_Nest
    , e_Fat3 = e_Fat3, d_Fat3 = d_Fat3, u_Fat3 = u_Fat3
    , e_Hist = e_Hist, d_Hist = d_Hist, u_Hist = u_Hist
    , e_Color = e_Color, d_Color = d_Color, u_Color = u_Color
    , e_Id = e_Id, d_Id = d_Id, u_Id = u_Id
    , e_Wrapped = e_Wrapped, d_Wrapped = d_Wrapped, u_Wrapped = u_Wrapped
    , e_Multi = e_Multi, d_Multi = d_Multi, u_Multi = u_Multi
    , e_Mixed = e_Mixed, d_Mixed = d_Mixed, u_Mixed = u_Mixed
    , e_Tree = e_Tree, d_Tree = d_Tree, u_Tree = u_Tree
    , e_Forest = e_Forest, d_Forest = d_Forest, u_Forest = u_Forest
    , e_TreeB = e_TreeB, d_TreeB = d_TreeB, u_TreeB = u_TreeB
    , e_Thing = e_Thing, d_Thing = d_Thing, u_Thing = u_Thing
    , e_HasFn = e_HasFn, d_HasFn = d_HasFn, u_HasFn = u_HasFn
    , e_CtorOrder = e_CtorOrder, d_CtorOrder = d_CtorOrder, u_CtorOrder = u_CtorOrder
    , e_Big = e_Big, d_Big = d_Big, u_Big = u_Big
    , e_Enum300 = e_Enum300, d_Enum300 = d_Enum300, u_Enum300 = u_Enum300
    , e_Enum256 = e_Enum256, d_Enum256 = d_Enum256, u_Enum256 = u_Enum256
    , e_Enum255 = e_Enum255, d_Enum255 = d_Enum255, u_Enum255 = u_Enum255
    , e_IP0_0 = e_IP0_0, d_IP0_0 = d_IP0_0, u_IP0_0 = u_IP0_0
    , e_IP0_1 = e_IP0_1, d_IP0_1 = d_IP0_1, u_IP0_1 = u_IP0_1
    , e_IP0_2 = e_IP0_2, d_IP0_2 = d_IP0_2, u_IP0_2 = u_IP0_2
    , e_IP1_0 = e_IP1_0, d_IP1_0 = d_IP1_0, u_IP1_0 = u_IP1_0
    , e_IP1_1 = e_IP1_1, d_IP1_1 = d_IP1_1, u_IP1_1 = u_IP1_1
    , e_IP1_2 = e_IP1_2, d_IP1_2 = d_IP1_2, u_IP1_2 = u_IP1_2
    , e_IP2_0 = e_IP2_0, d_IP2_0 = d_IP2_0, u_IP2_0 = u_IP2_0
    , e_IP2_1 = e_IP2_1, d_IP2_1 = d_IP2_1, u_IP2_1 = u_IP2_1
    , e_IP2_2 = e_IP2_2, d_IP2_2 = d_IP2_2, u_IP2_2 = u_IP2_2
    , e_IP3_0 = e_IP3_0, d_IP3_0 = d_IP3_0, u_IP3_0 = u_IP3_0
    , e_IP4_0 = e_IP4_0, d_IP4_0 = d_IP4_0, u_IP4_0 = u_IP4_0
    , e_IP4_1 = e_IP4_1, d_IP4_1 = d_IP4_1, u_IP4_1 = u_IP4_1
    , e_IP4_2 = e_IP4_2, d_IP4_2 = d_IP4_2, u_IP4_2 = u_IP4_2
    , e_IP5_0 = e_IP5_0, d_IP5_0 = d_IP5_0, u_IP5_0 = u_IP5_0
    , e_IP6_0 = e_IP6_0, d_IP6_0 = d_IP6_0, u_IP6_0 = u_IP6_0
    , e_IP6_1 = e_IP6_1, d_IP6_1 = d_IP6_1, u_IP6_1 = u_IP6_1
    , e_IP6_2 = e_IP6_2, d_IP6_2 = d_IP6_2, u_IP6_2 = u_IP6_2
    , e_IP7_0 = e_IP7_0, d_IP7_0 = d_IP7_0, u_IP7_0 = u_IP7_0
    , e_IP7_1 = e_IP7_1, d_IP7_1 = d_IP7_1, u_IP7_1 = u_IP7_1
    , e_IP7_2 = e_IP7_2, d_IP7_2 = d_IP7_2, u_IP7_2 = u_IP7_2
    , e_IP8_0 = e_IP8_0, d_IP8_0 = d_IP8_0, u_IP8_0 = u_IP8_0
    , e_IP8_1 = e_IP8_1, d_IP8_1 = d_IP8_1, u_IP8_1 = u_IP8_1
    , e_IP8_2 = e_IP8_2, d_IP8_2 = d_IP8_2, u_IP8_2 = u_IP8_2
    , e_IP9_0 = e_IP9_0, d_IP9_0 = d_IP9_0, u_IP9_0 = u_IP9_0
    , e_IP9_1 = e_IP9_1, d_IP9_1 = d_IP9_1, u_IP9_1 = u_IP9_1
    , e_IP9_2 = e_IP9_2, d_IP9_2 = d_IP9_2, u_IP9_2 = u_IP9_2
    , e_IP10_0 = e_IP10_0, d_IP10_0 = d_IP10_0, u_IP10_0 = u_IP10_0
    , e_IP10_1 = e_IP10_1, d_IP10_1 = d_IP10_1, u_IP10_1 = u_IP10_1
    , e_IP10_2 = e_IP10_2, d_IP10_2 = d_IP10_2, u_IP10_2 = u_IP10_2
    , e_IP11_0 = e_IP11_0, d_IP11_0 = d_IP11_0, u_IP11_0 = u_IP11_0
    , e_IP11_1 = e_IP11_1, d_IP11_1 = d_IP11_1, u_IP11_1 = u_IP11_1
    , e_IP11_2 = e_IP11_2, d_IP11_2 = d_IP11_2, u_IP11_2 = u_IP11_2
    , e_IP12_0 = e_IP12_0, d_IP12_0 = d_IP12_0, u_IP12_0 = u_IP12_0
    , e_IP12_1 = e_IP12_1, d_IP12_1 = d_IP12_1, u_IP12_1 = u_IP12_1
    , e_IP12_2 = e_IP12_2, d_IP12_2 = d_IP12_2, u_IP12_2 = u_IP12_2
    , e_IP13_0 = e_IP13_0, d_IP13_0 = d_IP13_0, u_IP13_0 = u_IP13_0
    , e_IP13_1 = e_IP13_1, d_IP13_1 = d_IP13_1, u_IP13_1 = u_IP13_1
    , e_IP13_2 = e_IP13_2, d_IP13_2 = d_IP13_2, u_IP13_2 = u_IP13_2
    , e_PModel = e_PModel, d_PModel = d_PModel, u_PModel = u_PModel
    }


main : Program () Api ()
main =
    Platform.worker
        { init = \_ -> ( api, out 0 )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
