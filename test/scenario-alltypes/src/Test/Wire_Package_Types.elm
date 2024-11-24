module Test.Wire_Package_Types exposing (..)

import Bytes
import Bytes.Decode
import Bytes.Encode
import Lamdera.Wire3
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import SeqDict exposing (..)
import SeqSet exposing (..)
import Time
import Url


type PackageTypes
    = SDict (SeqDict Tag String)
    | SSet (SeqSet Tag)


type Tag
    = Foo
    | Bar


type alias PackageTypesRecord =
    { sDict : SeqDict Tag String
    , sSet : SeqSet Tag
    }


type LinearAlgebra
    = SMat4 Mat4
    | SVec4 Vec4
    | SVec3 Vec3
    | SVec2 Vec2


type alias LinearAlgebraRecord =
    { sMat4 : Mat4
    , sVec4 : Vec4
    , sVec3 : Vec3
    , sVec2 : Vec2
    }


expected_w3_encode_LinearAlgebra : LinearAlgebra -> Lamdera.Wire3.Encoder
expected_w3_encode_LinearAlgebra w3v =
    case w3v of
        SMat4 v0 ->
            let
                m =
                    Math.Matrix4.toRecord v0
            in
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Bytes.Encode.unsignedInt8 0
                , Lamdera.Wire3.encodeFloat m.m11
                , Lamdera.Wire3.encodeFloat m.m21
                , Lamdera.Wire3.encodeFloat m.m31
                , Lamdera.Wire3.encodeFloat m.m41
                , Lamdera.Wire3.encodeFloat m.m12
                , Lamdera.Wire3.encodeFloat m.m22
                , Lamdera.Wire3.encodeFloat m.m32
                , Lamdera.Wire3.encodeFloat m.m42
                , Lamdera.Wire3.encodeFloat m.m13
                , Lamdera.Wire3.encodeFloat m.m23
                , Lamdera.Wire3.encodeFloat m.m33
                , Lamdera.Wire3.encodeFloat m.m43
                , Lamdera.Wire3.encodeFloat m.m14
                , Lamdera.Wire3.encodeFloat m.m24
                , Lamdera.Wire3.encodeFloat m.m34
                , Lamdera.Wire3.encodeFloat m.m44
                ]

        SVec4 v0 ->
            let
                v =
                    Math.Vector4.toRecord v0
            in
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Bytes.Encode.unsignedInt8 1
                , Lamdera.Wire3.encodeFloat v.x
                , Lamdera.Wire3.encodeFloat v.y
                , Lamdera.Wire3.encodeFloat v.z
                , Lamdera.Wire3.encodeFloat v.w
                ]

        SVec3 v0 ->
            let
                v =
                    Math.Vector3.toRecord v0
            in
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Bytes.Encode.unsignedInt8 2
                , Lamdera.Wire3.encodeFloat v.x
                , Lamdera.Wire3.encodeFloat v.y
                , Lamdera.Wire3.encodeFloat v.z
                ]

        SVec2 v0 ->
            let
                v =
                    Math.Vector2.toRecord v0
            in
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Bytes.Encode.unsignedInt8 3
                , Lamdera.Wire3.encodeFloat v.x
                , Lamdera.Wire3.encodeFloat v.y
                ]


expected_m3_decode_LinearAlgebra =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode
                            SMat4
                            |> Lamdera.Wire3.andMapDecode
                                (Lamdera.Wire3.succeedDecode
                                    (\m11 m21 m31 m41 m12 m22 m32 m42 m13 m23 m33 m43 m14 m24 m34 m44 ->
                                        Math.Matrix4.fromRecord { m11 = m11, m21 = m21, m31 = m31, m41 = m41, m12 = m12, m22 = m22, m32 = m32, m42 = m42, m13 = m13, m23 = m23, m33 = m33, m43 = m43, m14 = m14, m24 = m24, m34 = m34, m44 = m44 }
                                    )
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                )

                    1 ->
                        Lamdera.Wire3.succeedDecode
                            SVec4
                            |> Lamdera.Wire3.andMapDecode
                                (Lamdera.Wire3.succeedDecode Math.Vector4.vec4
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                )

                    2 ->
                        Lamdera.Wire3.succeedDecode
                            SVec3
                            |> Lamdera.Wire3.andMapDecode
                                (Lamdera.Wire3.succeedDecode Math.Vector3.vec3
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                )

                    3 ->
                        Lamdera.Wire3.succeedDecode
                            SVec2
                            |> Lamdera.Wire3.andMapDecode
                                (Lamdera.Wire3.succeedDecode Math.Vector2.vec2
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                                )

                    _ ->
                        Lamdera.Wire3.failDecode
            )


expected_w3_encode_PackageTypes : PackageTypes -> Lamdera.Wire3.Encoder
expected_w3_encode_PackageTypes w3v =
    case w3v of
        SDict v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Bytes.Encode.unsignedInt8
                    0
                , SeqDict.encodeDict
                    w3_encode_Tag
                    Lamdera.Wire3.encodeString
                    v0
                ]

        SSet v0 ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Bytes.Encode.unsignedInt8
                    1
                , SeqSet.encodeSet
                    w3_encode_Tag
                    v0
                ]


expected_w3_decode_PackageTypes =
    Bytes.Decode.unsignedInt8
        |> Lamdera.Wire3.andThenDecode
            (\w3v ->
                case w3v of
                    0 ->
                        Lamdera.Wire3.succeedDecode
                            SDict
                            |> Lamdera.Wire3.andMapDecode
                                (SeqDict.decodeDict
                                    w3_decode_Tag
                                    Lamdera.Wire3.decodeString
                                )

                    1 ->
                        Lamdera.Wire3.succeedDecode
                            SSet
                            |> Lamdera.Wire3.andMapDecode
                                (SeqSet.decodeSet
                                    w3_decode_Tag
                                )

                    _ ->
                        Lamdera.Wire3.failDecode
            )


expected_w3_encode_LinearAlgebraRecord : LinearAlgebraRecord -> Lamdera.Wire3.Encoder
expected_w3_encode_LinearAlgebraRecord =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ let
                m =
                    Math.Matrix4.toRecord w3_rec_var0.sMat4
              in
              Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeFloat m.m11
                , Lamdera.Wire3.encodeFloat m.m21
                , Lamdera.Wire3.encodeFloat m.m31
                , Lamdera.Wire3.encodeFloat m.m41
                , Lamdera.Wire3.encodeFloat m.m12
                , Lamdera.Wire3.encodeFloat m.m22
                , Lamdera.Wire3.encodeFloat m.m32
                , Lamdera.Wire3.encodeFloat m.m42
                , Lamdera.Wire3.encodeFloat m.m13
                , Lamdera.Wire3.encodeFloat m.m23
                , Lamdera.Wire3.encodeFloat m.m33
                , Lamdera.Wire3.encodeFloat m.m43
                , Lamdera.Wire3.encodeFloat m.m14
                , Lamdera.Wire3.encodeFloat m.m24
                , Lamdera.Wire3.encodeFloat m.m34
                , Lamdera.Wire3.encodeFloat m.m44
                ]
            , let
                v =
                    Math.Vector4.toRecord w3_rec_var0.sVec4
              in
              Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeFloat v.x
                , Lamdera.Wire3.encodeFloat v.y
                , Lamdera.Wire3.encodeFloat v.z
                , Lamdera.Wire3.encodeFloat v.w
                ]
            , let
                v =
                    Math.Vector3.toRecord w3_rec_var0.sVec3
              in
              Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeFloat v.x
                , Lamdera.Wire3.encodeFloat v.y
                , Lamdera.Wire3.encodeFloat v.z
                ]
            , let
                v =
                    Math.Vector2.toRecord w3_rec_var0.sVec2
              in
              Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeFloat v.x
                , Lamdera.Wire3.encodeFloat v.y
                ]
            ]


expected_w3_decode_LinearAlgebraRecord =
    Lamdera.Wire3.succeedDecode
        (\sMat40 sVec20 sVec30 sVec40 -> { sMat4 = sMat40, sVec2 = sVec20, sVec3 = sVec30, sVec4 = sVec40 })
        |> Lamdera.Wire3.andMapDecode
            (Lamdera.Wire3.succeedDecode
                (\m11 m21 m31 m41 m12 m22 m32 m42 m13 m23 m33 m43 m14 m24 m34 m44 ->
                    Math.Matrix4.fromRecord { m11 = m11, m21 = m21, m31 = m31, m41 = m41, m12 = m12, m22 = m22, m32 = m32, m42 = m42, m13 = m13, m23 = m23, m33 = m33, m43 = m43, m14 = m14, m24 = m24, m34 = m34, m44 = m44 }
                )
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
            )
        |> Lamdera.Wire3.andMapDecode
            (Lamdera.Wire3.succeedDecode Math.Vector2.vec2
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
            )
        |> Lamdera.Wire3.andMapDecode
            (Lamdera.Wire3.succeedDecode Math.Vector3.vec3
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
            )
        |> Lamdera.Wire3.andMapDecode
            (Lamdera.Wire3.succeedDecode Math.Vector4.vec4
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
                |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeFloat
            )


expected_w3_encode_PackageTypesRecord : PackageTypesRecord -> Lamdera.Wire3.Encoder
expected_w3_encode_PackageTypesRecord =
    \w3_rec_var0 ->
        Lamdera.Wire3.encodeSequenceWithoutLength
            [ SeqDict.encodeDict
                w3_encode_Tag
                Lamdera.Wire3.encodeString
                w3_rec_var0.sDict
            , SeqSet.encodeSet
                w3_encode_Tag
                w3_rec_var0.sSet
            ]


expected_w3_decode_PackageTypesRecord =
    Lamdera.Wire3.succeedDecode
        (\sDict0 sSet0 -> { sDict = sDict0, sSet = sSet0 })
        |> Lamdera.Wire3.andMapDecode
            (SeqDict.decodeDict
                w3_decode_Tag
                Lamdera.Wire3.decodeString
            )
        |> Lamdera.Wire3.andMapDecode
            (SeqSet.decodeSet
                w3_decode_Tag
            )
