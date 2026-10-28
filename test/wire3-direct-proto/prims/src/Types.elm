module Types exposing (..)

{-| Every type Wire3 special-cases, or whose runtime representation differs from its
declared Elm type, reached the ways a codec can reach it: as a constructor argument, a
record field, a type argument, through an alias, and wrapped in an unboxed constructor.
check.js compares the Elm codecs (LAMDERA_WIRE3_DIRECT=0) with the direct codecs on each
one, value by value.
-}

import Array exposing (Array)
import Browser.Navigation
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Dict exposing (Dict)
import Effect.Browser.Dom
import Effect.Browser.Navigation
import Effect.File
import Effect.Http
import Effect.Lamdera
import Effect.Time
import Effect.WebGL
import Effect.WebGL.Texture
import File exposing (File)
import Html exposing (Html)
import Http
import Json.Decode
import Json.Encode
import Lamdera
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Parser
import Process
import Random
import Regex exposing (Regex)
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Set exposing (Set)
import Task exposing (Task)
import Time
import Url exposing (Url)
import Url.Parser
import WebGL
import WebGL.Texture



-- UNBOX / ENUM / NORMAL CLASSIFICATION


type UBool
    = UBool Bool


type UOrder
    = UOrder Order


type UUnit
    = UUnit ()


type UFn
    = UFn (Int -> Int)


type URec
    = URec { a : Int, b : String }


type UU
    = UU UOrder


type UUU
    = UUU UU


type Box a
    = Box a


type E1
    = E1


type Phantom a
    = Phantom


type N1
    = N1 Int Int


type Mixed
    = MNone
    | MOne Int
    | MTwo Int String


type Rec
    = Rec (List Rec)


type alias Ext a =
    { a | x : Int }


type alias AKey =
    Browser.Navigation.Key


type alias ARegex =
    Regex


type alias ABoxKey =
    Box Browser.Navigation.Key


type alias Vecs =
    { v2 : Vec2, v3 : Vec3, v4 : Vec4, m4 : Mat4 }


type alias WebGLThings =
    { shader : WebGL.Shader { pos : Vec3 } {} {}
    , mesh : WebGL.Mesh { pos : Vec3 }
    , entity : WebGL.Entity
    }


type alias HttpThings =
    { body : Http.Body
    , part : Http.Part
    , expect : Http.Expect Int
    , resolver : Http.Resolver String Int
    , header : Http.Header
    }


type alias FileRec =
    { file : File, name : String }


type alias TextureRec =
    { texture : WebGL.Texture.Texture, n : Int }


{-| An Enum with more than 256 constructors (two-byte tags), declared in neither wire
(Lamdera.Lexical) nor Elm name order, so prod's declaration indices, the wire tags and
the names all differ.
-}
type BigEnum
    = Z41
    | Z258
    | Z27
    | Z4
    | Z196
    | Z289
    | Z36
    | Z40
    | Z262
    | Z7
    | Z219
    | Z95
    | Z99
    | Z159
    | Z180
    | Z103
    | Z251
    | Z86
    | Z144
    | Z34
    | Z240
    | Z83
    | Z120
    | Z74
    | Z85
    | Z188
    | Z117
    | Z82
    | Z212
    | Z257
    | Z290
    | Z164
    | Z211
    | Z70
    | Z53
    | Z256
    | Z143
    | Z119
    | Z58
    | Z185
    | Z207
    | Z253
    | Z65
    | Z54
    | Z75
    | Z129
    | Z57
    | Z245
    | Z192
    | Z205
    | Z50
    | Z264
    | Z145
    | Z161
    | Z22
    | Z277
    | Z176
    | Z294
    | Z158
    | Z51
    | Z47
    | Z275
    | Z265
    | Z231
    | Z296
    | Z168
    | Z169
    | Z64
    | Z28
    | Z154
    | Z233
    | Z270
    | Z84
    | Z213
    | Z11
    | Z153
    | Z104
    | Z163
    | Z248
    | Z189
    | Z132
    | Z227
    | Z108
    | Z2
    | Z66
    | Z130
    | Z93
    | Z175
    | Z216
    | Z184
    | Z232
    | Z286
    | Z39
    | Z141
    | Z61
    | Z151
    | Z244
    | Z122
    | Z128
    | Z268
    | Z112
    | Z298
    | Z287
    | Z139
    | Z162
    | Z115
    | Z197
    | Z111
    | Z255
    | Z9
    | Z150
    | Z78
    | Z208
    | Z12
    | Z272
    | Z220
    | Z281
    | Z0
    | Z137
    | Z6
    | Z241
    | Z182
    | Z14
    | Z200
    | Z56
    | Z138
    | Z8
    | Z293
    | Z201
    | Z133
    | Z283
    | Z26
    | Z135
    | Z167
    | Z173
    | Z172
    | Z206
    | Z13
    | Z230
    | Z261
    | Z81
    | Z94
    | Z136
    | Z223
    | Z295
    | Z1
    | Z263
    | Z67
    | Z247
    | Z124
    | Z3
    | Z210
    | Z267
    | Z181
    | Z45
    | Z21
    | Z225
    | Z59
    | Z97
    | Z91
    | Z106
    | Z194
    | Z166
    | Z110
    | Z284
    | Z71
    | Z140
    | Z102
    | Z191
    | Z228
    | Z243
    | Z215
    | Z100
    | Z101
    | Z249
    | Z33
    | Z234
    | Z55
    | Z198
    | Z239
    | Z291
    | Z156
    | Z43
    | Z90
    | Z118
    | Z5
    | Z88
    | Z221
    | Z98
    | Z183
    | Z72
    | Z235
    | Z174
    | Z242
    | Z299
    | Z79
    | Z179
    | Z292
    | Z15
    | Z16
    | Z170
    | Z246
    | Z121
    | Z69
    | Z273
    | Z17
    | Z252
    | Z204
    | Z271
    | Z127
    | Z152
    | Z89
    | Z177
    | Z226
    | Z80
    | Z209
    | Z297
    | Z146
    | Z142
    | Z195
    | Z288
    | Z171
    | Z10
    | Z229
    | Z125
    | Z38
    | Z237
    | Z193
    | Z42
    | Z107
    | Z131
    | Z260
    | Z18
    | Z155
    | Z269
    | Z278
    | Z186
    | Z87
    | Z224
    | Z126
    | Z134
    | Z250
    | Z147
    | Z20
    | Z62
    | Z199
    | Z178
    | Z282
    | Z276
    | Z279
    | Z76
    | Z266
    | Z116
    | Z236
    | Z149
    | Z238
    | Z160
    | Z218
    | Z254
    | Z105
    | Z280
    | Z32
    | Z49
    | Z190
    | Z96
    | Z52
    | Z92
    | Z157
    | Z60
    | Z73
    | Z285
    | Z148
    | Z68
    | Z23
    | Z113
    | Z25
    | Z203
    | Z31
    | Z114
    | Z63
    | Z30
    | Z217
    | Z46
    | Z123
    | Z35
    | Z214
    | Z222
    | Z44
    | Z19
    | Z109
    | Z259
    | Z29
    | Z187
    | Z48
    | Z274
    | Z37
    | Z24
    | Z202
    | Z77
    | Z165



-- EVERY ITEM


type Item
    = -- Browser.Navigation.Key: `type Key = Key`, a JS function at runtime
      IKey Browser.Navigation.Key
    | IKeyAlias AKey
    | IKeyMaybe (Maybe Browser.Navigation.Key)
    | IKeyList (List Browser.Navigation.Key)
    | IKeyBox (Box Browser.Navigation.Key)
    | IKeyBoxAlias ABoxKey
    | IKeyRec { key : Browser.Navigation.Key, n : Int }
    | IKeyExt (Ext { key : Browser.Navigation.Key })
    | IKeyDict (Dict String Browser.Navigation.Key)
    | IKeyPair ( Browser.Navigation.Key, Int )
    | IKeyResult (Result Browser.Navigation.Key Browser.Navigation.Key)
    | IEffKey Effect.Browser.Navigation.Key
      -- elm/file File: encodeRef / decodeRef
    | IFile File
    | IFileList (List File)
    | IFileRec FileRec
    | IFileMaybe (Maybe File)
    | IFileBox (Box File)
    | IEffFile Effect.File.File
      -- WebGL Texture: decodeRef, but encodeRef only where encoderForType sees it
    | ITexture WebGL.Texture.Texture
    | ITextureRec TextureRec
    | ITextureList (List WebGL.Texture.Texture)
    | IEffTexture Effect.WebGL.Texture.Texture
      -- kernel-backed single-variant types, values built by kernel code
    | IShader (WebGL.Shader { pos : Vec3 } {} {})
    | IMesh (WebGL.Mesh { pos : Vec3 })
    | IEntity WebGL.Entity
    | IWebGL WebGLThings
    | IEffShader (Effect.WebGL.Shader { pos : Vec3 } {} {})
    | IEffMesh (Effect.WebGL.Mesh { pos : Vec3 })
    | IVec2 Vec2
    | IVec3 Vec3
    | IVec4 Vec4
    | IMat4 Mat4
    | IVecs Vecs
    | IVec3List (List Vec3)
    | IVec3Box (Box Vec3)
    | IRegex Regex
    | IRegexAlias ARegex
    | IRegexMaybe (Maybe Regex)
    | IHttpBody Http.Body
    | IHttpPart Http.Part
    | IHttpExpect (Http.Expect Int)
    | IHttpResolver (Http.Resolver String Int)
    | IHttpHeader Http.Header
    | IHttp HttpThings
    | IHttpError Http.Error
    | IHttpProgress Http.Progress
    | IHttpMetadata Http.Metadata
    | IHttpResponse (Http.Response String)
    | IEffHttpError Effect.Http.Error
    | IEffHttpBody Effect.Http.Body
      -- elm/time
    | IPosix Time.Posix
    | IPosixList (List Time.Posix)
    | IPosixBox (Box Time.Posix)
    | IPosixRec { t : Time.Posix }
    | IEffPosix Effect.Time.Posix
    | IZone Time.Zone
    | IWeekday Time.Weekday
    | IMonth Time.Month
    | IZoneName Time.ZoneName
      -- elm/url, elm/random, elm/parser
    | IUrl Url
    | IProtocol Url.Protocol
    | IUrlParser (Url.Parser.Parser (Int -> Int) Int)
    | ISeed Random.Seed
    | IGenerator (Random.Generator Int)
    | IParser (Parser.Parser Int)
    | IParserDeadEnds (List Parser.DeadEnd)
      -- unsupported: encode nothing, never decode
    | IJsonValue Json.Encode.Value
    | IJsonDecodeValue Json.Decode.Value
    | IJsonDecoder (Json.Decode.Decoder Int)
    | IJsonError Json.Decode.Error
    | IHtml (Html Int)
    | IHtmlAttr (Html.Attribute Int)
    | ICmd (Cmd Int)
    | ISub (Sub Int)
    | ITask (Task Never Int)
    | IProcessId Process.Id
    | IBytesEncoder Bytes.Encode.Encoder
    | IBytesDecoder (Bytes.Decode.Decoder Int)
    | IEndianness Bytes.Endianness
    | IFn (Int -> Int)
    | INever (List Never)
      -- Lamdera ids
    | IClientId Lamdera.ClientId
    | IEffClientId Effect.Lamdera.ClientId
    | IEffSessionId Effect.Lamdera.SessionId
    | IHtmlId Effect.Browser.Dom.HtmlId
      -- core prims and containers
    | IInt Int
    | IFloat Float
    | IChar Char
    | IString String
    | IBool Bool
    | IOrder Order
    | IUnit ()
    | IBytes Bytes
    | ITriple ( Int, Char, Float )
    | IDict (Dict Int Float)
    | ISet (Set Char)
    | IArray (Array Float)
    | ISeqDict (SeqDict String Float)
    | ISeqSet (SeqSet Int)
      -- classification
    | IUBool UBool
    | IUOrder UOrder
    | IUUnit UUnit
    | IUFn UFn
    | IURec URec
    | IUU UU
    | IUUU UUU
    | IBoxBool (Box Bool)
    | IBoxOrder (Box Order)
    | IBoxUnit (Box ())
    | IBoxBox (Box (Box (Maybe Int)))
    | IE1 E1
    | IPhantom (Phantom Browser.Navigation.Key)
    | IN1 N1
    | IMixed Mixed
    | IRec Rec
    | IExt (Ext { y : Float })
    | IBigEnum BigEnum
    | IBigEnumList (List BigEnum)
      -- record fields named like JS reserved words and Object.prototype members
    | IReserved { class : Int, default : Int, new : Int, function : Int, constructor : Int, toString : Int, valueOf : Int, hasOwnProperty : Int, this : Int, var : Int, delete : Int, arguments : Int, eval : Int, proto : Int, a : Int, b : Int, c : Int }
