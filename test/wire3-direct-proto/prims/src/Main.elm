port module Main exposing (..)

import Array
import Browser.Navigation
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Dict
import Effect.Browser.Dom
import Effect.Browser.Navigation
import Effect.File
import Effect.Http
import Effect.Lamdera
import Effect.WebGL
import File exposing (File)
import Html
import Html.Attributes
import Http
import Json.Decode
import Json.Encode
import Lamdera.Wire3 as W
import Math.Matrix4
import Math.Vector2
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4
import Parser
import Platform
import Process
import Random
import Regex
import SeqDict
import SeqSet
import Set
import Task
import Time
import Types exposing (..)
import Url
import Url.Parser
import WebGL
import WebGL.Texture


port out : Int -> Cmd msg


encode : Item -> Bytes
encode item =
    W.bytesEncode (w3_encode_Item item)


decode : Bytes -> Maybe Item
decode b =
    W.bytesDecode w3_decode_Item b


decodeUnsafe : Bytes -> Maybe Item
decodeUnsafe b =
    W.bytesDecode w3_unsafe_decode_Item b


vertexShader : WebGL.Shader { pos : Vec3 } {} {}
vertexShader =
    [glsl|
        attribute vec3 pos;
        void main () { gl_Position = vec4(pos, 1.0); }
    |]


fragmentShader : WebGL.Shader {} {} {}
fragmentShader =
    [glsl|
        precision mediump float;
        void main () { gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0); }
    |]


mesh : WebGL.Mesh { pos : Vec3 }
mesh =
    WebGL.triangles [ ( { pos = vec3 0 0 0 }, { pos = vec3 1 0 0 }, { pos = vec3 0 1 0 } ) ]


entity : WebGL.Entity
entity =
    WebGL.entity vertexShader fragmentShader mesh {}


regex : Regex.Regex
regex =
    Regex.fromString "a+b" |> Maybe.withDefault Regex.never


httpThings : HttpThings
httpThings =
    { body = Http.stringBody "text/plain" "hi"
    , part = Http.stringPart "k" "v"
    , expect = Http.expectString (\_ -> 1)
    , resolver = Http.stringResolver (\_ -> Ok 1)
    , header = Http.header "X-A" "b"
    }


url : Url.Url
url =
    { protocol = Url.Https, host = "example.com", port_ = Just 8443, path = "/a/b", query = Just "q=1", fragment = Nothing }


metadata : Http.Metadata
metadata =
    { url = "https://x", statusCode = 200, statusText = "OK", headers = Dict.fromList [ ( "a", "b" ) ] }


{-| Items built in Elm. Values only JS can make (a navigation key, a File, a Texture,
a Process.Id, odd numbers) come in through the inj* functions.
-}
items : List ( String, Item )
items =
    [ ( "Shader", IShader vertexShader )
    , ( "Mesh", IMesh mesh )
    , ( "Entity", IEntity entity )
    , ( "WebGL record", IWebGL { shader = vertexShader, mesh = mesh, entity = entity } )
    , ( "Effect.WebGL.Shader", IEffShader vertexShader )
    , ( "Effect.WebGL.Mesh", IEffMesh (Effect.WebGL.triangles [ ( { pos = vec3 0 0 0 }, { pos = vec3 1 0 0 }, { pos = vec3 0 1 0 } ) ]) )
    , ( "Vec2", IVec2 (Math.Vector2.vec2 1 2) )
    , ( "Vec3", IVec3 (vec3 1 2 3) )
    , ( "Vec4", IVec4 (Math.Vector4.vec4 1 2 3 4) )
    , ( "Mat4", IMat4 Math.Matrix4.identity )
    , ( "Vecs record", IVecs { v2 = Math.Vector2.vec2 1 2, v3 = vec3 1 2 3, v4 = Math.Vector4.vec4 1 2 3 4, m4 = Math.Matrix4.identity } )
    , ( "List Vec3", IVec3List [ vec3 1 2 3, vec3 4 5 6 ] )
    , ( "Box Vec3", IVec3Box (Box (vec3 1 2 3)) )
    , ( "Regex", IRegex regex )
    , ( "Regex alias", IRegexAlias regex )
    , ( "Maybe Regex", IRegexMaybe (Just regex) )
    , ( "Http.Body empty", IHttpBody Http.emptyBody )
    , ( "Http.Body string", IHttpBody (Http.stringBody "text/plain" "x") )
    , ( "Http.Body json", IHttpBody (Http.jsonBody (Json.Encode.int 1)) )
    , ( "Http.Body bytes", IHttpBody (Http.bytesBody "a/b" (Bytes.Encode.encode (Bytes.Encode.unsignedInt8 1))) )
    , ( "Http.Body multipart", IHttpBody (Http.multipartBody [ Http.stringPart "a" "b" ]) )
    , ( "Http.Part", IHttpPart (Http.stringPart "a" "b") )
    , ( "Http.Expect", IHttpExpect (Http.expectString (\_ -> 1)) )
    , ( "Http.Expect whatever", IHttpExpect (Http.expectWhatever (\_ -> 1)) )
    , ( "Http.Resolver", IHttpResolver (Http.stringResolver (\_ -> Ok 1)) )
    , ( "Http.Header", IHttpHeader (Http.header "a" "b") )
    , ( "Http record", IHttp httpThings )
    , ( "Http.Error", IHttpError (Http.BadStatus 404) )
    , ( "Http.Error Timeout", IHttpError Http.Timeout )
    , ( "Http.Progress", IHttpProgress (Http.Receiving { received = 5, size = Just 9 }) )
    , ( "Http.Metadata", IHttpMetadata metadata )
    , ( "Http.Response", IHttpResponse (Http.GoodStatus_ metadata "body") )
    , ( "Effect.Http.Error", IEffHttpError (Effect.Http.BadStatus 500) )
    , ( "Effect.Http.Body empty", IEffHttpBody Effect.Http.emptyBody )
    , ( "Effect.Http.Body string", IEffHttpBody (Effect.Http.stringBody "a/b" "x") )
    , ( "Effect.Http.Body json", IEffHttpBody (Effect.Http.jsonBody (Json.Encode.int 1)) )
    , ( "Posix", IPosix (Time.millisToPosix 1700000000123) )
    , ( "Posix negative", IPosix (Time.millisToPosix -5) )
    , ( "List Posix", IPosixList [ Time.millisToPosix 0, Time.millisToPosix 99 ] )
    , ( "Box Posix", IPosixBox (Box (Time.millisToPosix 42)) )
    , ( "Posix record", IPosixRec { t = Time.millisToPosix 7 } )
    , ( "Effect.Time.Posix", IEffPosix (Time.millisToPosix 8) )
    , ( "Zone utc", IZone Time.utc )
    , ( "Zone custom", IZone (Time.customZone 60 [ { start = 1000, offset = 120 } ]) )
    , ( "Weekday", IWeekday Time.Sat )
    , ( "Month", IMonth Time.Dec )
    , ( "ZoneName", IZoneName (Time.Name "Europe/Berlin") )
    , ( "ZoneName offset", IZoneName (Time.Offset 600) )
    , ( "Url", IUrl url )
    , ( "Protocol", IProtocol Url.Http )
    , ( "Url.Parser", IUrlParser (Url.Parser.map (\n -> n) Url.Parser.int) )
    , ( "Seed", ISeed (Random.initialSeed 42) )
    , ( "Generator", IGenerator (Random.int 0 10) )
    , ( "Parser", IParser Parser.int )
    , ( "Parser.DeadEnd", IParserDeadEnds [ { row = 1, col = 2, problem = Parser.Expecting "x" } ] )
    , ( "Json.Encode.Value", IJsonValue (Json.Encode.int 1) )
    , ( "Json.Decode.Value", IJsonDecodeValue (Json.Encode.string "s") )
    , ( "Json.Decode.Decoder", IJsonDecoder Json.Decode.int )
    , ( "Json.Decode.Error", IJsonError (Json.Decode.Failure "x" (Json.Encode.int 1)) )
    , ( "Json.Decode.Error field", IJsonError (Json.Decode.Field "f" (Json.Decode.Index 0 (Json.Decode.OneOf []))) )
    , ( "Html", IHtml (Html.text "x") )
    , ( "Html.Attribute", IHtmlAttr (Html.Attributes.class "c") )
    , ( "Cmd", ICmd Cmd.none )
    , ( "Sub", ISub Sub.none )
    , ( "Task", ITask (Task.succeed 1) )
    , ( "Bytes.Encode.Encoder", IBytesEncoder (Bytes.Encode.unsignedInt8 1) )
    , ( "Bytes.Decode.Decoder", IBytesDecoder Bytes.Decode.unsignedInt8 )
    , ( "Endianness", IEndianness Bytes.BE )
    , ( "function", IFn (\n -> n + 1) )
    , ( "List Never", INever [] )
    , ( "Lamdera.ClientId", IClientId "client-1" )
    , ( "Effect.Lamdera.ClientId", IEffClientId (Effect.Lamdera.clientIdFromString "c") )
    , ( "Effect.Lamdera.SessionId", IEffSessionId (Effect.Lamdera.sessionIdFromString "s") )
    , ( "HtmlId", IHtmlId (Effect.Browser.Dom.id "x") )
    , ( "Int", IInt 5 )
    , ( "Float", IFloat 1.5 )
    , ( "Char", IChar 'x' )
    , ( "Char astral", IChar '😀' )
    , ( "Char lone surrogate", IChar (Char.fromCode 0xD800) )
    , ( "String", IString "héllo 😀" )
    , ( "Bool True", IBool True )
    , ( "Bool False", IBool False )
    , ( "Order LT", IOrder LT )
    , ( "Order GT", IOrder GT )
    , ( "Unit", IUnit () )
    , ( "Bytes", IBytes (Bytes.Encode.encode (Bytes.Encode.sequence [ Bytes.Encode.unsignedInt8 1, Bytes.Encode.unsignedInt8 2 ])) )
    , ( "Triple", ITriple ( 1, 'c', -0.5 ) )
    , ( "Dict", IDict (Dict.fromList [ ( 3, 1.5 ), ( 1, 2.5 ) ]) )
    , ( "Set", ISet (Set.fromList [ 'b', 'a' ]) )
    , ( "Array", IArray (Array.initialize 70 toFloat) )
    , ( "SeqDict", ISeqDict (SeqDict.fromList [ ( "b", 1 ), ( "a", 2 ) ]) )
    , ( "SeqSet", ISeqSet (SeqSet.fromList [ 3, 1, 2 ]) )
    , ( "UBool", IUBool (UBool True) )
    , ( "UBool False", IUBool (UBool False) )
    , ( "UOrder", IUOrder (UOrder GT) )
    , ( "UUnit", IUUnit (UUnit ()) )
    , ( "UFn", IUFn (UFn identity) )
    , ( "URec", IURec (URec { a = 1, b = "b" }) )
    , ( "UU", IUU (UU (UOrder EQ)) )
    , ( "UUU", IUUU (UUU (UU (UOrder LT))) )
    , ( "Box Bool", IBoxBool (Box False) )
    , ( "Box Order", IBoxOrder (Box EQ) )
    , ( "Box Unit", IBoxUnit (Box ()) )
    , ( "Box Box Maybe", IBoxBox (Box (Box (Just 3))) )
    , ( "E1", IE1 E1 )
    , ( "Phantom", IPhantom Phantom )
    , ( "N1", IN1 (N1 1 2) )
    , ( "Mixed none", IMixed MNone )
    , ( "Mixed one", IMixed (MOne 1) )
    , ( "Mixed two", IMixed (MTwo 1 "x") )
    , ( "Rec", IRec (Rec [ Rec [], Rec [ Rec [] ] ]) )
    , ( "Ext", IExt { x = 1, y = 2.5 } )
    , ( "BigEnum Z41", IBigEnum Z41 )
    , ( "BigEnum Z258", IBigEnum Z258 )
    , ( "BigEnum Z3", IBigEnum Z3 )
    , ( "BigEnum Z165", IBigEnum Z165 )
    , ( "BigEnum Z0", IBigEnum Z0 )
    , ( "BigEnum Z299", IBigEnum Z299 )
    , ( "BigEnum Z10", IBigEnum Z10 )
    , ( "BigEnum Z9", IBigEnum Z9 )
    , ( "reserved field names", IReserved { class = 1, default = 2, new = 3, function = 4, constructor = 5, toString = 6, valueOf = 7, hasOwnProperty = 8, this = 9, var = 10, delete = 11, arguments = 12, eval = 13, proto = 14, a = 15, b = 16, c = 17 } )
    , ( "List BigEnum", IBigEnumList [ Z41, Z258, Z3, Z165, Z0, Z299, Z10, Z9 ] )
    ]


injKey : Browser.Navigation.Key -> List ( String, Item )
injKey key =
    [ ( "Key", IKey key )
    , ( "Key alias", IKeyAlias key )
    , ( "Maybe Key", IKeyMaybe (Just key) )
    , ( "List Key", IKeyList [ key, key ] )
    , ( "Box Key", IKeyBox (Box key) )
    , ( "Box Key alias", IKeyBoxAlias (Box key) )
    , ( "Key record", IKeyRec { key = key, n = 3 } )
    , ( "Key extensible record", IKeyExt { x = 1, key = key } )
    , ( "Dict String Key", IKeyDict (Dict.fromList [ ( "a", key ), ( "b", key ) ]) )
    , ( "( Key, Int )", IKeyPair ( key, 9 ) )
    , ( "Result Key Key", IKeyResult (Ok key) )
    ]


{-| An Effect.Browser.Navigation.Key, built in JS (its constructors are not exposed).
-}
injEffKey : Effect.Browser.Navigation.Key -> List ( String, Item )
injEffKey key =
    [ ( "Effect key", IEffKey key ) ]


injFile : File -> List ( String, Item )
injFile file =
    [ ( "File", IFile file )
    , ( "List File", IFileList [ file, file ] )
    , ( "File record", IFileRec { file = file, name = "n" } )
    , ( "Maybe File", IFileMaybe (Just file) )
    , ( "Box File", IFileBox (Box file) )
    ]


{-| Two different files: refs are numbered in a different order by the Elm codecs (List.map
runs right to left) and the direct codecs, so each must still decode to its own file.
-}
injFiles : File -> File -> List ( String, Item )
injFiles a b =
    [ ( "List File (two files)", IFileList [ a, b ] ) ]


injEffFile : Effect.File.File -> List ( String, Item )
injEffFile file =
    [ ( "Effect file", IEffFile file ) ]


injTexture : WebGL.Texture.Texture -> List ( String, Item )
injTexture texture =
    [ ( "Texture", ITexture texture )
    , ( "Texture record", ITextureRec { texture = texture, n = 1 } )
    , ( "List Texture", ITextureList [ texture ] )
    , ( "Effect.WebGL.Texture", IEffTexture texture )
    ]


injProcessId : Process.Id -> List ( String, Item )
injProcessId pid =
    [ ( "Process.Id", IProcessId pid ) ]


injInt : Int -> List ( String, Item )
injInt i =
    [ ( "Int", IInt i ), ( "Posix", IPosix (Time.millisToPosix i) ), ( "Char", IChar (Char.fromCode i) ) ]


injFloat : Float -> List ( String, Item )
injFloat f =
    [ ( "Float", IFloat f ), ( "Dict key", IDict (Dict.singleton 1 f) ), ( "Array", IArray (Array.fromList [ f ]) ) ]


injBytes : Bytes -> List ( String, Item )
injBytes b =
    [ ( "Bytes", IBytes b ) ]


injString : String -> List ( String, Item )
injString s =
    [ ( "String", IString s ) ]


type alias Api =
    { encode : Item -> Bytes
    , decode : Bytes -> Maybe Item
    , decodeUnsafe : Bytes -> Maybe Item
    , items : List ( String, Item )
    , injKey : Browser.Navigation.Key -> List ( String, Item )
    , injEffKey : Effect.Browser.Navigation.Key -> List ( String, Item )
    , injFile : File -> List ( String, Item )
    , injFiles : File -> File -> List ( String, Item )
    , injEffFile : Effect.File.File -> List ( String, Item )
    , injTexture : WebGL.Texture.Texture -> List ( String, Item )
    , injProcessId : Process.Id -> List ( String, Item )
    , injInt : Int -> List ( String, Item )
    , injFloat : Float -> List ( String, Item )
    , injBytes : Bytes -> List ( String, Item )
    , injString : String -> List ( String, Item )
    }


main : Program () Api ()
main =
    Platform.worker
        { init = \_ -> ( { encode = encode, decode = decode, decodeUnsafe = decodeUnsafe, items = items, injKey = injKey, injEffKey = injEffKey, injFile = injFile, injFiles = injFiles, injEffFile = injEffFile, injTexture = injTexture, injProcessId = injProcessId, injInt = injInt, injFloat = injFloat, injBytes = injBytes, injString = injString }, out 0 )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
