module Types exposing (..)

{-| The message types of the wire schema scenario (test/Test/WireSchema.hs): every shape
the schema describes, used from the Lamdera core types so they are its roots.
-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import File exposing (File)
import Lamdera exposing (ClientId, SessionId)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Set exposing (Set)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { count : Int }


type alias BackendModel =
    { sessions : SeqDict SessionId (List ClientId)
    , todos : Dict Int Todo
    }


type FrontendMsg
    = Picked File
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend
    | AddTodo String
    | Numbers Int Float
    | Edge Order Char ()
    | Saved Todo
    | Grow (Tree Int)
    | Wrapped (Box (Maybe (Result String Int)))
    | Containers (Dict String Int) (Set Int) (Array Float) (List ( Int, String ))
    | Seqs (SeqDict ( Int, Int ) String) (SeqSet String)
    | Triple ( Bool, String, Float )
    | Blob Bytes
    | Kernel Time.Posix SessionId ClientId Vec2 Vec3 Mat4
    | Visit Url
    | Deep (Expr String)


type BackendMsg
    = Many000
    | Many001
    | Many002
    | Many003
    | Many004
    | Many005
    | Many006
    | Many007
    | Many008
    | Many009
    | Many010
    | Many011
    | Many012
    | Many013
    | Many014
    | Many015
    | Many016
    | Many017
    | Many018
    | Many019
    | Many020
    | Many021
    | Many022
    | Many023
    | Many024
    | Many025
    | Many026
    | Many027
    | Many028
    | Many029
    | Many030
    | Many031
    | Many032
    | Many033
    | Many034
    | Many035
    | Many036
    | Many037
    | Many038
    | Many039
    | Many040
    | Many041
    | Many042
    | Many043
    | Many044
    | Many045
    | Many046
    | Many047
    | Many048
    | Many049
    | Many050
    | Many051
    | Many052
    | Many053
    | Many054
    | Many055
    | Many056
    | Many057
    | Many058
    | Many059
    | Many060
    | Many061
    | Many062
    | Many063
    | Many064
    | Many065
    | Many066
    | Many067
    | Many068
    | Many069
    | Many070
    | Many071
    | Many072
    | Many073
    | Many074
    | Many075
    | Many076
    | Many077
    | Many078
    | Many079
    | Many080
    | Many081
    | Many082
    | Many083
    | Many084
    | Many085
    | Many086
    | Many087
    | Many088
    | Many089
    | Many090
    | Many091
    | Many092
    | Many093
    | Many094
    | Many095
    | Many096
    | Many097
    | Many098
    | Many099
    | Many100
    | Many101
    | Many102
    | Many103
    | Many104
    | Many105
    | Many106
    | Many107
    | Many108
    | Many109
    | Many110
    | Many111
    | Many112
    | Many113
    | Many114
    | Many115
    | Many116
    | Many117
    | Many118
    | Many119
    | Many120
    | Many121
    | Many122
    | Many123
    | Many124
    | Many125
    | Many126
    | Many127
    | Many128
    | Many129
    | Many130
    | Many131
    | Many132
    | Many133
    | Many134
    | Many135
    | Many136
    | Many137
    | Many138
    | Many139
    | Many140
    | Many141
    | Many142
    | Many143
    | Many144
    | Many145
    | Many146
    | Many147
    | Many148
    | Many149
    | Many150
    | Many151
    | Many152
    | Many153
    | Many154
    | Many155
    | Many156
    | Many157
    | Many158
    | Many159
    | Many160
    | Many161
    | Many162
    | Many163
    | Many164
    | Many165
    | Many166
    | Many167
    | Many168
    | Many169
    | Many170
    | Many171
    | Many172
    | Many173
    | Many174
    | Many175
    | Many176
    | Many177
    | Many178
    | Many179
    | Many180
    | Many181
    | Many182
    | Many183
    | Many184
    | Many185
    | Many186
    | Many187
    | Many188
    | Many189
    | Many190
    | Many191
    | Many192
    | Many193
    | Many194
    | Many195
    | Many196
    | Many197
    | Many198
    | Many199
    | Many200
    | Many201
    | Many202
    | Many203
    | Many204
    | Many205
    | Many206
    | Many207
    | Many208
    | Many209
    | Many210
    | Many211
    | Many212
    | Many213
    | Many214
    | Many215
    | Many216
    | Many217
    | Many218
    | Many219
    | Many220
    | Many221
    | Many222
    | Many223
    | Many224
    | Many225
    | Many226
    | Many227
    | Many228
    | Many229
    | Many230
    | Many231
    | Many232
    | Many233
    | Many234
    | Many235
    | Many236
    | Many237
    | Many238
    | Many239
    | Many240
    | Many241
    | Many242
    | Many243
    | Many244
    | Many245
    | Many246
    | Many247
    | Many248
    | Many249
    | Many250
    | Many251
    | Many252
    | Many253
    | Many254
    | Many255
    | Many256
    | Many257
    | Many258
    | Many259


type ToFrontend
    = Welcome { reordered : Reordered, nested : Box (Box Int) }
    | Pong


{-| Field names whose wire order (Lamdera.Lexical) differs from their source order. -}
type alias Reordered =
    { zeta : Int
    , alpha : String
    , b : Bool
    , aa : Int
    , a_b : Int
    , aB : Int
    }


type alias Todo =
    { title : String
    , done : Bool
    , tags : List String
    }


type alias Box a =
    { value : a }


type Tree a
    = Leaf
    | Node (Tree a) a (Tree a)


{-| Mutually recursive, with a type parameter. -}
type Expr a
    = Lit a
    | Add (Expr a) (Expr a)
    | Block (Stmt a)


type Stmt a
    = Return (Expr a)
    | Seq (List (Stmt a))
