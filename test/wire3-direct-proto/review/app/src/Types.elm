module Types exposing (..)

import Array exposing (Array)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Other exposing (..)
import Random
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Set exposing (Set)
import Time


type alias TInt = Int


type Fat
    = Fat { a01 : Int, a02 : Int, a03 : Int, a04 : Int, a05 : Int, a06 : Int, a07 : Int, a08 : Int, a09 : Int, a10 : Int, a11 : Int, a12 : Int, a13 : Int, a14 : Int, a15 : Int, a16 : Int, a17 : Int, a18 : Int, a19 : Int, a20 : Int, a21 : Int, a22 : Int, a23 : Int, a24 : Int, a25 : Int, a26 : Int, a27 : Int, a28 : Int, a29 : Int, a30 : Int, a31 : Int, a32 : Int, a33 : Int, a34 : Int, a35 : Int, a36 : Int, a37 : Int, a38 : Int, a39 : Int, a40 : Int, a41 : Int, a42 : Int, a43 : Int, a44 : Int, a45 : Int, a46 : Int, a47 : Int, a48 : Int, a49 : Int, a50 : Int, a51 : Int, a52 : Int, a53 : Int, a54 : Int, a55 : Int, a56 : Int, a57 : Int, a58 : Int, a59 : Int, a60 : Int, next : Fat }
    | FatEnd


type Fat2
    = Fat2 { aaa : Fat2, b01 : String, b02 : String, b03 : String, b04 : String, b05 : String, b06 : String, b07 : String, b08 : String, b09 : String, b10 : String, b11 : String, b12 : String, b13 : String, b14 : String, b15 : String, b16 : String, b17 : String, b18 : String, b19 : String, b20 : String, b21 : String, b22 : String, b23 : String, b24 : String, b25 : String, b26 : String, b27 : String, b28 : String, b29 : String, b30 : String, b31 : String, b32 : String, b33 : String, b34 : String, b35 : String, b36 : String, b37 : String, b38 : String, b39 : String, b40 : String }
    | Fat2End


type Nest
    = Nest (List Nest)


type Fat3
    = Fat3 { c001 : Int, c002 : Int, c003 : Int, c004 : Int, c005 : Int, c006 : Int, c007 : Int, c008 : Int, c009 : Int, c010 : Int, c011 : Int, c012 : Int, c013 : Int, c014 : Int, c015 : Int, c016 : Int, c017 : Int, c018 : Int, c019 : Int, c020 : Int, c021 : Int, c022 : Int, c023 : Int, c024 : Int, c025 : Int, c026 : Int, c027 : Int, c028 : Int, c029 : Int, c030 : Int, c031 : Int, c032 : Int, c033 : Int, c034 : Int, c035 : Int, c036 : Int, c037 : Int, c038 : Int, c039 : Int, c040 : Int, c041 : Int, c042 : Int, c043 : Int, c044 : Int, c045 : Int, c046 : Int, c047 : Int, c048 : Int, c049 : Int, c050 : Int, c051 : Int, c052 : Int, c053 : Int, c054 : Int, c055 : Int, c056 : Int, c057 : Int, c058 : Int, c059 : Int, c060 : Int, c061 : Int, c062 : Int, c063 : Int, c064 : Int, c065 : Int, c066 : Int, c067 : Int, c068 : Int, c069 : Int, c070 : Int, c071 : Int, c072 : Int, c073 : Int, c074 : Int, c075 : Int, c076 : Int, c077 : Int, c078 : Int, c079 : Int, c080 : Int, c081 : Int, c082 : Int, c083 : Int, c084 : Int, c085 : Int, c086 : Int, c087 : Int, c088 : Int, c089 : Int, c090 : Int, c091 : Int, c092 : Int, c093 : Int, c094 : Int, c095 : Int, c096 : Int, c097 : Int, c098 : Int, c099 : Int, c100 : Int, c101 : Int, c102 : Int, c103 : Int, c104 : Int, c105 : Int, c106 : Int, c107 : Int, c108 : Int, c109 : Int, c110 : Int, c111 : Int, c112 : Int, c113 : Int, c114 : Int, c115 : Int, c116 : Int, c117 : Int, c118 : Int, c119 : Int, c120 : Int, c121 : Int, c122 : Int, c123 : Int, c124 : Int, c125 : Int, c126 : Int, c127 : Int, c128 : Int, c129 : Int, c130 : Int, c131 : Int, c132 : Int, c133 : Int, c134 : Int, c135 : Int, c136 : Int, c137 : Int, c138 : Int, c139 : Int, c140 : Int, c141 : Int, c142 : Int, c143 : Int, c144 : Int, c145 : Int, c146 : Int, c147 : Int, c148 : Int, c149 : Int, c150 : Int, c151 : Int, c152 : Int, c153 : Int, c154 : Int, c155 : Int, c156 : Int, c157 : Int, c158 : Int, c159 : Int, c160 : Int, c161 : Int, c162 : Int, c163 : Int, c164 : Int, c165 : Int, c166 : Int, c167 : Int, c168 : Int, c169 : Int, c170 : Int, c171 : Int, c172 : Int, c173 : Int, c174 : Int, c175 : Int, c176 : Int, c177 : Int, c178 : Int, c179 : Int, c180 : Int, c181 : Int, c182 : Int, c183 : Int, c184 : Int, c185 : Int, c186 : Int, c187 : Int, c188 : Int, c189 : Int, c190 : Int, c191 : Int, c192 : Int, c193 : Int, c194 : Int, c195 : Int, c196 : Int, c197 : Int, c198 : Int, c199 : Int, c200 : Int, next : Fat3 }
    | Fat3End


type Hist
    = Hist { at : Int, by : String, kind : Maybe Int, note : String, amount : Float, tags : List String, ok : Bool, ref : ( Int, Int ), sub : { x : Int, y : Int }, z : Int } Hist
    | HistEnd


type alias TFloat = Float
type alias TString = String
type alias TChar = Char
type alias TBool = Bool
type alias TOrder = Order
type alias TUnit = ()
type alias TBytes = Bytes
type alias TPosix = Time.Posix
type alias TMaybe = Maybe Int
type alias TMaybeMaybe = Maybe (Maybe Bool)
type alias TResult = Result String Int
type alias TList = List Int
type alias TListUnit = List ()
type alias TListStr = List String
type alias TArray = Array String
type alias TSetInt = Set Int
type alias TDictIntFloat = Dict Int Float
type alias TArrayFloat = Array Float
type alias TListFloat = List Float
type alias TRecFloat = { x : Float, y : Maybe Float }
type alias TSeqDictFloat = SeqDict Float Float
type alias TSetFloat = Set Float
type alias TSetString = Set String
type alias TSetChar = Set Char
type alias TSetTuple = Set ( Float, String )
type alias TSetList = Set (List Int)
type alias TDictInt = Dict Int String
type alias TDictFloat = Dict Float Int
type alias TDictString = Dict String Int
type alias TDictChar = Dict Char Bool
type alias TDictTuple = Dict ( Int, String ) Float
type alias TDictTriple = Dict ( Int, Int, Int ) ()
type alias TDictList = Dict (List String) Int
type alias TSeqDict = SeqDict String Int
type alias TSeqDictRec = SeqDict { a : Int, b : String } Int
type alias TSeqDictDict = SeqDict (Dict Int Int) String
type alias TSeqSet = SeqSet (Maybe Int)
type alias TPair = ( Int, String )
type alias TTriple = ( Bool, Char, Float )


type Color
    = Red
    | Green
    | Blue


type alias TColor = Color


type Id
    = Id String


type Wrapped
    = Wrapped Id


type Multi
    = Multi Int String Bool


type Mixed
    = Zed
    | Alpha Int
    | Mid String Float
    | Beta (Maybe Mixed)
    | Omega


type Tree
    = Leaf
    | Node Tree Int Tree


type Forest
    = Forest (List TreeB)
    | NoForest


type TreeB
    = TreeB Int Forest


type Thing
    = Thing (Rose Thing)
    | NoThing


type alias TBoxes = Box (Box (List (Maybe Int)))
type alias TTwo = Two String (Two Int Bool)
type alias TWrap = Wrap (Maybe Color)
type alias TRose = Rose (Maybe Char)


type alias Reserved =
    { class : Int, default : String, new : Bool, this : Float, var : Int, delete : String, function : Int, return : Int, null : Int, a : Int, b : Int }


type alias Person = Named { age : Int }
type alias TNamedField = { who : Named { x : Int }, w : Person }
type alias Nested = { outer : { inner : { deep : List Int }, flag : Bool }, z : Int }
type alias WithFn = { f : Int -> Int, n : Int }


type HasFn
    = HasFn WithFn
    | NoFn Int


type alias TNever = Maybe Never
type alias TResultNever = Result Never Int
type alias TListNever = List Never
type alias TPhantom = Phantom Tree
type alias TSeed = Random.Seed


type MX a
    = MX0 a
    | MX1 (List (MY a)) (Box (List Int))
    | MX2 (Maybe (MZ a))


type MY a
    = MY0 (Dict Int a)
    | MY1 (MX a) (Maybe (Box (Maybe String)))


type MZ a
    = MZ0
    | MZ1 { zz : MY a, aa : List (Box Int), mm : a }


type alias TMX = MX (Result String (Box Int))


type MP a b
    = MP0 a (Box (List b))
    | MP1 (List (MQ b a))
    | MPEnd


type MQ a b
    = MQ0 b (Maybe (Box (Dict String a)))
    | MQ1 (Maybe (MP b a))


type alias TMP = MP Char (List Int)


type Holder a
    = Holder a (Box (List Int)) (Maybe (Box (Maybe a))) ( Wrap (List a), Dict Int (Maybe a) )
    | HolderNone


type alias THolder = Holder Bool
type alias PRec a = { zeta : a, alpha : Box (Dict String Int), mid : List a, bB : Box ( a, Maybe Int ) }
type alias TPRec = PRec Char
type alias Order1 = { zz : Int, aa : String, mm : Bool, b : Float, a10 : Int, a2 : Char, a1 : Maybe Int, aB : String, ab : Int, aZ : Bool }
type alias TOrderedTuple = ( Order1, { y : Int, x : String }, ( Int, { q : Char, p : List Order1 } ) )


type CtorOrder
    = CtorOrder { zz : Int, aa : String } ( Int, Char ) (List { k : Int, j : String }) Order1
    | CtorB Float Int String


type alias TCtorOrder = CtorOrder
type alias TExtOrder = Ext { zed : Int, ant : String }


type MA x
    = MA x (Maybe (MB x))


type MB x
    = MB (List (MA x)) (Dict Int x)


type alias Ext r = { r | v : Int, zz : Maybe Int }
type alias TExt = Ext { w : String, a : Char }
type alias Swap a b = Two b (Maybe a)
type alias TSwap = Swap Int String
type alias Deep3 a = Maybe (Maybe (Maybe a))
type alias TDeep3 = Box (Deep3 (Wrap Bool))
type alias DB a = { db : Dict String a, seed : Int }
type alias TDB = DB (List (Maybe Color))


type Big
    = BigK289 Int
    | BigK251 String (Maybe Big)
    | BigK029
    | BigK262 Int
    | BigK021
    | BigK253
    | BigK031 Int
    | BigK244
    | BigK058 String (Maybe Big)
    | BigK164 Int
    | BigK025
    | BigK055
    | BigK066 Int
    | BigK290
    | BigK024
    | BigK206 Int
    | BigK168
    | BigK073
    | BigK013 Int
    | BigK280
    | BigK242
    | BigK283 Int
    | BigK056 String (Maybe Big)
    | BigK040
    | BigK098 Int
    | BigK003
    | BigK214
    | BigK233 Int
    | BigK076
    | BigK007 String (Maybe Big)
    | BigK138 Int
    | BigK255
    | BigK189
    | BigK129 Int
    | BigK112
    | BigK028
    | BigK121 Int
    | BigK299
    | BigK082
    | BigK017 Int
    | BigK215
    | BigK231
    | BigK019 Int
    | BigK049 String (Maybe Big)
    | BigK125
    | BigK089 Int
    | BigK236
    | BigK218
    | BigK061 Int
    | BigK293
    | BigK069 String (Maybe Big)
    | BigK222 Int
    | BigK109
    | BigK224
    | BigK119 Int
    | BigK132
    | BigK205
    | BigK054 Int
    | BigK113
    | BigK213
    | BigK165 Int
    | BigK010
    | BigK102
    | BigK167 Int
    | BigK254 String (Maybe Big)
    | BigK192
    | BigK291 Int
    | BigK097
    | BigK117
    | BigK285 Int
    | BigK072
    | BigK248 String (Maybe Big)
    | BigK044 Int
    | BigK161
    | BigK106
    | BigK135 Int
    | BigK059
    | BigK298
    | BigK166 Int
    | BigK032
    | BigK137
    | BigK287 Int
    | BigK001
    | BigK284
    | BigK259 Int
    | BigK083 String (Maybe Big)
    | BigK114
    | BigK163 Int
    | BigK145
    | BigK185
    | BigK035 Int
    | BigK149
    | BigK286 String (Maybe Big)
    | BigK275 Int
    | BigK229
    | BigK039
    | BigK219 Int
    | BigK249
    | BigK162
    | BigK157 Int
    | BigK172
    | BigK099
    | BigK263 Int
    | BigK108
    | BigK151
    | BigK281 Int
    | BigK256 String (Maybe Big)
    | BigK128
    | BigK260 Int
    | BigK225
    | BigK034
    | BigK203 Int
    | BigK210
    | BigK177 String (Maybe Big)
    | BigK133 Int
    | BigK141
    | BigK063
    | BigK232 Int
    | BigK022
    | BigK068
    | BigK146 Int
    | BigK048
    | BigK158
    | BigK018 Int
    | BigK211
    | BigK051
    | BigK207 Int
    | BigK074 String (Maybe Big)
    | BigK228
    | BigK122 Int
    | BigK174
    | BigK124
    | BigK198 Int
    | BigK142
    | BigK155 String (Maybe Big)
    | BigK276 Int
    | BigK204
    | BigK246
    | BigK015 Int
    | BigK169
    | BigK095
    | BigK111 Int
    | BigK043
    | BigK195
    | BigK105 Int
    | BigK147
    | BigK144
    | BigK033 Int
    | BigK187 String (Maybe Big)
    | BigK266
    | BigK288 Int
    | BigK170
    | BigK226
    | BigK182 Int
    | BigK221
    | BigK116 String (Maybe Big)
    | BigK197 Int
    | BigK217
    | BigK243
    | BigK060 Int
    | BigK008
    | BigK220
    | BigK136 Int
    | BigK153
    | BigK171
    | BigK012 Int
    | BigK041
    | BigK230
    | BigK176 Int
    | BigK038 String (Maybe Big)
    | BigK264
    | BigK020 Int
    | BigK274
    | BigK235
    | BigK277 Int
    | BigK175
    | BigK295 String (Maybe Big)
    | BigK188 Int
    | BigK139
    | BigK179
    | BigK070 Int
    | BigK186
    | BigK252
    | BigK092 Int
    | BigK156
    | BigK200
    | BigK087 Int
    | BigK053
    | BigK023
    | BigK268 Int
    | BigK009 String (Maybe Big)
    | BigK075
    | BigK094 Int
    | BigK223
    | BigK245
    | BigK027 Int
    | BigK202
    | BigK273 String (Maybe Big)
    | BigK134 Int
    | BigK237
    | BigK131
    | BigK088 Int
    | BigK118
    | BigK062
    | BigK193 Int
    | BigK030
    | BigK050
    | BigK159 Int
    | BigK046
    | BigK241
    | BigK257 Int
    | BigK115 String (Maybe Big)
    | BigK270
    | BigK265 Int
    | BigK096
    | BigK261
    | BigK269 Int
    | BigK201
    | BigK140 String (Maybe Big)
    | BigK004 Int
    | BigK103
    | BigK216
    | BigK091 Int
    | BigK191
    | BigK005
    | BigK065 Int
    | BigK296
    | BigK045
    | BigK180 Int
    | BigK120
    | BigK234
    | BigK047 Int
    | BigK292 String (Maybe Big)
    | BigK178
    | BigK123 Int
    | BigK078
    | BigK247
    | BigK090 Int
    | BigK079
    | BigK250 String (Maybe Big)
    | BigK086 Int
    | BigK181
    | BigK173
    | BigK258 Int
    | BigK297
    | BigK150
    | BigK154 Int
    | BigK209
    | BigK077
    | BigK267 Int
    | BigK016
    | BigK272
    | BigK037 Int
    | BigK239 String (Maybe Big)
    | BigK042
    | BigK212 Int
    | BigK184
    | BigK011
    | BigK152 Int
    | BigK196
    | BigK100 String (Maybe Big)
    | BigK279 Int
    | BigK104
    | BigK101
    | BigK160 Int
    | BigK148
    | BigK085
    | BigK084 Int
    | BigK282
    | BigK107
    | BigK002 Int
    | BigK000
    | BigK067
    | BigK064 Int
    | BigK227 String (Maybe Big)
    | BigK071
    | BigK036 Int
    | BigK081
    | BigK199
    | BigK093 Int
    | BigK143
    | BigK208 String (Maybe Big)
    | BigK110 Int
    | BigK006
    | BigK294
    | BigK052 Int
    | BigK278
    | BigK194
    | BigK126 Int
    | BigK240
    | BigK190
    | BigK057 Int
    | BigK080
    | BigK026
    | BigK127 Int
    | BigK238 String (Maybe Big)
    | BigK014
    | BigK271 Int
    | BigK183
    | BigK130


type Enum300
    = Enum300K059
    | Enum300K025
    | Enum300K043
    | Enum300K299
    | Enum300K106
    | Enum300K142
    | Enum300K277
    | Enum300K054
    | Enum300K171
    | Enum300K199
    | Enum300K191
    | Enum300K172
    | Enum300K215
    | Enum300K096
    | Enum300K265
    | Enum300K167
    | Enum300K148
    | Enum300K065
    | Enum300K061
    | Enum300K044
    | Enum300K134
    | Enum300K026
    | Enum300K269
    | Enum300K248
    | Enum300K099
    | Enum300K094
    | Enum300K294
    | Enum300K259
    | Enum300K088
    | Enum300K063
    | Enum300K207
    | Enum300K111
    | Enum300K131
    | Enum300K053
    | Enum300K164
    | Enum300K014
    | Enum300K114
    | Enum300K107
    | Enum300K177
    | Enum300K069
    | Enum300K256
    | Enum300K060
    | Enum300K234
    | Enum300K276
    | Enum300K222
    | Enum300K067
    | Enum300K253
    | Enum300K024
    | Enum300K206
    | Enum300K029
    | Enum300K038
    | Enum300K242
    | Enum300K225
    | Enum300K057
    | Enum300K002
    | Enum300K210
    | Enum300K261
    | Enum300K240
    | Enum300K218
    | Enum300K041
    | Enum300K097
    | Enum300K193
    | Enum300K285
    | Enum300K022
    | Enum300K049
    | Enum300K229
    | Enum300K093
    | Enum300K082
    | Enum300K146
    | Enum300K289
    | Enum300K288
    | Enum300K205
    | Enum300K208
    | Enum300K162
    | Enum300K183
    | Enum300K188
    | Enum300K129
    | Enum300K228
    | Enum300K112
    | Enum300K160
    | Enum300K185
    | Enum300K282
    | Enum300K203
    | Enum300K170
    | Enum300K037
    | Enum300K169
    | Enum300K105
    | Enum300K047
    | Enum300K083
    | Enum300K045
    | Enum300K091
    | Enum300K223
    | Enum300K204
    | Enum300K087
    | Enum300K040
    | Enum300K011
    | Enum300K273
    | Enum300K001
    | Enum300K202
    | Enum300K266
    | Enum300K051
    | Enum300K292
    | Enum300K015
    | Enum300K104
    | Enum300K158
    | Enum300K007
    | Enum300K035
    | Enum300K197
    | Enum300K095
    | Enum300K178
    | Enum300K291
    | Enum300K013
    | Enum300K071
    | Enum300K257
    | Enum300K201
    | Enum300K156
    | Enum300K008
    | Enum300K020
    | Enum300K102
    | Enum300K052
    | Enum300K153
    | Enum300K250
    | Enum300K023
    | Enum300K019
    | Enum300K130
    | Enum300K117
    | Enum300K090
    | Enum300K074
    | Enum300K235
    | Enum300K028
    | Enum300K056
    | Enum300K255
    | Enum300K243
    | Enum300K145
    | Enum300K124
    | Enum300K103
    | Enum300K144
    | Enum300K290
    | Enum300K085
    | Enum300K274
    | Enum300K163
    | Enum300K021
    | Enum300K267
    | Enum300K058
    | Enum300K017
    | Enum300K012
    | Enum300K138
    | Enum300K108
    | Enum300K268
    | Enum300K137
    | Enum300K084
    | Enum300K127
    | Enum300K140
    | Enum300K231
    | Enum300K042
    | Enum300K246
    | Enum300K079
    | Enum300K120
    | Enum300K217
    | Enum300K264
    | Enum300K110
    | Enum300K186
    | Enum300K077
    | Enum300K016
    | Enum300K119
    | Enum300K198
    | Enum300K050
    | Enum300K233
    | Enum300K213
    | Enum300K098
    | Enum300K123
    | Enum300K270
    | Enum300K161
    | Enum300K147
    | Enum300K152
    | Enum300K176
    | Enum300K109
    | Enum300K232
    | Enum300K296
    | Enum300K139
    | Enum300K182
    | Enum300K027
    | Enum300K175
    | Enum300K116
    | Enum300K141
    | Enum300K064
    | Enum300K286
    | Enum300K224
    | Enum300K033
    | Enum300K295
    | Enum300K287
    | Enum300K174
    | Enum300K092
    | Enum300K089
    | Enum300K118
    | Enum300K280
    | Enum300K135
    | Enum300K187
    | Enum300K220
    | Enum300K157
    | Enum300K086
    | Enum300K181
    | Enum300K230
    | Enum300K271
    | Enum300K076
    | Enum300K032
    | Enum300K293
    | Enum300K004
    | Enum300K030
    | Enum300K190
    | Enum300K258
    | Enum300K165
    | Enum300K132
    | Enum300K226
    | Enum300K155
    | Enum300K126
    | Enum300K238
    | Enum300K247
    | Enum300K184
    | Enum300K039
    | Enum300K216
    | Enum300K278
    | Enum300K249
    | Enum300K227
    | Enum300K009
    | Enum300K236
    | Enum300K121
    | Enum300K284
    | Enum300K070
    | Enum300K143
    | Enum300K136
    | Enum300K263
    | Enum300K003
    | Enum300K100
    | Enum300K211
    | Enum300K034
    | Enum300K200
    | Enum300K010
    | Enum300K075
    | Enum300K062
    | Enum300K113
    | Enum300K159
    | Enum300K168
    | Enum300K031
    | Enum300K006
    | Enum300K122
    | Enum300K036
    | Enum300K073
    | Enum300K237
    | Enum300K125
    | Enum300K078
    | Enum300K251
    | Enum300K241
    | Enum300K260
    | Enum300K005
    | Enum300K212
    | Enum300K128
    | Enum300K254
    | Enum300K297
    | Enum300K133
    | Enum300K081
    | Enum300K196
    | Enum300K194
    | Enum300K245
    | Enum300K149
    | Enum300K252
    | Enum300K115
    | Enum300K221
    | Enum300K219
    | Enum300K192
    | Enum300K272
    | Enum300K283
    | Enum300K166
    | Enum300K055
    | Enum300K298
    | Enum300K000
    | Enum300K179
    | Enum300K046
    | Enum300K068
    | Enum300K101
    | Enum300K244
    | Enum300K195
    | Enum300K080
    | Enum300K072
    | Enum300K239
    | Enum300K189
    | Enum300K275
    | Enum300K209
    | Enum300K214
    | Enum300K066
    | Enum300K180
    | Enum300K150
    | Enum300K173
    | Enum300K262
    | Enum300K279
    | Enum300K151
    | Enum300K048
    | Enum300K154
    | Enum300K281
    | Enum300K018


type Enum256
    = Enum256K202
    | Enum256K052
    | Enum256K107
    | Enum256K090
    | Enum256K064
    | Enum256K075
    | Enum256K218
    | Enum256K067
    | Enum256K195
    | Enum256K032
    | Enum256K034
    | Enum256K254
    | Enum256K042
    | Enum256K022
    | Enum256K197
    | Enum256K151
    | Enum256K086
    | Enum256K224
    | Enum256K157
    | Enum256K156
    | Enum256K146
    | Enum256K055
    | Enum256K206
    | Enum256K145
    | Enum256K106
    | Enum256K113
    | Enum256K046
    | Enum256K136
    | Enum256K085
    | Enum256K021
    | Enum256K138
    | Enum256K081
    | Enum256K037
    | Enum256K180
    | Enum256K019
    | Enum256K004
    | Enum256K133
    | Enum256K084
    | Enum256K108
    | Enum256K183
    | Enum256K099
    | Enum256K150
    | Enum256K111
    | Enum256K003
    | Enum256K112
    | Enum256K166
    | Enum256K230
    | Enum256K091
    | Enum256K097
    | Enum256K096
    | Enum256K100
    | Enum256K154
    | Enum256K058
    | Enum256K248
    | Enum256K148
    | Enum256K245
    | Enum256K207
    | Enum256K087
    | Enum256K121
    | Enum256K149
    | Enum256K193
    | Enum256K130
    | Enum256K026
    | Enum256K134
    | Enum256K137
    | Enum256K175
    | Enum256K036
    | Enum256K233
    | Enum256K185
    | Enum256K247
    | Enum256K063
    | Enum256K007
    | Enum256K035
    | Enum256K077
    | Enum256K128
    | Enum256K135
    | Enum256K163
    | Enum256K031
    | Enum256K184
    | Enum256K124
    | Enum256K126
    | Enum256K236
    | Enum256K179
    | Enum256K122
    | Enum256K187
    | Enum256K033
    | Enum256K170
    | Enum256K190
    | Enum256K241
    | Enum256K196
    | Enum256K014
    | Enum256K070
    | Enum256K219
    | Enum256K057
    | Enum256K208
    | Enum256K198
    | Enum256K161
    | Enum256K173
    | Enum256K016
    | Enum256K205
    | Enum256K066
    | Enum256K153
    | Enum256K225
    | Enum256K214
    | Enum256K054
    | Enum256K142
    | Enum256K129
    | Enum256K210
    | Enum256K056
    | Enum256K002
    | Enum256K203
    | Enum256K027
    | Enum256K178
    | Enum256K237
    | Enum256K131
    | Enum256K242
    | Enum256K181
    | Enum256K117
    | Enum256K162
    | Enum256K164
    | Enum256K152
    | Enum256K139
    | Enum256K229
    | Enum256K160
    | Enum256K253
    | Enum256K029
    | Enum256K068
    | Enum256K048
    | Enum256K255
    | Enum256K045
    | Enum256K232
    | Enum256K009
    | Enum256K143
    | Enum256K078
    | Enum256K114
    | Enum256K104
    | Enum256K220
    | Enum256K221
    | Enum256K159
    | Enum256K101
    | Enum256K127
    | Enum256K012
    | Enum256K062
    | Enum256K013
    | Enum256K199
    | Enum256K023
    | Enum256K252
    | Enum256K212
    | Enum256K228
    | Enum256K024
    | Enum256K246
    | Enum256K144
    | Enum256K132
    | Enum256K140
    | Enum256K188
    | Enum256K165
    | Enum256K043
    | Enum256K105
    | Enum256K115
    | Enum256K204
    | Enum256K234
    | Enum256K001
    | Enum256K069
    | Enum256K110
    | Enum256K155
    | Enum256K011
    | Enum256K053
    | Enum256K044
    | Enum256K191
    | Enum256K189
    | Enum256K177
    | Enum256K017
    | Enum256K072
    | Enum256K092
    | Enum256K238
    | Enum256K047
    | Enum256K079
    | Enum256K174
    | Enum256K093
    | Enum256K102
    | Enum256K240
    | Enum256K020
    | Enum256K168
    | Enum256K059
    | Enum256K147
    | Enum256K006
    | Enum256K182
    | Enum256K051
    | Enum256K217
    | Enum256K216
    | Enum256K186
    | Enum256K061
    | Enum256K201
    | Enum256K200
    | Enum256K074
    | Enum256K083
    | Enum256K171
    | Enum256K030
    | Enum256K231
    | Enum256K223
    | Enum256K116
    | Enum256K213
    | Enum256K005
    | Enum256K176
    | Enum256K015
    | Enum256K080
    | Enum256K094
    | Enum256K215
    | Enum256K088
    | Enum256K211
    | Enum256K249
    | Enum256K008
    | Enum256K060
    | Enum256K172
    | Enum256K073
    | Enum256K192
    | Enum256K041
    | Enum256K209
    | Enum256K071
    | Enum256K028
    | Enum256K065
    | Enum256K141
    | Enum256K038
    | Enum256K243
    | Enum256K226
    | Enum256K098
    | Enum256K222
    | Enum256K095
    | Enum256K018
    | Enum256K040
    | Enum256K167
    | Enum256K039
    | Enum256K049
    | Enum256K025
    | Enum256K118
    | Enum256K119
    | Enum256K082
    | Enum256K244
    | Enum256K235
    | Enum256K158
    | Enum256K250
    | Enum256K251
    | Enum256K076
    | Enum256K120
    | Enum256K109
    | Enum256K000
    | Enum256K089
    | Enum256K194
    | Enum256K169
    | Enum256K123
    | Enum256K239
    | Enum256K227
    | Enum256K125
    | Enum256K010
    | Enum256K050
    | Enum256K103


type Enum255
    = Enum255K124
    | Enum255K016
    | Enum255K216
    | Enum255K170
    | Enum255K062
    | Enum255K227
    | Enum255K166
    | Enum255K173
    | Enum255K176
    | Enum255K042
    | Enum255K178
    | Enum255K095
    | Enum255K156
    | Enum255K174
    | Enum255K056
    | Enum255K162
    | Enum255K224
    | Enum255K201
    | Enum255K065
    | Enum255K168
    | Enum255K075
    | Enum255K059
    | Enum255K232
    | Enum255K215
    | Enum255K191
    | Enum255K040
    | Enum255K076
    | Enum255K014
    | Enum255K084
    | Enum255K110
    | Enum255K010
    | Enum255K045
    | Enum255K248
    | Enum255K080
    | Enum255K127
    | Enum255K148
    | Enum255K129
    | Enum255K240
    | Enum255K145
    | Enum255K020
    | Enum255K219
    | Enum255K087
    | Enum255K190
    | Enum255K046
    | Enum255K064
    | Enum255K185
    | Enum255K243
    | Enum255K018
    | Enum255K007
    | Enum255K247
    | Enum255K001
    | Enum255K207
    | Enum255K104
    | Enum255K134
    | Enum255K189
    | Enum255K138
    | Enum255K017
    | Enum255K183
    | Enum255K144
    | Enum255K103
    | Enum255K057
    | Enum255K117
    | Enum255K033
    | Enum255K140
    | Enum255K250
    | Enum255K029
    | Enum255K252
    | Enum255K108
    | Enum255K209
    | Enum255K028
    | Enum255K165
    | Enum255K167
    | Enum255K074
    | Enum255K021
    | Enum255K188
    | Enum255K034
    | Enum255K039
    | Enum255K141
    | Enum255K027
    | Enum255K226
    | Enum255K031
    | Enum255K157
    | Enum255K199
    | Enum255K212
    | Enum255K116
    | Enum255K229
    | Enum255K223
    | Enum255K237
    | Enum255K086
    | Enum255K112
    | Enum255K067
    | Enum255K083
    | Enum255K151
    | Enum255K048
    | Enum255K049
    | Enum255K179
    | Enum255K210
    | Enum255K164
    | Enum255K054
    | Enum255K069
    | Enum255K238
    | Enum255K213
    | Enum255K206
    | Enum255K225
    | Enum255K119
    | Enum255K066
    | Enum255K205
    | Enum255K244
    | Enum255K071
    | Enum255K254
    | Enum255K197
    | Enum255K094
    | Enum255K214
    | Enum255K146
    | Enum255K088
    | Enum255K044
    | Enum255K233
    | Enum255K038
    | Enum255K235
    | Enum255K011
    | Enum255K163
    | Enum255K004
    | Enum255K177
    | Enum255K236
    | Enum255K130
    | Enum255K202
    | Enum255K035
    | Enum255K003
    | Enum255K078
    | Enum255K105
    | Enum255K194
    | Enum255K022
    | Enum255K153
    | Enum255K019
    | Enum255K073
    | Enum255K172
    | Enum255K052
    | Enum255K133
    | Enum255K246
    | Enum255K092
    | Enum255K106
    | Enum255K037
    | Enum255K137
    | Enum255K187
    | Enum255K142
    | Enum255K115
    | Enum255K184
    | Enum255K139
    | Enum255K221
    | Enum255K128
    | Enum255K098
    | Enum255K160
    | Enum255K079
    | Enum255K006
    | Enum255K234
    | Enum255K005
    | Enum255K099
    | Enum255K047
    | Enum255K150
    | Enum255K154
    | Enum255K131
    | Enum255K217
    | Enum255K002
    | Enum255K096
    | Enum255K050
    | Enum255K023
    | Enum255K159
    | Enum255K222
    | Enum255K120
    | Enum255K122
    | Enum255K082
    | Enum255K198
    | Enum255K072
    | Enum255K025
    | Enum255K101
    | Enum255K228
    | Enum255K249
    | Enum255K009
    | Enum255K097
    | Enum255K220
    | Enum255K193
    | Enum255K182
    | Enum255K242
    | Enum255K089
    | Enum255K102
    | Enum255K126
    | Enum255K253
    | Enum255K015
    | Enum255K169
    | Enum255K063
    | Enum255K204
    | Enum255K093
    | Enum255K061
    | Enum255K192
    | Enum255K091
    | Enum255K132
    | Enum255K143
    | Enum255K077
    | Enum255K200
    | Enum255K070
    | Enum255K051
    | Enum255K055
    | Enum255K231
    | Enum255K100
    | Enum255K090
    | Enum255K180
    | Enum255K123
    | Enum255K060
    | Enum255K147
    | Enum255K008
    | Enum255K218
    | Enum255K053
    | Enum255K013
    | Enum255K175
    | Enum255K107
    | Enum255K208
    | Enum255K032
    | Enum255K026
    | Enum255K000
    | Enum255K230
    | Enum255K125
    | Enum255K121
    | Enum255K136
    | Enum255K251
    | Enum255K171
    | Enum255K196
    | Enum255K012
    | Enum255K113
    | Enum255K245
    | Enum255K239
    | Enum255K152
    | Enum255K118
    | Enum255K058
    | Enum255K241
    | Enum255K081
    | Enum255K085
    | Enum255K030
    | Enum255K109
    | Enum255K203
    | Enum255K111
    | Enum255K195
    | Enum255K114
    | Enum255K036
    | Enum255K181
    | Enum255K161
    | Enum255K043
    | Enum255K155
    | Enum255K135
    | Enum255K068
    | Enum255K024
    | Enum255K186
    | Enum255K041
    | Enum255K211
    | Enum255K158
    | Enum255K149


type alias TAll =
    { i : Int, f : Float, s : String, c : Char, b : Bool, o : Order
    , l : List ( Int, Maybe String ), d : Dict String (List Float), st : Set ( Int, Char )
    , m : Mixed, t : Tree, big : Big, e300 : Enum300, e256 : Enum256, bs : Bytes
    , r : Result ( Int, Int ) (Array Bool), w : Wrapped, sd : SeqDict Int String
    , fo : Forest, th : Thing, two : TTwo, n : Nested
    }