# Generates review/app/src/{Types,Main}.elm: one small type per shape, each exported
# from Main as an encoder (T -> Bytes) and the raw Wire3 decoder (so JS can see the
# consumed offset), for the differential fuzzer (fuzz.js).
import os, random

here = os.path.dirname(os.path.abspath(__file__))
src = os.path.join(here, 'app', 'src')

random.seed(5)

def big_union(name, n, with_args):
    names = ['%s%03d' % ('K', i) for i in range(n)]
    random.shuffle(names)  # declaration order != alphabetical wire order
    alts = []
    for i, c in enumerate(names):
        c = name + c
        if with_args and i % 3 == 0:
            alts.append('%s Int' % c)
        elif with_args and i % 7 == 1:
            alts.append('%s String (Maybe %s)' % (c, name))
        else:
            alts.append(c)
    return 'type %s\n    = %s\n' % (name, '\n    | '.join(alts))

types = r'''module Types exposing (..)

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


type alias TMA = MA (Result String Float)
type alias Ext r = { r | v : Int, zz : Maybe Int }
type alias TExt = Ext { w : String, a : Char }
type alias Swap a b = Two b (Maybe a)
type alias TSwap = Swap Int String
type alias Deep3 a = Maybe (Maybe (Maybe a))
type alias TDeep3 = Box (Deep3 (Wrap Bool))
type alias DB a = { db : Dict String a, seed : Int }
type alias TDB = DB (List (Maybe Color))


''' + big_union('Big', 300, True) + '\n\n' + big_union('Enum300', 300, False) + '\n\n' + big_union('Enum256', 256, False) + '\n\n' + big_union('Enum255', 255, False) + r'''

type alias TAll =
    { i : Int, f : Float, s : String, c : Char, b : Bool, o : Order
    , l : List ( Int, Maybe String ), d : Dict String (List Float), st : Set ( Int, Char )
    , m : Mixed, t : Tree, big : Big, e300 : Enum300, e256 : Enum256, bs : Bytes
    , r : Result ( Int, Int ) (Array Bool), w : Wrapped, sd : SeqDict Int String
    , fo : Forest, th : Thing, two : TTwo, n : Nested
    }
'''



# ---- Random parameterised types (Params.elm), for attacking decoder parameter threading.
# PSEED picks the set. Each P<i> has 0-3 tvars (some phantom, some used only in some
# ctors), args mixing tvar-using and tvar-free containers at varied nesting, references
# to earlier P<j> and to Other's Box/Two/Wrap/Rose (cross-module), and some mutually
# recursive pairs. Every P<i> is instantiated at several different argument types.
prng = random.Random(int(os.environ.get('PSEED', '1')))
NP = int(os.environ.get('NPARAMS', '14'))
TV = ['a', 'b', 'c']
ptypes = []   # (name, tvars)
defs = []

def closed_leaf():
    # no `()` here: a List of zero-width elements with a random huge length exhausts memory
    # in both builds; `()` only appears as a direct ctor argument (below)
    return prng.choice(['Int', 'String', 'Float', 'Bool', 'Char', 'Order'])

def texpr(tvars, depth, i, allow_self, mutual):
    r = prng.random()
    if depth <= 0 or r < 0.25:
        if tvars and prng.random() < 0.55:
            return prng.choice(tvars)
        return closed_leaf()
    d = depth - 1
    sub = lambda: texpr(tvars, d, i, allow_self, mutual)
    closed = lambda: texpr([], d, i, False, None)
    k = prng.randrange(16)
    if k == 0: return 'List (%s)' % sub()
    if k == 1: return 'Maybe (%s)' % sub()
    if k == 2: return 'Dict Int (%s)' % sub()
    if k == 3: return 'Set Int' if prng.random() < 0.5 else 'Set String'
    if k == 4: return 'Result (%s) (%s)' % (sub(), closed())
    if k == 5: return '( %s, %s )' % (sub(), sub())
    if k == 6: return '( %s, %s, %s )' % (sub(), closed(), sub())
    if k == 7: return '{ zf%d : %s, af%d : %s }' % (i, sub(), i, closed())
    if k == 8: return 'Box (%s)' % (closed() if prng.random() < 0.5 else sub())
    if k == 9: return 'Two (%s) (%s)' % (closed(), sub())
    if k == 10: return 'Wrap (%s)' % sub()
    if k == 11: return 'Array (%s)' % sub()
    if k == 12 and ptypes:
        name, tv = prng.choice(ptypes)
        return '(%s %s)' % (name, ' '.join('(%s)' % (sub() if prng.random() < 0.6 else closed()) for _ in tv)) if tv else name
    if k == 13 and allow_self:
        return '(Maybe (%s))' % allow_self
    if k == 14 and mutual:
        return '(List (%s))' % mutual
    return 'List (Maybe (Box (%s)))' % closed()

i = 0
while i < NP:
    ntv = prng.choice([0, 1, 1, 2, 2, 3])
    tvars = TV[:ntv]
    name = 'P%d' % i
    selfref = '%s %s' % (name, ' '.join(tvars)) if prng.random() < 0.3 else None
    pair = prng.random() < 0.2 and i + 1 < NP and ntv > 0
    names_ = [name, 'P%d' % (i + 1)] if pair else [name]
    for j, nm in enumerate(names_):
        other = names_[1 - j] if pair else None
        mutual = ('%s %s' % (other, ' '.join(tvars))) if other else None
        selfr = '%s %s' % (nm, ' '.join(tvars)) if (selfref and j == 0) else None
        nctor = prng.choice([1, 1, 2, 3, 4])
        used = tvars[:max(0, len(tvars) - (1 if prng.random() < 0.3 else 0))]  # last tvar sometimes phantom
        ctors = []
        for c in range(nctor):
            ctv = used if c == 0 or prng.random() < 0.6 else used[:1]      # some ctors use fewer tvars
            nargs = prng.choice([0, 1, 1, 2, 3]) if c > 0 else prng.choice([1, 2, 3])
            args = [('()' if prng.random() < 0.08 else texpr(ctv, prng.choice([1, 2, 3]), i, selfr, mutual)) for _ in range(nargs)]
            ctors.append('%sC%d%s' % (nm, c, ''.join(' (%s)' % a for a in args)))
        if (selfr or mutual) and nctor == 1:
            ctors.append('%sEnd' % nm)
        defs.append('type %s%s\n    = %s\n' % (nm, ''.join(' ' + t for t in tvars), '\n    | '.join(ctors)))
    for nm in names_:
        ptypes.append((nm, tvars))
    i += len(names_)

insts = []
for name, tv in ptypes:
    for j in range(3):
        args = ' '.join('(%s)' % texpr([], prng.choice([0, 1, 2]), 99, False, None) for _ in tv)
        insts.append(('I%s_%d' % (name, j), '%s %s' % (name, args) if tv else name))
        if not tv: break
defs.append('\n'.join('type alias %s = %s' % (a, t) for a, t in insts) + '\n')
# the same types at several args in one model
defs.append('type alias PModel =\n    { ' + '\n    , '.join('m%d : %s' % (k, t) for k, (a, t) in enumerate(insts)) + '\n    }\n')
params_src = 'module Params exposing (..)\n\nimport Array exposing (Array)\nimport Dict exposing (Dict)\nimport Other exposing (..)\nimport Set exposing (Set)\n\n\n' + '\n\n'.join(defs)
open(os.path.join(src, 'Params.elm'), 'w').write(params_src)
pnames = [a for a, _ in insts] + ['PModel']

names = [l.split()[2] for l in types.splitlines() if l.startswith('type alias ') and l.split()[3] == '='] + \
        [l.split()[1] for l in types.splitlines() if l.startswith('type ') and not l.startswith('type alias') and len(l.split()) == 2]
skip = {'DB', 'WithFn'}
names = [n for n in names if n not in skip] + pnames

excl = [e for e in os.environ.get('EXCLUDE', '').split(',') if e]
if excl:
    types = '\n'.join(l for l in types.splitlines() if not any(l.startswith('type alias %s =' % e) for e in excl))
    names = [n for n in names if n not in excl]
open(os.path.join(src, 'Types.elm'), 'w').write(types)

main = ['port module Main exposing (..)', '', 'import Lamdera.Wire3 as W', 'import Bytes exposing (Bytes)', 'import Bytes.Encode', 'import Bytes.Decode', 'import Array', 'import Dict', 'import Set', 'import SeqDict', 'import Other exposing (..)', 'import Types exposing (..)', 'import Params exposing (..)', '', '', 'port out : Int -> Cmd msg', '']
main.append('''readBytesPast : Int -> Bytes -> Maybe Int
readBytesPast n b =
    Bytes.Decode.decode (Bytes.Decode.bytes n) b |> Maybe.map Bytes.width


''')
main.append('''s_TAll : TAll
s_TAll =
    { i = -5, f = 0 / 0, s = "héllo wörld, long enough", c = 'x', b = True, o = GT
    , l = [ ( 1, Just "a" ), ( 2, Nothing ) ], d = Dict.fromList [ ( "k", [ 1.5, -0.0 ] ), ( "a", [] ) ], st = Set.fromList [ ( 3, 'z' ), ( -1, 'a' ) ]
    , m = Beta (Just (Mid "m" 2.5)), t = Node Leaf 3 (Node Leaf 4 Leaf), big = BigK000, e300 = Enum300K299, e256 = Enum256K255
    , bs = Bytes.Encode.encode (Bytes.Encode.sequence [ Bytes.Encode.unsignedInt8 1, Bytes.Encode.unsignedInt8 2 ])
    , r = Err ( 1, 2 ), w = Wrapped (Id "w"), sd = SeqDict.fromList [ ( 5, "five" ), ( 1, "one" ) ]
    , fo = Forest [ TreeB 1 NoForest, TreeB 2 (Forest []) ], th = Thing (Rose NoThing [ Rose (Thing (Rose NoThing [])) [] ])
    , two = TwoAB "s" (TwoB True), n = { outer = { inner = { deep = [ 1, 2 ] }, flag = False }, z = 9 }
    }


''')
for n in names:
    main.append('e_%s : %s -> Bytes\ne_%s v =\n    W.bytesEncode (w3_encode_%s v)\n\n' % (n, n, n, n))
    main.append('d_%s : W.Decoder %s\nd_%s =\n    w3_decode_%s\n\n' % (n, n, n, n))
main.append('type alias Api =\n    { s_TAll : TAll\n    , readBytesPast : Int -> Bytes -> Maybe Int\n    , ' + '\n    , '.join('e_%s : %s -> Bytes, d_%s : W.Decoder %s' % (n, n, n, n) for n in names) + '\n    }\n\n\napi : Api\n')
main.append('api =\n    { s_TAll = s_TAll\n    , readBytesPast = readBytesPast\n    , ' + '\n    , '.join('e_%s = e_%s, d_%s = d_%s' % (n, n, n, n) for n in names) + '\n    }\n\n')
main.append('main : Program () Api ()\nmain =\n    Platform.worker\n        { init = \\_ -> ( api, out 0 )\n        , update = \\_ m -> ( m, Cmd.none )\n        , subscriptions = \\_ -> Sub.none\n        }\n')
open(os.path.join(src, 'Main.elm'), 'w').write('\n'.join(main))
open(os.path.join(here, 'names.json'), 'w').write(__import__('json').dumps(names))
print(len(names), 'types')
