// ---- Hand-written stand-in for what the C2 generator would emit, --optimize mode ----
// Record fields are written as the Elm name in double braces; build.sh substitutes this build's ShortFieldNames
// (recovered from the record literals in the compiled output), standing in for the
// generator's `Mode.Prod fields ! name` lookup at JS-generation time.
// Ctor representation (prod): Shape Normal {$:declIndex}, Id Unbox (raw), Color Enum (int),
// Tree Normal. Wire tags are alphabetical ctor order (Wire3/Core.hs sortOn name).

// ENCODERS: (v[, tvar encoders]) -> void

function $author$project$Types$w3e_Rec(v) {
  _W3_u8(v.{{active}} ? 1 : 0);                                   // active : Bool
  var l = v.{{counts}}, n = 0;                                    // counts : List Int
  for (var c = l; c.b; c = c.b) n++;
  _W3_int(n);
  for (var c = l; c.b; c = c.b) _W3_int(c.a);
  _W3_int(v.{{id}});                                          // id : Int
  _W3_str(v.{{name}});                                          // name : String
  var m = v.{{note}};                                           // note : Maybe String
  if (m.$ === 1) { _W3_u8(0); } else { _W3_u8(1); _W3_str(m.a); }
  _W3_f64(v.{{score}});                                          // score : Float
  var l = v.{{tags}}, n = 0;                                    // tags : List String
  for (var c = l; c.b; c = c.b) n++;
  _W3_int(n);
  for (var c = l; c.b; c = c.b) _W3_str(c.a);
}

function $author$project$Types$w3e_Shape(v) {
  switch (v.$) {
    case 0: _W3_u8(0); _W3_f64(v.a); return;                         // Circle
    case 2: _W3_u8(1); return;                                        // Empty
    case 3:                                                           // Labelled
      _W3_u8(2); _W3_str(v.a);
      var m = v.b;
      if (m.$ === 1) { _W3_u8(0); } else { _W3_u8(1); $author$project$Types$w3e_Shape(m.a); }
      return;
    default: _W3_u8(3); _W3_f64(v.a); _W3_f64(v.b); return;          // Rect
  }
}

function $author$project$Types$w3e_Id(v) { _W3_u8(0); _W3_str(v); }  // Unbox: v is the String

var $author$project$Types$w3t_Color = [2, 1, 0];                    // decl index -> wire tag
function $author$project$Types$w3e_Color(v) { _W3_u8($author$project$Types$w3t_Color[v]); }

function $author$project$Types$w3e_Tree(v) {
  if (v.$ === 0) { _W3_u8(0); return; }                                // Leaf
  _W3_u8(1);                                                           // Node
  $author$project$Types$w3e_Tree(v.a); _W3_int(v.b); _W3_str(v.c); $author$project$Types$w3e_Tree(v.d);
}

// Parameterised: one extra direct-encoder argument per type variable.
function $author$project$Types$w3e_DB(v, e_a) {
  var d = v.{{db}};                                                         // db : Dict String a
  _W3_int(_W3_dictSize(d));
  var st = [], node = d;
  for (;;) {
    while (node.$ === -1) { st.push(node); node = node.d; }
    if (st.length === 0) break;
    node = st.pop();
    _W3_str(node.b); e_a(node.c);
    node = node.e;
  }
  _W3_int(v.{{seed}});                                                        // seed : Int
}

function $author$project$Types$w3e_Model(v) {
  var arr = _W3_arrayToJs(v.{{arr}});                                        // arr : Array Int
  _W3_int(arr.length);
  for (var i = 0; i < arr.length; i++) _W3_int(arr[i]);
  _W3_int(_Char_toCode(v.{{ch}}));                                          // ch : Char
  var l = v.{{colors}}, n = 0;                                                  // colors : List Color
  for (var c = l; c.b; c = c.b) n++;
  _W3_int(n);
  for (var c = l; c.b; c = c.b) $author$project$Types$w3e_Color(c.a);
  $author$project$Types$w3e_DB(v.{{db}}, $author$project$Types$w3e_Rec);   // db : DB Rec
  var l = v.{{idList}}, n = 0;                                                  // idList : List Id
  for (var c = l; c.b; c = c.b) n++;
  _W3_int(n);
  for (var c = l; c.b; c = c.b) $author$project$Types$w3e_Id(c.a);
  var d = v.{{ids}};                                                         // ids : Set String (Set unboxed = Dict k ())
  _W3_int(_W3_dictSize(d));
  var st = [], node = d;
  for (;;) {
    while (node.$ === -1) { st.push(node); node = node.d; }
    if (st.length === 0) break;
    node = st.pop(); _W3_str(node.b); node = node.e;
  }
  var l = v.{{ints}}, n = 0;                                                  // ints : List Int
  for (var c = l; c.b; c = c.b) n++;
  _W3_int(n);
  for (var c = l; c.b; c = c.b) _W3_int(c.a);
  _W3_int(v.{{pair}}.a); _W3_str(v.{{pair}}.b);                                      // pair : (Int, String)
  var d = v.{{recs}};                                                         // recs : Dict String Rec
  _W3_int(_W3_dictSize(d));
  var st = [], node = d;
  for (;;) {
    while (node.$ === -1) { st.push(node); node = node.d; }
    if (st.length === 0) break;
    node = st.pop(); _W3_str(node.b); $author$project$Types$w3e_Rec(node.c); node = node.e;
  }
  var r = v.{{res}};                                                         // res : Result String Int
  if (r.$ === 0) { _W3_u8(0); _W3_int(r.a); } else { _W3_u8(1); _W3_str(r.a); }
  var l = v.{{shapes}}, n = 0;                                                  // shapes : List Shape
  for (var c = l; c.b; c = c.b) n++;
  _W3_int(n);
  for (var c = l; c.b; c = c.b) $author$project$Types$w3e_Shape(c.a);
  $author$project$Types$w3e_Tree(v.{{tree}});                                 // tree : Tree
                                                                       // unit : () writes nothing
}

// DECODERS: () -> value, reading _W3_rdv at _W3_rpos. Field order == wire order ==
// Elm's own record-literal key order (both are the sorted field names).

function $author$project$Types$w3d_Rec() {
  var n, h, t, c;
  var f_o; switch (_W3_ru8()) { case 0: f_o = false; break; case 1: f_o = true; break; default: throw 0; }
  n = _W3_rint(); h = _List_Nil;
  if (n > 0) { t = h = _List_Cons(_W3_rint(), _List_Nil); for (n--; n > 0; n--) { c = _List_Cons(_W3_rint(), _List_Nil); t.b = c; t = c; } }
  var f_s = h;
  var f_t = _W3_rint();
  var f_y = _W3_rstr();
  var f_z; switch (_W3_ru8()) { case 0: f_z = $elm$core$Maybe$Nothing; break; case 1: f_z = $elm$core$Maybe$Just(_W3_rstr()); break; default: throw 0; }
  var f_D = _W3_rf64();
  n = _W3_rint(); h = _List_Nil;
  if (n > 0) { t = h = _List_Cons(_W3_rstr(), _List_Nil); for (n--; n > 0; n--) { c = _List_Cons(_W3_rstr(), _List_Nil); t.b = c; t = c; } }
  return { {{active}}: f_o, {{counts}}: f_s, {{id}}: f_t, {{name}}: f_y, {{note}}: f_z, {{score}}: f_D, {{tags}}: h };
}

function $author$project$Types$w3d_Shape() {
  switch (_W3_ru8()) {
    case 0: return $author$project$Types$Circle(_W3_rf64());
    case 1: return $author$project$Types$Empty;
    case 2:
      var a = _W3_rstr(), b;
      switch (_W3_ru8()) { case 0: b = $elm$core$Maybe$Nothing; break; case 1: b = $elm$core$Maybe$Just($author$project$Types$w3d_Shape()); break; default: throw 0; }
      return $author$project$Types$Labelled$(a, b);
    case 3: var a = _W3_rf64(); return $author$project$Types$Rect$(a, _W3_rf64());
    default: throw 0;
  }
}

function $author$project$Types$w3d_Id() {
  if (_W3_ru8() !== 0) throw 0;
  return _W3_rstr();                                                   // Unbox: the value is the String
}

var $author$project$Types$w3f_Color = [2, 1, 0];                      // wire tag -> decl index
function $author$project$Types$w3d_Color() {
  var t = _W3_ru8();
  if (t > 2) throw 0;
  return $author$project$Types$w3f_Color[t];
}

function $author$project$Types$w3d_Tree() {
  switch (_W3_ru8()) {
    case 0: return $author$project$Types$Leaf;
    case 1:
      var a = $author$project$Types$w3d_Tree(), b = _W3_rint(), c = _W3_rstr();
      return $author$project$Types$Node$(a, b, c, $author$project$Types$w3d_Tree());
    default: throw 0;
  }
}

function $author$project$Types$w3d_DB(d_a) {
  var n = _W3_rint(), ks = [], vs = [];
  for (; n > 0; n--) { ks.push(_W3_rstr()); vs.push(d_a()); }
  var d = _W3_dictFromPairs(ks, vs);
  return { {{db}}: d, {{seed}}: _W3_rint() };
}

function $author$project$Types$w3d_Model() {
  var n, h, t, c, d;
  n = _W3_rint(); h = _List_Nil;                                        // arr : Array Int
  if (n > 0) { t = h = _List_Cons(_W3_rint(), _List_Nil); for (n--; n > 0; n--) { c = _List_Cons(_W3_rint(), _List_Nil); t.b = c; t = c; } }
  var f_p = $elm$core$Array$fromList(h);
  var f_q = _Char_fromCode(_W3_rint());                                // ch : Char
  n = _W3_rint(); h = _List_Nil;                                        // colors : List Color
  if (n > 0) { t = h = _List_Cons($author$project$Types$w3d_Color(), _List_Nil); for (n--; n > 0; n--) { c = _List_Cons($author$project$Types$w3d_Color(), _List_Nil); t.b = c; t = c; } }
  var f_r = h;
  var f_h = $author$project$Types$w3d_DB($author$project$Types$w3d_Rec);   // db : DB Rec
  n = _W3_rint(); h = _List_Nil;                                        // idList : List Id
  if (n > 0) { t = h = _List_Cons($author$project$Types$w3d_Id(), _List_Nil); for (n--; n > 0; n--) { c = _List_Cons($author$project$Types$w3d_Id(), _List_Nil); t.b = c; t = c; } }
  var f_u = h;
  n = _W3_rint(); var ks = [], vs = [];                                // ids : Set String
  for (; n > 0; n--) { ks.push(_W3_rstr()); vs.push(_Utils_Tuple0); }
  var f_v = _W3_dictFromPairs(ks, vs);                                  // prod: Set is the unboxed Dict
  n = _W3_rint(); h = _List_Nil;                                        // ints : List Int
  if (n > 0) { t = h = _List_Cons(_W3_rint(), _List_Nil); for (n--; n > 0; n--) { c = _List_Cons(_W3_rint(), _List_Nil); t.b = c; t = c; } }
  var f_x = h;
  var p0 = _W3_rint();                                                  // pair : (Int, String)
  var f_A = _Utils_Tuple2(p0, _W3_rstr());
  n = _W3_rint(); ks = []; vs = [];                                      // recs : Dict String Rec
  for (; n > 0; n--) { ks.push(_W3_rstr()); vs.push($author$project$Types$w3d_Rec()); }
  var f_B = _W3_dictFromPairs(ks, vs);
  var f_C;                                                              // res : Result String Int
  switch (_W3_ru8()) { case 0: f_C = $elm$core$Result$Ok(_W3_rint()); break; case 1: f_C = $elm$core$Result$Err(_W3_rstr()); break; default: throw 0; }
  n = _W3_rint(); h = _List_Nil;                                        // shapes : List Shape
  if (n > 0) { t = h = _List_Cons($author$project$Types$w3d_Shape(), _List_Nil); for (n--; n > 0; n--) { c = _List_Cons($author$project$Types$w3d_Shape(), _List_Nil); t.b = c; t = c; } }
  var f_F = h;
  var f_I = $author$project$Types$w3d_Tree();                           // tree : Tree
  return { {{arr}}: f_p, {{ch}}: f_q, {{colors}}: f_r, {{db}}: f_h, {{idList}}: f_u, {{ids}}: f_v, {{ints}}: f_x, {{pair}}: f_A, {{recs}}: f_B, {{res}}: f_C, {{shapes}}: f_F, {{tree}}: f_I, {{unit}}: _Utils_Tuple0 };
}

// Shared helpers the generator would reference (monomorphic, no callbacks).
function _W3_dictSize(d) {
  var n = 0, st = [], node = d;
  for (;;) {
    while (node.$ === -1) { st.push(node); node = node.d; }
    if (st.length === 0) return n;
    node = st.pop(); n++; node = node.e;
  }
}
function _W3_arrayToJs(arr) {
  var out = [];
  (function walk(tree) {
    for (var i = 0; i < tree.length; i++) {
      var nd = tree[i];
      if (nd.$ === 0) walk(nd.a); else { var leaf = nd.a; for (var j = 0; j < leaf.length; j++) out.push(leaf[j]); }
    }
  })(arr.c);
  var tail = arr.d;
  for (var k = 0; k < tail.length; k++) out.push(tail[k]);
  return out;
}

// ELM-FACING WRAPPERS (what w3_encode_Model / w3_decode_Model become)
var W3P_encode_Model = function (v) { return { $: 10, a: _W3_runEncode($author$project$Types$w3e_Model, v) }; };
W3P_encode_Model.w3e = $author$project$Types$w3e_Model;
var W3P_decode_Model = _W3_decoder($author$project$Types$w3d_Model);

scope.W3P = {
  gen: $author$project$Main$gen,
  edge: $author$project$Main$edge,
  baseEncode: $author$project$Main$encode,
  baseDecode: $author$project$Main$decode,
  fastEncodeRaw: function (m) { return _W3_runEncode($author$project$Types$w3e_Model, m); },
  fastEncode: function (m) { return $lamdera$codecs$Lamdera$Wire3$bytesEncode(W3P_encode_Model(m)); },
  fastDecode: function (b) { return A2($lamdera$codecs$Lamdera$Wire3$bytesDecode, W3P_decode_Model, b); },
  eq: _Utils_eq,
  cmp: _Utils_cmp,
  F: { recs: '{{recs}}', ids: '{{ids}}', db: '{{db}}' },
  Just: $elm$core$Maybe$Just
};
