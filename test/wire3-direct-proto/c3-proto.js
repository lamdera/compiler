// ---- C3 stand-in: compact descriptors + one generic interpreter (no per-type JS) ----
var D_INT = 0, D_F64 = 1, D_STR = 2, D_BOOL = 3, D_LIST = 4, D_MAYBE = 5, D_DICT = 6, D_REC = 7;
function C3_enc(d, v) {
  switch (d.k) {
    case D_INT: _W3_int(v); return;
    case D_F64: _W3_f64(v); return;
    case D_STR: _W3_str(v); return;
    case D_BOOL: _W3_u8(v ? 1 : 0); return;
    case D_LIST:
      var n = 0; for (var c = v; c.b; c = c.b) n++;
      _W3_int(n);
      for (var c = v; c.b; c = c.b) C3_enc(d.e, c.a);
      return;
    case D_MAYBE: if (v.$ === 1) _W3_u8(0); else { _W3_u8(1); C3_enc(d.e, v.a); } return;
    case D_DICT:
      _W3_int(_W3_dictSize(v));
      var st = [], node = v;
      for (;;) { while (node.$ === -1) { st.push(node); node = node.d; } if (!st.length) break; node = st.pop(); C3_enc(d.k2, node.b); C3_enc(d.e, node.c); node = node.e; }
      return;
    case D_REC:
      var fs = d.f;
      for (var i = 0; i < fs.length; i++) C3_enc(fs[i].d, v[fs[i].n]);
      return;
  }
}
function C3_dec(d) {
  switch (d.k) {
    case D_INT: return _W3_rint();
    case D_F64: return _W3_rf64();
    case D_STR: return _W3_rstr();
    case D_BOOL: var b = _W3_ru8(); if (b > 1) throw 0; return b === 1;
    case D_LIST:
      var h = _List_Nil, t = null;
      for (var n = _W3_rint(); n > 0; n--) { var c = _List_Cons(C3_dec(d.e), _List_Nil); if (t === null) h = c; else t.b = c; t = c; }
      return h;
    case D_MAYBE: var tag = _W3_ru8(); if (tag === 0) return $elm$core$Maybe$Nothing; if (tag !== 1) throw 0; return $elm$core$Maybe$Just(C3_dec(d.e));
    case D_DICT:
      var ks = [], vs = [];
      for (var n = _W3_rint(); n > 0; n--) { ks.push(C3_dec(d.k2)); vs.push(C3_dec(d.e)); }
      return _W3_dictFromPairs(ks, vs);
    case D_REC:
      var fs = d.f, args = new Array(fs.length);
      for (var i = 0; i < fs.length; i++) args[i] = C3_dec(fs[i].d);
      return d.ctor.apply(null, args); // the record-constructor lambda: same hidden class as Elm literals
  }
}
var C3_REC = { k: D_REC, f: [
  { n: '{{active}}', d: { k: D_BOOL } }, { n: '{{counts}}', d: { k: D_LIST, e: { k: D_INT } } }, { n: '{{id}}', d: { k: D_INT } },
  { n: '{{name}}', d: { k: D_STR } }, { n: '{{note}}', d: { k: D_MAYBE, e: { k: D_STR } } }, { n: '{{score}}', d: { k: D_F64 } },
  { n: '{{tags}}', d: { k: D_LIST, e: { k: D_STR } } } ],
  ctor: function (active0, counts0, id0, name0, note0, score0, tags0) {
    return { {{active}}: active0, {{counts}}: counts0, {{id}}: id0, {{name}}: name0, {{note}}: note0, {{score}}: score0, {{tags}}: tags0 };
  } };
var C3_RECS = { k: D_DICT, k2: { k: D_STR }, e: C3_REC };
var RECS_c3 = _W3_decoder(function () { return C3_dec(C3_RECS); });
scope.RECS.c3 = function (b) { return A2($lamdera$codecs$Lamdera$Wire3$bytesDecode, RECS_c3, b); };
scope.RECS.encC2 = scope.RECS.encode;
scope.RECS.encC3 = function (m) { return _W3_runEncode(function (v) { C3_enc(C3_RECS, v.{{recs}}); }, m); };
scope.RECS.encElm = function (m) { return $lamdera$codecs$Lamdera$Wire3$bytesEncode($lamdera$codecs$Lamdera$Wire3$encodeDict$($lamdera$codecs$Lamdera$Wire3$encodeString, $author$project$Types$w3_encode_Rec, m.{{recs}})); };
