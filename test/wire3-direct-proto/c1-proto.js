// ---- C1 stand-in: fused record decoders + kernel primitives, but still the elm/bytes
// Decoder protocol (every decoder is F2(bytes, offset) -> Tuple2(offset, value)) ----
var C1_int = F2(function (bytes, off) { var s = _W3_rdv, p = _W3_rpos; _W3_rdv = bytes; _W3_rpos = off; var v = _W3_rint(); var r = _Utils_Tuple2(_W3_rpos, v); _W3_rdv = s; _W3_rpos = p; return r; });
var C1_f64 = F2(function (bytes, off) { return _Utils_Tuple2(off + 8, bytes.getFloat64(off, true)); });
var C1_str = F2(function (bytes, off) { var s = _W3_rdv, p = _W3_rpos; _W3_rdv = bytes; _W3_rpos = off; var v = _W3_rstr(); var r = _Utils_Tuple2(_W3_rpos, v); _W3_rdv = s; _W3_rpos = p; return r; });
var C1_bool = F2(function (bytes, off) { var b = bytes.getUint8(off); if (b > 1) throw 0; return _Utils_Tuple2(off + 1, b === 1); });
function C1_list(d) {
  return F2(function (bytes, off) {
    var t = A2(C1_int, bytes, off), n = t.b; off = t.a;
    var h = _List_Nil, last = null;
    for (; n > 0; n--) { var x = A2(d, bytes, off); off = x.a; var c = _List_Cons(x.b, _List_Nil); if (last === null) h = c; else last.b = c; last = c; }
    return _Utils_Tuple2(off, h);
  });
}
function C1_maybe(d) {
  return F2(function (bytes, off) {
    var tag = bytes.getUint8(off);
    if (tag === 0) return _Utils_Tuple2(off + 1, $elm$core$Maybe$Nothing);
    if (tag !== 1) throw 0;
    var x = A2(d, bytes, off + 1);
    return _Utils_Tuple2(x.a, $elm$core$Maybe$Just(x.b));
  });
}
function C1_dict(kd, vd) {
  return F2(function (bytes, off) {
    var t = A2(C1_int, bytes, off), n = t.b; off = t.a;
    var ks = [], vs = [];
    for (; n > 0; n--) { var k = A2(kd, bytes, off); ks.push(k.b); var v = A2(vd, bytes, k.a); vs.push(v.b); off = v.a; }
    return _Utils_Tuple2(off, _W3_dictFromPairs(ks, vs));
  });
}
// decodeRecord7 ctor d1..d7: one closure, A2 per field, A7 on the record-constructor lambda
function C1_record7(ctor, d1, d2, d3, d4, d5, d6, d7) {
  return F2(function (bytes, off) {
    var a = A2(d1, bytes, off), b = A2(d2, bytes, a.a), c = A2(d3, bytes, b.a), d = A2(d4, bytes, c.a),
        e = A2(d5, bytes, d.a), f = A2(d6, bytes, e.a), g = A2(d7, bytes, f.a);
    return _Utils_Tuple2(g.a, A7(ctor, a.b, b.b, c.b, d.b, e.b, f.b, g.b));
  });
}
var C1_rec = C1_record7(
  F7(function (active0, counts0, id0, name0, note0, score0, tags0) {
    return { {{active}}: active0, {{counts}}: counts0, {{id}}: id0, {{name}}: name0, {{note}}: note0, {{score}}: score0, {{tags}}: tags0 };
  }),
  C1_bool, C1_list(C1_int), C1_int, C1_str, C1_maybe(C1_str), C1_f64, C1_list(C1_str));
var C1_recs = C1_dict(C1_str, C1_rec);

// The same payload (Dict String Rec) three ways
var RECS_elm = $lamdera$codecs$Lamdera$Wire3$decodeDict$($lamdera$codecs$Lamdera$Wire3$decodeString, $author$project$Types$w3_decode_Rec);
var RECS_direct = _W3_decoder(function () {
  var n = _W3_rint(), ks = [], vs = [];
  for (; n > 0; n--) { ks.push(_W3_rstr()); vs.push($author$project$Types$w3d_Rec()); }
  return _W3_dictFromPairs(ks, vs);
});
scope.RECS = {
  encode: function (m) {
    return _W3_runEncode(function (v) {
      var d = v.{{recs}};
      _W3_int(_W3_dictSize(d));
      var st = [], node = d;
      for (;;) { while (node.$ === -1) { st.push(node); node = node.d; } if (!st.length) break; node = st.pop(); _W3_str(node.b); $author$project$Types$w3e_Rec(node.c); node = node.e; }
    }, m);
  },
  elm: function (b) { return A2($lamdera$codecs$Lamdera$Wire3$bytesDecode, RECS_elm, b); },
  c1: function (b) { return A2($lamdera$codecs$Lamdera$Wire3$bytesDecode, C1_recs, b); },
  c2: function (b) { return A2($lamdera$codecs$Lamdera$Wire3$bytesDecode, RECS_direct, b); }
};
