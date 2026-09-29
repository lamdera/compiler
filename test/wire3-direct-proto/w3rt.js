// ---- Wire3 direct codec runtime (C2 prototype) ----
// Lives inside the Elm IIFE (it references kernel/Elm globals). Mode-agnostic:
// everything representation-specific is in the generated per-type code.
//
// Every primitive below is a transcription of the lamdera/codecs + elm/bytes
// behaviour it replaces, including the odd corners (fractional Ints, NaN, lone
// surrogates, truncated UTF-8, negative lengths), so bytes and failures match.

// ENCODER STATE: one growable buffer, reused across encodes.
var _W3_buf = new Uint8Array(1 << 16);
var _W3_dv = new DataView(_W3_buf.buffer);
var _W3_pos = 0;
var _W3_active = false;

function _W3_grow(n) {
  var need = _W3_pos + n;
  if (need <= _W3_buf.length) return;
  var cap = _W3_buf.length * 2;
  while (cap < need) cap *= 2;
  var nb = new Uint8Array(cap);
  nb.set(_W3_buf.subarray(0, _W3_pos));
  _W3_buf = nb;
  _W3_dv = new DataView(nb.buffer);
}

function _W3_u8(n) {
  if (_W3_pos >= _W3_buf.length) _W3_grow(1);
  _W3_buf[_W3_pos++] = n; // ToUint8, same conversion as DataView.setUint8
}

function _W3_u16(n) {
  _W3_grow(2);
  _W3_dv.setUint16(_W3_pos, n, true);
  _W3_pos += 2;
}

function _W3_f64(f) {
  _W3_grow(8);
  _W3_dv.setFloat64(_W3_pos, f, true);
  _W3_pos += 8;
}

// Lamdera.Wire3.intDivBy / modBy, exactly (Ints can be fractional in Elm JS).
function _W3_intDivBy(b, a) {
  var v = a / b;
  return v < 0 ? -Math.floor(-v) : Math.floor(v);
}
function _W3_modBy(modulus, x) {
  var answer = x % modulus;
  return ((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0)) ? answer + modulus : answer;
}

// Lamdera.Wire3.encodeInt64
function _W3_int(i) {
  var n = i < 0 ? -2 * i - 1 : 2 * i;
  if (n <= 215) { _W3_u8(n); return; }
  _W3_grow(9);
  var b = _W3_buf;
  if ((n >>> 0) === n) {
    // Fast path: n is an integer in [216, 2^32), where modBy 256 == & 255 and
    // intDivBy 256 == >>> 8 exactly. Everything else takes the transcription below.
    if (n <= 9431) { var m = n - 216; b[_W3_pos++] = 216 + (m >>> 8); b[_W3_pos++] = m & 255; }
    else if (n < 65536) { b[_W3_pos++] = 252; b[_W3_pos++] = n >>> 8; b[_W3_pos++] = n & 255; }
    else if (n < 16777216) { b[_W3_pos++] = 253; b[_W3_pos++] = n >>> 16; b[_W3_pos++] = (n >>> 8) & 255; b[_W3_pos++] = n & 255; }
    else { b[_W3_pos++] = 254; b[_W3_pos++] = n >>> 24; b[_W3_pos++] = (n >>> 16) & 255; b[_W3_pos++] = (n >>> 8) & 255; b[_W3_pos++] = n & 255; }
    return;
  }
  if (n <= 9431) {
    b[_W3_pos++] = 216 + _W3_intDivBy(256, n - 216);
    b[_W3_pos++] = _W3_modBy(256, n - 216);
  } else if (n < 65536) {
    b[_W3_pos++] = 252;
    b[_W3_pos++] = _W3_modBy(256, _W3_intDivBy(256, n));
    b[_W3_pos++] = _W3_modBy(256, n);
  } else if (n < 16777216) {
    b[_W3_pos++] = 253;
    b[_W3_pos++] = _W3_modBy(256, _W3_intDivBy(256, _W3_intDivBy(256, n)));
    b[_W3_pos++] = _W3_modBy(256, _W3_intDivBy(256, n));
    b[_W3_pos++] = _W3_modBy(256, n);
  } else if (n < 4294967296) {
    b[_W3_pos++] = 254;
    b[_W3_pos++] = _W3_modBy(256, _W3_intDivBy(256, _W3_intDivBy(256, _W3_intDivBy(256, n))));
    b[_W3_pos++] = _W3_modBy(256, _W3_intDivBy(256, _W3_intDivBy(256, n)));
    b[_W3_pos++] = _W3_modBy(256, _W3_intDivBy(256, n));
    b[_W3_pos++] = _W3_modBy(256, n);
  } else {
    b[_W3_pos++] = 255;
    _W3_dv.setFloat64(_W3_pos, i, true);
    _W3_pos += 8;
  }
}

// Lamdera.Wire3.encodeString: varint(getStringWidth s) then elm/bytes' own UTF-8
// writer (NOT TextEncoder: that differs on lone surrogates).
function _W3_str(s) {
  var len = s.length, width = 0, ascii = true;
  for (var i = 0; i < len; i++) {
    var code = s.charCodeAt(i);
    if (code < 0x80) { width += 1; continue; }
    ascii = false;
    width += (code < 0x800) ? 2 : (code < 0xD800 || 0xDBFF < code) ? 3 : (i++, 4);
  }
  _W3_int(width);
  _W3_grow(width);
  var b = _W3_buf, p = _W3_pos;
  if (ascii) {
    for (var j = 0; j < len; j++) b[p + j] = s.charCodeAt(j);
    _W3_pos = p + len;
    return;
  }
  // Transcription of _Bytes_write_string
  var mb = _W3_dv, offset = p;
  for (var i = 0; i < len; i++) {
    var code = s.charCodeAt(i);
    offset +=
      (code < 0x80)
        ? (mb.setUint8(offset, code), 1)
        :
      (code < 0x800)
        ? (mb.setUint16(offset, 0xC080 | (code >>> 6 & 0x1F) << 8 | code & 0x3F), 2)
        :
      (code < 0xD800 || 0xDBFF < code)
        ? (mb.setUint16(offset, 0xE080 | (code >>> 12 & 0xF) << 8 | code >>> 6 & 0x3F),
           mb.setUint8(offset + 2, 0x80 | code & 0x3F), 3)
        :
      (code = (code - 0xD800) * 0x400 + s.charCodeAt(++i) - 0xDC00 + 0x10000,
       mb.setUint32(offset, 0xF0808080 | (code >>> 18 & 0x7) << 24 | (code >>> 12 & 0x3F) << 16 | (code >>> 6 & 0x3F) << 8 | code & 0x3F),
       4);
  }
  _W3_pos = offset;
}

// Lamdera.Wire3.encodeBytes
function _W3_bytes(bs) {
  var len = bs.byteLength;
  _W3_int(len);
  _W3_grow(len);
  _W3_buf.set(new Uint8Array(bs.buffer, bs.byteOffset, len), _W3_pos);
  _W3_pos += len;
}

// Run a direct encoder to a fresh, exactly-sized DataView. Re-entrant: a nested
// run (an Elm-level encoder called back from a bridge) gets its own buffer.
function _W3_runEncode(f, v, extra) {
  var sbuf = _W3_buf, sdv = _W3_dv, spos = _W3_pos, sactive = _W3_active;
  if (sactive) { _W3_buf = new Uint8Array(256); _W3_dv = new DataView(_W3_buf.buffer); }
  _W3_pos = 0;
  _W3_active = true;
  try {
    f(v, extra);
    var out = _W3_buf.slice(0, _W3_pos);
    if (!sactive && _W3_buf.length > (1 << 20)) { _W3_buf = new Uint8Array(1 << 16); _W3_dv = new DataView(_W3_buf.buffer); }
    return new DataView(out.buffer);
  } finally {
    if (sactive) { _W3_buf = sbuf; _W3_dv = sdv; }
    _W3_pos = spos;
    _W3_active = sactive;
  }
}

// DECODER STATE
var _W3_rdv = null;
var _W3_rpos = 0;

function _W3_ru8() {
  var v = _W3_rdv.getUint8(_W3_rpos); // throws RangeError past the end, like elm/bytes
  _W3_rpos += 1;
  return v;
}

function _W3_ru16() {
  var v = _W3_rdv.getUint16(_W3_rpos, true);
  _W3_rpos += 2;
  return v;
}

function _W3_rf64() {
  var v = _W3_rdv.getFloat64(_W3_rpos, true);
  _W3_rpos += 8;
  return v;
}

// Lamdera.Wire3.unsignedToSigned. Only ever called with an integer in [0, 2^32),
// where modBy 2 / intDivBy reduce exactly to these.
function _W3_u2s(i) {
  return (i % 2 === 1) ? -((i + 1) / 2) : i / 2;
}

// Lamdera.Wire3.decodeInt64
function _W3_rint() {
  var dv = _W3_rdv;
  var n0 = dv.getUint8(_W3_rpos); _W3_rpos += 1;
  if (n0 <= 215) return _W3_u2s(n0);
  if (n0 < 252) {
    var b0 = dv.getUint8(_W3_rpos); _W3_rpos += 1;
    return _W3_u2s((n0 - 216) * 256 + b0 + 216);
  }
  if (n0 === 252) {
    var b0 = dv.getUint8(_W3_rpos), b1 = dv.getUint8(_W3_rpos + 1); _W3_rpos += 2;
    return _W3_u2s(b0 * 256 + b1);
  }
  if (n0 === 253) {
    var b0 = dv.getUint8(_W3_rpos), b1 = dv.getUint8(_W3_rpos + 1), b2 = dv.getUint8(_W3_rpos + 2); _W3_rpos += 3;
    return _W3_u2s((b0 * 256 + b1) * 256 + b2);
  }
  if (n0 === 254) {
    var b0 = dv.getUint8(_W3_rpos), b1 = dv.getUint8(_W3_rpos + 1), b2 = dv.getUint8(_W3_rpos + 2), b3 = dv.getUint8(_W3_rpos + 3); _W3_rpos += 4;
    return _W3_u2s(((b0 * 256 + b1) * 256 + b2) * 256 + b3);
  }
  var f = dv.getFloat64(_W3_rpos, true); _W3_rpos += 8;
  if (f !== f || f === Infinity || f === -Infinity) throw 0;
  return Math.floor(f);
}

var _W3_utf8 = typeof TextDecoder !== 'undefined' ? new TextDecoder('utf-8', { fatal: true, ignoreBOM: true }) : null;

// Lamdera.Wire3.decodeString = decodeInt64 |> andThen D.string
function _W3_rstr() {
  var len = _W3_rint();
  var dv = _W3_rdv, offset = _W3_rpos, end = offset + len;
  if (len <= 0) return ''; // elm/bytes: loop never runs, offset unchanged
  if (end <= dv.byteLength) {
    if (len < 24) {
      var s = '', ok = true;
      for (var i = offset; i < end; i++) {
        var c = dv.getUint8(i);
        if (c >= 128) { ok = false; break; }
        s += String.fromCharCode(c);
      }
      if (ok) { _W3_rpos = end; return s; }
    } else if (_W3_utf8 !== null) {
      // Valid UTF-8 decodes identically; anything TextDecoder rejects (overlong,
      // surrogates, truncation) takes the exact elm/bytes path below.
      try {
        var s = _W3_utf8.decode(new Uint8Array(dv.buffer, dv.byteOffset + offset, len));
        _W3_rpos = end;
        return s;
      } catch (e) {}
    }
  }
  // Transcription of _Bytes_read_string (may read past `end`, may throw)
  var bytes = dv, string = '';
  for (; offset < end;) {
    var byte = bytes.getUint8(offset++);
    string +=
      (byte < 128)
        ? String.fromCharCode(byte)
        :
      ((byte & 0xE0) === 0xC0)
        ? String.fromCharCode((byte & 0x1F) << 6 | bytes.getUint8(offset++) & 0x3F)
        :
      ((byte & 0xF0) === 0xE0)
        ? String.fromCharCode((byte & 0xF) << 12 | (bytes.getUint8(offset++) & 0x3F) << 6 | bytes.getUint8(offset++) & 0x3F)
        :
        (byte = ((byte & 0x7) << 18 | (bytes.getUint8(offset++) & 0x3F) << 12 | (bytes.getUint8(offset++) & 0x3F) << 6 | bytes.getUint8(offset++) & 0x3F) - 0x10000,
         String.fromCharCode(Math.floor(byte / 0x400) + 0xD800, byte % 0x400 + 0xDC00));
  }
  _W3_rpos = offset;
  return string;
}

// Lamdera.Wire3.decodeBytes: a view into the input, as elm/bytes does
function _W3_rbytes() {
  var len = _W3_rint();
  var v = new DataView(_W3_rdv.buffer, _W3_rdv.byteOffset + _W3_rpos, len);
  _W3_rpos += len;
  return v;
}

// Build an Elm List front to back without an intermediate array: cells are
// mutated only before the list is visible to Elm.
// (usage is inlined by the generator; see w3d_* below)

// Wrap a direct decoder as an elm/bytes Decoder (Bytes -> Int -> (Int, a)).
function _W3_decoder(d) {
  var fn = F2(function (bytes, offset) {
    var sdv = _W3_rdv, spos = _W3_rpos;
    _W3_rdv = bytes; _W3_rpos = offset;
    try {
      var v = d();
      return _Utils_Tuple2(_W3_rpos, v);
    } finally {
      _W3_rdv = sdv; _W3_rpos = spos;
    }
  });
  fn.w3d = d;
  return fn;
}

// Dict from decoded pairs. Wire3 always writes dicts in ascending key order, so
// when the keys are strictly ascending we build a valid left-leaning red-black
// tree bottom-up in O(n) with no garbage (a 2-3 tree, 3-nodes as a black node with
// a red left child). Otherwise, Dict.fromList semantics exactly: insert in order,
// later duplicates win. The shape can differ from fromList's, which nothing in
// Elm can observe (==, toList, Debug.toString all go through toList).
function _W3_dictFromPairs(ks, vs) {
  var n = ks.length;
  for (var i = 1; i < n; i++) {
    if (_Utils_cmp(ks[i - 1], ks[i]) >= 0) {
      var d = $elm$core$Dict$empty;
      for (var j = 0; j < n; j++) d = $elm$core$Dict$insert$(ks[j], vs[j], d);
      return d;
    }
  }
  if (n === 0) return $elm$core$Dict$empty;
  var h = Math.floor(Math.log2(n + 1));
  while (Math.pow(2, h + 1) - 1 <= n) h++;   // guard against log2 rounding
  while (Math.pow(2, h) - 1 > n) h--;
  return _W3_build23(ks, vs, 0, n, h);
}

var _W3_pow3 = [];
function _W3_max23(h) { // max keys in a 2-3 tree of height h
  return _W3_pow3[h] !== undefined ? _W3_pow3[h] : (_W3_pow3[h] = Math.pow(3, h) - 1);
}

function _W3_build23(ks, vs, lo, s, h) {
  if (s === 0) return $elm$core$Dict$empty;
  var cmax = _W3_max23(h - 1);
  if (s - 1 <= 2 * cmax) {
    var ls = (s - 1) >>> 1, m = lo + ls;
    return $elm$core$Dict$RBNode_elm_builtin$($elm$core$Dict$Black, ks[m], vs[m],
      _W3_build23(ks, vs, lo, ls, h - 1),
      _W3_build23(ks, vs, m + 1, s - 1 - ls, h - 1));
  }
  var r = s - 2, a = Math.floor(r / 3), b = Math.floor((r - a) / 2), c = r - a - b;
  var k1 = lo + a, k2 = k1 + 1 + b;
  var red = $elm$core$Dict$RBNode_elm_builtin$($elm$core$Dict$Red, ks[k1], vs[k1],
    _W3_build23(ks, vs, lo, a, h - 1),
    _W3_build23(ks, vs, k1 + 1, b, h - 1));
  return $elm$core$Dict$RBNode_elm_builtin$($elm$core$Dict$Black, ks[k2], vs[k2], red,
    _W3_build23(ks, vs, k2 + 1, c, h - 1));
}
