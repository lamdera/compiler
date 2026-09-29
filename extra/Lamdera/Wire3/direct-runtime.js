// ---- Wire3 direct codec runtime (see Lamdera/Wire3/Direct.hs) ----
// Emitted once as a kernel node inside the Elm IIFE, after a small mode-specific
// prelude Direct.hs generates (_W3_bytesNode, _W3_decoderValue, _W3_decoderFnOf,
// _W3_T_RBNODE, _W3_T_SUBTREE). It references kernel and Elm globals only inside
// function bodies, because kernel code runs before any Elm definition.
//
// Every primitive below is a transcription of the lamdera/codecs + elm/bytes
// behaviour it replaces, including the odd corners (fractional Ints, NaN, lone
// surrogates, truncated UTF-8, negative lengths), so bytes and failures match.

// PLATFORM FEATURES (Node's Buffer when present, else the browser's TextDecoder/
// TextEncoder, else the exact kernel loops). Every fast path is only taken when it is
// provably identical to the kernel; see the string functions.
var _W3_hasBuffer = typeof Buffer === 'function' && !!Buffer.prototype
  && typeof Buffer.prototype.utf8Slice === 'function' && typeof Buffer.prototype.utf8Write === 'function';
var _W3_utf8Slice = _W3_hasBuffer ? Buffer.prototype.utf8Slice : null;
var _W3_utf8Write = _W3_hasBuffer ? Buffer.prototype.utf8Write : null;
var _W3_byteLength = _W3_hasBuffer ? Buffer.byteLength : null;
var _W3_tdLoose = typeof TextDecoder !== 'undefined' ? new TextDecoder('utf-8', { ignoreBOM: true }) : null;
var _W3_tdFatal = typeof TextDecoder !== 'undefined' ? new TextDecoder('utf-8', { fatal: true, ignoreBOM: true }) : null;
var _W3_te = typeof TextEncoder !== 'undefined' ? new TextEncoder() : null;
var _W3_hasWellFormed = typeof ''.isWellFormed === 'function';

// WRITER
//
// One writer at a time: a byte buffer, a DataView over it, a position, and a flush
// callback. With _W3_flush === null the buffer grows (doubling) and the result is one
// Bytes value (_W3_runEncode). In streaming mode (_W3_streamEncode) the buffer is a
// fixed chunk, and whenever the next write would not fit, the filled part is handed to
// _W3_flush and the position resets. A single item larger than the chunk (a huge
// string) temporarily gets a bigger buffer.
var _W3_buf = new Uint8Array(1 << 16);
var _W3_dv = new DataView(_W3_buf.buffer);
var _W3_pos = 0;
var _W3_flush = null;
var _W3_active = false;

function _W3_grow(n) {
  var need = _W3_pos + n;
  if (need <= _W3_buf.length) return;
  if (_W3_flush !== null && _W3_pos > 0) {
    _W3_flush(_W3_buf.subarray(0, _W3_pos));
    _W3_pos = 0;
    if (n <= _W3_buf.length) return;
    need = n;
  }
  var cap = _W3_buf.length * 2;
  while (cap < need) cap *= 2;
  var nb = new Uint8Array(cap);
  if (_W3_pos > 0) nb.set(_W3_buf.subarray(0, _W3_pos));
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

// Lamdera.Wire3.encodeString: varint(getStringWidth s) then the UTF-8 bytes.
// A well-formed string (no lone surrogates) encodes identically through Buffer#utf8Write
// or TextEncoder, and Buffer.byteLength equals the kernel's width for it. Anything else
// (lone surrogates, which elm/bytes writes as WTF-8 or as a 4-byte sequence swallowing
// the next char) goes through the exact kernel loop.
function _W3_str(s) {
  var len = s.length;
  if (len > 4 && _W3_utf8Write !== null && _W3_hasWellFormed && s.isWellFormed()) {
    var w = _W3_byteLength(s, 'utf8');
    _W3_int(w);
    _W3_grow(w);
    _W3_utf8Write.call(_W3_buf, s, _W3_pos, w);
    _W3_pos += w;
    return;
  }
  var width = 0, ascii = true;
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
  if (len > 32 && _W3_te !== null && _W3_hasWellFormed && s.isWellFormed()) {
    // Browser: no Buffer. The kernel width above is exact for well-formed strings.
    _W3_te.encodeInto(s, b.subarray(p, p + width));
    _W3_pos = p + width;
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

// Run a direct encoder to one fresh, exactly-sized DataView. Re-entrant: a nested run
// (an Elm-level encoder called back from a bridge) gets its own buffer.
function _W3_runEncode(f, v) {
  var sbuf = _W3_buf, sdv = _W3_dv, spos = _W3_pos, sflush = _W3_flush, sactive = _W3_active;
  if (sactive) { _W3_buf = new Uint8Array(256); _W3_dv = new DataView(_W3_buf.buffer); }
  _W3_pos = 0;
  _W3_flush = null;
  _W3_active = true;
  try {
    f(v);
    if (_W3_pos > (1 << 20)) {
      // Big result: hand the grown buffer over as the result (a view of exactly the
      // written bytes, as elm/bytes Bytes values may be) instead of copying it, and start
      // the next encode from a small fresh buffer. At most half of it is unused.
      var result = new DataView(_W3_buf.buffer, 0, _W3_pos);
      _W3_buf = new Uint8Array(1 << 16); _W3_dv = new DataView(_W3_buf.buffer);
      return result;
    }
    return new DataView(_W3_buf.slice(0, _W3_pos).buffer);
  } finally {
    if (sactive) { _W3_buf = sbuf; _W3_dv = sdv; }
    // Never keep a big buffer between encodes (also when an encode threw halfway).
    else if (_W3_buf.length > (1 << 20)) { _W3_buf = new Uint8Array(1 << 16); _W3_dv = new DataView(_W3_buf.buffer); }
    _W3_pos = spos;
    _W3_flush = sflush;
    _W3_active = sactive;
  }
}

// Streaming mode: encode `v` with direct encoder `f` through a reusable chunk of
// `chunkSize` bytes, calling `onChunk(u8)` for each filled chunk and once at the end
// for the remainder (never with an empty array). `u8` is a view into the reused chunk:
// it is only valid during the call, so consume it synchronously (fs.writeSync) or copy
// it. The bytes, concatenated, are exactly what _W3_runEncode would return. Returns
// the total byte count.
function _W3_streamEncode(f, v, chunkSize, onChunk) {
  var sbuf = _W3_buf, sdv = _W3_dv, spos = _W3_pos, sflush = _W3_flush, sactive = _W3_active;
  var total = 0;
  var flush = function (u8) { total += u8.length; onChunk(u8); };
  _W3_buf = new Uint8Array(chunkSize > 16 ? chunkSize : 16);
  _W3_dv = new DataView(_W3_buf.buffer);
  _W3_pos = 0;
  _W3_flush = flush;
  _W3_active = true;
  try {
    f(v);
    if (_W3_pos > 0) flush(_W3_buf.subarray(0, _W3_pos));
    return total;
  } finally {
    _W3_buf = sbuf; _W3_dv = sdv; _W3_pos = spos; _W3_flush = sflush; _W3_active = sactive;
  }
}

// READER STATE
var _W3_rdv = null;
var _W3_rpos = 0;
var _W3_ru8a = null;   // Uint8Array over _W3_rdv, made lazily for string decoding
var _W3_ru8aFor = null;

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

// Lamdera.Wire3.unsignedToSigned. Only ever called with an integer in [0, 2^32), where
// modBy 2 / intDivBy reduce exactly to these, and the result always fits an int32:
// `| 0` keeps it a small int instead of a heap number.
function _W3_u2s(i) {
  return ((i % 2 === 1) ? -((i + 1) / 2) : i / 2) | 0;
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

function _W3_u8view() {
  if (_W3_ru8aFor !== _W3_rdv) {
    _W3_ru8a = new Uint8Array(_W3_rdv.buffer, _W3_rdv.byteOffset, _W3_rdv.byteLength);
    _W3_ru8aFor = _W3_rdv;
  }
  return _W3_ru8a;
}

// Lamdera.Wire3.decodeString = decodeInt64 |> andThen D.string
//
// Exactly equivalent to elm/bytes 1.0.8 (measured in a native spike outside this repo):
// - up to 8 bytes of ASCII: a char loop (fastest there, and V8 keeps results under 13
//   chars flat rather than as cons strings);
// - otherwise Buffer#utf8Slice (or a non-fatal TextDecoder in browsers). Both replace
//   every ill-formed sequence with U+FFFD, so an output without U+FFFD proves the input
//   was well-formed, and then the kernel would produce the same string and offset. If
//   there is a U+FFFD, a fatal decode decides whether the input was valid (a literal
//   U+FFFD) or not;
// - everything else (invalid UTF-8, a length past the end) runs the kernel loop itself,
//   which keeps its lone surrogates, CESU-8 pairs, reads past the declared length and
//   RangeError-to-Nothing behaviour bit for bit.
function _W3_rstr() {
  var len = _W3_rint();
  var dv = _W3_rdv, offset = _W3_rpos, end = offset + len;
  if (len <= 0) return ''; // elm/bytes: the loop never runs, offset unchanged
  if (end <= dv.byteLength) {
    if (len <= 8) {
      var s = '', i = offset;
      for (; i < end; i++) {
        var c = dv.getUint8(i);
        if (c >= 128) break;
        s += String.fromCharCode(c);
      }
      if (i === end) { _W3_rpos = end; return s; }
    } else {
      var u8 = _W3_u8view();
      var t = _W3_utf8Slice !== null ? _W3_utf8Slice.call(u8, offset, end)
        : _W3_tdLoose !== null ? _W3_tdLoose.decode(u8.subarray(offset, end))
        : null;
      if (t !== null) {
        if (t.indexOf('\uFFFD') < 0 || _W3_validUtf8(u8, offset, end)) { _W3_rpos = end; return t; }
      }
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

function _W3_validUtf8(u8, start, end) {
  if (_W3_tdFatal === null) return false;
  try { _W3_tdFatal.decode(u8.subarray(start, end)); return true; } catch (e) { return false; }
}

// Lamdera.Wire3.decodeBytes: same bounds and failures as elm/bytes (the view is checked
// against the whole underlying buffer, and a negative length throws), but the result is
// COPIED out of the input. elm/bytes returns a view, which would keep the entire input
// (a whole snapshot) alive for as long as any decoded Bytes value lives. Bytes are
// immutable in Elm, so the copy is not observable; it costs one memcpy per Bytes value.
function _W3_rbytes() {
  var len = _W3_rint();
  var view = new DataView(_W3_rdv.buffer, _W3_rdv.byteOffset + _W3_rpos, len);
  _W3_rpos += len;
  return new DataView(_W3_rdv.buffer.slice(view.byteOffset, view.byteOffset + len));
}

// The function inside an elm/bytes Decoder (Bytes -> Int -> (Int, a)) for a direct
// decoder, tagged with `.w3d` so other direct codecs can call the direct decoder.
function _W3_decoderFn(d) {
  var fn = F2(function (bytes, offset) {
    var sdv = _W3_rdv, spos = _W3_rpos;
    _W3_rdv = bytes; _W3_rpos = offset;
    try {
      var v = d();
      return _Utils_Tuple2(_W3_rpos, v);
    } finally {
      _W3_rdv = sdv; _W3_rpos = spos;
      if (sdv === null) {
        // The outermost decode is over: drop the cached view of its input, which would
        // otherwise keep the whole input (a whole snapshot) alive until the next decode.
        _W3_ru8a = null; _W3_ru8aFor = null;
      }
    }
  });
  fn.w3d = d;
  return fn;
}

// ELM-FACING WRAPPERS AND CONVERSIONS
//
// An Elm encoder `a -> Encoder` made from a direct encoder: returns an elm/bytes `Bytes`
// node holding the encoded value, tagged with `.w3e`.
function _W3_encWrap(f) {
  var g = function (v) { return _W3_bytesNode(_W3_runEncode(f, v)); };
  g.w3e = f;
  return g;
}
// An elm/bytes Decoder made from a direct decoder.
function _W3_decWrap(d) {
  return _W3_decoderValue(_W3_decoderFn(d));
}
// A direct encoder from an Elm encoder: its `.w3e` if it has one, else a bridge.
function _W3_toDirectEnc(f) {
  return f.w3e || function (v) { _W3_elmEnc(f(v)); };
}
// A direct decoder from an elm/bytes Decoder: its `.w3d` if it has one, else a bridge.
function _W3_toDirectDec(dec) {
  var fn = _W3_decoderFnOf(dec);
  return fn.w3d || function () { return _W3_elmDecFn(fn); };
}

// BRIDGES to Elm-level codecs, for types that have no direct codec.
function _W3_elmEnc(e) {
  var w = $elm$bytes$Bytes$Encode$getWidth(e);
  _W3_grow(w);
  _W3_pos = A3($elm$bytes$Bytes$Encode$write, e, _W3_dv, _W3_pos);
}
function _W3_elmDecFn(fn) {
  var t = A2(fn, _W3_rdv, _W3_rpos);
  _W3_rpos = t.a;
  return t.b;
}
function _W3_elmDec(dec) {
  return _W3_elmDecFn(_W3_decoderFnOf(dec));
}

// Number of entries in an Elm Dict.
function _W3_dictSize(d) {
  var n = 0, st = [], node = d;
  for (;;) {
    while (node.$ === _W3_T_RBNODE) { st.push(node); node = node.d; }
    if (st.length === 0) return n;
    node = st.pop(); n++; node = node.e;
  }
}

// Elements of an Elm Array in order.
function _W3_arrayToJs(arr) {
  var out = [];
  (function walk(tree) {
    for (var i = 0; i < tree.length; i++) {
      var nd = tree[i];
      if (nd.$ === _W3_T_SUBTREE) walk(nd.a);
      else { var leaf = nd.a; for (var j = 0; j < leaf.length; j++) out.push(leaf[j]); }
    }
  })(arr.c);
  var tail = arr.d;
  for (var k = 0; k < tail.length; k++) out.push(tail[k]);
  return out;
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
