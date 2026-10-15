// Differential fuzzer: Elm Wire3 codecs (base build) vs direct codecs (direct build),
// on random and mutated BYTES (decode outcome, consumed offset, value), and on values
// (encode bytes, streamed bytes), per mode.
//
//   node --stack-size=60000 fuzz.js [iterations-per-type] [seed] [opt|dev ...]
//   NO_BUFFER=1 / NO_TEXTCODEC=1 hide Node's Buffer / TextDecoder+TextEncoder from the
//   direct bundle while it loads (browser paths).
const args = process.argv.slice(2);
const ITER = +(args[0] || 3000);
const SEED = +(args[1] || 1);
const modes = args.slice(2).length ? args.slice(2) : ['opt', 'dev', 'leg']; // leg = --optimize-legible (what prod backends use)
const ONLY = process.env.ONLY ? process.env.ONLY.split(',') : null;
const NodeBuffer = Buffer;

function mulberry32(a) {
  return function () {
    a |= 0; a = (a + 0x6D2B79F5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

function load(path, hide) {
  const hidden = {};
  if (hide && process.env.NO_BUFFER) { hidden.Buffer = globalThis.Buffer; delete globalThis.Buffer; }
  if (hide && process.env.NO_TEXTCODEC) {
    hidden.TextDecoder = globalThis.TextDecoder; hidden.TextEncoder = globalThis.TextEncoder;
    delete globalThis.TextDecoder; delete globalThis.TextEncoder;
  }
  const scope = {};
  const src = require('fs').readFileSync(path, 'utf8');
  new Function('scope', src.replace(/\}\(this\)\);\s*$/, '}(scope));'))(scope);
  Object.assign(globalThis, hidden);
  return scope;
}

const bytesOf = dv => NodeBuffer.from(dv.buffer, dv.byteOffset, dv.byteLength);
const isNode = x => x.$ === -1 || x.$ === 'RBNode_elm_builtin';
const isDictish = x => x !== null && typeof x === 'object' && (x.$ === -1 || x.$ === -2 || x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin');
const dictWalk = (n, o) => { if (isNode(n)) { dictWalk(n.d, o); o.push(n.b, n.c); dictWalk(n.e, o); } return o; };

// NaN-aware, -0-aware, key-order-aware; Dicts compared by in-order contents. Iterates
// along the last key of an object (a list's tail) so long lists don't overflow.
function same(x, y) {
  for (;;) {
    if (typeof x === 'number') return Object.is(x, y);
    if (typeof x === 'function') return typeof y === 'function';
    if (typeof x !== 'object' || x === null) return x === y;
    if (x instanceof String) return y instanceof String && String(x) === String(y);
    if (isDictish(x)) {
      if (!isDictish(y)) return false;
      const ix = dictWalk(x, []), iy = dictWalk(y, []);
      return ix.length === iy.length && ix.every((v, i) => same(v, iy[i]));
    }
    if (x instanceof DataView) return y instanceof DataView && bytesOf(x).equals(bytesOf(y)) && x.byteLength === y.byteLength;
    if (Array.isArray(x)) return Array.isArray(y) && x.length === y.length && x.every((v, i) => same(v, y[i]));
    if (typeof y !== 'object' || y === null || y instanceof String || Array.isArray(y) || isDictish(y)) return false;
    const kx = Object.keys(x), ky = Object.keys(y);
    if (kx.join() !== ky.join()) return false;
    if (kx.length === 0) return true;
    for (let i = 0; i < kx.length - 1; i++) if (!same(x[kx[i]], y[kx[i]])) return false;
    const last = kx[kx.length - 1];
    x = x[last]; y = y[last];
  }
}

// elm/core _Utils_cmp, both modes
function cmp(x, y) {
  if (typeof x !== 'object') return x === y ? 0 : x < y ? -1 : 1;
  if (x instanceof String) { const a = x.valueOf(), b = y.valueOf(); return a === b ? 0 : a < b ? -1 : 1; }
  const tuple = typeof x.$ === 'undefined' || (typeof x.$ === 'string' && x.$[0] === '#');
  if (tuple) { let o; return (o = cmp(x.a, y.a)) ? o : (o = cmp(x.b, y.b)) ? o : cmp(x.c, y.c); }
  let o = 0;
  for (; x.b && y.b && !(o = cmp(x.a, y.a)); x = x.b, y = y.b) {}
  return o || (x.b ? 1 : y.b ? -1 : 0);
}

// Left-leaning red-black invariants as elm/core's Dict assumes them.
function checkDict(d, dev) {
  const RED = dev ? (c => c.$ === 'Red') : (c => c === 0);
  let err = null;
  function go(n, lo, hi, parentRed) {
    if (!isNode(n)) return 1;
    const red = RED(n.a);
    if (red && parentRed) err = err || 'red-red';
    if (isNode(n.e) && RED(n.e.a)) err = err || 'red right child';
    if (lo !== undefined && cmp(lo, n.b) >= 0) err = err || 'order';
    if (hi !== undefined && cmp(n.b, hi) >= 0) err = err || 'order';
    const l = go(n.d, lo, n.b, red), r = go(n.e, n.b, hi, red);
    if (l !== r) err = err || 'black height';
    return l + (red ? 0 : 1);
  }
  if (isNode(d) && RED(d.a)) err = 'red root';
  go(d, undefined, undefined, false);
  return err;
}
function checkAllDicts(v, dev, seen = new Set()) {
  if (v === null || typeof v !== 'object' || seen.has(v)) return null;
  seen.add(v);
  if (isDictish(v)) {
    const e = checkDict(v, dev); if (e) return e;
    for (const x of dictWalk(v, [])) { const e2 = checkAllDicts(x, dev, seen); if (e2) return e2; }
    return null;
  }
  for (const k of Object.keys(v)) { const e = checkAllDicts(v[k], dev, seen); if (e) return e; }
  return null;
}

function runDec(dec, dev, dv) {
  const fn = dev ? dec.a : dec;
  try {
    const t = fn.a === 2 ? fn.f(dv, 0) : fn(dv)(0);
    return { ok: true, off: t.a, v: t.b };
  } catch (e) {
    return { ok: false, err: e };
  }
}
function runEnc(enc, v) {
  try { return { ok: true, b: bytesOf(enc(v)) }; } catch (e) { return { ok: false, err: e }; }
}

const INTERESTING = [0, 1, 2, 3, 4, 5, 107, 108, 215, 216, 217, 250, 251, 252, 253, 254, 255, 127, 128, 0x80, 0xC0, 0xC2, 0xE0, 0xED, 0xEF, 0xBB, 0xBF, 0xF0, 0xF4, 0xF8, 0x7F, 0x3F, 0x41];

function randomBytes(rnd) {
  const n = Math.floor(Math.pow(rnd(), 2) * 48);
  const u = new Uint8Array(n);
  const z = rnd() < 0.4 ? Math.floor(rnd() * 4) : 0; // leading zero tags reach nested single-ctor types
  for (let i = 0; i < n; i++) if (i < z) u[i] = 0; else u[i] = rnd() < 0.6 ? INTERESTING[Math.floor(rnd() * INTERESTING.length)] : Math.floor(rnd() * 256);
  return u;
}

function mutate(u, rnd, pool) {
  let c = Array.from(u);
  const k = 1 + Math.floor(rnd() * 3);
  for (let j = 0; j < k; j++) {
    const op = Math.floor(rnd() * 9);
    const i = Math.floor(rnd() * (c.length + 1));
    const b = rnd() < 0.5 ? INTERESTING[Math.floor(rnd() * INTERESTING.length)] : Math.floor(rnd() * 256);
    if (op === 0 && c.length) c[i % c.length] = b;
    else if (op === 1) c.splice(i, 0, b);
    else if (op === 2 && c.length) c.splice(i % c.length, 1);
    else if (op === 3) c = c.slice(0, i);
    else if (op === 4 && pool.length) { const o = pool[Math.floor(rnd() * pool.length)]; const s = Math.floor(rnd() * (o.length + 1)); c = c.slice(0, i).concat(Array.from(o.slice(s))); }
    else if (op === 5 && c.length) { const s = Math.floor(rnd() * c.length), e = s + Math.floor(rnd() * 6); c.splice(i, 0, ...c.slice(s, e)); }
    else if (op === 6 && c.length) { // replace a byte with a non-canonical varint of the same small value
      const p = i % c.length, v = c[p];
      if (v <= 215) { const n = rnd() < 0.5 ? [252, 0, v] : rnd() < 0.5 ? [253, 0, 0, v] : [254, 0, 0, 0, v]; c.splice(p, 1, ...n); }
    }
    else if (op === 7 && c.length) { // replace a byte with a 255 float form
      const p = i % c.length, fs = [0, -0, 0.5, -1.5, 1e300, NaN, Infinity, -Infinity, 2 ** 53 + 2, -(2 ** 31), 4294967296, 7, 5e-324];
      const f = new DataView(new ArrayBuffer(8)); f.setFloat64(0, fs[Math.floor(rnd() * fs.length)], true);
      c.splice(p, 1, 255, ...new Uint8Array(f.buffer));
    }
    else if (op === 8) c.push(b);
  }
  return new Uint8Array(c);
}

const WEIRD_INTS = [1.5, -1.5, 0.3, -0.3, 215.5, 9431.5, 65535.5, NaN, Infinity, -Infinity, -0, 2 ** 53 + 2, -(2 ** 53) - 2, 2 ** 31, -(2 ** 31) - 1, 4294967295.5, 4294967296, 1e300, 5e-324, -5e-324, 107.5];
const WEIRD_STRS = ['', '\uD800', '\uDC00', 'ab\uD800', '\uD800cd', 'abcde\uD800', 'abcdef􏿿', '﻿bom', 'x'.repeat(40) + '\uD83D', '\uD83D' + 'y'.repeat(40), 'é'.repeat(40) + '\uDC00', '\u0000\u0000', '�', '😀', 'ascii only long string with 33+ chars', 'ünï😀✓'.repeat(9)];
const WEIRD_CHARS = ['\uD800', '\uDC00', '😀', '￿', '\u0000', 'é', '\uDBFF'];

// dev only: mutate Int/Float leaves and strings/chars of a decoded value in place (a copy)
function mutateValue(v, rnd, depth = 0) {
  if (typeof v === 'number') return rnd() < 0.3 ? WEIRD_INTS[Math.floor(rnd() * WEIRD_INTS.length)] : v;
  if (typeof v === 'string') return rnd() < 0.3 ? WEIRD_STRS[Math.floor(rnd() * WEIRD_STRS.length)] : v;
  if (v instanceof String) return rnd() < 0.3 ? new String(WEIRD_CHARS[Math.floor(rnd() * WEIRD_CHARS.length)]) : v;
  if (v === null || typeof v !== 'object' || v instanceof DataView || typeof v === 'function') return v;
  const o = Array.isArray(v) ? [] : {};
  for (const k of Object.keys(v)) {
    // keep tags, dict colours, Array length/shift as they are
    if (k === '$' || (isDictish(v) && k === 'a') || (v.$ === 'Array_elm_builtin' && (k === 'a' || k === 'b'))) o[k] = v[k];
    else o[k] = mutateValue(v[k], rnd, depth + 1);
  }
  return o;
}

let failures = 0;
const failuresByKind = {};
function fail(kind, detail) {
  failures++;
  failuresByKind[kind] = (failuresByKind[kind] || 0) + 1;
  if (failuresByKind[kind] <= 5) console.log('DIVERGENCE', kind, detail);
}
const f64 = new DataView(new ArrayBuffer(8));
function hasSNaN(v, seen = new Set()) {
  if (typeof v === 'number') { if (v === v) return false; f64.setFloat64(0, v); return (f64.getUint8(1) & 0x08) === 0; }
  if (v === null || typeof v !== 'object' || seen.has(v) || v instanceof DataView) return false;
  seen.add(v);
  if (isDictish(v)) return isNode(v) && (hasSNaN(v.b, seen) || hasSNaN(v.c, seen) || hasSNaN(v.d, seen) || hasSNaN(v.e, seen)); // not dictWalk: a JS array would quiet the NaN
  return Object.keys(v).some(k => hasSNaN(v[k], seen));
}
const hex = u => NodeBuffer.from(u).toString('hex');
const show = r => r.ok ? `Just off=${r.off}` : `Nothing (${r.err && r.err.constructor ? r.err.constructor.name : r.err})`;

// Run f with X's validator-call trace (build.sh wraps every w3_validate_*) and
// console.log (a rejection logs its message through Lamdera.Wire3.debug) captured.
const origLog = console.log;
function traced(X, f) {
  X.W3VT.length = 0;
  const logs = [];
  console.log = (...a) => logs.push(a.map(String).join(' '));
  try { return { r: f(), trace: X.W3VT.slice(), logs }; } finally { console.log = origLog; }
}
let vcalls = 0, vrejects = 0;

for (const mode of modes) {
  const dev = mode === 'dev';
  vcalls = 0; vrejects = 0;
  const B = load(`${__dirname}/${process.env.BUILD || "build"}/base-${mode}-x.js`, false);
  const D = load(`${__dirname}/${process.env.BUILD || "build"}/direct-${mode}-x.js`, true);
  if (!D.W3R_direct) throw new Error('direct bundle has no direct runtime');
  if (!(B.W3VN > 10 && D.W3VN === B.W3VN)) throw new Error(`validators not wrapped: ${B.W3VN} / ${D.W3VN}`);
  const rnd = mulberry32(SEED * 7919 + (dev ? 1 : 0));
  const stats = [];
  const pools = {};
  for (const name of Object.keys(B.W3R)) {
    if (ONLY && !ONLY.includes(name)) continue;
    const b = B.W3R[name], d = D.W3R[name];
    const pool = [];
    const valid = [];
    let justs = 0, n = 0;
    const compareDecode = (u, origin) => {
      if (name === 'TListUnit' && u.length && u[0] >= 252) return; // both would loop ~2^32 times
      n++;
      // the same bytes, sometimes inside a larger buffer at an offset
      let dv;
      if (rnd() < 0.2) { const big = new Uint8Array(u.length + 13); big.set(u, 5); for (let i = 0; i < 5; i++) big[i] = 0xAB; for (let i = u.length + 5; i < big.length; i++) big[i] = (i * 37) & 255; dv = new DataView(big.buffer, 5, u.length); }
      else dv = new DataView(new Uint8Array(u).buffer);
      // the non-validating decoder (w3_unsafe_decode_): same outcome, offset and value,
      // and it never runs a validator
      {
        const ub = traced(B, () => runDec(b.u, dev, dv)), ud = traced(D, () => runDec(d.u, dev, dv));
        if (ub.r.ok !== ud.r.ok) return fail(`${mode} ${name} unsafe decode outcome`, `${origin} bytes=${hex(u)} elm=${show(ub.r)} direct=${show(ud.r)}`);
        if (ub.r.ok && ub.r.off !== ud.r.off) return fail(`${mode} ${name} unsafe decode offset`, `${origin} bytes=${hex(u)} elm=${ub.r.off} direct=${ud.r.off}`);
        if (ub.r.ok && !same(ub.r.v, ud.r.v)) return fail(`${mode} ${name} unsafe decode value`, `${origin} bytes=${hex(u)}`);
        if (ub.trace.length || ud.trace.length || ub.logs.length || ud.logs.length) return fail(`${mode} ${name} unsafe decode ran validators`, `${origin} bytes=${hex(u)}`);
      }
      // the validating decoder (w3_decode_): also the same validator calls (validator,
      // argument, result) in the same order, and the same messages logged on rejection
      const tb = traced(B, () => runDec(b.d, dev, dv)), td = traced(D, () => runDec(d.d, dev, dv));
      const rb = tb.r, rd = td.r;
      vcalls += tb.trace.length;
      if (tb.logs.length) vrejects++;
      if (!same(tb.trace, td.trace) || !same(tb.logs, td.logs)) return fail(`${mode} ${name} validator calls`, `${origin} bytes=${hex(u)} elm=${JSON.stringify(tb.trace.map((t) => t[0]))} ${JSON.stringify(tb.logs)} direct=${JSON.stringify(td.trace.map((t) => t[0]))} ${JSON.stringify(td.logs)}`);
      if (rb.ok !== rd.ok) return fail(`${mode} ${name} decode outcome`, `${origin} bytes=${hex(u)} elm=${show(rb)} direct=${show(rd)}`);
      if (!rb.ok) return;
      justs++;
      if (rb.off !== rd.off) return fail(`${mode} ${name} decode offset`, `${origin} bytes=${hex(u)} elm=${rb.off} direct=${rd.off}`);
      if (!same(rb.v, rd.v)) return fail(`${mode} ${name} decode value`, `${origin} bytes=${hex(u)}`);
      const de = checkAllDicts(rd.v, dev);
      if (de && !checkAllDicts(rb.v, dev)) fail(`${mode} ${name} dict invariant`, `${de} bytes=${hex(u)}`);
      valid.push(rb.v);
      // encode both decoded values with both encoders
      const e1 = runEnc(b.e, rb.v), e2 = runEnc(d.e, rb.v), e3 = runEnc(d.e, rd.v);
      if (e1.ok !== e2.ok || e1.ok !== e3.ok) return fail(`${mode} ${name} encode outcome`, `bytes=${hex(u)} elm=${e1.ok} direct=${e2.ok}/${e3.ok} ${e2.err || e3.err || e1.err}`);
      if (e1.ok) {
        if (e1.b.equals(e2.b) && !e1.b.equals(e3.b) && hasSNaN(rb.v)) return fail(`${mode} ${name} sNaN quieted by direct decode`, `bytes=${hex(u)} reencoded elm=${hex(e1.b)} direct=${hex(e3.b)}`);
        if (!e1.b.equals(e2.b) || !e1.b.equals(e3.b)) return fail(`${mode} ${name} encode bytes`, `from bytes=${hex(u)} elm=${hex(e1.b)} direct=${hex(e2.b)}`);
        pool.push(new Uint8Array(e1.b));
      }
    };
    if (name === 'TPRec') compareDecode(new Uint8Array(7), 'sample');
    if (name === 'THolder') compareDecode(new Uint8Array(8), 'sample');
    if (name === 'PModel') { // the generated instantiations, one per field m<k>, in wire (sorted field name) order
      const fields = Object.keys(B.W3R).filter(n => n.startsWith('IP'));
      const order = fields.map((n, k) => ['m' + k, n]).sort((x, y) => x[0] < y[0] ? -1 : 1);
      for (let t = 0; t < 200 && order.every(([, n]) => (pools[n] || []).length); t++) {
        const parts = order.map(([, n]) => pools[n][Math.floor(rnd() * pools[n].length)]);
        compareDecode(new Uint8Array(Buffer.concat(parts.map(x => Buffer.from(x)))), 'sample');
      }
    }
    if (b.s) { const e = runEnc(b.e, b.s); if (e.ok) { compareDecode(new Uint8Array(e.b), 'sample'); } else fail(`${mode} ${name} sample encode`, String(e.err)); }
    for (let i = 0; i < ITER; i++) compareDecode(randomBytes(rnd), 'random');
    for (let i = 0; i < ITER * 2 && pool.length; i++) compareDecode(mutate(pool[Math.floor(rnd() * pool.length)], rnd, pool), 'mutated');
    // value-level: weird Ints/strings/chars in decoded values (dev: tags are strings, so numbers are data)
    // prod: only types where every number in the value is an Int/Float (no enums, Array
    // metadata or SeqDict internals)
    const PROD_OK = ['TInt', 'TFloat', 'TString', 'TList', 'TListStr', 'TListFloat', 'TPair', 'TDictInt', 'TDictString', 'TDictTuple', 'TDictIntFloat', 'TDictList', 'TSetInt', 'TSetString', 'TSetList', 'TMaybe', 'TResult', 'TRecFloat', 'Reserved', 'Person', 'TNamedField', 'Nested', 'Multi', 'Id', 'Wrapped', 'Tree', 'Hist', 'TBoxes', 'TTwo', 'TPosix', 'Mixed', 'Forest', 'TreeB', 'Fat'];
    if (dev || PROD_OK.includes(name)) {
      for (let i = 0; i < ITER && valid.length; i++) {
        const v = mutateValue(valid[Math.floor(rnd() * valid.length)], rnd);
        const e1 = runEnc(b.e, v), e2 = runEnc(d.e, v);
        if (e1.ok !== e2.ok) { fail(`${mode} ${name} weird-value encode outcome`, `elm=${e1.ok ? 'ok' : e1.err} direct=${e2.ok ? 'ok' : e2.err} v=${JSON.stringify(v, (k, x) => typeof x === 'number' && !isFinite(x) ? String(x) : x).slice(0, 300)}`); continue; }
        if (e1.ok && !e1.b.equals(e2.b)) fail(`${mode} ${name} weird-value encode bytes`, `elm=${hex(e1.b).slice(0, 200)} direct=${hex(e2.b).slice(0, 200)} v=${JSON.stringify(v, (k, x) => typeof x === 'number' && !isFinite(x) ? String(x) : x).slice(0, 300)}`);
      }
    }
    // streaming: every valid value, random chunk sizes
    if (D.W3S && d.w.w3e) {
      for (let i = 0; i < Math.min(valid.length, 300); i++) {
        const v = valid[i];
        const e1 = runEnc(b.e, v);
        if (!e1.ok) continue;
        const chunks = [], size = 1 + Math.floor(rnd() * 20);
        try {
          const total = D.W3S(d.w.w3e, v, size, u8 => { if (!u8.length) throw new Error('empty chunk'); chunks.push(NodeBuffer.from(u8)); });
          const all = NodeBuffer.concat(chunks);
          if (total !== all.length || !all.equals(e1.b)) fail(`${mode} ${name} stream`, `size=${size} elm=${hex(e1.b)} stream=${hex(all)}`);
        } catch (e) { fail(`${mode} ${name} stream threw`, String(e)); }
      }
    } else if (D.W3S && !process.env.BRIDGE) fail(`${mode} ${name} not direct`, 'no .w3e');
    pools[name] = pool;
    stats.push(`${name}:${justs}/${n}`);
  }
  console.log(`${mode}: Just/total per type:`, stats.join(' '));
  console.log(`${mode}: validator calls ${vcalls}, validating decodes rejected by a validator ${vrejects}`);
}
console.log(failures === 0 ? `NO DIVERGENCES (${modes.join(',')}, iter=${ITER}, seed=${SEED})` : `${failures} divergences: ${JSON.stringify(failuresByKind)}`);
process.exit(failures ? 1 : 0);
