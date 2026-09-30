// Targeted checks the byte fuzzer can't reach: recursion depth limits (decode and encode),
// decoded-Bytes view semantics, streaming with a throwing/re-entrant onChunk, weird
// top-level values in --optimize.
//   node targeted.js [opt|dev]
const fs = require('fs');
const mode = process.argv[2] || 'opt';
const dev = mode === 'dev';
function load(p) { const scope = {}; new Function('scope', fs.readFileSync(p, 'utf8').replace(/\}\(this\)\);\s*$/, '}(scope));'))(scope); return scope; }
const B = load(`${__dirname}/build/base-${mode}-x.js`), D = load(`${__dirname}/build/direct-${mode}-x.js`);
const hex = dv => Buffer.from(dv.buffer, dv.byteOffset, dv.byteLength).toString('hex');
function runDec(dec, dv) {
  const fn = dev ? dec.a : dec;
  try { const t = fn.a === 2 ? fn.f(dv, 0) : fn(dv)(0); return { ok: true, off: t.a, v: t.b }; } catch (e) { return { ok: false, err: e }; }
}
const dvOf = arr => new DataView(new Uint8Array(arr).buffer);

// ---- 1. recursion depth
const zeros = n => new Array(n).fill(0);
const builders = {
  Fat: n => { const a = []; for (let i = 0; i < n; i++) a.push(0, ...zeros(60)); a.push(1); return a; },
  Fat2: n => [...zeros(n), 1, ...zeros(n * 40)],
  Nest: n => { const a = []; for (let i = 0; i < n; i++) a.push(0, 2); a.push(0, 0); return a; },
  Tree: n => { const a = []; for (let i = 0; i < n; i++) a.push(1); a.push(0); for (let i = 0; i < n; i++) a.push(0, 0); return a; },
  Mixed: n => { const a = []; for (let i = 0; i < n; i++) a.push(1, 1); a.push(4); return a; },
};
function maxDepth(pred) {
  let lo = 0, hi = 1;
  while (hi < 4000000 && pred(hi)) { lo = hi; hi *= 2; }
  if (hi >= 4000000) return '>=' + lo;
  while (hi - lo > 1) { const m = (lo + hi) >> 1; if (pred(m)) lo = m; else hi = m; }
  return lo;
}
console.log(`== ${mode}: max recursion depth (default Node stack)`);
for (const [name, build] of Object.entries(builders)) {
  const b = B.W3R[name], d = D.W3R[name];
  const decB = maxDepth(n => runDec(b.d, dvOf(build(n))).ok);
  const decD = maxDepth(n => runDec(d.d, dvOf(build(n))).ok);
  const encWith = enc => n => { const r = runDec(d.d, dvOf(build(n))); if (!r.ok) return false; try { enc(r.v); return true; } catch (e) { return false; } };
  const encB = maxDepth(encWith(b.e));
  const encD = maxDepth(encWith(d.e));
  // does what one side encodes decode on the other? (bytes are identical where both succeed)
  console.log(`${name.padEnd(6)} decode elm=${decB} direct=${decD}   encode elm=${encB} direct=${encD}` +
    ((typeof decD === 'number' && typeof decB === 'number' && decD < decB) ? '   <-- DIRECT DECODE SHALLOWER' : '') +
    ((typeof encD === 'number' && typeof encB === 'number' && encD < encB) ? '   <-- DIRECT ENCODE SHALLOWER' : ''));
}

// ---- 2. decoded Bytes: Elm returns a view into the input buffer, direct a copy
console.log(`== ${mode}: decoded Bytes then Bytes.Decode.bytes past its end`);
{
  // TBytes = varint len 3, then 3 bytes, then 10 more bytes in the same buffer
  const buf = dvOf([6, 1, 2, 3, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9]);
  const rb = runDec(B.W3R.TBytes.d, buf), rd = runDec(D.W3R.TBytes.d, buf);
  const isJust = m => m.$ === 0 || m.$ === 'Just';
  for (const n of [3, 4, 10, 13, 14]) {
    const eb = B.W3readBytesPast(n)(rb.v), ed = D.W3readBytesPast(n)(rd.v);
    console.log(`read ${n} bytes from a decoded 3-byte Bytes: elm=${isJust(eb) ? 'Just ' + eb.a : 'Nothing'} direct=${isJust(ed) ? 'Just ' + ed.a : 'Nothing'}${isJust(eb) !== isJust(ed) ? '   <-- DIVERGES' : ''}`);
  }
}

// ---- 3. streaming: onChunk throws, then normal encodes still work; onChunk re-enters
console.log(`== ${mode}: streaming state after a throwing / re-entrant onChunk`);
{
  const t = D.W3R.TAll, v = runDec(B.W3R.TAll.d, B.W3R.TAll.e(B.W3R.TAll.s)).v;
  const good = hex(B.W3R.TAll.e(v));
  let n = 0;
  try { D.W3S(t.w.w3e, v, 8, () => { if (++n === 2) throw new Error('disk full'); }); } catch (e) { }
  const after = hex(t.e(v));
  console.log('encode after a throwing stream matches elm:', after === good);
  const chunks = []; let inner = null;
  D.W3S(t.w.w3e, v, 8, u8 => { chunks.push(Buffer.from(u8)); if (!inner) inner = hex(t.e(v)); });
  console.log('re-entrant encode inside onChunk matches elm:', inner === good, '; outer stream matches elm:', Buffer.concat(chunks).toString('hex') === good);
  // an encode that throws midway (a function value where a String is expected) leaves state usable
  try { D.W3R.TString.e(null); } catch (e) { console.log('TString.e(null) threw in direct:', e.constructor.name); }
  try { B.W3R.TString.e(null); } catch (e) { console.log('TString.e(null) threw in elm:', e.constructor.name); }
  console.log('encode after a throwing encode matches elm:', hex(t.e(v)) === good);
}

// ---- 4. top-level weird scalars (both modes; no representation guessing needed)
console.log(`== ${mode}: weird scalar values`);
{
  const cases = {
    TInt: [1.5, -1.5, 0.3, 215.5, 9431.5, 65535.5, NaN, Infinity, -Infinity, -0, 2 ** 53 + 2, -(2 ** 31) - 1, 4294967295.5, 1e300, 5e-324, 107.5, -107.5, 2 ** 32 - 0.5, -(2 ** 31) + 0.5, 16777215.5],
    TFloat: [NaN, -0, 5e-324, -5e-324, Infinity],
    TPosix: dev ? [] : [1.5, NaN, -0, 2 ** 53 + 2],
    TString: ['\uD800', 'ab\uD800', 'abcde\uD800', 'x'.repeat(40) + '\uD83D', '﻿', '😀'.repeat(20) + '\uDBFF', 'abcd􏿿'],
    TChar: dev ? [] : ['\uD800', '\uDC00', '😀', '', 'ab', '\uD800\uD800'],
  };
  for (const [name, vals] of Object.entries(cases)) {
    for (const x of vals) {
      let eb, ed;
      try { eb = hex(B.W3R[name].e(x)); } catch (e) { eb = 'THROW ' + e.constructor.name; }
      try { ed = hex(D.W3R[name].e(x)); } catch (e) { ed = 'THROW ' + e.constructor.name; }
      if (eb !== ed) console.log(`DIVERGES ${name}(${JSON.stringify(String(x))}): elm=${eb} direct=${ed}`);
    }
  }
  if (dev) for (const c of ['\uD800', '\uDC00', '😀', '', 'ab']) {
    const x = new String(c); let eb, ed;
    try { eb = hex(B.W3R.TChar.e(x)); } catch (e) { eb = 'THROW'; }
    try { ed = hex(D.W3R.TChar.e(x)); } catch (e) { ed = 'THROW'; }
    if (eb !== ed) console.log(`DIVERGES TChar(${JSON.stringify(c)}): elm=${eb} direct=${ed}`);
  }
  console.log('done');
}
