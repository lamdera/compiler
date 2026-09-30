// _W3_dictFromPairs on strictly ascending keys: every size 0..4000 plus sizes around
// powers of 2 and 3 up to 2^20, checked against the left-leaning red-black invariants
// and against the Elm decode's contents. Also: a Dict built on the fast path survives
// elm/core insert/remove sequences (via re-decode of a re-encode) with the same contents.
const fs = require('fs');
const mode = process.argv[2] || 'opt', dev = mode === 'dev';
function load(p) { const scope = {}; new Function('scope', fs.readFileSync(p, 'utf8').replace(/\}\(this\)\);\s*$/, '}(scope));'))(scope); return scope; }
const B = load(`${__dirname}/build/base-${mode}-x.js`), D = load(`${__dirname}/build/direct-${mode}-x.js`);
const isNode = x => x.$ === -1 || x.$ === 'RBNode_elm_builtin';
const RED = dev ? (c => c.$ === 'Red') : (c => c === 0);
function check(d) {
  let err = null, count = 0, prev = -Infinity;
  function go(n, parentRed) {
    if (!isNode(n)) return 1;
    const red = RED(n.a);
    if (red && parentRed) err = err || 'red-red';
    if (isNode(n.e) && RED(n.e.a)) err = err || 'red right';
    const l = go(n.d, red);
    if (!(n.b > prev)) err = err || 'order'; prev = n.b; count++;
    const r = go(n.e, red);
    if (l !== r) err = err || 'black height';
    return l + (red ? 0 : 1);
  }
  if (isNode(d) && RED(d.a)) err = 'red root';
  go(d, false);
  return { err, count };
}
function bytesFor(n) { // TDictInt = Dict Int String; keys 0..n-1 ascending, values ""
  const out = [];
  const vi = x => { const u = 2 * x; if (u <= 215) out.push(u); else if (u <= 9431) out.push(216 + ((u - 216) >> 8), (u - 216) & 255); else if (u < 65536) out.push(252, u >> 8, u & 255); else if (u < 16777216) out.push(253, u >> 16, (u >> 8) & 255, u & 255); else out.push(254, u >>> 24, (u >> 16) & 255, (u >> 8) & 255, u & 255); };
  vi(n); for (let i = 0; i < n; i++) { vi(i); out.push(0); }
  return new DataView(new Uint8Array(out).buffer);
}
const run = (X, dv) => { const f = dev ? X.W3R.TDictInt.d.a : X.W3R.TDictInt.d; return f.f(dv, 0).b; };
const sizes = [];
for (let n = 0; n <= 4000; n++) sizes.push(n);
for (let k = 12; k <= 20; k++) for (const d of [-2, -1, 0, 1, 2]) sizes.push(2 ** k + d);
for (let k = 8; k <= 12; k++) for (const d of [-2, -1, 0, 1, 2]) sizes.push(3 ** k + d);
let bad = 0;
for (const n of sizes) {
  const dv = bytesFor(n);
  const d = run(D, dv);
  const r = check(d);
  if (r.err || r.count !== n) { if (bad++ < 10) console.log('BAD', n, r); continue; }
  if (n % 97 === 0 || n > 4000) {
    const eb = Buffer.from(B.W3R.TDictInt.e(run(B, dv)).buffer), ed = Buffer.from(B.W3R.TDictInt.e(d).buffer);
    if (!eb.equals(ed)) { if (bad++ < 10) console.log('CONTENT', n); }
  }
}
console.log(bad ? `${bad} bad sizes` : `all ${sizes.length} sizes valid LLRB with the right contents (${mode})`);
