// String decode/encode differential: well-formed and ill-formed UTF-8 bodies behind a
// correct (or off-by-some) length prefix, at offsets, in both modes, plus random
// UTF-16 strings (lone surrogates included) through the encoder.
//   node strings.js [iterations] [seed]
const fs = require('fs');
const ITER = +(process.argv[2] || 200000), SEED = +(process.argv[3] || 1);
function load(p, hide) {
  const hidden = {};
  if (hide && process.env.NO_BUFFER) { hidden.Buffer = globalThis.Buffer; delete globalThis.Buffer; }
  if (hide && process.env.NO_TEXTCODEC) { hidden.TextDecoder = globalThis.TextDecoder; hidden.TextEncoder = globalThis.TextEncoder; delete globalThis.TextDecoder; delete globalThis.TextEncoder; }
  const scope = {}; new Function('scope', fs.readFileSync(p, 'utf8').replace(/\}\(this\)\);\s*$/, '}(scope));'))(scope);
  Object.assign(globalThis, hidden); return scope;
}
const NB = Buffer;
function mulberry32(a) { return function () { a |= 0; a = (a + 0x6D2B79F5) | 0; let t = Math.imul(a ^ (a >>> 15), 1 | a); t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t; return ((t ^ (t >>> 14)) >>> 0) / 4294967296; }; }
const rnd = mulberry32(SEED);
const pick = a => a[Math.floor(rnd() * a.length)];
const varint = n => { // Wire3 int encoding (n >= 0 small)
  const u = 2 * n;
  if (u <= 215) return [u];
  if (u <= 9431) return [216 + ((u - 216) >> 8), (u - 216) & 255];
  return [252, u >> 8, u & 255];
};
const pieces = [
  () => [0x41 + Math.floor(rnd() * 26)], () => [0x00], () => [0x7F],
  () => [0xC3, 0xA9], () => [0xE2, 0x9C, 0x93], () => [0xF0, 0x9F, 0x98, 0x80], () => [0xEF, 0xBB, 0xBF], () => [0xEF, 0xBF, 0xBD],
  () => [0xED, 0xA0, 0x80], () => [0xED, 0xB0, 0x80], () => [0xED, 0xA0, 0xBD, 0xED, 0xB8, 0x80], // surrogates / CESU-8
  () => [0xC0, 0x80], () => [0xE0, 0x80, 0x80], () => [0xF0, 0x80, 0x80, 0x80], () => [0xC1, 0xBF], // overlong
  () => [0xF4, 0x90, 0x80, 0x80], () => [0xF8, 0x88, 0x80, 0x80, 0x80], () => [0xFF], () => [0xFE], // out of range
  () => [0x80], () => [0xBF], () => [0xC3], () => [0xE2, 0x9C], () => [0xF0, 0x9F, 0x98], // stray / truncated
  () => [Math.floor(rnd() * 256)],
];
function body() { const out = []; const k = Math.floor(rnd() * 20); for (let i = 0; i < k; i++) out.push(...pick(pieces)()); return out; }
const hex = a => NB.from(a).toString('hex');
let fails = 0;
for (const mode of ['opt', 'dev', 'leg']) {
  const dev = mode === 'dev';
  const B = load(`${__dirname}/build/base-${mode}-x.js`, false), D = load(`${__dirname}/build/direct-${mode}-x.js`, true);
  const run = (X, dv) => { const f = dev ? X.W3R.TString.d.a : X.W3R.TString.d; try { const t = f.f(dv, 0); return { ok: true, off: t.a, v: t.b }; } catch (e) { return { ok: false }; } };
  for (let i = 0; i < ITER; i++) {
    const b = body();
    const len = rnd() < 0.8 ? b.length : Math.max(0, b.length + Math.floor(rnd() * 7) - 3);
    const tail = rnd() < 0.5 ? [] : body();
    const bytes = [...varint(len), ...b, ...tail];
    let dv;
    if (rnd() < 0.3) { const big = new Uint8Array(bytes.length + 9); big.set(bytes, 4); big.fill(0xE2, 0, 4); big.fill(0x9C, bytes.length + 4); dv = new DataView(big.buffer, 4, bytes.length); }
    else dv = new DataView(new Uint8Array(bytes).buffer);
    const rb = run(B, dv), rd = run(D, dv);
    if (rb.ok !== rd.ok || (rb.ok && (rb.off !== rd.off || rb.v !== rd.v))) {
      if (++fails < 10) console.log(`DIVERGENCE ${mode} decode bytes=${hex(bytes)} elm=${rb.ok ? rb.off + ':' + JSON.stringify(rb.v) : 'Nothing'} direct=${rd.ok ? rd.off + ':' + JSON.stringify(rd.v) : 'Nothing'}`);
    }
  }
  // encoder: random UTF-16 including lone surrogates, all lengths around the 4/8/32 thresholds
  const units = [0x41, 0x7F, 0x80, 0x7FF, 0x800, 0xFFFD, 0xFEFF, 0xD800, 0xDBFF, 0xDC00, 0xDFFF, 0xD83D, 0xDE00, 0xFFFF, 0];
  for (let i = 0; i < ITER / 4; i++) {
    const n = Math.floor(rnd() * 70), a = [];
    for (let j = 0; j < n; j++) a.push(rnd() < 0.5 ? pick(units) : Math.floor(rnd() * 0x10000));
    const s = String.fromCharCode(...a);
    let eb, ed;
    try { eb = hex(NB.from(B.W3R.TString.e(s).buffer)); } catch (e) { eb = 'THROW'; }
    try { const x = D.W3R.TString.e(s); ed = hex(NB.from(x.buffer, x.byteOffset, x.byteLength)); } catch (e) { ed = 'THROW'; }
    if (eb !== ed && ++fails < 20) console.log(`DIVERGENCE ${mode} encode ${JSON.stringify(s)} elm=${eb} direct=${ed}`);
  }
}
console.log(fails ? `${fails} divergences` : `NO DIVERGENCES (strings, iter=${ITER})`);
