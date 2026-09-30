// Oracles for the compiler-generated direct codecs (the default) against the
// same compiler's Elm codecs (LAMDERA_WIRE3_DIRECT=0), in both --optimize and dev mode.
const modes = process.argv.slice(2).length ? process.argv.slice(2) : ['opt', 'dev'];

function bytesOf(dv) { return Buffer.from(dv.buffer, dv.byteOffset, dv.byteLength); }
function isJust(m) { return m.$ === 0 || m.$ === 'Just'; }
const isNode = x => x.$ === -1 || x.$ === 'RBNode_elm_builtin';
const isDictish = x => x.$ === -1 || x.$ === -2 || x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin';

// NaN-aware, -0-aware, key-order-aware; Dicts compared by ascending contents (as _Utils_eq does).
function same(x, y) {
  if (typeof x === 'number') return Object.is(x, y);
  if (typeof x !== 'object' || x === null) return x === y;
  if (x instanceof String) return y instanceof String && String(x) === String(y);
  if (isDictish(x)) {
    if (!(typeof y === 'object' && isDictish(y))) return false;
    const walk = (n, o) => { if (isNode(n)) { walk(n.d, o); o.push(n.b, n.c); walk(n.e, o); } return o; };
    const ix = walk(x, []), iy = walk(y, []);
    return ix.length === iy.length && ix.every((v, i) => same(v, iy[i]));
  }
  if (x instanceof DataView) return y instanceof DataView && bytesOf(x).equals(bytesOf(y));
  const kx = Object.keys(x), ky = Object.keys(y);
  if (kx.join() !== ky.join()) return false;
  for (const k of kx) if (!same(x[k], y[k])) return false;
  return true;
}

let failures = 0;
function check(name, cond) { if (!cond) { failures++; console.log('FAIL', name); } }

for (const mode of modes) {
  const B = require(`./build/c-base-${mode}-x.js`).W3X;
  const D = require(`./build/c-direct-${mode}-x.js`).W3X;
  const stats = [];
  for (const n of [0, 1, 2, 3, 7, 50, 300, 2000, 'e0', 'e5', 'e30', 'e31', 'e40']) {
    const m = typeof n === 'string' ? B.edge(+n.slice(1)) : B.gen(n);
    const base = bytesOf(B.encode(m));
    check(`${mode} encode n=${n}`, base.equals(bytesOf(D.encode(m))));
    const dv = B.encode(m);
    const bd = B.decode(dv), fd = D.decode(dv);
    check(`${mode} decode outcome n=${n}`, isJust(bd) === isJust(fd));
    if (!isJust(bd)) continue;
    check(`${mode} decode value n=${n}`, same(bd, fd));
    check(`${mode} re-encode n=${n}`, bytesOf(D.encode(fd.a)).equals(bytesOf(B.encode(bd.a))));
    if (typeof n === 'string' || n <= 50) {
      const step = Math.max(1, Math.floor(base.length / 2000));
      for (let cut = 0; cut < base.length; cut += (cut < 64 || base.length - cut < 64) ? 1 : step) {
        const t = new DataView(new Uint8Array(base.subarray(0, cut)).buffer);
        const b1 = B.decode(t), f1 = D.decode(t);
        if (isJust(b1) !== isJust(f1) || (isJust(b1) && !same(b1, f1))) { check(`${mode} truncate n=${n} cut=${cut}`, false); break; }
      }
      let seed = 777, nothings = 0;
      const rnd = () => (seed = (seed * 1103515245 + 12345) % 2147483648) / 2147483648;
      for (let k = 0; k < 2000; k++) {
        const c = new Uint8Array(base);
        const i = Math.floor(rnd() * c.length);
        c[i] = Math.floor(rnd() * 256);
        const t = new DataView(c.buffer);
        const b1 = B.decode(t), f1 = D.decode(t);
        if (!(isJust(b1) === isJust(f1) && (!isJust(b1) || same(b1, f1)))) { check(`${mode} corrupt n=${n} i=${i}`, false); break; }
        if (!isJust(b1)) nothings++;
      }
      stats.push(`${n}:${nothings}`);
    }
  }
  console.log(`${mode}: corruptions the baseline rejected (direct agreed on all) per 2000:`, stats.join(' '));
}
console.log(failures === 0 ? 'ALL ORACLES PASS' : `${failures} failures`);
process.exit(failures ? 1 : 0);
