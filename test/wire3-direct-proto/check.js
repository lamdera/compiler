// Oracles for the C2 prototype: byte identity, value identity, failure identity.
const W = require('./build/opt-w3p.js').W3P;

function bytesOf(dv) { return Buffer.from(dv.buffer, dv.byteOffset, dv.byteLength); }
function isJust(m) { return m.$ === 0; }

// Structural equality: NaN-aware, -0-aware, and checks object key order (hidden
// classes). Stricter than _Utils_eq, which is false for NaN even baseline-vs-baseline.
function same(x, y) {
  if (typeof x === 'number') return Object.is(x, y);
  if (typeof x !== 'object' || x === null) return x === y;
  if (x.$ === -1 || x.$ === -2) { // Dict (and prod Set): compare as ascending pairs, like _Utils_eq
    const ix = [], iy = [];
    (function walk(n, o) { if (n.$ === -1) { walk(n.d, o); o.push(n.b, n.c); walk(n.e, o); } })(x, ix);
    (function walk(n, o) { if (n.$ === -1) { walk(n.d, o); o.push(n.b, n.c); walk(n.e, o); } })(y, iy);
    return ix.length === iy.length && ix.every((v, i) => same(v, iy[i]));
  }
  if (x instanceof DataView) return y instanceof DataView && Buffer.from(x.buffer, x.byteOffset, x.byteLength).equals(Buffer.from(y.buffer, y.byteOffset, y.byteLength));
  const kx = Object.keys(x), ky = Object.keys(y);
  if (kx.join() !== ky.join()) return false;
  for (const k of kx) if (!same(x[k], y[k])) return false;
  return true;
}

// Left-leaning red-black invariants for every Dict/Set in a decoded value (prod reps).
function checkLLRB(node, lo, hi) { // returns black height, throws on violation
  if (node.$ === -2) return 0;
  if (node.$ !== -1) throw new Error('not a dict node');
  const W3 = W;
  if (lo !== undefined && !(W3.cmp(lo, node.b) < 0)) throw new Error('order');
  if (hi !== undefined && !(W3.cmp(node.b, hi) < 0)) throw new Error('order');
  if (node.e.$ === -1 && node.e.a === 0) throw new Error('red right child');
  if (node.a === 0 && node.d.$ === -1 && node.d.a === 0) throw new Error('red-red');
  const lh = checkLLRB(node.d, lo, node.b), rh = checkLLRB(node.e, node.b, hi);
  if (lh !== rh) throw new Error('black height');
  return lh + (node.a === 1 ? 1 : 0);
}
function dictsOf(model, F) { return [model[F.recs], model[F.ids], model[F.db][F.db]]; }

let failures = 0;
const corruptStats = [];
function check(name, cond) { if (!cond) { failures++; console.log('FAIL', name); } }

for (const n of [0, 1, 2, 3, 7, 50, 300, 2000, 'e0', 'e1', 'e5', 'e28', 'e30', 'e31', 'e40']) {
  const m = typeof n === 'string' ? W.edge(+n.slice(1)) : W.gen(n);
  const base = bytesOf(W.baseEncode(m));
  const fastRaw = bytesOf(W.fastEncodeRaw(m));
  const fast = bytesOf(W.fastEncode(m));
  check(`encode n=${n} raw identical`, base.equals(fastRaw));
  check(`encode n=${n} via bytesEncode identical`, base.equals(fast));

  const dv = W.baseEncode(m);
  const bd = W.baseDecode(dv), fd = W.fastDecode(dv);
    check(`decode n=${n} same outcome`, isJust(bd) === isJust(fd));
  if (!isJust(bd)) { console.log(`n=${n}: baseline decode is Nothing (expected for NaN/Infinity Ints), fast agrees:`, !isJust(fd)); continue; }
  check(`decode n=${n} values identical`, same(bd, fd) && (typeof n === 'string' || W.eq(fd.a, m)));
  // re-encode of the fast-decoded value is byte identical
  for (const d of dictsOf(fd.a, W.F)) { try { if (d.$ === -1 && d.a !== 1) throw new Error('red root'); checkLLRB(d); } catch (e) { check(`LLRB n=${n}: ${e.message}`, false); } }
  // encode . decode is not the identity for lone-surrogate strings, so compare re-encodes instead
  check(`re-encode n=${n}`, bytesOf(W.baseEncode(fd.a)).equals(bytesOf(W.baseEncode(bd.a))) && bytesOf(W.fastEncodeRaw(fd.a)).equals(bytesOf(W.baseEncode(bd.a))));

  // failure identity: every truncation, plus a sample of single-byte corruptions
  if (typeof n === 'string' || n <= 50) {
    const step = Math.max(1, Math.floor(base.length / 3000));
    for (let cut = 0; cut < base.length; cut += (cut < 64 || base.length - cut < 64) ? 1 : step) {
      const t = new DataView(new Uint8Array(base.subarray(0, cut)).buffer);
      const b1 = W.baseDecode(t), f1 = W.fastDecode(t);
      if (isJust(b1) !== isJust(f1) || (isJust(b1) && !same(b1, f1))) { check(`truncate n=${n} cut=${cut}`, false); break; }
    }
    let seed = 12345;
    const rnd = () => (seed = (seed * 1103515245 + 12345) % 2147483648) / 2147483648;
    let nothings = 0;
    for (let k = 0; k < 3000; k++) {
      const c = new Uint8Array(base);
      const i = Math.floor(rnd() * c.length);
      c[i] = Math.floor(rnd() * 256);
      const t = new DataView(c.buffer);
      const b1 = W.baseDecode(t), f1 = W.fastDecode(t);
      const ok = isJust(b1) === isJust(f1) && (!isJust(b1) || same(b1, f1));
      if (!ok) { check(`corrupt n=${n} i=${i} byte=${c[i]}`, false); break; }
      if (!isJust(b1)) nothings++;
    }
    corruptStats.push(`${n}:${nothings}/3000`);
  }
}
console.log('corruptions that made the baseline fail (fast agreed on all):', corruptStats.join(' '));
console.log(failures === 0 ? 'ALL ORACLES PASS' : `${failures} failures`);
process.exit(failures ? 1 : 0);
