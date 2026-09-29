// Oracles: the same compiler with LAMDERA_WIRE3_DIRECT off (base) vs on (direct), per mode.
// node --expose-gc --stack-size=60000 check.js [opt|dev ...]   (the comparison recurses down long lists)
const modes = process.argv.slice(2).length ? process.argv.slice(2) : ['opt', 'dev', 'legible'];

// Browser simulation: hide Node's Buffer (NO_BUFFER=1) and/or TextDecoder/TextEncoder
// (NO_TEXTCODEC=1) while the bundles load, so the runtime takes its fallback paths.
const NodeBuffer = Buffer;
const hidden = {};
function hideGlobals() {
  if (process.env.NO_BUFFER) { hidden.Buffer = globalThis.Buffer; delete globalThis.Buffer; }
  if (process.env.NO_TEXTCODEC) {
    hidden.TextDecoder = globalThis.TextDecoder; hidden.TextEncoder = globalThis.TextEncoder;
    delete globalThis.TextDecoder; delete globalThis.TextEncoder;
  }
}
function restoreGlobals() { Object.assign(globalThis, hidden); }

function bytesOf(dv) { return NodeBuffer.from(dv.buffer, dv.byteOffset, dv.byteLength); }
function isJust(m) { return m.$ === 0 || m.$ === 'Just'; }
const isNode = x => x.$ === -1 || x.$ === 'RBNode_elm_builtin';
const isDictish = x => x.$ === -1 || x.$ === -2 || x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin';

// NaN-aware, -0-aware, key-order-aware; Dicts compared by ascending contents (as _Utils_eq does).
function same(x, y) {
  if (typeof x === 'number') return Object.is(x, y);
  if (typeof x === 'function') return typeof y === 'function';
  if (typeof x !== 'object' || x === null) return x === y;
  if (x instanceof String) return y instanceof String && String(x) === String(y);
  if (isDictish(x)) {
    if (!(typeof y === 'object' && y !== null && isDictish(y))) return false;
    const walk = (n, o) => { if (isNode(n)) { walk(n.d, o); o.push(n.b, n.c); walk(n.e, o); } return o; };
    const ix = walk(x, []), iy = walk(y, []);
    return ix.length === iy.length && ix.every((v, i) => same(v, iy[i]));
  }
  if (x instanceof DataView) return y instanceof DataView && bytesOf(x).equals(bytesOf(y));
  if (Array.isArray(x)) return Array.isArray(y) && x.length === y.length && x.every((v, i) => same(v, y[i]));
  const kx = Object.keys(x), ky = Object.keys(y);
  if (kx.join() !== ky.join()) return false;
  for (const k of kx) if (!same(x[k], y[k])) return false;
  return true;
}

let failures = 0, checks = 0;
const retention = [];
function check(name, cond) { checks++; if (!cond) { failures++; console.log('FAIL', name); } }

for (const mode of modes) {
  const B = require(`./build/base-${mode}-x.js`).W3X;
  hideGlobals();
  const D = require(`./build/direct-${mode}-x.js`).W3X;
  restoreGlobals();
  const stats = [];
  // HasFn: a record with a function field encodes as nothing; decoding it fails
  let hf = B.hasFns;
  for (let i = 0; hf.b; hf = hf.b, i++) {
    const eb = bytesOf(B.encodeHasFn(hf.a)), ed = bytesOf(D.encodeHasFn(hf.a));
    check(`${mode} HasFn encode ${i}`, eb.equals(ed));
    const db = B.decodeHasFn(B.encodeHasFn(hf.a)), dd = D.decodeHasFn(B.encodeHasFn(hf.a));
    check(`${mode} HasFn decode ${i}`, isJust(db) === isJust(dd) && (!isJust(db) || same(db, dd)));
  }
  for (const n of [0, 1, 2, 3, 7, 16, 50, 300, 2000, 20000, 'e0', 'e1', 'e5', 'e30', 'e31', 'e40']) {
    const m = typeof n === 'string' ? B.edge(+n.slice(1)) : B.gen(n);
    const base = bytesOf(B.encode(m));
    check(`${mode} encode n=${n}`, base.equals(bytesOf(D.encode(m))));
    if (D.streamModel) {
      for (const size of [1, 7, 64, 4096]) {
        const chunks = [];
        const total = D.streamModel(m, size, u8 => { if (u8.length === 0) throw new Error('empty chunk'); chunks.push(Buffer.from(u8)); });
        check(`${mode} stream n=${n} chunk=${size}`, total === base.length && Buffer.concat(chunks).equals(base));
      }
    } else check(`${mode} stream available`, false);
    const dv = B.encode(m);
    const bd = B.decode(dv), fd = D.decode(dv);
    check(`${mode} decode outcome n=${n}`, isJust(bd) === isJust(fd));
    if (!isJust(bd)) continue;
    check(`${mode} decode value n=${n}`, same(bd, fd));
    check(`${mode} re-encode n=${n}`, bytesOf(D.encode(fd.a)).equals(bytesOf(B.encode(bd.a))));
    if (typeof n === 'string' || n <= 50) {
      const step = Math.max(1, Math.floor(base.length / 1500));
      for (let cut = 0; cut < base.length; cut += (cut < 64 || base.length - cut < 64) ? 1 : step) {
        const t = new DataView(new Uint8Array(base.subarray(0, cut)).buffer);
        const b1 = B.decode(t), f1 = D.decode(t);
        if (isJust(b1) !== isJust(f1) || (isJust(b1) && !same(b1, f1))) { check(`${mode} truncate n=${n} cut=${cut}`, false); break; }
      }
      let seed = 777, nothings = 0;
      const rnd = () => (seed = (seed * 1103515245 + 12345) % 2147483648) / 2147483648;
      for (let k = 0; k < 1500; k++) {
        const c = new Uint8Array(base);
        const i = Math.floor(rnd() * c.length);
        c[i] = Math.floor(rnd() * 256);
        const t = new DataView(c.buffer);
        const b1 = B.decode(t), f1 = D.decode(t);
        if (!(isJust(b1) === isJust(f1) && (!isJust(b1) || same(b1, f1)))) { check(`${mode} corrupt n=${n} i=${i} byte=${c[i]}`, false); break; }
        if (!isJust(b1)) nothings++;
      }
      // the same bytes inside a bigger buffer, at an offset (views into larger buffers)
      const padded = new Uint8Array(base.length + 20); padded.set(base, 7);
      const pv = new DataView(padded.buffer, 7, base.length);
      const b2 = B.decode(pv), f2 = D.decode(pv);
      check(`${mode} offset view n=${n}`, isJust(b2) === isJust(f2) && (!isJust(b2) || same(b2, f2)));
      stats.push(`${n}:${nothings}`);
    }
  }
  // Signalling NaNs keep their bits through Dict/Set decoding (a numbers-only JS array
  // would quiet them). Record SNaN = { d : Dict Int Float, k : Dict Float Int, s : Set Float }.
  {
    const sNaN = [0x01, 0, 0, 0, 0, 0, 0xf0, 0x7f];
    const sNaNneg = [0xed, 0xce, 0xca, 0x7f, 0x0c, 0x69, 0xf0, 0xff];
    const input = [2, 0, ...sNaN, 2, ...sNaNneg, 0, 2, ...sNaN];
    const dv = new DataView(new Uint8Array(input).buffer);
    const b = B.decodeSNaN(dv), d = D.decodeSNaN(dv);
    check(`${mode} sNaN decodes`, isJust(b) && isJust(d));
    if (isJust(b) && isJust(d)) {
      const ref = bytesOf(B.encodeSNaN(b.a));
      check(`${mode} sNaN bits survive Dict/Set decoding (base re-encode)`, bytesOf(B.encodeSNaN(d.a)).equals(ref));
      check(`${mode} sNaN bits survive Dict/Set decoding (direct re-encode)`, bytesOf(D.encodeSNaN(d.a)).equals(ref));
      check(`${mode} sNaN round trip is the identity`, ref.equals(NodeBuffer.from(input)));
    }
  }
  // Nothing in the direct runtime may keep a decoded input (or an encode's output buffer)
  // alive after the call: decode a big input, drop every reference, and check (at the end,
  // in a later job, since WeakRef targets live until the current job ends) that a WeakRef
  // to its ArrayBuffer dies. Needs --expose-gc.
  retention.push((() => {
    const m = B.gen(2000);
    const src = B.encode(m);
    const copy = new Uint8Array(src.byteLength); copy.set(new Uint8Array(src.buffer, src.byteOffset, src.byteLength));
    const decoded = D.decode(new DataView(copy.buffer));
    const out = D.encode(isJust(decoded) ? decoded.a : m);
    return { mode, input: new WeakRef(copy.buffer), output: new WeakRef(out.buffer), keep: decoded };
  })());
  console.log(`${mode}: corruptions the baseline rejected (direct agreed) per 1500:`, stats.join(' '));
}
// Cold decode depth (fresh process per probe): direct >= Elm, per mode.
if (!process.env.SKIP_DEPTH) {
  const r = require('child_process').spawnSync(process.execPath, [require('path').join(__dirname, 'depth.js'), ...modes], { encoding: 'utf8' });
  for (const line of r.stdout.trim().split('\n').filter(Boolean)) {
    const x = JSON.parse(line);
    check(`${x.mode} cold decode depth ${x.name}: direct ${x.direct} >= Elm ${x.elm}`, x.ok);
  }
  console.log('cold decode depth:', r.stdout.trim().split('\n').map((l) => { const x = JSON.parse(l); return `${x.mode}/${x.name} ${x.elm}->${x.direct}`; }).join(', '));
}

setTimeout(() => {
  if (typeof global.gc !== 'function') check('run with --expose-gc for the retention checks', false);
  else {
    global.gc(); global.gc();
    for (const r of retention) {
      // the decoded value itself stays alive (r.keep): it must not pin its input
      check(`${r.mode} decoded input is collectable while the decoded value lives`, r.input.deref() === undefined && r.keep !== null);
      check(`${r.mode} encode output is collectable`, r.output.deref() === undefined);
    }
  }
  console.log(failures === 0 ? `ALL ${checks} ORACLE CHECKS PASS` : `${failures} of ${checks} failed`);
  process.exit(failures ? 1 : 0);
}, 0);
