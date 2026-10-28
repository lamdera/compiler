// Special-types oracle: every Item in src/Types.elm (types Wire3 special-cases, or whose
// runtime representation differs from the declared type, see src/Main.elm) encoded and
// decoded by the Elm codecs (base, LAMDERA_WIRE3_DIRECT=0) and by the direct codecs, per
// mode. For each item: the encoded bytes (or the exception), and decoding base's bytes
// with both (validating and non-validating), must be identical.
// node check.js [opt|dev|legible ...]   (VERBOSE=1 prints every item's bytes)
const modes = process.argv.slice(2).length ? process.argv.slice(2) : ['opt', 'dev', 'legible'];

function bytesOf(dv) { return Buffer.from(dv.buffer, dv.byteOffset, dv.byteLength); }
function isJust(m) { return m.$ === 0 || m.$ === 'Just'; }
function listToArray(l) { const out = []; for (; l.b; l = l.b) out.push(l.a); return out; }
const isNode = x => x.$ === -1 || x.$ === 'RBNode_elm_builtin';
const isDictish = x => x.$ === -1 || x.$ === -2 || x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin';

// NaN-aware, -0-aware, key-order-aware; Dicts by contents; functions are equal to functions.
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
  if (ArrayBuffer.isView(x)) return ArrayBuffer.isView(y) && x.constructor === y.constructor && x.length === y.length && Array.from(x).every((v, i) => Object.is(v, y[i]));
  if (Array.isArray(x)) return Array.isArray(y) && x.length === y.length && x.every((v, i) => same(v, y[i]));
  const kx = Object.keys(x), ky = Object.keys(y);
  if (kx.join() !== ky.join()) return false;
  for (const k of kx) if (!same(x[k], y[k])) return false;
  return true;
}

function attempt(f) { try { return { ok: true, v: f() }; } catch (e) { return { ok: false, e: String(e && e.message || e) }; } }

let failures = 0, checks = 0;
const notes = [];
function check(name, cond) { checks++; if (!cond) { failures++; console.log('FAIL', name); } }

for (const mode of modes) {
  const B = require(`./build/base-${mode}-x.js`).W3X;
  const D = require(`./build/direct-${mode}-x.js`).W3X;
  const prod = mode !== 'dev';
  // Values only JS can make. A navigation key is the function Browser.application makes;
  // a File a browser File; a Texture the kernel's object; an Effect key or file the
  // program-test constructors (not exposed), in this mode's representation.
  const key = function () {};
  // Files and textures are written as refs (Lamdera.Wire3.encodeRef): numbers into a table
  // in the encoding JS context, which decodeRef takes them back out of, once. Distinct
  // plain objects, so a ref that decodes to the wrong one is caught.
  const file = { file: 1 }, file2 = { file: 2 };
  const texture = { $: 0, a: function () {}, b: 2, c: 3, texture: 1 };
  const effKey = (X) => [prod ? { $: 0, a: key } : { $: 'RealNavigationKey', a: key }, prod ? { $: 1 } : { $: 'MockNavigationKey' }];
  const effFile = prod ? { $: 0, a: file } : { $: 'RealFile', a: file };
  const sNaN = new DataView(new Uint8Array([0x01, 0, 0, 0, 0, 0, 0xf0, 0x7f]).buffer).getFloat64(0, true);
  const offsetBytes = new DataView(new Uint8Array([9, 9, 1, 2, 3, 9]).buffer, 2, 3);
  const injected = (X) => [
    ...listToArray(X.items),
    ...listToArray(X.injKey(key)),
    ...effKey(X).flatMap((k) => listToArray(X.injEffKey(k))),
    ...listToArray(X.injFile(file)),
    ...listToArray(X.injFiles(file)(file2)),
    ...listToArray(X.injEffFile(effFile)),
    ...listToArray(X.injTexture(texture)),
    ...listToArray(X.injProcessId({ $: 0, e: 1 })),
    ...[0, -0, 0.5, -0.5, 2.5, 215, 216, 9431, 9432, 65535, 65536, 2 ** 31, -(2 ** 31), 2 ** 32, 2 ** 53, -(2 ** 53), 1e300, -1e300, NaN, Infinity, -Infinity, 0x10FFFF, 0xDFFF]
      .flatMap((i) => listToArray(X.injInt(i)).map((t) => ({ a: `${t.a} ${Object.is(i, -0) ? '-0' : i}`, b: t.b }))),
    ...[0, -0, NaN, sNaN, Infinity, -Infinity, 5e-324, 1.7976931348623157e308]
      .flatMap((f) => listToArray(X.injFloat(f)).map((t) => ({ a: `${t.a} ${Object.is(f, -0) ? '-0' : Object.is(f, sNaN) ? 'sNaN' : f}`, b: t.b }))),
    ...listToArray(X.injBytes(offsetBytes)).map((t) => ({ a: `${t.a} (view at an offset)`, b: t.b })),
    ...['', '\uD800', 'a\uDC00b', '😀', 'x'.repeat(30) + '\uD800'].flatMap((s) => listToArray(X.injString(s)).map((t) => ({ a: `${t.a} ${JSON.stringify(s)}`, b: t.b }))),
  ];
  const bItems = injected(B), dItems = injected(D);
  check(`${mode} same items`, bItems.length === dItems.length);
  let n = 0;
  for (let i = 0; i < bItems.length; i++) {
    const name = bItems[i].a, item = bItems[i].b;
    n++;
    if (/File|file|Texture|Shader|Entity|WebGL record/.test(name)) {
      // Ref numbers depend on the order the encoder visits the values (the Elm codecs' List.map
      // runs right to left, the direct codecs left to right), not part of the format: each
      // build must give the same number of bytes and decode its own bytes back to exactly
      // the same values (refs are only valid in the context that wrote them).
      const rt = (X) => { const e = attempt(() => bytesOf(X.encode(item))); const d = e.ok && attempt(() => X.decode(new DataView(new Uint8Array(e.v).buffer))); return { e, d }; };
      const rb = rt(B), rd = rt(D);
      check(`${mode} ${name}: ref encode length base ${rb.e.ok && rb.e.v.toString('hex')} direct ${rd.e.ok && rd.e.v.toString('hex')}`, rb.e.ok && rd.e.ok && rb.e.v.length === rd.e.v.length);
      check(`${mode} ${name}: refs decode to the values encoded, base ${rb.d && rb.d.ok && isJust(rb.d.v) && same(rb.d.v.a, item)} direct ${rd.d && rd.d.ok && isJust(rd.d.v) && same(rd.d.v.a, item)}`,
        rb.d && rd.d && rb.d.ok && rd.d.ok && isJust(rb.d.v) && isJust(rd.d.v) && same(rb.d.v.a, item) && same(rd.d.v.a, item));
      if (process.env.VERBOSE) console.log(mode, name, rb.e.ok && rb.e.v.toString('hex'), rd.e.ok && rd.e.v.toString('hex'));
      continue;
    }
    const eb = attempt(() => bytesOf(B.encode(item))), ed = attempt(() => bytesOf(D.encode(item)));
    // Kernel values with a real codec (linear-algebra's Float64Arrays, and the Meshes made
    // of them) must come back as exactly the values encoded, not just agree between the
    // two codecs.
    if (/Vec|Mat4|Mesh/.test(name)) {
      for (const [side, X, e] of [['base', B, eb], ['direct', D, ed]]) {
        const d = e.ok && attempt(() => X.decode(new DataView(new Uint8Array(e.v).buffer)));
        check(`${mode} ${name}: ${side} decodes to the value encoded`, d && d.ok && isJust(d.v) && same(d.v.a, item));
      }
      // 1.5.1 and earlier wrote just the tag 0 for each of these: it decodes to all zeros
      if (/^(Vec[234]|Mat4)$/.test(name) && eb.ok) {
        const legacy = new DataView(new Uint8Array([eb.v[0], 0]).buffer);
        const n = { Vec2: 2, Vec3: 3, Vec4: 4, Mat4: 16 }[name];
        for (const [side, X] of [['base', B], ['direct', D]]) {
          const d = attempt(() => X.decode(legacy));
          check(`${mode} ${name}: ${side} decodes a 1.5.1 tag 0 to zeros`, d.ok && isJust(d.v) && same(d.v.a.a, new Float64Array(n)));
        }
      }
    }
    const sameEnc = eb.ok === ed.ok && (eb.ok ? eb.v.equals(ed.v) : eb.e === ed.e);
    check(`${mode} ${name}: encode base ${eb.ok ? eb.v.toString('hex') : 'threw ' + eb.e} direct ${ed.ok ? ed.v.toString('hex') : 'threw ' + ed.e}`, sameEnc);
    if (process.env.VERBOSE) console.log(mode, name, eb.ok ? eb.v.toString('hex') : 'threw ' + eb.e, ed.ok ? ed.v.toString('hex') : 'threw ' + ed.e);
    // decode base's bytes (and direct's, if they differ) with both
    const inputs = [eb, ed].filter((e) => e.ok).map((e) => e.v);
    for (const [k, buf] of inputs.entries()) {
      if (k === 1 && inputs[0].equals(buf)) continue;
      for (const fn of ['decode', 'decodeUnsafe']) {
        const dv = new DataView(new Uint8Array(buf).buffer);
        const db = attempt(() => B[fn](dv)), dd = attempt(() => D[fn](dv));
        const okB = db.ok && isJust(db.v), okD = dd.ok && isJust(dd.v);
        check(`${mode} ${name}: ${fn} of ${k ? 'direct' : 'base'} bytes: base ${db.ok ? (okB ? 'Just' : 'Nothing') : 'threw ' + db.e}, direct ${dd.ok ? (okD ? 'Just' : 'Nothing') : 'threw ' + dd.e}`,
          db.ok === dd.ok && okB === okD && (!okB || same(db.v, dd.v)));
        if (k === 0 && fn === 'decode' && mode === 'opt' && !okB) notes.push(`${name}: the Elm codecs do not round trip it either (${eb.v.toString('hex') || 'no bytes'})`);
        if (okB && okD) {
          const rb = attempt(() => bytesOf(B.encode(db.v.a))), rd = attempt(() => bytesOf(D.encode(dd.v.a)));
          check(`${mode} ${name}: ${fn} re-encode`, rb.ok === rd.ok && (!rb.ok || rb.v.equals(rd.v)));
        }
      }
    }
  }
  console.log(`${mode}: ${n} items`);
}
if (notes.length) console.log('Not round tripped by the Elm codecs either (opt):\n  ' + notes.join('\n  '));
console.log(failures === 0 ? `ALL ${checks} SPECIAL TYPE CHECKS PASS` : `${failures} of ${checks} failed`);
process.exit(failures ? 1 : 0);
