// _W3_STRICT_BYTES = true: decoded Bytes must behave exactly like elm/bytes views when
// Elm code reads past their end, including inputs that are views into bigger buffers.
const fs = require('fs');
let bad = 0;
for (const mode of ['opt', 'dev', 'leg']) {
  const dev = mode === 'dev';
  const load = (p, strict) => { const s = {}; let src = fs.readFileSync(p, 'utf8').replace(/\}\(this\)\);\s*$/, '}(scope));');
    if (strict) src = src.replace(/\n\}\(scope\)\);$/, "\n_W3_STRICT_BYTES = true;\n}(scope));");
    new Function('scope', src)(s); return s; };
  const B = load(`${__dirname}/build/base-${mode}-x.js`, false), D = load(`${__dirname}/build/direct-${mode}-x.js`, true);
  const isJust = m => m.$ === 0 || m.$ === 'Just';
  const inputs = [];
  const plain = [6, 1, 2, 3, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9];
  inputs.push(['plain', new DataView(new Uint8Array(plain).buffer)]);
  const big = new Uint8Array(40); big.fill(7); big.set(plain, 5);
  inputs.push(['view at offset 5 inside 40 bytes', new DataView(big.buffer, 5, plain.length)]);
  inputs.push(['zero-length Bytes', new DataView(new Uint8Array([0, 5, 5]).buffer)]);
  for (const [label, dv] of inputs) {
    const fb = dev ? B.W3R.TBytes.d.a : B.W3R.TBytes.d, fd = dev ? D.W3R.TBytes.d.a : D.W3R.TBytes.d;
    const vb = fb.f(dv, 0).b, vd = fd.f(dv, 0).b;
    for (let n = 0; n <= 40; n++) {
      const eb = B.W3readBytesPast(n)(vb), ed = D.W3readBytesPast(n)(vd);
      if (isJust(eb) !== isJust(ed) || (isJust(eb) && eb.a !== ed.a)) { bad++; console.log(`DIVERGES ${mode} ${label} n=${n}`); }
    }
    // the bytes themselves and re-encoding
    const hb = Buffer.from(B.W3R.TBytes.e(vb).buffer).toString('hex'), hd = Buffer.from(B.W3R.TBytes.e(vd).buffer).toString('hex');
    if (hb !== hd) { bad++; console.log(`DIVERGES ${mode} ${label} reencode ${hb} ${hd}`); }
  }
}
console.log(bad ? `${bad} divergences with _W3_STRICT_BYTES` : '_W3_STRICT_BYTES = true: identical to elm/bytes (reads past the end, offsets, zero length, re-encode)');
