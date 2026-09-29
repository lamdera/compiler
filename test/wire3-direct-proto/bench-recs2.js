// node --expose-gc bench-recs2.js <n> <elm|c1|c2|c3|encElm|encC2|encC3>
const mod = require('./build/opt-w3p.js');
const W = mod.W3P, R = mod.RECS;
const [n, which] = [+process.argv[2], process.argv[3]];
const m = W.gen(n);
const bytes = R.encode(m);
const isEnc = which.startsWith('enc');
const fn = isEnc ? () => R[which](m) : () => R[which](bytes);
const B = x => Buffer.from(x.buffer, x.byteOffset, x.byteLength);
if (isEnc) { if (!B(fn()).equals(B(R.encElm(m)))) { console.log('ENCODE MISMATCH'); process.exit(1); } }
else { const a = R.elm(bytes), b = fn(); if (!(a.$ === 0 && b.$ === 0 && W.eq(a.a, b.a))) { console.log('MISMATCH'); process.exit(1); } }
fn(); fn(); global.gc();
const times = [];
for (let i = 0; i < 7; i++) { const t0 = process.hrtime.bigint(); fn(); times.push(Number(process.hrtime.bigint() - t0) / 1e6); }
times.sort((a, b) => a - b);
console.log(JSON.stringify({ which, bytes: bytes.byteLength, medianMs: +times[3].toFixed(1), minMs: +times[0].toFixed(1) }));
