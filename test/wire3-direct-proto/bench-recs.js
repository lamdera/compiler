// node --expose-gc [--trace-gc] bench-recs.js <n> <elm|c1|c2>
const mod = require('./build/opt-w3p.js');
const W = mod.W3P, R = mod.RECS;
const [n, which] = [+process.argv[2], process.argv[3]];
const m = W.gen(n);
const bytes = R.encode(m);
const fn = () => R[which](bytes);
// correctness vs the Elm decoder
const a = R.elm(bytes), b = fn();
if (!(a.$ === 0 && b.$ === 0 && W.eq(a.a, b.a))) { console.log('MISMATCH'); process.exit(1); }
fn(); fn(); global.gc(); global.gc();
const h0 = process.memoryUsage().heapUsed / 1048576;
console.log('@@START ' + h0.toFixed(1));
const t0 = process.hrtime.bigint();
let keep = fn();
const ms = Number(process.hrtime.bigint() - t0) / 1e6;
console.log('@@END ' + (process.memoryUsage().heapUsed / 1048576).toFixed(1));
global.gc();
console.log('@@RETAINED ' + (process.memoryUsage().heapUsed / 1048576 - h0).toFixed(1));
console.error(JSON.stringify({ which, bytes: bytes.byteLength, ms: +ms.toFixed(1) }));
