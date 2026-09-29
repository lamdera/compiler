// node --expose-gc bench.js <n> <which: baseEnc|fastEnc|baseDec|fastDec>
// One measurement per process so heap numbers are not polluted by the other variant.
const W = require('./build/opt-w3p.js').W3P;
const n = +process.argv[2];
const which = process.argv[3];
const reps = +(process.argv[4] || 5);

const m = W.gen(n);
const bytes = W.baseEncode(m);
global.gc();
const heapBase = process.memoryUsage().heapUsed;

let peak = 0;
const sampler = setInterval(() => {}, 1000); // keep loop alive only
clearInterval(sampler);

const fn = {
  baseEnc: () => W.baseEncode(m),
  fastEnc: () => W.fastEncode(m),
  baseDec: () => W.baseDecode(bytes),
  fastDec: () => W.fastDecode(bytes),
}[which];

// warmup
for (let i = 0; i < 2; i++) fn();
global.gc();
const times = [];
let out;
for (let i = 0; i < reps; i++) {
  const t0 = process.hrtime.bigint();
  out = fn();
  const t1 = process.hrtime.bigint();
  times.push(Number(t1 - t0) / 1e6);
  const h = process.memoryUsage().heapUsed;
  if (h > peak) peak = h;
}
times.sort((a, b) => a - b);
console.log(JSON.stringify({
  n, which, bytes: bytes.byteLength,
  medianMs: +times[Math.floor(times.length / 2)].toFixed(2),
  minMs: +times[0].toFixed(2),
  maxRss: process.resourceUsage().maxRSS,
}));
