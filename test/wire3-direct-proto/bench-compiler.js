// node --expose-gc bench-compiler.js <n> <base|direct> <enc|dec> [mode]
const [n, which, op, mode = 'opt'] = [+process.argv[2], process.argv[3], process.argv[4], process.argv[5]];
const W = require(`./build/c-${which}-${mode}-x.js`).W3X;
const m = W.gen(n);
const bytes = W.encode(m);
const fn = op === 'enc' ? () => W.encode(m) : () => W.decode(bytes);
for (let i = 0; i < 2; i++) fn();
global.gc();
const times = [];
for (let i = 0; i < 5; i++) { const t0 = process.hrtime.bigint(); fn(); times.push(Number(process.hrtime.bigint() - t0) / 1e6); }
times.sort((a, b) => a - b);
console.log(JSON.stringify({ n, which, op, mode, bytes: bytes.byteLength, medianMs: +times[2].toFixed(1) }));
