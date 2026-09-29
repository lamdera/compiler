// node --expose-gc --trace-gc alloc.js <bundle> <n> <enc|dec>   (bundle exports W3X or W3P)
const [bundle, n, op] = [process.argv[2], +process.argv[3], process.argv[4]];
const mod = require(bundle);
const W = mod.W3X || { gen: mod.W3P.gen, encode: mod.W3P.fastEncode, decode: mod.W3P.fastDecode };
const m = W.gen(n);
const bytes = W.encode(m);
const fn = op === 'enc' ? () => W.encode(m) : () => W.decode(bytes);
fn(); fn();
global.gc(); global.gc();
const h0 = process.memoryUsage().heapUsed / 1048576;
console.log('@@START ' + h0.toFixed(1));
let keep = fn();
const h1 = process.memoryUsage().heapUsed / 1048576;
console.log('@@END ' + h1.toFixed(1));
global.gc();
console.log('@@RETAINED ' + (process.memoryUsage().heapUsed / 1048576 - h0).toFixed(1));
keep = null;
