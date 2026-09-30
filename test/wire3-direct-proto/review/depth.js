// Cold recursion-depth limits (a fresh process per probe, as in a restore): the largest
// nesting each side decodes with Node's default stack. (Encode: encdepth.js.)
//   node depth.js [opt|dev] [Type ...]
const { spawnSync } = require('child_process');
const fs = require('fs');
const zeros = n => new Array(n).fill(0);
const builders = {
  Fat: n => { const a = []; for (let i = 0; i < n; i++) a.push(0, ...zeros(60)); a.push(1); return a; },
  Fat3: n => { const a = []; for (let i = 0; i < n; i++) a.push(0, ...zeros(200)); a.push(1); return a; },
  Hist: n => { const a = []; for (let i = 0; i < n; i++) a.push(0, ...zeros(19)); a.push(1); return a; },
  Fat2: n => [...zeros(n), 1, ...zeros(n * 40)],
  Nest: n => { const a = []; for (let i = 0; i < n; i++) a.push(0, 2); a.push(0, 0); return a; },
  Tree: n => { const a = []; for (let i = 0; i < n; i++) a.push(1); a.push(0); for (let i = 0; i < n; i++) a.push(0, 0); return a; },
  Big: n => { const a = []; for (let i = 0; i < n; i++) a.push(251, 0, 0, 1); a.push(251, 0, 0, 0); return a; }, // BigK251 String (Maybe Big), u16 tag
  Mixed: n => { const a = []; for (let i = 0; i < n; i++) a.push(1, 1); a.push(4); return a; },
};

if (process.argv[2] === '--probe') {
  const [, , , mode, name, side, op, n] = process.argv;
  const dev = mode === 'dev';
  const load = p => { const scope = {}; new Function('scope', fs.readFileSync(p, 'utf8').replace(/\}\(this\)\);\s*$/, '}(scope));'))(scope); return scope; };
  const S = load(`${__dirname}/build/${side}-${mode}-x.js`);
  const dv = new DataView(new Uint8Array(builders[name](+n)).buffer);
  const dec = (X) => { const fn = dev ? X.W3R[name].d.a : X.W3R[name].d; return fn.f(dv, 0).b; };
  try {
    if (op === 'decode') { dec(S); }
    else {
      // build the value with the other build's decoder too if needed; encode is the cold op
      const O = load(`${__dirname}/build/${side === 'base' ? 'direct' : 'base'}-${mode}-x.js`);
      let v; try { v = dec(O); } catch (e) { v = dec(S); }
      S.W3R[name].e(v);
    }
    process.exit(0);
  } catch (e) { process.exit(3); }
}

const mode = process.argv[2] || 'opt';
const names = process.argv.slice(3).length ? process.argv.slice(3) : Object.keys(builders);
const probe = (name, side, op, n) => spawnSync(process.execPath, [__filename, '--probe', mode, name, side, op, String(n)]).status === 0;
function maxDepth(pred) {
  let lo = 0, hi = 64;
  while (hi < 2000000 && pred(hi)) { lo = hi; hi *= 2; }
  while (hi - lo > Math.max(1, lo >> 6)) { const m = (lo + hi) >> 1; if (pred(m)) lo = m; else hi = m; }
  return lo;
}
for (const name of names) {
  const r = {};
  for (const op of ['decode']) for (const side of ['base', 'direct']) r[op + side] = maxDepth(n => probe(name, side, op, n));
  console.log(`${mode} ${name.padEnd(6)} cold decode elm=${r.decodebase} direct=${r.decodedirect}${r.decodedirect < r.decodebase ? ' <-- DIRECT SHALLOWER' : ''}`);
}
