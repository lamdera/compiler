// Cold ENCODE depth limits with values built directly in JS (so the decoder's limit
// doesn't cap the test): a depth-2 value is decoded once, the path from the root to its
// nested copy is found, and that path is repeated n times.
//   node encdepth.js [opt|dev] [Type ...]
const { spawnSync } = require('child_process');
const fs = require('fs');
const zeros = n => new Array(n).fill(0);
const builders = {
  Fat: n => { const a = []; for (let i = 0; i < n; i++) a.push(0, ...zeros(60)); a.push(1); return a; },
  Fat3: n => { const a = []; for (let i = 0; i < n; i++) a.push(0, ...zeros(200)); a.push(1); return a; },
  Hist: n => { const a = []; for (let i = 0; i < n; i++) a.push(0, ...zeros(19)); a.push(1); return a; },
  Big: n => { const a = []; for (let i = 0; i < n; i++) a.push(251, 0, 0, 1); a.push(251, 0, 0, 0); return a; },
  Mixed: n => { const a = []; for (let i = 0; i < n; i++) a.push(1, 1); a.push(4); return a; },
  Nest: n => { const a = []; for (let i = 0; i < n; i++) a.push(0, 2); a.push(0, 0); return a; },
  Fat2: n => [...zeros(n), 1, ...zeros(n * 40)],
  Tree: n => { const a = []; for (let i = 0; i < n; i++) a.push(1); a.push(0); for (let i = 0; i < n; i++) a.push(0, 0); return a; },
};
function findPath(root) {
  const q = [[root, []]];
  while (q.length) {
    const [o, p] = q.shift();
    if (p.length > 5) continue;
    for (const k of Object.keys(o)) {
      const c = o[k];
      if (c && typeof c === 'object' && !(c instanceof DataView)) {
        if (c.$ === root.$ && Object.keys(c).join() === Object.keys(root).join()) return p.concat(k);
        q.push([c, p.concat(k)]);
      }
    }
  }
  return null;
}
function deepen(v2, n) {
  const path = findPath(v2);
  const get = (o, p) => p.reduce((x, k) => x[k], o);
  let cur = get(v2, path); // depth-1 tail
  for (let i = 0; i < n; i++) {
    const top = Object.assign({}, v2);
    let o = top;
    for (let j = 0; j < path.length - 1; j++) { o[path[j]] = Object.assign({}, o[path[j]]); o = o[path[j]]; }
    o[path[path.length - 1]] = cur;
    cur = top;
  }
  return cur;
}
if (process.argv[2] === '--probe') {
  const [, , , mode, name, side, n] = process.argv;
  const dev = mode === 'dev';
  const load = p => { const scope = {}; new Function('scope', fs.readFileSync(p, 'utf8').replace(/\}\(this\)\);\s*$/, '}(scope));'))(scope); return scope; };
  const S = load(`${__dirname}/build/${side}-${mode}-x.js`);
  const fn = dev ? S.W3R[name].d.a : S.W3R[name].d;
  const v2 = fn.f(new DataView(new Uint8Array(builders[name](2)).buffer), 0).b;
  const v = deepen(v2, +n);
  try { S.W3R[name].e(v); process.exit(0); } catch (e) { process.exit(3); }
}
const mode = process.argv[2] || 'opt';
const names = process.argv.slice(3).length ? process.argv.slice(3) : Object.keys(builders);
const probe = (name, side, n) => spawnSync(process.execPath, [__filename, '--probe', mode, name, side, String(n)]).status === 0;
function maxDepth(pred) {
  let lo = 0, hi = 64;
  while (hi < 2000000 && pred(hi)) { lo = hi; hi *= 2; }
  while (hi - lo > Math.max(1, lo >> 7)) { const m = (lo + hi) >> 1; if (pred(m)) lo = m; else hi = m; }
  return lo;
}
for (const name of names) {
  const b = maxDepth(n => probe(name, 'base', n)), d = maxDepth(n => probe(name, 'direct', n));
  console.log(`${mode} ${name.padEnd(6)} cold encode elm=${b} direct=${d}${d < b ? ' <-- DIRECT SHALLOWER' : ''}`);
}
