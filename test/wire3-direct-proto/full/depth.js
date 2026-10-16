// Cold decode-depth oracle: the deepest nesting each build decodes with Node's default
// stack, in a fresh process per probe (as a restore runs). The direct decoder must reach
// at least the Elm decoder's depth, so a snapshot that restores today always restores.
//   node depth.js [opt|dev|legible ...]      prints JSON lines, exits 1 if direct < Elm
const { spawnSync } = require('child_process');
const zeros = (n) => new Array(n).fill(0);
const builders = {
  Fat3: (n) => { const a = []; for (let i = 0; i < n; i++) a.push(0, ...zeros(200)); a.push(1); return a; },
  Fat: (n) => { const a = []; for (let i = 0; i < n; i++) a.push(0, ...zeros(60)); a.push(1); return a; },
  Tree: (n) => { const a = []; for (let i = 0; i < n; i++) a.push(1); a.push(0); for (let i = 0; i < n; i++) a.push(0, 0, 0); return a; },
  Big: (n) => { const a = []; for (let i = 0; i < n; i++) a.push(0, 0, 0, 1); a.push(0, 0, 0, 0); return a; }, // BigRec String (Maybe Big), u16 tag 0
  // Validated.VTree = VLeaf Int | VNode VTree Int VTree, validated at every level (and its
  // non-validating decoder): VNode^n, the innermost VLeaf 0, then each level's label 0 and
  // right VLeaf 0.
  VTree: (n) => { const a = []; for (let i = 0; i < n; i++) a.push(1); a.push(0, 0); for (let i = 0; i < n; i++) a.push(0, 0, 0); return a; },
  VTreeUnsafe: (n) => { const a = []; for (let i = 0; i < n; i++) a.push(1); a.push(0, 0); for (let i = 0; i < n; i++) a.push(0, 0, 0); return a; },
};

if (process.argv[2] === '--probe') {
  const [, , , mode, side, name, n] = process.argv;
  const W = require(`./build/${side}-${mode}-x.js`).W3X;
  let probes = W.depth, fn = null;
  for (; probes.b; probes = probes.b) if (String(probes.a.a) === name) fn = probes.a.b;
  process.exit(fn(new DataView(new Uint8Array(builders[name](+n)).buffer)) ? 0 : 3);
}

const modes = process.argv.slice(2).length ? process.argv.slice(2) : ['opt', 'dev', 'legible'];
const probe = (mode, side, name, n) =>
  spawnSync(process.execPath, [__filename, '--probe', mode, side, name, String(n)], { cwd: __dirname }).status === 0;
function maxDepth(pred) {
  let lo = 0, hi = 64;
  while (hi < 4000000 && pred(hi)) { lo = hi; hi *= 2; }
  while (hi - lo > Math.max(1, lo >> 6)) { const m = (lo + hi) >> 1; if (pred(m)) lo = m; else hi = m; }
  return lo;
}
let bad = 0;
for (const mode of modes) for (const name of Object.keys(builders)) {
  const elm = maxDepth((n) => probe(mode, 'base', name, n));
  const direct = maxDepth((n) => probe(mode, 'direct', name, n));
  if (direct < elm) bad++;
  console.log(JSON.stringify({ mode, name, elm, direct, ok: direct >= elm }));
}
process.exit(bad ? 1 : 0);
