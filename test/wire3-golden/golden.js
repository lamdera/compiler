// Wire3 goldens: the bytes the generated codecs write must never change between compiler
// versions (deployed apps decode snapshots and messages written by older builds).
//
//   node golden.js [--accept]
//
// Reads the six builds made by build.sh (build/{base,direct}-{dev,opt,legible}.js) and
//   1. runs the golden app in each and collects every sample's hex and its round-trip flags;
//   2. fails if any round trip fails, or if the six builds disagree on any byte;
//   3. fails if the bytes differ from expected/wire.txt (recorded with Lamdera 1.5.0 on Elm
//      0.19.2), and if any direct codec fell back to the Elm codecs;
//   4. compares the dev-mode JS of the generated codecs with expected/codecs-*.js. That text
//      also moves with unrelated upstream codegen changes, so a difference is reported for
//      review but only fails with STRICT_CODEC_JS=1.
// --accept rewrites the expected files from this run instead of comparing.
const fs = require('fs')
const path = require('path')

const here = __dirname
const accept = process.argv.includes('--accept')
const variants = ['base-dev', 'base-opt', 'base-legible', 'direct-dev', 'direct-opt', 'direct-legible']
let failed = false
const fail = (msg) => { failed = true; console.log('FAIL ' + msg) }

function runApp(file) {
  return new Promise((resolve, reject) => {
    const scope = {}
    const src = fs.readFileSync(file, 'utf8')
    new Function('scope', src.replace(/\}\(this\)\);\s*$/, '}(scope));'))(scope)
    const app = scope.Elm.Main.init()
    app.ports.out.subscribe(resolve)
    app.ports.go.send(null)
    setTimeout(() => reject(new Error('no samples from ' + file)), 10000)
  })
}

function extractCodecs(file) {
  // Top-level definitions of the generated codecs of the golden modules, sorted by their
  // first line: the order of top-level definitions in the output follows the dependency
  // graph traversal, which has no bearing on the wire format.
  const lines = fs.readFileSync(file, 'utf8').split('\n')
  const blocks = []
  let on = false
  for (const line of lines) {
    if (/^\S/.test(line)) {
      on = /^(var|function) \$author\$project\$(Types|Other)\$w3/.test(line)
      if (on) blocks.push([])
    }
    if (on) blocks[blocks.length - 1].push(line)
  }
  const texts = blocks.map((b) => b.join('\n'))
  texts.sort((x, y) => (x < y ? -1 : x > y ? 1 : 0))
  return texts.join('\n') + '\n'
}

function compareText(label, expectedFile, actual, strict) {
  const p = path.join(here, 'expected', expectedFile)
  if (accept) { fs.writeFileSync(p, actual); return }
  const expected = fs.existsSync(p) ? fs.readFileSync(p, 'utf8') : null
  if (expected === actual) { console.log(`ok   ${label} identical`); return }
  const a = (expected || '').split('\n'), b = actual.split('\n')
  let i = 0
  while (i < a.length && i < b.length && a[i] === b[i]) i++
  const msg = `${label} differs from expected/${expectedFile} at line ${i + 1}:\n  expected: ${a[i]}\n  actual:   ${b[i]}`
  fs.mkdirSync(path.join(here, 'out'), { recursive: true })
  fs.writeFileSync(path.join(here, 'out', expectedFile), actual)
  if (strict) fail(msg)
  else console.log(`NOTE ${msg}\n  (informational; full output in out/${expectedFile})`)
}

async function main() {
  const results = {}
  for (const v of variants) {
    const file = path.join(here, 'build', v + '.js')
    if (!fs.existsSync(file)) { fail(`missing ${file}: run build.sh first`); continue }
    const samples = await runApp(file)
    results[v] = samples
    for (const [name, , [reencodes, equal]] of samples) {
      if (!reencodes) fail(`${v}: ${name} does not re-encode to the same bytes`)
      if (!equal) fail(`${v}: ${name} decodes to a different value`)
    }
    console.log(`ok   ${v}: ${samples.length} samples`)
  }
  const text = (samples) => samples.map(([name, hex]) => `${name}\t${hex}`).join('\n') + '\n'
  const ref = results['base-dev'] && text(results['base-dev'])
  for (const v of variants) if (results[v] && text(results[v]) !== ref) fail(`${v} bytes differ from base-dev`)
  if (ref) compareText('wire bytes', 'wire.txt', ref, true)

  for (const mode of ['dev', 'opt', 'legible']) {
    const log = path.join(here, 'build', `direct-${mode}.log`)
    const m = fs.existsSync(log) && fs.readFileSync(log, 'utf8').match(/(\d+) codecs? falls? back to Elm/)
    if (!m) fail(`direct-${mode}: no [wire3-direct] summary in ${log}`)
    else if (m[1] !== '0') fail(`direct-${mode}: ${m[1]} codecs fall back to Elm`)
  }

  const strict = process.env.STRICT_CODEC_JS === '1'
  compareText('codec JS (base, dev)', 'codecs-base-dev.js', extractCodecs(path.join(here, 'build', 'base-dev.js')), strict)
  compareText('codec JS (direct, dev)', 'codecs-direct-dev.js', extractCodecs(path.join(here, 'build', 'direct-dev.js')), strict)

  console.log(failed ? 'GOLDEN FAILED' : (accept ? 'GOLDEN ACCEPTED' : 'GOLDEN OK'))
  process.exit(failed ? 1 : 0)
}

main().catch((e) => { console.log(e); process.exit(1) })
