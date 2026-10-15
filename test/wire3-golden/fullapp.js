// Absolute goldens for the wire3-direct-proto/full app: sha256 of the bytes its Model
// encoder writes for fixed inputs, in all six builds made by that app's build.sh
// (build/{base,direct}-{dev,opt,legible}-x.js). check.js there only compares the two codec
// implementations of one compiler with each other; this pins the bytes across compilers.
//
//   node fullapp.js [--accept]
const fs = require('fs')
const path = require('path')
const crypto = require('crypto')

const here = __dirname
const full = path.join(here, '..', 'wire3-direct-proto', 'full', 'build')
const accept = process.argv.includes('--accept')
const sha = (dv) => crypto.createHash('sha256').update(Buffer.from(dv.buffer, dv.byteOffset, dv.byteLength)).digest('hex')
let failed = false

const lines = {}
for (const codecs of ['base', 'direct']) {
  for (const mode of ['dev', 'opt', 'legible']) {
    const v = `${codecs}-${mode}`
    const X = require(path.join(full, `${v}-x.js`)).W3X
    const out = []
    for (const n of [0, 1, 7, 50, 300]) {
      const b = X.encode(X.gen(n))
      out.push(`gen ${n}\t${b.byteLength}\t${sha(b)}`)
      const d = X.decode(b)
      const ok = (d.$ === 0 || d.$ === 'Just') && sha(X.encode(d.a)) === sha(b)
      if (!ok) { failed = true; console.log(`FAIL ${v}: gen ${n} does not round-trip`) }
    }
    for (const k of [0, 1, 2, 3, 31]) {
      const b = X.encode(X.edge(k))
      out.push(`edge ${k}\t${b.byteLength}\t${sha(b)}`)
    }
    let hf = X.hasFns
    for (let i = 0; hf.b; hf = hf.b, i++) {
      const b = X.encodeHasFn(hf.a)
      out.push(`hasFn ${i}\t${b.byteLength}\t${sha(b)}`)
    }
    lines[v] = out.join('\n') + '\n'
  }
}
const ref = lines['base-dev']
for (const v of Object.keys(lines)) if (lines[v] !== ref) { failed = true; console.log(`FAIL ${v} differs from base-dev`) }
const p = path.join(here, 'expected', 'full-app.txt')
if (accept) fs.writeFileSync(p, ref)
else if (fs.readFileSync(p, 'utf8') !== ref) {
  failed = true
  fs.mkdirSync(path.join(here, 'out'), { recursive: true })
  fs.writeFileSync(path.join(here, 'out', 'full-app.txt'), ref)
  console.log('FAIL full app bytes differ from expected/full-app.txt (see out/full-app.txt)')
} else console.log('ok   full app bytes identical (' + ref.split('\n').length + ' lines)')
console.log(failed ? 'FULL APP GOLDEN FAILED' : (accept ? 'FULL APP GOLDEN ACCEPTED' : 'FULL APP GOLDEN OK'))
process.exit(failed ? 1 : 0)
