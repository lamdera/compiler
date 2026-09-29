// Static check: every identifier the direct codecs reference must be bound. Parses each
// bundle with espree and resolves scopes with eslint-scope; any reference that falls
// through to the global scope in the direct build but not in the same-compiler Elm-codec
// build (and isn't a platform feature the runtime detects) is a codegen scoping bug,
// e.g. a module-level closure calling a helper with a decoder parameter that only exists
// inside the codec function.
//   node scopecheck.js <direct.js> <base.js> [...more pairs]
const path = require('path')
const NM = process.env.SCOPECHECK_NODE_MODULES || path.join(__dirname, '../../../../lamdera/runtime/node_modules')
const espree = require(path.join(NM, 'espree'))
const eslintScope = require(path.join(NM, 'eslint-scope'))
const fs = require('fs')

// Globals the direct runtime feature-detects (typeof-guarded) on purpose.
const PLATFORM = new Set(['Buffer', 'TextDecoder', 'TextEncoder', 'WeakRef'])

function unresolved(file) {
  const src = fs.readFileSync(file, 'utf8')
  const ast = espree.parse(src, { ecmaVersion: 2020, sourceType: 'script', range: true, loc: true })
  const sm = eslintScope.analyze(ast, { ecmaVersion: 2020, sourceType: 'script' })
  const out = new Map()
  for (const ref of sm.globalScope.through) {
    const n = ref.identifier.name
    if (!out.has(n)) out.set(n, ref.identifier.loc.start.line)
  }
  // implicit globals from assignments (`x = ...` without a declaration)
  for (const v of sm.globalScope.implicit.variables) if (!out.has(v.name)) out.set(v.name, 0)
  return out
}

const args = process.argv.slice(2)
let bad = 0
for (let i = 0; i < args.length; i += 2) {
  const direct = unresolved(args[i]), base = unresolved(args[i + 1])
  const extra = [...direct].filter(([n]) => !base.has(n) && !PLATFORM.has(n))
  if (extra.length) {
    bad++
    console.log(`${args[i]}: unbound identifiers: ${extra.map(([n, l]) => `${n} (line ${l})`).join(', ')}`)
  } else console.log(`${args[i]}: no unbound identifiers`)
}
process.exit(bad ? 1 : 0)
