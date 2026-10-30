// The wire schema against the real codecs (test/Test/WireSchema.hs):
//
//   node check.js <app.js> <.lamdera-wire-schema.json> <wireSchema.js> [--fixture <out.json>]
//
// Runs the scenario app (src/Main.elm), which encodes sample values with the generated
// Wire3 codecs, decodes every sample's bytes with the reference decoder
// (runtime/js/wireSchema.js in the platform repo) and the schema the compiler wrote, and
// compares with the JSON view each sample must have. Exit code 0 only if all match.
// --fixture writes the schema and samples as one JSON file (for the runtime's own tests).
const fs = require('fs')
const assert = require('assert')

const [appFile, schemaFile, decoderFile] = process.argv.slice(2)
const fixtureAt = process.argv.indexOf('--fixture')
const { decode, tagName } = require(decoderFile)
const schema = JSON.parse(fs.readFileSync(schemaFile, 'utf8'))

let failed = 0
let passed = 0
const check = (label, f) => {
  try { f(); passed++ } catch (e) { failed++; console.log(`FAIL ${label}: ${e.message}`) }
}

const nothing = { $: 'Nothing' }
const just = (x) => ({ $: 'Just', args: [x] })
const c = (name, ...args) => (args.length ? { $: name, args } : { $: name })

const expected = {
  noop: c('NoOpToBackend'),
  addTodo: c('AddTodo', 'milk ünï ✓ 😀'),
  numbersSmall: c('Numbers', 0, 0.5),
  numbersNeg: c('Numbers', -1, -0),
  numbers215: c('Numbers', 107, 1.5e300),
  numbers216: c('Numbers', 108, 'Infinity'),
  numbers9431: c('Numbers', -4716, '-Infinity'),
  numbers65535: c('Numbers', 32767, 'NaN'),
  numbersInt32: c('Numbers', 2147483647, 5e-324),
  numbersIntMin32: c('Numbers', -2147483648, 3),
  numbersBig: c('Numbers', 1099511627776, 0),
  numbersMaxSafe: c('Numbers', 9007199254740991, 0),
  numbersPastSafe: c('Numbers', '36028797018963968', 0),
  edge: c('Edge', c('GT'), '😀', []),
  saved: c('Saved', { done: true, tags: ['a', 'b'], title: 't' }),
  grow: c('Grow', c('Node', c('Node', c('Leaf'), 1, c('Leaf')), 2, c('Leaf'))),
  wrappedNothing: c('Wrapped', { value: nothing }),
  wrappedOk: c('Wrapped', { value: just(c('Ok', 42)) }),
  wrappedErr: c('Wrapped', { value: just(c('Err', 'bad')) }),
  containers: c('Containers', [['a', 1], ['b', 2]], [-1, 2, 3], [1.5, -2], [[1, 'one'], [2, 'two']]),
  seqs: c('Seqs', [[[2, 1], 'x'], [[1, 2], 'y']], ['z', 'a']),
  triple: c('Triple', [false, '', -7.25]),
  blob: c('Blob', { bytes: 4, base64: 'AAEC/w==' }),
  kernel: c('Kernel', 1700000000123, 'session-1', 'client-1', [1, 2], [0.5, -1, 3], [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]),
  visit: c('Visit', { fragment: nothing, host: 'example.com', path: '/a/b', port_: just(8443), protocol: c('Https'), query: just('q=1') }),
  deep: c('Deep', c('Add', c('Lit', 'x'), c('Block', c('Seq', [c('Return', c('Lit', 'y')), c('Seq', [])])))),
  many000: c('Many000'),
  many259: c('Many259'),
  many128: c('Many128'),
  welcome: c('Welcome', { nested: { value: { value: 5 } }, reordered: { aB: 4, a_b: 3, aa: 2, alpha: 'a', b: true, zeta: 1 } }),
  pong: c('Pong'),
  model: { sessions: [['s1', ['c1', 'c2']]], todos: [[7, { done: false, tags: [], title: 'x' }]] },
}

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

async function main() {
  check('schema: version 1 with the six roots', () => {
    assert.strictEqual(schema.version, 1)
    assert.deepStrictEqual(Object.keys(schema.roots).sort(), ['BackendModel', 'BackendMsg', 'FrontendModel', 'FrontendMsg', 'ToBackend', 'ToFrontend'])
  })
  check('schema: every reference resolves, nothing missing', () => {
    const walk = (w) => {
      if (typeof w === 'string' || !w) return
      if (w.$ === 'Ref') { assert.ok(schema.types[w.type], `unresolved ${w.type}`); w.args.forEach(walk) }
      for (const k of ['of', 'key', 'value', 'ok', 'err']) if (w[k]) walk(w[k])
      if (w.items) w.items.forEach(walk)
      if (w.fields) w.fields.forEach((f) => walk(f.type))
    }
    for (const [k, def] of Object.entries(schema.types)) {
      assert.ok(!def.missing, `${k} is missing`)
      if (def.alias) walk(def.alias)
      if (def.union) def.union.ctors.forEach((ct) => ct.args.forEach(walk))
    }
  })
  check('schema: a union of more than 256 constructors has a 2 byte tag', () => {
    assert.strictEqual(schema.types[schema.roots.BackendMsg].union.tagWidth, 2)
    assert.strictEqual(schema.types[schema.roots.BackendMsg].union.ctors.length, 260)
  })
  check('schema: File in FrontendMsg is a JsRef', () => {
    const picked = schema.types[schema.roots.FrontendMsg].union.ctors.find((ct) => ct.name === 'Picked')
    assert.deepStrictEqual(picked.args, ['JsRef'])
  })

  const samples = await runApp(appFile)
  const seen = new Set()
  for (const [type, label, hex] of samples) {
    seen.add(label)
    const bytes = Buffer.from(hex, 'hex')
    check(`${type} ${label}`, () => {
      assert.ok(Object.prototype.hasOwnProperty.call(expected, label), 'no expectation')
      const actual = decode(schema, type, bytes, { exact: true })
      assert.deepStrictEqual(actual, expected[label])
      const exp = expected[label]
      if (exp && exp.$) assert.strictEqual(tagName(schema, type, bytes), exp.$)
    })
  }
  check('every expectation has a sample', () => {
    assert.deepStrictEqual(Object.keys(expected).filter((l) => !seen.has(l)), [])
  })
  check('record keys come out in wire order', () => {
    const [, , hex] = samples.find(([, l]) => l === 'welcome')
    const v = decode(schema, 'ToFrontend', Buffer.from(hex, 'hex'))
    assert.deepStrictEqual(Object.keys(v.args[0].reordered), ['aB', 'a_b', 'aa', 'alpha', 'b', 'zeta'])
  })
  check('tagName: not a union, too short, bad tag', () => {
    assert.strictEqual(tagName(schema, 'BackendModel', Buffer.from('00', 'hex')), null)
    assert.strictEqual(tagName(schema, 'ToBackend', Buffer.alloc(0)), null)
    assert.strictEqual(tagName(schema, 'BackendMsg', Buffer.from('00', 'hex')), null)
    assert.strictEqual(tagName(schema, 'ToBackend', Buffer.from('ff', 'hex')), null)
    assert.strictEqual(tagName(schema, 'Nope', Buffer.from('00', 'hex')), null)
  })
  check('decode: truncated bytes and leftovers fail', () => {
    const [, , hex] = samples.find(([, l]) => l === 'saved')
    const bytes = Buffer.from(hex, 'hex')
    assert.throws(() => decode(schema, 'ToBackend', bytes.subarray(0, bytes.length - 1)), /end of bytes/)
    assert.throws(() => decode(schema, 'ToBackend', Buffer.concat([bytes, Buffer.from([0])]), { exact: true }), /left over/)
    assert.deepStrictEqual(decode(schema, 'ToBackend', Buffer.concat([bytes, Buffer.from([0])])), expected.saved)
  })
  check('decode: Bytes past maxBytes are truncated', () => {
    const [, , hex] = samples.find(([, l]) => l === 'blob')
    assert.deepStrictEqual(decode(schema, 'ToBackend', Buffer.from(hex, 'hex'), { maxBytes: 2 }), c('Blob', { bytes: 4, base64: 'AAE=', truncated: true }))
  })

  if (fixtureAt > 0) {
    fs.writeFileSync(process.argv[fixtureAt + 1], JSON.stringify({ schema, samples }, null, 1))
  }
  console.log(`${passed} passed, ${failed} failed`)
  process.exit(failed === 0 ? 0 : 1)
}

main().catch((e) => { console.log('FAIL ' + e.stack); process.exit(1) })
