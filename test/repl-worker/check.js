// Checks one app's REPL run (see run.sh): every answer as expected, and the worker
// read only the 0.19.3 tree and reused the compiler's package artifacts.
//
//   node check.js <app> <expected.json> <out.json> <live.log>
//
// expected.json: [[input, [messages...]], ...]. With UPDATE_EXPECTED=1 it is
// rewritten from the run instead (review the diff before committing it).
const fs = require('fs')
const [app, expectedFile, outFile, logFile] = process.argv.slice(2)
const out = JSON.parse(fs.readFileSync(outFile, 'utf8'))
const log = fs.readFileSync(logFile, 'utf8')
const actual = out.results.map((r) => [r.input, r.running ? r.messages.filter((m) => !m.startsWith('---- Elm ')) : ['STOPPED: ' + r.prefillOrError]])
let failures = 0
const fail = (msg) => { failures++; console.log(`FAIL  ${app}: ${msg}`) }

if (process.env.UPDATE_EXPECTED) {
  fs.writeFileSync(expectedFile, JSON.stringify(actual, null, 2) + '\n')
  console.log(`${app}: wrote ${expectedFile}`)
} else {
  const expected = JSON.parse(fs.readFileSync(expectedFile, 'utf8'))
  if (expected.length !== actual.length) fail(`${actual.length} answers, expected ${expected.length}`)
  expected.forEach(([input, messages], i) => {
    const a = actual[i]
    if (!a || a[0] !== input || JSON.stringify(a[1]) !== JSON.stringify(messages)) {
      fail(`${JSON.stringify(input)}\n        expected ${JSON.stringify(messages)}\n        actual   ${JSON.stringify(a && a[1])}`)
    }
  })
  console.log(`${app}: ${expected.length - failures} of ${expected.length} answers as expected`)
}

const served = log.split('\n').map((l) => (l.match(/"GET ([^ ]+)/) || [])[1]).filter(Boolean).map(decodeURIComponent)
const requested = out.requested.map((r) => r.replace(/^GET /, ''))
const elmHome = served.filter((p) => p.startsWith('/_c/~/.elm/'))
const old = served.concat(requested).filter((p) => /0\.19\.[012]\//.test(p))
const artifacts = elmHome.filter((p) => /\/artifacts\.lamdera-[^/]*-0\.19\.3\.dat$/.test(p))
const sources = elmHome.filter((p) => /\/packages\/[^/]+\/[^/]+\/[^/]+\/src\//.test(p))
if (old.length) fail(`requested older Elm paths: ${old.slice(0, 5).join(' ')}`)
if (!elmHome.includes('/_c/~/.elm/0.19.3/packages/registry.dat')) fail('never read ~/.elm/0.19.3/packages/registry.dat')
if (!artifacts.length) fail('read no compiler artifacts from ~/.elm/0.19.3')
if (sources.length) fail(`compiled packages from source instead of reusing artifacts: ${sources.slice(0, 5).join(' ')}`)
console.log(`${app}: ${served.length} requests served, ${artifacts.length} package artifacts reused, ${sources.length} package sources read, ${old.length} older-Elm paths`)
process.exit(failures ? 1 : 0)
