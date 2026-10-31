// A stand-in dashboard for the CLI's app management commands (test/Test/CLI.hs):
//
//   node stub.js <requests.jsonl>
//
// Listens on a free port on 127.0.0.1 and prints it as the first line of stdout. Answers
// /_r/apiSessionJson ("success" for the token `test-token`) and /_r/cli_<op> with fixed
// answers, in the {ok} / {error} / {pending} envelope, and appends every request as
// {"path", "body"} to requests.jsonl so the test can check what the CLI sent.
const http = require('http')
const fs = require('fs')

const log = process.argv[2]

const event = { id: 42, version: 3, tipe: 1, kind: 'ToBackend', name: 'AddTodo', start: 1700000000123, duration: 1500, bytes: 7 }

// Each pending ticket answers pending once more, then this.
const tickets = {
  info1: { ok: { appId: 'demo', plan: 'hobby', owner: 'mario', version: 3, created: 1700000000000, url: 'https://demo.lamdera.app', deploys: [{ version: 3, sha: 'abcdef1234567', at: 1700000000000 }], domains: [], logging: true } },
  event1: { ok: { event, payload: { $: 'AddTodo', args: ['milk'] }, logs: ['[2026-10-30 10:00:00.000] #42 Debug: added'] } },
}
const polled = {}

function answer(op, body) {
  switch (op) {
    case 'appList':
      return { ok: [{ appId: 'demo', plan: 'hobby', owner: 'mario', version: 3, created: 1700000000000, url: 'https://demo.lamdera.app', preview: false }] }
    case 'appInfo':
      return { pending: 'info1' }
    case 'liveShare':
      // what the live relay asks (lamdera live --serve)
      return body.key === 'test-token' ? { ok: { appId: body.appId, username: 'mario' } } : { error: 'invalid token, please run `lamdera login` again' }
    case 'appDelete':
      return body.confirm === body.appId ? { ok: { deleted: body.appId } } : { error: 'confirm must be the app name' }
    case 'envSet':
      return { error: 'That name is not a valid Elm name' }
    case 'envList':
      return { ok: [{ name: 'greeting', value: 'Hello', used: true, secret: false }] }
    case 'event':
      return { pending: 'event1' }
    case 'logs':
      return { ok: { lines: ['[2026-10-30 10:00:00.000] #42 Debug: added', '\tcontinued'] } }
    case 'result': {
      const t = tickets[body.ticket]
      if (!t) return { error: 'unknown ticket' }
      polled[body.ticket] = (polled[body.ticket] || 0) + 1
      return polled[body.ticket] < 2 ? { pending: body.ticket } : t
    }
    default:
      return { error: 'stub: no answer for ' + op }
  }
}

const server = http.createServer((req, res) => {
  let data = ''
  req.on('data', (c) => { data += c })
  req.on('end', () => {
    let body = null
    try { body = JSON.parse(data) } catch (e) { body = data }
    fs.appendFileSync(log, JSON.stringify({ path: req.url, body }) + '\n')
    res.setHeader('content-type', 'application/json')
    if (req.url === '/_r/apiSessionJson') {
      res.end(JSON.stringify(body && body.token === 'test-token' ? 'success' : 'invalid'))
    } else if (req.url.startsWith('/_r/cli_')) {
      res.end(JSON.stringify(answer(req.url.slice('/_r/cli_'.length), body || {})))
    } else {
      res.statusCode = 404
      res.end('{}')
    }
  })
})

server.listen(0, '127.0.0.1', () => {
  process.stdout.write(server.address().port + '\n')
})
