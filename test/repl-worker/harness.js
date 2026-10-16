// Drives the REPL worker of a running `lamdera live` the way its UI does
// (extra/elm-pkg-js/repl.js): load /_c/_repl-worker.js, start it with the same
// flags, send each input line, and evaluate the JavaScript it asks for.
//
//   node harness.js <live base URL> <inputs.json> [out.json]
//
// inputs.json is a list of input strings, sent one at a time. Each result is
// { input, running, prefillOrError, messages, ms }. Every URL the worker
// requested is recorded too. Node has no XMLHttpRequest (elm/http needs one), so a
// minimal one is built on fetch.
const fs = require('fs')
const vm = require('vm')

const [base, inputsFile, outFile] = process.argv.slice(2)
const inputs = JSON.parse(fs.readFileSync(inputsFile, 'utf8'))
const requested = []

class XMLHttpRequest {
  constructor() { this.listeners = {}; this.headers = {}; this.responseType = ''; this.status = 0 }
  addEventListener(type, f) { (this.listeners[type] = this.listeners[type] || []).push(f) }
  open(method, url) { this.method = method; this.url = new URL(url, base).toString() }
  setRequestHeader(k, v) { this.headers[k] = v }
  getAllResponseHeaders() { return this.rawHeaders || '' }
  abort() { this.aborted = true }
  fire(type) { (this.listeners[type] || []).forEach((f) => f()) }
  send(body) {
    requested.push(this.method + ' ' + decodeURIComponent(new URL(this.url).pathname))
    fetch(this.url, { method: this.method, headers: this.headers, body: body == null ? undefined : body })
      .then(async (r) => {
        this.status = r.status
        this.statusText = r.statusText
        this.responseURL = r.url
        this.rawHeaders = [...r.headers].map(([k, v]) => k + ': ' + v).join('\r\n')
        this.response = this.responseType === 'arraybuffer' ? await r.arrayBuffer() : await r.text()
        if (!this.aborted) this.fire('load')
      })
      .catch(() => { if (!this.aborted) this.fire('error') })
  }
}
globalThis.XMLHttpRequest = XMLHttpRequest

let worker = null
let pending = null

// As in extra/elm-pkg-js/repl.js, including its direct eval.
function interpret(code) {
  code = code.replace(
    /\$author\$project\$Lamdera\$Repl\$Interface\$jsImpl\('(.*)'\);$/gm,
    (_, p) => p.replace(/\\/g, '') + ';'
  )
  try {
    const evalResult = eval(code + '\n_result;')
    worker.ports.receiveFromJavaScriptPort.send([true, evalResult])
  } catch (error) {
    worker.ports.receiveFromJavaScriptPort.send([false, error.message])
  }
}

function send(input) {
  return new Promise((resolve) => {
    const t0 = Date.now()
    pending = (data) => resolve({ input, running: data[0], prefillOrError: data[1], messages: data[2], ms: Date.now() - t0 })
    if (worker) worker.ports.receiveFromClientPort.send(input)
    else start(input)
  })
}

function start(payload) {
  worker = globalThis.Elm.Repl.Worker.init({
    flags:
      [ ['mountPrefix', '/_c/', '']
      , ['mountLocal', '.', '.']
      , ['mountLocal', '~/.elm', '.elm']
      , ['srcDir', 'elm-stuff/lamdera', '']
      , ['start', payload, '']
      ],
    errorHandler: (error) => pending && pending([false, error.message, []]),
  })
  worker.ports.sendToClientPort.subscribe((data) => pending && pending(data))
  worker.ports.sendToJavaScriptPort.subscribe(interpret)
}

;(async () => {
  const response = await fetch(new URL('/_c/_repl-worker.js', base))
  if (!response.ok) throw new Error('loading the worker: HTTP ' + response.status)
  vm.runInThisContext(await response.text())
  const results = []
  for (const input of inputs) {
    const r = await send(input)
    results.push(r)
    console.log(JSON.stringify({ input: r.input, running: r.running, error: r.prefillOrError, messages: r.messages, ms: r.ms }))
  }
  if (outFile) fs.writeFileSync(outFile, JSON.stringify({ results, requested }, null, 2))
  process.exit(0)
})().catch((e) => { console.error('HARNESS ERROR', e); process.exit(2) })
