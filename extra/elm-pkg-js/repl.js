
// load the REPL worker on demand

exports.init = async function(app) {

    let worker = null

    function sendToWorker(data) {
      worker.ports.receiveFromClientPort.send(data)
    }

    function sendToClient(data) {
      app.ports.receiveFromWorkerPort.send(data)
    }

    // this survives esbuild --minify
    getApp = () => app

    function interpret(code) {
      code = code.replace(
        /\$author\$project\$Lamdera\$Repl\$Interface\$jsImpl\('(.*)'\);$/gm,
        (_,p) => p.replace(/\\/g, "")+';'
      )
      try {
        const evalResult = (0, eval)(code + '\n_result;')
        worker.ports.receiveFromJavaScriptPort.send([true, evalResult])
      } catch (error) {
        worker.ports.receiveFromJavaScriptPort.send([false, error.message])
      }
    }

    function sendWorkerError(error) {
      sendToClient([false, error.message, []])
    }

    function initWorker(payload) {
      app.ports.sendToWorkerPort.unsubscribe(loadWorker)

      worker = Elm.Repl.Worker.init({
        flags:
          [ ['mountPrefix', '/_c/', '']
          , ['mountLocal', '.', '.']
          , ['mountLocal', '~/.elm', '.elm']
          , ['srcDir', 'elm-stuff/lamdera', '']
          , ['start', payload, '']
          ],
        errorHandler: sendWorkerError,
      })

      app.ports.sendToWorkerPort.subscribe(sendToWorker)
      worker.ports.sendToClientPort.subscribe(sendToClient)
      worker.ports.sendToJavaScriptPort.subscribe(interpret)
    }

    function loadWorker(payload) {
      fetch('/_c/_repl-worker.js')
        .then(response => {
          if (response.ok) return response.text()
          else throw new Error("HTTP " + response.status + " " + response.statusText)
        })
        .then(code => (window.eval(code), initWorker(payload)))
        .catch(sendWorkerError)
    }
    app.ports.sendToWorkerPort.subscribe(loadWorker)
}
