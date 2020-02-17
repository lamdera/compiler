// This file needs to be minified into dist. Do it like so:
// cd ui/browser
// parcel build live.js --no-source-maps

var isLiveReload = false
const clientId = getClientId()

const Sockette = require('sockette')

const ws = Sockette.default(((window.location.protocol === "https:") ? "wss://" : "ws://") + window.location.host + "/_w", {
  onopen: e => console.log('[lamdera-debug-alpha] live watch connected'),
  onmessage: e => {
    if (e.data == "r") {
      isLiveReload = true
      document.location.reload()
    }
  }
})

setupApp = function(name, elid) {

  const leaderId = getLeaderId()
  console.log('clientid:', clientId)
  console.log('leaderId:', leaderId)
  var nodeType = null
  if (leaderId == null) {
    // No leaders set, we'll become the leader
    localStorage.setItem('lamdera-lid', clientId)
    nodeType = "l"
  } else if (leaderId == clientId) {
    nodeType = "l"
  } else {
    nodeType = "f"
  }

  var app = Elm[name].init({
    node: document.getElementById(elid),
    flags: { c: clientId, nt: nodeType }
  })
  if (document.getElementById(elid))
  {
    document.getElementById(elid).innerText = 'This is a headless program, meaning there is nothing to show here.\n\nI started the program anyway though, and you can access it as `app` in the developer console.'
  }

  app.ports.sendToFrontend.subscribe(function (payload) {
    bc.postMessage(payload)
  })

  app.ports.sendToBackend.subscribe(function (payload) {
    bc.postMessage(payload)
  })

  const bc = new BroadcastChannel('lamdera-local')

  bc.onmessage = function (msg) {
    const d = msg.data
    console.log(d)
    switch(d.t) {
      case "init":
        break;
      case "appoint":
        if (d.appointed == clientId) {
          nodeType = "l"
        }
        break;
      case "stepdown":

        // Should not be possible?
        if (nodeType == "l") { break; }

        const leaderId = getLeaderId()
        if (leaderId == null) {
          // No leaders set, we'll become the leader
          localStorage.setItem('lamdera-lid', clientId)
          nodeType = "l"
        } else if (leaderId == clientId) {
          nodeType = "l"
        } else {
          nodeType = "f"
        }

        break;
      case "ToBackend":
        if (nodeType == "l") {
          app.ports.receiveFromFrontend.send({ t: "ToBackend", c: d.c, i: d.i })
        } else {
          console.log(`Non-leader ignoring msg: ${d}`)
        }
        break;
      case "ToFrontend":
        // Only process messages for our clientId
        if (d.c == clientId) {
          app.ports.receiveFromBackend.send({ t: "ToFrontend", c: d.c, i: d.i })
        }
        break;

      default:
        console.log(`[localdev] unexpected msg`, d)
    }
  }

  // app.ports.nodeInit.send({ clientId: clientId, nodeType: nodeType})


  window.onunload = processExit
  function processExit(){
    if (nodeType == "l") {
      // The leader is stepping down, someone else needs to act as leader now
      if (!isLiveReload) {
        bc.postMessage({ clientId: clientId, t: "stepdown"})
      }
    }
    // alert("confirm exit is being called")
    return null
  }

}



function getRandomInt(max) {
  return Math.floor(Math.random() * Math.floor(max))
}

function getClientId() {
  let data = sessionStorage.getItem('lamdera-cid')
  if (data == null) {
    const cid = getRandomInt(100000).toString()
    sessionStorage.setItem('lamdera-cid', cid)
    return cid
  } else {
    return data
  }
}

function getLeaderId() {
  return localStorage.getItem('lamdera-lid')
}
