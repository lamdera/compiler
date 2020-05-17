// This file needs to be minified into dist. Do it like so:
// cd ui/browser
// parcel build live.js --no-source-maps

const Sockette = require('sockette')

var isLiveReload = false
var leaderSeen = true

var clientId = ""
const sessionId = getSessionId()
var connected = false
var bufferOutbound = []

var leaderId = null
var nodeType = "f"

// Null checking as we might be on an error page, which doesn't initiate an app
// but we still want the livereload to function
var app = null
var initBackendModel = []

var msgHandler = function(e) {
  const d = JSON.parse(e.data)
  switch(d.t) {
    case "r":
      isLiveReload = true
      document.location.reload()
      break;
  }
}

const ws = Sockette.default(((window.location.protocol === "https:") ? "wss://" : "ws://") + window.location.host + "/_w", {
  timeout: 2e3,
  maxAttempts: Infinity,
  onopen: e => {
    if (clientId !== "") { connected = true }
    if (app !== null) { app.ports.liveStatusSet.send(connected) }
    flushOutbound()
  },
  onmessage: e => {
    msgHandler(e)
    if (clientId !== "" && connected === false) {
      connected = true
      flushOutbound()
    }
  },
  onreconnect: e => {}, // Called when connection is already down and a reconnect is attempted
  onmaximum: e => {}, // Will never be hit
  onclose: e => { // Called whenever the connection is terminated
    // console.log(`ws closed`, e)
    connected = false
    if (app !== null) { app.ports.liveStatusSet.send(connected) }
  },
  onerror: e => { //
    // console.log(`ws error`, e)
    connected = false
    if (app !== null) { app.ports.liveStatusSet.send(connected) }
  }
})

const flushOutbound = function() {
  if (connected) {
    while(bufferOutbound.length > 0) {
      var out = bufferOutbound.pop()
      if (out.t == "ToBackend") { out.c = clientId }
      console.log('ws sending',out)
      ws.json(out)
    }
  }
}

setupApp = function(name, elid) {

  const msgEmitter = function(payload) {
    if (connected) {
      ws.json(payload)
    } else {
      bufferOutbound.unshift(payload)
    }
  }

  function initApp() {
    if (app !== null) { return } // Don't init when already initialised

    app = Elm[name].init({
      node: document.getElementById(elid),
      flags: { c: clientId, s: sessionId, nt: nodeType, i: initBackendModel }
    })
    if (document.getElementById(elid))
    {
      document.getElementById(elid).innerText = 'This is a headless program, meaning there is nothing to show here.\n\nI started the program anyway though, and you can access it as `app` in the developer console.'
    }

    app.ports.sendToFrontend.subscribe(function (payload) {
      msgEmitter(payload)
    })

    app.ports.sendToBackend.subscribe(function (payload) {
      msgEmitter(payload)
    })
  }

  // Upgrade the msg handler now that the app is binding
  msgHandler = function(e) {

    // console.log(`got message`,e)
    const d = JSON.parse(e.data)

    switch(d.t) {
      case "r":
        isLiveReload = true
        document.location.reload()
        break;

      case "s": // setup message, will get called again if websocket drops and reconnects
        clientId = d.c
        if (app !== null) { app.ports.setClientId.send(clientId) }

        leaderId = d.l
        if (clientId == leaderId) {
          nodeType = "l"
          if (app !== null) { app.ports.nodeTypeSetLeader.send(true) }
        } else {
          nodeType = "f"
          if (app !== null) { app.ports.nodeTypeSetLeader.send(false) }
        }

        initApp()
        break;

      case "e": // leader has been elected
        leaderId = d.l
        if (clientId == leaderId) {
          nodeType = "l"
          if (app !== null) { app.ports.nodeTypeSetLeader.send(true) }
        } else {
          nodeType = "f"
          if (app !== null) { app.ports.nodeTypeSetLeader.send(false) }
        }
        break;

      case "ToBackend":
        app.ports.receiveFromFrontend.send(d)
        break;

      case "ToFrontend":
        // Only process messages for our clientId
        if (d.c == clientId) {
          app.ports.receiveFromBackend.send(d)
        } else {
          // console.log(`dropped message`, d)
        }
        break;

      case "BackendModel":
        if (app === null) {
          // We're being given a backend state to boot up with
          initBackendModel = d.i
        } else {
          // We're already live and being given a new backend state
          app.ports.receiveBackendModel.send(d)
        }
        break;

      case "x":
        // Dummy msg to ignore, i.e. for initial backendModel state which is empty
        break;

      default:
        console.warn(`unexpected msg`, d)
    }
  }
}

function getRandomInt(max, min=0) {
  return Math.abs(Math.floor(Math.random() * Math.floor(max)) - min)
}

function getSessionId() {
  let sid = localStorage.getItem('sid')
  if (sid === null) {
    // Make the cid look similar to production sec-websocket-key clientIds
    const newSid = getRandomInt(100000,10000).toString().padEnd(40,"c04b8f7b594cdeedebc2a8029b82943b0a620815")
    localStorage.setItem('sid', newSid)
    return newSid
  } else {
    return sid
  }
}
