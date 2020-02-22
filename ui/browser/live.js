// This file needs to be minified into dist. Do it like so:
// cd ui/browser
// parcel build live.js --no-source-maps

const Sockette = require('sockette')

var isLiveReload = false
var leaderSeen = true
const clientId = getClientId()

const leaderHeartbeatInterval = 3000
const followerHeartbeatInterval = leaderHeartbeatInterval + 2000

// Null checking as we might be on an error page, which doesn't initiate an app
// but we still want the livereload to function
var app = null;

const ws = Sockette.default(((window.location.protocol === "https:") ? "wss://" : "ws://") + window.location.host + "/_w", {
  onopen: e => {
    if (app !== null) {
      app.ports.liveStatusSet.send(true)
    }
    // console.log('[Λ] live watch connected')
  },
  onmessage: e => {
    if (e.data == "r") {
      isLiveReload = true
      document.location.reload()
    }
  },
  onerror: e => {
    if (app !== null) {
      app.ports.liveStatusSet.send(false)
    }
  }
})


setupApp = function(name, elid) {

  const leaderId = getLeaderId()
  var nodeType = null
  if (leaderId == null) {
    // No leaders set, we'll become the leader
    localStorage.setItem('llid', clientId)
    nodeType = "l"
  } else if (leaderId == clientId) {
    nodeType = "l"
  } else {
    nodeType = "f"
  }

  app = Elm[name].init({
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
    // console.log(d)
    switch(d.t) {
      case "h": // leader is announcing it is still alive (heartbeat)
        leaderSeen = true
        break;

      case "k": // node is announcing it's death/end of leadership (kill)
        // Should not be possible?
        if (nodeType == "l") { break; }

        const leaderId = getLeaderId()
        if (leaderId == null) {
          // No leaders set, we'll become the leader
          localStorage.setItem('llid', clientId)
          nodeType = "l"
          app.ports.nodeTypeSetLeader.send(true)
          bc.postMessage({ clientId: clientId, t: "h"})

        } else if (leaderId == d.clientId) {
          nodeType = "l"
          localStorage.setItem('llid', clientId)
          app.ports.nodeTypeSetLeader.send(true)
          bc.postMessage({ clientId: clientId, t: "h"})

        } else {
          nodeType = "f"
          app.ports.nodeTypeSetLeader.send(false)
        }

        break;

      case "ToBackend":
        if (nodeType == "l") {
          app.ports.receiveFromFrontend.send({ t: "ToBackend", c: d.c, i: d.i })
        } else {
          // console.log(`Non-leader ignoring msg: ${d}`)
        }
        break;

      case "ToFrontend":
        // Only process messages for our clientId
        if (d.c == clientId) {
          app.ports.receiveFromBackend.send({ t: "ToFrontend", c: d.c, i: d.i })
        }
        break;

      default:
        console.log(`[Λ] unexpected msg`, d)
    }
  }

  // Something to look into in future. The "easy" bit would be as below, but the
  // issue arises if you navigate away while there's only one tab. Maybe all the
  // windows do need to know about each other after all?
  // document.addEventListener(
  //   "visibilitychange",
  //   function() {
  //     if (document.hidden) {
  //       if (nodeType == "l") {
  //         // The leader is stepping down, someone else needs to act as leader now
  //         if (!isLiveReload) {
  //           nodeType = "f"
  //           app.ports.nodeTypeSetLeader.send(false)
  //           bc.postMessage({ clientId: clientId, t: "k"})
  //         }
  //       }
  //     } else  {
  //       // ???
  //     }
  //   },
  //   false
  // )

  const heartbeat = function() {
    if (nodeType == "l") {
      // Send out a heartbeat so everyone knows we're still alive
      bc.postMessage({ clientId: clientId, t: "h"})
      setTimeout(heartbeat, leaderHeartbeatInterval)
    } else {
      //
      if (leaderSeen) {
        // We've seen the leader since last interval, so reset
        leaderSeen = false
        setTimeout(heartbeat, followerHeartbeatInterval)
      } else {
        // Leader wasn't seen for a while! Take over leadership
        nodeType = "l"
        localStorage.setItem('llid', clientId)
        bc.postMessage({ clientId: clientId, t: "h"})
        app.ports.nodeTypeSetLeader.send(true)
        isLiveReload = true
        document.location.reload()
      }
    }
  }
  heartbeat()

  function processExit(){
    if (nodeType == "l") {
      // The leader is stepping down, someone else needs to act as leader now
      if (!isLiveReload) {
        bc.postMessage({ clientId: clientId, t: "k"})
      }
    }
    // alert("confirm exit is being called")
    return null
  }

  window.onunload = processExit

}


function getRandomInt(max, min=0) {
  return (Math.floor(Math.random() * Math.floor(max)) - min)
}

function getClientId() {
  let data = sessionStorage.getItem('lcid')
  if (data == null) {
    const cid = getRandomInt(100000).toString()
    sessionStorage.setItem('lcid', cid)
    return cid
  } else {
    return data
  }
}

function getLeaderId() {
  return localStorage.getItem('llid')
}
