// This file needs to be minified into dist for release:
// cd extra
// esbuild live.js --bundle --minify --target=chrome58,firefox57,safari11,edge16 > dist/live.js
// `lamdera live` does this automatically LDEBUG mode

import * as Sockette from 'sockette';
import * as Cookie  from 'js-cookie';

var clientId = ""
const sessionId = getSessionId()
var connected = false
var disconnectedTime = null
var bufferOutbound = []
var bufferInbound = []

var leaderId = null
var nodeType = "f"
var freezeMode = false
var timeTravelPaused = false
var timeTravelPauseOwner = false
var timeTravelPauseVersion = -1

// Time travel debugger: cross-client event bus + detached popup viewer mode.
// The bus runs through the lamdera live websocket (the server re-broadcasts
// any unrecognised message to every client), so it reaches other profiles,
// incognito windows and other devices — unlike BroadcastChannel.
// The viewer popup has no websocket (it must never count as an app client);
// its opener tab relays the bus to it over window.postMessage.
const ttViewerMode = window.location.search.indexOf('lamdera-tt=viewer') !== -1
var ttViewers = [] // viewer popup windows registered via tt-hello

function ttForwardToViewers(payload) {
  ttViewers = ttViewers.filter(function (w) { return !w.closed })
  ttViewers.forEach(function (w) { w.postMessage({ t: 'tt-down', d: payload }, window.location.origin) })
}

function ttEncode(payload) {
  var d = Object.assign({}, payload)
  if (d.f) { d.f = bytesToBase64(d.f) }
  if (d.b) { d.b = bytesToBase64(d.b) }
  return d
}

function ttDecode(d) {
  var p = Object.assign({}, d)
  if (p.f) { p.f = base64ToBytes(p.f) }
  if (p.b) { p.b = base64ToBytes(p.b) }
  return p
}

var ttDumpTarget = null // viewer awaiting the history dump after tt-hello

window.addEventListener('message', function (e) {
  if (!e.data) { return }
  if (e.data.t === 'tt-hello' && e.source) {
    ttViewers.push(e.source)
    // ask our Elm app to dump its timeline for this fresh viewer
    ttDumpTarget = e.source
    ttNotify('hello')
  } else if (e.data.t === 'tt-up') {
    // A viewer popup emitted an order: runtime pause ownership is handled
    // authoritatively by the server; all other debugger traffic uses the
    // relayed event bus.
    if (e.data.d.t === 'tp' || e.data.d.t === 'tpr') {
      timeTravelPauseOwner = e.data.d.t === 'tp'
      msgEmitter({ t: e.data.d.t })
    } else {
      msgEmitter({ t: 'tt', d: ttEncode(e.data.d) })
    }
  } else if (e.data.t === 'tt-down' && ttViewerMode) {
    if (app !== null && app.ports.tt_receive) { app.ports.tt_receive.send(e.data.d) }
  } else if (e.data.t === 'tt-down-batch' && ttViewerMode) {
    if (app !== null && app.ports.tt_receive) {
      e.data.d.forEach(function (p) { app.ports.tt_receive.send(p) })
    }
  }
})

// Local-only notifications to this tab's Elm app (never hit the server)
function ttNotify(t) {
  if (app !== null && app.ports.tt_receive) {
    app.ports.tt_receive.send({ t: t, o: '', k: '', c: '', s: '', l: '', f: null, b: null })
  }
}

// Pop out: a real popup window, opened synchronously inside the native
// click so the user gesture is preserved (Elm ports are async and would
// trip the popup blocker). While it lives, the inline panel hides ("po");
// when it closes, the inline panel comes back ("pc").
var ttPopupWindow = null
var ttPopupPoller = null

document.addEventListener('click', function (e) {
  var el = e.target && e.target.closest ? e.target.closest('#lamdera-tt-popout') : null
  if (!el) { return }
  e.preventDefault()
  e.stopPropagation()
  if (ttPopupWindow && !ttPopupWindow.closed) { ttPopupWindow.focus(); return }
  ttPopupWindow = window.open(window.location.pathname + '?lamdera-tt=viewer', 'lamdera-tt-viewer', 'popup=yes,width=1280,height=800')
  if (!ttPopupWindow) { return } // blocked: keep the inline panel
  ttNotify('po')
  ttPopupPoller = setInterval(function () {
    if (ttPopupWindow === null || ttPopupWindow.closed) {
      clearInterval(ttPopupPoller)
      ttPopupWindow = null
      ttNotify('pc')
    }
  }, 500)
}, true)

// Null checking as we might be on an error page, which doesn't initiate an app
// but we still want the livereload to function
var app = null
var initBackendModel = null
var initFrontendModel = null

var msgHandler = function(e) {
  const d = JSON.parse(e.data)
  switch(d.t) {
    case "r":
      document.location.reload()
      break;
  }
}

// Don't connect if we are inside the error iframe, or the time travel viewer popup.
const ws = (window.location.href === "about:srcdoc" || ttViewerMode) ? null : Sockette.default(((window.location.protocol === "https:") ? "wss://" : "ws://") + window.location.host + "/_w", {
  timeout: 2e3,
  maxAttempts: Infinity,
  onopen: e => {
    // Pause versions are scoped to one live-server process. A reconnect may
    // target a freshly restarted server whose counter begins again at zero.
    timeTravelPauseVersion = -1
    if (clientId !== "") {
      connected = true
      // If we've been disconnected longer than 10s, refresh entirely, as it's likely
      // in such a long time we changed live to a new project, or other things may have changed
      if (disconnectedTime !== null && disconnectedTime - new Date() < -10e3) { window.location.reload() }
      disconnectedTime = null
    }
    if (app !== null) { app.ports.setLiveStatus.send(connected) }
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
    disconnectedTime = disconnectedTime || new Date()
    if (app !== null) { app.ports.setLiveStatus.send(connected) }
  },
  onerror: e => { //
    // console.log(`ws error`, e)
    connected = false
    disconnectedTime = disconnectedTime || new Date()
    if (app !== null) { app.ports.setLiveStatus.send(connected) }
  }
})

const flushOutbound = function() {
  if (connected) {
    while(bufferOutbound.length > 0) {
      var out = bufferOutbound.pop()
      if (out.t == "ToBackend") { out.c = clientId }
      ws.json(out)
    }
  }
}

const msgEmitter = function(payload) {
  if (connected) {
    ws.json(payload)
  } else {
    bufferOutbound.unshift(payload)
  }
}

const flushInbound = function() {
  if (app !== null) {
    while(bufferInbound.length > 0) {
      var inbound = bufferInbound.pop()
      app.ports[inbound.n].send(inbound.a)
    }
  }
}

const msgInbound = function(portname, arg) {
  if (app !== null) {
    app.ports[portname].send(arg)
  } else {
    bufferInbound.unshift({ n: portname, a: arg })
  }
}

window.setupApp = function(name, elid) {

  function initApp() {
    if (app !== null) { return } // Don't init when already initialised
    // console.log(`booting with`, { c: clientId, s: sessionId, nt: nodeType, b: initBackendModel })

    const elm = name.split('.').reduce((o,i)=> o[i], Elm)
    if (name !== "Lamdera.Live") {
      console.warn('Not a Lamdera app, loading as normal Elm.')
      app = elm.init({ node: document.getElementById(elid) })
      if (document.getElementById(elid)) {
        document.getElementById(elid).innerText = 'This is a headless program, meaning there is nothing to show here.\n\nI started the program anyway though, and you can access it as `app` in the developer console.'
      }
      return;
    }

    const femLsKey = "lamdera-debug-fem"
    const frontendModelString = localStorage.getItem(femLsKey)
    initFrontendModel = (frontendModelString === null || ttViewerMode) ? null : base64ToBytes(frontendModelString)

    app = elm.init({
      node: document.getElementById(elid),
      flags: { c: clientId, s: sessionId, nt: ttViewerMode ? "v" : nodeType, p: timeTravelPaused, b: ttViewerMode ? null : initBackendModel, f: initFrontendModel }
    })
    if (document.getElementById(elid)) {
      document.getElementById(elid).innerText = 'This is a headless program, meaning there is nothing to show here.\n\nI started the program anyway though, and you can access it as `app` in the developer console.'
    }
    // window.app = app
    app.ports.send_ToFrontend.subscribe(function (payload) {
      if (payload.b !== null) {
        payload.b = bytesToBase64(payload.b)
      }
      // console.log(`[S] ToFrontend`, payload)
      msgEmitter(payload)
    })

    app.ports.save_BackendModel.subscribe(function (payload) {
      initBackendModel = payload.b
      payload.b = bytesToBase64(payload.b)
      payload.f = (payload.f) ? "force" : ""
      msgEmitter(payload)
    })

    app.ports.save_FrontendModel.subscribe(function (bytes) {
      if (ttViewerMode) { return } // never clobber the real tabs' saved model
      initFrontendModel = bytes
      localStorage.setItem(femLsKey, bytesToBase64(bytes))
    })

    app.ports.send_EnvMode.subscribe(function (payload) {
      msgEmitter(payload)
    })

    app.ports.send_ToBackend.subscribe(function (bytes) {
      var b64 = bytesToBase64(bytes)
      // console.log(`[S] ToBackend`, { t:"ToBackend", s: sessionId, c: clientId, b: b64 })
      msgEmitter({ t:"ToBackend", s: sessionId, c: clientId, b: b64 })
    })

    app.ports.setFreezeMode.subscribe(function (freeze) {
      freezeMode = freeze
    })

    // Auto-generated by extra/Lamdera/Injection.hs
    if (typeof elmPkgJsIncludes !== "undefined") elmPkgJsIncludes.init(app)

    // Time travel event bus
    if (app.ports.tt_broadcast && app.ports.tt_receive) {
      if (ttViewerMode) {
        // Viewer popup: everything goes through the opener tab
        app.ports.tt_broadcast.subscribe(function (payload) {
          if (window.opener && !window.opener.closed) {
            window.opener.postMessage({ t: 'tt-up', d: payload }, window.location.origin)
          }
        })
        if (window.opener && !window.opener.closed) {
          window.opener.postMessage({ t: 'tt-hello' }, window.location.origin)
        }
      } else {
        app.ports.tt_broadcast.subscribe(function (payload) {
          if (payload.t === 'tp' || payload.t === 'tpr') {
            timeTravelPauseOwner = payload.t === 'tp'
            msgEmitter({ t: payload.t })
          } else {
            ttForwardToViewers(payload)
            msgEmitter({ t: 'tt', d: ttEncode(payload) })
          }
        })
        // Full-history dump for a freshly opened viewer: one ordered batch,
        // sent only to the viewer that said hello (never to the server)
        if (app.ports.tt_dump) {
          app.ports.tt_dump.subscribe(function (frames) {
            if (ttDumpTarget && !ttDumpTarget.closed) {
              ttDumpTarget.postMessage({ t: 'tt-down-batch', d: frames }, window.location.origin)
            }
            ttDumpTarget = null
          })
        }
      }
    }

    flushInbound()
  }

  // Upgrade the msg handler now that the app is binding
  msgHandler = function(e) {

    // console.log(`got message`,e)
    let d = null;
    try {
      d = JSON.parse(e.data)
    } catch(err) {
      console.log(err, e.data);
      return;
    }

    switch(d.t) {
      case "r":
        if (app !== null && freezeMode) {
          hotReload().then(() => {
            if (initBackendModel !== null) {
              app.ports.verifyBackendModelDecodableAfterHotReload.send(initBackendModel)
            }
            if (initFrontendModel !== null) {
              app.ports.verifyFrontendModelDecodableAfterHotReload.send(initFrontendModel)
            }
          })
        } else {
          document.location.reload()
        }
        break;

      case "s": // setup message, will get called again if websocket drops and reconnects
        clientId = d.c
        const setupPauseVersion = Number.isInteger(d.v) ? d.v : 0
        if (setupPauseVersion >= timeTravelPauseVersion) {
          timeTravelPauseVersion = setupPauseVersion
          timeTravelPaused = d.p === true
        }

        // The websocket client id changes on reconnect. Keep a locally open
        // panel frozen while its ownership is registered under the new id.
        if (timeTravelPauseOwner) {
          timeTravelPaused = true
          msgEmitter({ t: 'tp' })
        }

        if (app !== null) {
          ttNotify(timeTravelPaused ? 'tp' : 'tpr')
          app.ports.setClientId.send(clientId)
        }

        leaderId = d.l
        if (clientId == leaderId) {
          nodeType = "l"
          if (app !== null) { app.ports.setNodeTypeLeader.send(true) }
        } else {
          nodeType = "f"
          if (app !== null) { app.ports.setNodeTypeLeader.send(false) }
        }

        initApp()
        break;

      case "tp":
      case "tpr":
        // Authoritative global runtime status from the live server. It keeps
        // multiple open panels and newly connected tabs consistent. Statuses
        // carry a monotonic version because websocket sends from different
        // client threads may complete out of order.
        const pauseVersion = Number.isInteger(d.v) ? d.v : timeTravelPauseVersion + 1
        if (pauseVersion < timeTravelPauseVersion) { break }
        timeTravelPauseVersion = pauseVersion
        timeTravelPaused = d.t === "tp" || timeTravelPauseOwner
        if (d.t === "tpr" && timeTravelPauseOwner) {
          // The old websocket may just have disconnected. Re-register without
          // briefly waking the local runtime that still owns an open panel.
          msgEmitter({ t: 'tp' })
        }
        const pausePayload = { t: timeTravelPaused ? 'tp' : 'tpr', o: '', k: '', c: '', s: '', l: '', f: null, b: null }
        ttForwardToViewers(pausePayload)
        if (app !== null && app.ports.tt_receive) { app.ports.tt_receive.send(pausePayload) }
        break;

      case "e": // leader has been elected
        leaderId = d.l
        if (clientId == leaderId) {
          nodeType = "l"
          if (app !== null) { app.ports.setNodeTypeLeader.send(true) }
        } else {
          nodeType = "f"
          if (app !== null) { app.ports.setNodeTypeLeader.send(false) }
        }
        break;

      case "ToBackend":
        // console.log(`[R] ToBackend`, d)
        app.ports.receive_ToBackend.send([d.s, d.c, base64ToBytes(d.b)])
        break;

      case "ToFrontend":
        // Only process messages for our clientId, or a broadcast
        if (d.c == clientId || d.c == sessionId || d.c == "b") {
          // console.log(`[R] ToFrontend`, d)
          d.c = clientId
          if (d.b !== null) {
            d.b = base64ToBytes(d.b)
          }
          app.ports.receive_ToFrontend.send(d)
        } else {
          // console.log(`dropped message`, d)
        }
        break;

      case "p":
        if (app === null) {
          // We're being given a backend state to boot up with
          initBackendModel = base64ToBytes(d.b)
        } else {
          // We're already live and being given a new backend state
          // @TODO this isn't used currently but needs to adapt for state restore functions?
          app.ports.receive_BackendModel.send(base64ToBytes(d.b))
        }
        break;

      case "q":
        // RPC Query

        try {
          // console.log("got rpc req", d)

          var done = false
          var response = null

          const returnHandler = function(payload) {
            if (payload.r === d.r) {
              // console.log("got rpc resp:", payload)
              response = payload
              done = true
            }
          }

          app.ports.rpcOut.subscribe(returnHandler)

          if (d.i) { d.i = JSON.parse(d.i); }
          if (d.j) { d.j = JSON.parse(d.j); }

          app.ports.rpcIn.send(d)

          // Is there a nicer way to do this?
          waitUntil(() => {
            return done
          }, 10000)
          .then((result) => {
            app.ports.rpcOut.unsubscribe(returnHandler)
            msgEmitter(response)
          })
          .catch((error) => {
            console.log(error)
            app.ports.rpcOut.unsubscribe(returnHandler)
          });

        } catch (error) {
          console.log(error)
          console.log(d)
        }

        break;

      // case "qr":
      //   break;

      case "c":
        msgInbound("onConnection", { s: d.s, c: d.c })
        break;

      case "d":
        msgInbound("onDisconnection", { s: d.s, c: d.c })
        break;

      case "tt":
        // Time travel bus, re-broadcast by the server to every client;
        // drop our own echo, forward the rest to Elm and any viewer popups
        if (d.d && d.d.o !== clientId) {
          var ttPayload = ttDecode(d.d)
          ttForwardToViewers(ttPayload)
          if (app !== null && app.ports.tt_receive) { app.ports.tt_receive.send(ttPayload) }
        }
        break;

      case "x":
        // Dummy msg to ignore, i.e. for initial backendModel state which is empty
        break;

      default:
        console.warn(`unexpected msg`, d)
    }
  }

  // The viewer popup has no websocket, so no "s" setup message will ever
  // arrive: boot immediately with a fixed identity.
  if (ttViewerMode) {
    clientId = "viewer"
    initApp()
  }
}

function getRandomInt(max, min=0) {
  return Math.abs(Math.floor(Math.random() * Math.floor(max)) - min)
}

function getSessionId() {
  let sid = Cookie.get('sid')
  if (typeof sid == 'undefined') {
    // Make the cid look similar to production sec-websocket-key clientIds
    const newSid = getRandomInt(1000000,10000).toString().padEnd(40,"c04b8f7b594cdeedebc2a8029b82943b0a620815")
    Cookie.set('sid', newSid, { sameSite: 'lax' })
    return newSid
  } else {
    return sid
  }
}

var DEFAULT_INTERVAL = 50;
var DEFAULT_TIMEOUT = 5000;

function waitUntil(
  predicate,
  timeout,
  interval
) {
  var timerInterval = interval || DEFAULT_INTERVAL;
  var timerTimeout = timeout || DEFAULT_TIMEOUT;

  return new Promise(function promiseCallback(resolve, reject) {
    var timer;
    var timeoutTimer;
    var clearTimers;
    var doStep;

    clearTimers = function clearWaitTimers() {
      clearTimeout(timeoutTimer);
      clearInterval(timer);
    };

    doStep = function doTimerStep() {
      var result;

      try {
        result = predicate();

        if (result) {
          clearTimers();
          resolve(result);
        } else {
          timer = setTimeout(doStep, timerInterval);
        }
      } catch (e) {
        clearTimers();
        reject(e);
      }
    };

    timer = setTimeout(doStep, timerInterval);
    timeoutTimer = setTimeout(function onTimeout() {
      clearTimers();
      reject(new Error('Timed out after waiting for ' + timerTimeout + 'ms'));
    }, timerTimeout);
  });
}


function bytesToBase64(bytes_) {
  let base64 = '';
  const encodings = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

  const bytes = new Uint8Array(bytes_.buffer);
  const byteLength = bytes.byteLength;
  const byteRemainder = byteLength % 3;
  const mainLength = byteLength - byteRemainder;

  let a;
  let b;
  let c;
  let d;
  let chunk;

  // Main loop deals with bytes in chunks of 3
  for (let i = 0; i < mainLength; i += 3) {
    // Combine the three bytes into a single integer
    chunk = (bytes[i] << 16) | (bytes[i + 1] << 8) | bytes[i + 2];

    // Use bitmasks to extract 6-bit segments from the triplet
    a = (chunk & 16515072) >> 18; // 16515072 = (2^6 - 1) << 18
    b = (chunk & 258048) >> 12; // 258048   = (2^6 - 1) << 12
    c = (chunk & 4032) >> 6; // 4032     = (2^6 - 1) << 6
    d = chunk & 63;        // 63       = 2^6 - 1

    // Convert the raw binary segments to the appropriate ASCII encoding
    base64 += encodings[a] + encodings[b] + encodings[c] + encodings[d];
  }

  // Deal with the remaining bytes and padding
  if (byteRemainder === 1) {
    chunk = bytes[mainLength];

    a = (chunk & 252) >> 2; // 252 = (2^6 - 1) << 2

    // Set the 4 least significant bits to zero
    b = (chunk & 3) << 4; // 3   = 2^2 - 1

    base64 += `${encodings[a]}${encodings[b]}==`;
  } else if (byteRemainder === 2) {
    chunk = (bytes[mainLength] << 8) | bytes[mainLength + 1];

    a = (chunk & 64512) >> 10; // 64512 = (2^6 - 1) << 10
    b = (chunk & 1008) >> 4; // 1008  = (2^6 - 1) << 4

    // Set the 2 least significant bits to zero
    c = (chunk & 15) << 2; // 15    = 2^4 - 1

    base64 += `${encodings[a]}${encodings[b]}${encodings[c]}=`;
  }

  return base64;
}

function base64ToBytes(b64) {
  return new DataView(Base64Binary.decodeArrayBuffer(b64))
}

var Base64Binary = {
	_keyStr : "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=",

	/* will return a  Uint8Array type */
  decodeArrayBuffer: function(input) {
    input = this.removePaddingChars(input);
    var bytes = (input.length/4) * 3;
    var ab = new ArrayBuffer(bytes);
    this.decode(input, ab);
    return ab;
  },

  removePaddingChars: function(input){
    var lkey = this._keyStr.indexOf(input.charAt(input.length - 1));
		var lkey2 = this._keyStr.indexOf(input.charAt(input.length - 2));
		if(lkey2 == 64 && lkey == 64){
      return input.substring(0,input.length - 2);
		} else if(lkey == 64){
      return input.substring(0,input.length - 1);
    }
    return input;
  },

	decode: function (input, arrayBuffer) {

		var bytes = parseInt((input.length / 4) * 3, 10);

		var uarray;
		var chr1, chr2, chr3;
		var enc1, enc2, enc3, enc4;
		var i = 0;
		var j = 0;

		if (arrayBuffer)
			uarray = new Uint8Array(arrayBuffer);
		else
			uarray = new Uint8Array(bytes);

		input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");

		for (i=0; i<bytes; i+=3) {
			//get the 3 octects in 4 ascii chars
			enc1 = this._keyStr.indexOf(input.charAt(j++));
			enc2 = this._keyStr.indexOf(input.charAt(j++));
			enc3 = this._keyStr.indexOf(input.charAt(j++));
			enc4 = this._keyStr.indexOf(input.charAt(j++));

			chr1 = (enc1 << 2) | (enc2 >> 4);
			chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
			chr3 = ((enc3 & 3) << 6) | enc4;

			uarray[i] = chr1;
			if (enc3 != 64) uarray[i+1] = chr2;
			if (enc4 != 64) uarray[i+2] = chr3;
		}

		return uarray;
	}
}

let errorDialog = null

function hotReload() {
  return fetch("/")
    .then(response => response.text())
    .then(html => {
      if (errorDialog) {
        errorDialog.remove()
        errorDialog = null
      }
      const index1 = html.indexOf("\n// lamdera-elm-js-start")
      const index2 = html.indexOf("\n// lamdera-elm-js-end", index1)
      if (index1 === -1 || index2 === -1) {
        const dialog = document.createElement("dialog")
        const iframe = document.createElement("iframe")
        iframe.srcdoc = html
        dialog.style.all = "initial"
        iframe.style.all = "initial"
        iframe.style.position = "fixed"
        iframe.style.inset = "0"
        iframe.style.width = "100%"
        iframe.style.height = "100%"
        dialog.appendChild(iframe)
        document.documentElement.appendChild(dialog)
        dialog.showModal()
        errorDialog = dialog
      } else {
        const compiledElmJs = html.slice(index1, index2)
        const f = new Function(compiledElmJs)
        const newScope = {}
        f.call(newScope)
        Elm.hot.reload(newScope)
      }
    })
    .catch(error => {
      console.error(error)
    })
}
