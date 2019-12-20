// This file needs to be minified into dist. Do it like so:
// cd ui/browser
// parcel build live.js --no-source-maps

const Sockette = require('sockette')

const ws = Sockette.default(((window.location.protocol === "https:") ? "wss://" : "ws://") + window.location.host + "/_w", {
  onopen: e => console.log('[lamdera-debug-alpha] live watch connected'),
  onmessage: e => {
    if (e.data == "r") { document.location.reload() }
  }
})
