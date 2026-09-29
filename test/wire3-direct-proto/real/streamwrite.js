// Snapshot write through app.fns.w3StreamEncodeBackendModel's function: decode the real
// snapshot with the direct build, then stream-encode it to /dev/null in 1 MB chunks, and
// report time, peak RSS, V8 heap and arrayBuffers during the write.
//   node --expose-gc streamwrite.js <app> [chunkMB]
const fs = require('fs'), os = require('os'), path = require('path')
const BENCH = process.env.W3BENCH_DIR || path.join(__dirname, '../../../../lamdera/runtime/test/wire3-bench')
const { loadBundle } = require(path.join(BENCH, 'lib/load'))
const app = process.argv[2], chunk = (+process.argv[3] || 1) << 20
const snapDir = path.join(process.env.W3BENCH || path.join(__dirname, '../../../../lamdera-wire3-bench'), 'apps', app, 'snapshots')
const file = path.join(snapDir, fs.readdirSync(snapDir).filter((f) => f.endsWith('.bin')).sort().pop())
const on = loadBundle(path.join(process.env.W3BENCH || path.join(__dirname, '../../../../lamdera-wire3-bench'), 'direct-builds', app, 'on', 'backend-app.js'),
  { name: 'expose-stream', prelude: "globalThis.__W3S = function () { return _Lamdera_w3StreamEncodeBackendModel.apply(null, arguments); };" })
let model
{ const buf = fs.readFileSync(file); model = on.decodeRaw(new DataView(buf.buffer, buf.byteOffset, buf.byteLength)).value }
global.gc(); global.gc()
const m0 = process.memoryUsage()
let peakRss = m0.rss, peakHeap = m0.heapUsed, peakAb = m0.arrayBuffers
const sample = () => { const m = process.memoryUsage(); peakRss = Math.max(peakRss, m.rss); peakHeap = Math.max(peakHeap, m.heapUsed); peakAb = Math.max(peakAb, m.arrayBuffers) }
const fd = fs.openSync('/dev/null', 'w')
let n = 0
const t0 = process.hrtime.bigint()
const total = globalThis.__W3S(model, (u8) => { fs.writeSync(fd, u8); if ((++n & 15) === 0) sample() }, chunk)
const ms = Number(process.hrtime.bigint() - t0) / 1e6
sample()
const MB = (x) => Math.round(x / 1048576)
console.log(JSON.stringify({ app, bytesMB: MB(total), chunkMB: chunk >> 20, ms: Math.round(ms), rssBeforeMB: MB(m0.rss), rssPeakAboveMB: MB(peakRss - m0.rss), heapPeakAboveMB: MB(peakHeap - m0.heapUsed), arrayBuffersPeakAboveMB: MB(peakAb - m0.arrayBuffers) }))
