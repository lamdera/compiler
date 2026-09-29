// Real-snapshot oracles for the direct codecs (flag on) vs the same compiler's Elm codecs
// (flag off). App data stays where it is; nothing is written anywhere.
//
//   node realcheck.js <app> [<buildsDir>]
//
// buildsDir (default ../lamdera-wire3-bench/direct-builds) holds <app>/off/backend-app.js and
// <app>/on/backend-app.js; the snapshot is the newest ../lamdera-wire3-bench/apps/<app>/snapshots/*.bin.
// Uses the wire3-bench harness's bundle loader and structural hash.
const fs = require('fs')
const os = require('os')
const path = require('path')
const crypto = require('crypto')
const BENCH = process.env.W3BENCH_DIR || path.join(__dirname, '../../../../lamdera/runtime/test/wire3-bench')
const { loadBundle } = require(path.join(BENCH, 'lib/load'))
const { structuralHash } = require(path.join(BENCH, 'lib/model'))

const app = process.argv[2]
const builds = process.argv[3] || path.join(process.env.W3BENCH || path.join(__dirname, '../../../../lamdera-wire3-bench'), 'direct-builds')
const snapDir = path.join(process.env.W3BENCH || path.join(__dirname, '../../../../lamdera-wire3-bench'), 'apps', app, 'snapshots')
const bin = fs.readdirSync(snapDir).filter((f) => f.endsWith('.bin')).sort().pop()
const file = path.join(snapDir, bin)

// Exposes the backend's streaming API (the function behind app.fns.w3StreamEncodeBackendModel).
const prelude = { name: 'expose-stream', prelude: "globalThis.__W3S = function () { return _Lamdera_w3StreamEncodeBackendModel.apply(null, arguments); };" }

const results = { app, snapshot: bin, bytes: fs.statSync(file).size, node: process.version }
const ok = (k, v) => { results[k] = v; if (v === false) results.failed = (results.failed || []).concat(k) }
const time = (f) => { const t0 = process.hrtime.bigint(); const r = f(); return [r, Number(process.hrtime.bigint() - t0) / 1e6] }
const B = (dv) => Buffer.from(dv.buffer, dv.byteOffset, dv.byteLength)

const fileBuf = fs.readFileSync(file)
const dv = new DataView(fileBuf.buffer, fileBuf.byteOffset, fileBuf.byteLength)

const on = loadBundle(path.join(builds, app, 'on', 'backend-app.js'), prelude)
const onStream = globalThis.__W3S

// 1. direct decode
const [dOn, msDecOn] = time(() => on.decodeRaw(dv))
results.msDecodeDirect = Math.round(msDecOn)
ok('directDecodeOk', dOn.ok)
if (!dOn.ok) { console.log(JSON.stringify({ ...results, error: dOn.error })); process.exit(1) }
ok('directConsumedAll', dOn.consumed === dv.byteLength)

// 2. direct encode reproduces the file
const [eOn, msEncOn] = time(() => on.encode(dOn.value))
results.msEncodeDirect = Math.round(msEncOn)
ok('directEncodeEqualsFile', B(eOn).equals(fileBuf))

// 3. streaming encode (1MB chunks) reproduces the file, compared chunk by chunk
let off0 = 0, streamSame = true, chunks = 0
const [total, msStream] = time(() => onStream(dOn.value, (u8) => {
  chunks++
  if (streamSame && !Buffer.from(u8.buffer, u8.byteOffset, u8.length).equals(fileBuf.subarray(off0, off0 + u8.length))) streamSame = false
  off0 += u8.length
}, 1 << 20))
results.msStreamEncode = Math.round(msStream); results.streamChunks = chunks
ok('streamEqualsFile', streamSame && total === fileBuf.length && off0 === fileBuf.length)

// 4. baseline (Elm codecs) decode, and structural equality with the direct decode
const off = loadBundle(path.join(builds, app, 'off', 'backend-app.js'))
const [dOff, msDecOff] = time(() => off.decodeRaw(dv))
results.msDecodeElm = Math.round(msDecOff)
ok('elmDecodeOk', dOff.ok)
const cmp = (a, b) => off.utilsCmp(a, b)
const dictProblems = []
const hOn = structuralHash(dOn.value, { cmp, dictProblems, seqDictToList: off.seqDictToList })
const hOff = structuralHash(dOff.value, { seqDictToList: off.seqDictToList })
ok('decodedValuesEqual', hOn.hash === hOff.hash)
results.nodes = hOff.nodes; results.dictsChecked = hOn.dictsChecked
ok('directDictsValid', dictProblems.length === 0)
if (dictProblems.length) results.dictProblems = dictProblems

// 5. cross-checks: the Elm encoder on the direct-decoded value, and the direct encoder on the
// Elm-decoded value, both reproduce the file
const [eOff, msEncOff] = time(() => off.encode(dOn.value))
results.msEncodeElm = Math.round(msEncOff)
ok('elmEncodeOfDirectDecodeEqualsFile', B(eOff).equals(fileBuf))
ok('directEncodeOfElmDecodeEqualsFile', B(on.encode(dOff.value)).equals(fileBuf))

console.log(JSON.stringify(results))
process.exit(results.failed ? 1 : 0)
