// Runs a compiled test/msg-log-context backend (LBR) under node with stand-ins for the
// Lamdera runtime globals, and checks the log context contract of the backend injection
// in extra/Lamdera/Injection.hs. Exit code 0 = all checks passed.
//
//   node harness.js <compiled.js> <scenario> [dev|prod]
//
// Scenarios:
//   hooks       runtime with lamderaMsgBegin (which does not touch lamderaCtx itself)
//   hooks-sets  runtime with lamderaMsgBegin that also sets global.lamderaCtx = id
//   nohooks     runtime without lamderaMsgBegin (older runtimes): everything inert
//   notruntime  no isLamdera at all (lamdera live, tests, ...): everything inert
//   bench       per-step overhead measurement, prints numbers, no checks

'use strict';

const path = require('path');
const [, , jsFile, scenario, mode] = process.argv;

const failures = [];
function check(cond, what) {
  if (!cond) { failures.push(what); }
}
function eq(actual, expected, what) {
  check(actual === expected, what + ': expected ' + JSON.stringify(expected) + ', got ' + JSON.stringify(actual));
}

// Everything observed, in order: { kind, label, ctx }
const seen = [];
const ctxNow = () => global.lamderaCtx;
const record = (kind, label) => seen.push({ kind, label, ctx: ctxNow(), at: seen.length });
const find = (kind, label) => seen.filter(s => s.kind === kind && s.label === label);
const one = (kind, label) => {
  const all = find(kind, label);
  check(all.length === 1, 'expected exactly one ' + kind + ' ' + label + ', got ' + all.length);
  return all[0] || { ctx: '<missing>' };
};

// RUNTIME STAND-INS

const begins = [];
const events = [];
let nextId = 100;

if (scenario !== 'notruntime') {
  global.isLamdera = true;
  global.loggingEnabled = true;
  global.config = { version: 7 };
  global.insertEvent = function () { events.push(Array.prototype.slice.call(arguments)); };
}
if (scenario === 'hooks' || scenario === 'bench') {
  global.lamderaMsgBegin = () => { const id = nextId++; begins.push(id); return id; };
}
if (scenario === 'hooks-sets') {
  global.lamderaMsgBegin = () => { const id = nextId++; begins.push(id); global.lamderaCtx = id; return id; };
}

const origLog = console.log;
console.log = function (line) { record('log', String(line)); };

// A fake XMLHttpRequest: records the context at send time, responds asynchronously
global.XMLHttpRequest = class {
  constructor() { this.listeners = {}; this.upload = { addEventListener() {} }; }
  addEventListener(type, fn) { this.listeners[type] = fn; }
  open(method, url) { this.url = url; }
  setRequestHeader() {}
  getAllResponseHeaders() { return ''; }
  abort() {}
  send() {
    record('xhr', this.url);
    setTimeout(() => {
      record('eventloop', 'xhr-response ' + this.url);
      this.status = 200;
      this.statusText = 'OK';
      this.responseURL = this.url;
      this.response = 'ok';
      this.listeners.load();
    }, 5);
  }
};

const probe = new Proxy({}, {
  has() { return true; },
  get(_, key) { if (typeof key === 'string') { record('probe', key); } return 0; },
});

const { Elm } = require(path.resolve(jsFile));

if (scenario === 'bench') {
  bench();
} else {
  run();
}

function run() {
  const app = Elm.LBR.init({ flags: probe });
  app.ports.out.subscribe(v => record('port', v));

  for (const tag of ['A', 'B', 'sync', 'nested']) {
    app.ports.trigger.send(tag);
    record('afterSend', tag);
  }

  setTimeout(() => {
    record('end', 'end');
    console.log = origLog;
    verify();
    if (failures.length > 0) {
      console.error('FAIL ' + scenario + ' ' + (mode || '') + '\n  ' + failures.join('\n  '));
      console.error(JSON.stringify(seen, null, 1));
      process.exit(1);
    }
    origLog('OK ' + scenario + ' ' + (mode || '') + ' (' + seen.length + ' observations, ' + events.length + ' events)');
  }, 300);
}

function verify() {
  // The app ran to completion in every scenario
  const expectedPorts = ['port:A', 'port:B', 'port:sync', 'port:http:A', 'port:http:B',
    'port:now:s1', 'port:now:s2', 'port:now:n1', 'port:now:n2'];
  for (const p of expectedPorts) { one('port', p); }
  for (const t of ['A', 'B']) {
    one('xhr', 'http://test/' + t);
    one('xhr', 'http://test/task-' + t);
    for (const i of [1, 2, 3]) { one('probe', 'chain:' + t + ':' + i); }
    one('probe', 'upd:chaindone:' + t);
  }

  const inert = scenario === 'nohooks' || scenario === 'notruntime';
  if (inert) {
    check(!('lamderaCtx' in global), 'lamderaCtx must never be set without lamderaMsgBegin');
    for (const s of seen) { check(s.ctx === undefined, 'ctx must stay undefined, ' + s.kind + ' ' + s.label + ' saw ' + JSON.stringify(s.ctx)); }
    if (scenario === 'nohooks') {
      check(events.length > 0, 'insertEvent still called');
      for (const e of events) { eq(e[6], null, 'insertEvent 7th argument without lamderaMsgBegin'); eq(e[1], 7, 'insertEvent version'); }
    } else {
      eq(events.length, 0, 'no insertEvent outside the runtime');
    }
    return;
  }

  // Every update got its own id, in update
  const updProbes = seen.filter(s => s.kind === 'probe' && s.label.startsWith('upd:'));
  eq(updProbes.length, begins.length, 'one lamderaMsgBegin per update');
  updProbes.forEach((s, i) => eq(s.ctx, begins[i], 'ctx in update ' + s.label));
  const mid = label => one('probe', 'upd:' + label).ctx;

  // insertEvent got the id as 7th argument, one event per update, pos kept
  eq(events.length, begins.length, 'one insertEvent per update');
  events.forEach((e, i) => {
    eq(e[6], begins[i], 'insertEvent 7th argument #' + i);
    eq(e[0], i + 1, 'insertEvent pos #' + i);
    eq(e[1], 7, 'insertEvent version');
  });

  // Debug.log in update (dev build only, Debug is not allowed with --optimize)
  if (mode === 'dev') {
    for (const t of ['A', 'B', 'sync', 'nested']) {
      const logs = seen.filter(s => s.kind === 'log' && s.label.startsWith('dbg:upd:' + t + ':'));
      eq(logs.length, 1, 'one Debug.log for ' + t);
      if (logs[0]) { eq(logs[0].ctx, mid(t), 'Debug.log ctx in update ' + t); }
    }
  }

  for (const t of ['A', 'B']) {
    const cmd = mid(t) + ':cmd';
    eq(one('port', 'port:' + t).ctx, cmd, 'port Cmd of ' + t);
    eq(one('xhr', 'http://test/' + t).ctx, cmd, 'Http Cmd send of ' + t);
    eq(one('xhr', 'http://test/task-' + t).ctx, cmd, 'Http.task send of ' + t);
    for (const i of [1, 2, 3]) {
      eq(one('probe', 'chain:' + t + ':' + i).ctx, cmd, 'Task chain step ' + i + ' of ' + t);
    }
    // The response msgs get their own ids, and their Cmds their own contexts
    const httpMid = mid('http:' + t);
    check(typeof httpMid === 'number' && httpMid !== mid(t), 'Http response msg of ' + t + ' has its own id');
    eq(one('port', 'port:http:' + t).ctx, httpMid + ':cmd', 'port Cmd of the Http response msg of ' + t);
    const doneMid = mid('chaindone:' + t);
    check(typeof doneMid === 'number' && doneMid !== mid(t), 'chain result msg of ' + t + ' has its own id');
  }

  // The two chains really interleaved: B's first sleep is shorter
  const at = label => one('probe', label).at;
  check(at('chain:B:2') < at('chain:A:2') && at('chain:A:2') < at('chain:B:3'), 'chains A and B interleave');

  // Effects queued behind the ones being dispatched keep their own msg's context
  eq(one('port', 'port:sync').ctx, mid('sync') + ':cmd', 'port Cmd of sync');
  for (const t of ['s1', 's2', 'n1', 'n2']) {
    const m = mid('now:' + t);
    check(typeof m === 'number', 'Time.now msg ' + t + ' has an id');
    eq(one('port', 'port:now:' + t).ctx, m + ':cmd', 'port Cmd of Time.now msg ' + t);
  }
  check(mid('now:s1') !== mid('now:s2') && mid('now:n1') !== mid('now:n2'), 'Time.now msgs have distinct ids');

  // Outside of all that (event loop, between port sends) the context is back to unset
  for (const s of seen.filter(s => s.kind === 'afterSend' || s.kind === 'eventloop' || s.kind === 'end')) {
    check(s.ctx === undefined || s.ctx === null, 'ctx restored at ' + s.kind + ' ' + s.label + ', got ' + JSON.stringify(s.ctx));
  }
  check(global.lamderaCtx === undefined || global.lamderaCtx === null, 'ctx restored at the end');
}

// BENCH: msgs through a port (each "sync" trigger runs 3 updates, 3 effect dispatches and
// two Task processes). Counts scheduler steps when the build exposes them.
function bench() {
  global.insertEvent = () => {};
  const app = Elm.LBR.init({ flags: {} });
  app.ports.out.subscribe(() => {});
  // let the outgoing port manager finish its initial Process.sleep 0 first
  setTimeout(() => {
    const N = +(process.env.BENCH_N || 20000);
    for (let i = 0; i < 2000; i++) { app.ports.trigger.send('sync'); } // warm up
    const t0 = process.hrtime.bigint();
    for (let i = 0; i < N; i++) { app.ports.trigger.send('sync'); }
    const t1 = process.hrtime.bigint();
    origLog(JSON.stringify({ triggers: N, msgs: N * 3, ms: Number(t1 - t0) / 1e6 }));
    process.exit(0);
  }, 10);
}
