// One agent for `run-node.js --agents N`, and the node counterpart of
// `thread.js`. See that file for why an agent has to exist before it is
// needed.
globalThis.crossOriginIsolated = true;

const fs = require("node:fs");
const { parentPort } = require("node:worker_threads");
const { run_thread } = require("./ash_browser.js");

// Straight at the process's own stderr, not through `console`.
//
// An agent's output normally reaches the terminal through the parent's
// streams, and the parent here is inside a synchronous call into the program
// that does not return until the program ends. It never runs its event loop,
// so nothing piped through it is ever delivered -- and if the program hangs,
// which is the case this harness exists to look at, it is never delivered at
// all. Every line an agent printed was silently dropped, including the
// collector's, which made a hung run look like a run with nothing to say.
//
// `fs.writeSync` on fd 2 is the same descriptor the process was started with:
// a worker is a thread, not a child, so this needs no forwarding and no event
// loop on either side.
const toStderr = (...parts) => {
  try {
    fs.writeSync(2, parts.join(" ") + "\n");
  } catch (e) {
    // A closed descriptor at shutdown. Losing the line is better than
    // throwing out of a log call.
  }
};
console.log = toStderr;
console.error = toStderr;
console.warn = toStderr;

parentPort.postMessage({ ready: true });
parentPort.on("message", async ({ tid, startArg, module, memory, args, environ, control }) => {
  try {
    await run_thread(module, memory, tid, startArg, args ?? [], environ ?? [], undefined, control);
  } catch (e) {
    console.error(`thread ${tid}: ${e && e.message ? e.message : e}`);
  } finally {
    parentPort.postMessage({ idle: true });
  }
});
