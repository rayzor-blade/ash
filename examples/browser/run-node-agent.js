// One agent for `run-node.js --agents N`, and the node counterpart of
// `thread.js`. See that file for why an agent has to exist before it is
// needed.
globalThis.crossOriginIsolated = true;

const { parentPort } = require("node:worker_threads");
const { run_thread } = require("./ash_browser.js");

parentPort.postMessage({ ready: true });
parentPort.on("message", async ({ tid, startArg, module, memory, args, environ }) => {
  try {
    await run_thread(module, memory, tid, startArg, args ?? [], environ ?? []);
  } catch (e) {
    console.error(`thread ${tid}: ${e && e.message ? e.message : e}`);
  } finally {
    parentPort.postMessage({ idle: true });
  }
});
