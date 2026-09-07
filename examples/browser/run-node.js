// Run a program through the BROWSER host, under node.
//
// Not a convenience: it is the only way to exercise this host in a test lane,
// since there is no headless browser here. The code path is identical -- the
// same 69 imports, the same instantiate, the same entrypoint -- and what
// differs is only what the global object offers, which is why `crypto` and
// `performance` are looked up there rather than on `window`.
//
//   cargo build --release -p ash_browser --target wasm32-unknown-unknown
//   wasm-bindgen --target nodejs --out-dir <dir> \
//     target/wasm32-unknown-unknown/release/ash_browser.wasm
//   node --experimental-wasm-exnref <dir>/run-node.js prog.wasm
//   node --experimental-wasm-exnref <dir>/run-node.js --agents 7 threads.wasm
//
// `--agents` is the threads path, and it is the same one a page takes: warm
// some agents, hand the host a `spawn` that posts to an idle one, and let the
// runtime ask for as many as it wants. node's `worker_threads` stand in for
// Workers, which is what makes that path testable without a browser -- the
// module, the shared memory and the entry point are identical, and only who
// makes the agent differs.
//
// The flag is not optional. ash's exception handling is `setjmp`, lowered
// into the standardised exception-handling instructions, so a module carries
// `exnref` and node refuses one without it. Browsers that have shipped the
// standardised proposal need no flag.

// A `SharedArrayBuffer` is unconditional here, where a page has to earn it
// with COOP and COEP. Saying so lets the host take the branches it takes in
// an isolated page rather than the ones it takes without one.
globalThis.crossOriginIsolated = true;

const fs = require("fs");
const nodePath = require("node:path");
const { Worker } = require("node:worker_threads");
const { run } = require("./ash_browser.js");

const argv = process.argv.slice(2);
let agents = 0;
const at = argv.indexOf("--agents");
if (at >= 0) {
  agents = Number(argv[at + 1]);
  argv.splice(at, 2);
}
const path = argv[0];
if (!path) {
  console.error(
    "usage: node --experimental-wasm-exnref run-node.js [--agents N] <prog.wasm> [args...]",
  );
  process.exit(2);
}

const idle = [];
const started = [];

// Before the program runs, and before it can ask for a thread: see
// `thread.js`. An agent made at the moment one is wanted is an agent whose
// creator is already blocked waiting for it.
function warmAgents(count) {
  const ready = [];
  for (let i = 0; i < count; i++) {
    const worker = new Worker(nodePath.join(__dirname, "run-node-agent.js"));
    let announce;
    ready.push(new Promise((resolve) => (announce = resolve)));
    worker.on("message", (message) => {
      if (message.ready) announce();
      idle.push(worker);
    });
    worker.on("error", (e) => console.error(`agent: ${e.message}`));
    started.push(worker);
  }
  return Promise.all(ready);
}

const spawn = (request) => {
  const worker = idle.pop();
  // False, not a throw: the host runs the thread on its scheduler instead.
  if (!worker) return false;
  worker.postMessage(request);
  return true;
};

(async () => {
  if (agents > 0) await warmAgents(agents);
  const bytes = new Uint8Array(fs.readFileSync(path));
  // A page has no environment to lend a program and this harness does, which
  // is the one place the two hosts differ on purpose: it is how a diagnostic
  // switch like ASH_GC_STATS reaches a guest that a browser could not be
  // asked to set one for.
  const environ = Object.entries(process.env)
    .filter(([name]) => name.startsWith("ASH_"))
    .map(([name, value]) => `${name}=${value}`);
  const outcome = await run(bytes, [path, ...argv.slice(1)], environ, agents > 0 ? spawn : undefined);
  for (const worker of started) worker.terminate();
  if (outcome.trapped) {
    console.error(`trapped: ${outcome.trapped}`);
    process.exit(1);
  }
  process.exit(outcome.status);
})();
