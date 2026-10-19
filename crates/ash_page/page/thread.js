// One agent: a Worker that runs Haxe threads, one at a time.
//
// It is started before the program is, and idles until given work. That is
// not a warm-up optimisation, it is the only order that works: creating a
// Worker needs the creating agent to return to its event loop, and the agent
// asking for a thread is inside a synchronous call into wasm that will not
// return until the thread it is asking for has answered. Create on demand and
// nothing ever loads. (Emscripten's PTHREAD_POOL_SIZE exists for this.)
//
// A thread itself is a second instance of the module over the same memory.
// Both arrive here already made: the module compiled once by the worker that
// runs the program, and the memory shared, which is the whole point -- this
// instance sees that one's heap. It is entered through `wasi_thread_start`,
// not through `main`: where it goes was decided by the guest's own
// `pthread_create`, which wrote the stack it allocated and the closure to run
// into `startArg` before asking for an agent at all.

import init, { run_thread, prepare_program, prepare_library } from "./ash_browser.js";

const post = (message) => self.postMessage(message);

for (const [name, kind] of [["log", "out"], ["error", "err"]]) {
  const original = console[name].bind(console);
  console[name] = (...args) => {
    original(...args);
    post({ kind, text: args.join(" ") });
  };
}

// Loading the host is the slow part and it is done now, while nothing is
// waiting. What is left when a thread arrives is instantiating the module.
const ready = init().then(() => post({ kind: "agent", state: "ready" }));

// Where thread requests arrive and where this agent says it is idle again:
// the port the page hands over when the page made this agent, or this
// Worker's own channel when worker.js did. Output always goes to whoever
// created the agent.
let channel = self;

// A thread to run, or a module to prepare: the page's worker borrows an idle
// agent to read, rewrite and compile what it has downloaded, so that work
// happens beside the program's own downloads and not on the thread that is
// still reading them.
const dispatch = (request) => (request.kind === "prepare" ? prepare(request) : runThread(request));

self.onmessage = (event) => {
  if (event.data.port) {
    channel = event.data.port;
    channel.onmessage = (e) => dispatch(e.data);
    return;
  }
  dispatch(event.data);
};

// The bytes come back with the answer, so a module that will not prepare can
// still be handed to the host as bytes, which says why when it loads it.
async function prepare({ id, what, bytes, threads }) {
  try {
    await ready;
    const view = new Uint8Array(bytes);
    const prepared = what === "program" ? prepare_program(view) : prepare_library(view, threads);
    channel.postMessage({ kind: "prepared", id, prepared, bytes }, [bytes]);
  } catch (e) {
    channel.postMessage({ kind: "prepared", id, error: String(e && e.message ? e.message : e), bytes }, [bytes]);
  } finally {
    channel.postMessage({ kind: "agent", state: "idle" });
  }
}

async function runThread(request) {
  const { tid, startArg, module, memory, args, environ, libraries } = request;
  const started = performance.now();
  try {
    await ready;
    post({ kind: "meta", text: `thread ${tid}: agent entering` });
    // `libraries` is how the program's native libraries were laid out, so this
    // instance can load the same ones at the same table indices.
    await run_thread(module, memory, tid, startArg, args ?? [], environ ?? [], undefined, request.control, libraries);
    post({
      kind: "meta",
      text: `thread ${tid}: returned after ${Math.round(performance.now() - started)}ms`,
    });
  } catch (e) {
    post({ kind: "err", text: `thread ${tid}: ${e && e.message ? e.message : e}` });
  } finally {
    // Idle again, and reusable: a program with more threads than agents runs
    // them as agents come free, which is what a thread pool is.
    channel.postMessage({ kind: "agent", state: "idle" });
  }
}
