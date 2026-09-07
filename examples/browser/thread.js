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

import init, { run_thread } from "./ash_browser.js";

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

self.onmessage = async (event) => {
  const { tid, startArg, module, memory, args, environ } = event.data;
  const started = performance.now();
  try {
    await ready;
    post({ kind: "meta", text: `thread ${tid}: agent entering` });
    await run_thread(module, memory, tid, startArg, args ?? [], environ ?? [], undefined, event.data.control);
    post({
      kind: "meta",
      text: `thread ${tid}: returned after ${Math.round(performance.now() - started)}ms`,
    });
  } catch (e) {
    post({ kind: "err", text: `thread ${tid}: ${e && e.message ? e.message : e}` });
  } finally {
    // Idle again, and reusable: a program with more threads than agents runs
    // them as agents come free, which is what a thread pool is.
    post({ kind: "agent", state: "idle" });
  }
};
