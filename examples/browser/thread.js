// One Haxe thread, in a Worker of its own.
//
// A thread on wasm is a second instance of the same module over the same
// memory. Both arrive here already made: the module compiled once by the
// worker that started this one, and the memory shared, which is the whole
// point -- this instance sees the first one's heap.
//
// It is entered through `wasi_thread_start` and not through `main`. Where the
// thread goes from there was decided by the guest's own `pthread_create`,
// which wrote the stack it allocated and the closure to run into `startArg`
// before asking for a thread at all.

import init, { run_thread } from "./ash_browser.js";

const post = (kind, text) => self.postMessage({ kind, text });

for (const [name, kind] of [["log", "out"], ["error", "err"]]) {
  const original = console[name].bind(console);
  console[name] = (...args) => {
    original(...args);
    post(kind, args.join(" "));
  };
}

// A thread may start a thread, so it is given the same way to.
const spawn = (request) => {
  const worker = new Worker(new URL("./thread.js", import.meta.url), { type: "module" });
  worker.onmessage = (event) => self.postMessage(event.data);
  worker.postMessage(request);
};

self.onmessage = async (event) => {
  const { tid, startArg, module, memory, args, environ } = event.data;
  try {
    await init();
    await run_thread(module, memory, tid, startArg, args ?? [], environ ?? [], spawn);
  } catch (e) {
    post("err", `thread ${tid}: ${e && e.message ? e.message : e}`);
  } finally {
    // The thread has returned; wasi-libc has already told whoever joins it.
    self.close();
  }
};
