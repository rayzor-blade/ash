// The module runs here, not on the page's thread.
//
// A Haxe program's main loop does not return, and a fiber that computes
// without blocking never yields -- on the page's thread either of those is a
// frozen tab. In a worker they are not: the UI thread is free the whole time,
// and the worst a runaway fiber can do is stall this worker.
//
// It buys a second thing that matters more than it looks. `memory.atomic.wait`
// TRAPS on a browser's main thread and is allowed in a worker, so blocking
// synchronisation is only ever possible here.
//
// Output is posted to the page rather than left in the console, because a
// worker's console is not the page's document.

import init, { run } from "./ash_browser.js";

const post = (kind, text) => self.postMessage({ kind, text });

// The host writes through console.log and console.error. In a worker those
// reach the browser's console but nothing the page can show, so they are
// forwarded as well as kept.
for (const [name, kind] of [["log", "out"], ["error", "err"]]) {
  const original = console[name].bind(console);
  console[name] = (...args) => {
    original(...args);
    post(kind, args.join(" "));
  };
}

self.onmessage = async (event) => {
  const { module, args } = event.data;
  try {
    await init();
    const response = await fetch(module);
    if (!response.ok) throw new Error(`fetching ${module}: ${response.status}`);
    const bytes = new Uint8Array(await response.arrayBuffer());
    post("meta", `loaded ${module}, ${bytes.length.toLocaleString()} bytes`);

    const started = performance.now();
    const outcome = await run(bytes, args ?? [module], []);
    const took = Math.round(performance.now() - started);

    if (outcome.trapped) post("err", `trapped: ${outcome.trapped}`);
    else post("meta", `exited with ${outcome.status}, in ${took}ms`);
    self.postMessage({ kind: "done", status: outcome.status, trapped: outcome.trapped });
  } catch (e) {
    post("err", String(e && e.message ? e.message : e));
    self.postMessage({ kind: "done", status: -1, trapped: String(e) });
  }
};
