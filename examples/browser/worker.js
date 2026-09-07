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

// The agents Haxe threads run on, started before the program is.
//
// They have to exist first. Creating a Worker needs the creating agent to
// return to its event loop, and the agent that asks for a thread is inside a
// synchronous call into wasm which will not return until that thread has
// answered -- so a Worker created at that moment never loads and the program
// waits forever. Posting to a Worker that is already running has no such
// problem, which is why they are warmed here and only messaged later.
//
// This is the same reason Emscripten sizes a pool up front, and it is the one
// place a browser really does bound what "as many threads as you like" means:
// the bound is how many agents the page warmed, not anything the runtime
// asked for. The runtime asks for one agent per thread and takes what it gets
// -- a thread with no agent free runs on the main scheduler.
const idle = [];
const agents = [];

function warmAgents(count) {
  const ready = [];
  for (let i = 0; i < count; i++) {
    const worker = new Worker(new URL("./thread.js", import.meta.url), { type: "module" });
    let announce;
    ready.push(new Promise((resolve) => (announce = resolve)));
    worker.onmessage = ({ data }) => {
      if (data.kind !== "agent") {
        self.postMessage(data);
        return;
      }
      if (data.state === "ready") announce();
      idle.push(worker);
    };
    worker.onerror = (e) => post("err", `agent: ${e.message}`);
    agents.push(worker);
  }
  return Promise.all(ready);
}

// What the host calls to ask for one. It hands over everything the agent
// needs -- the compiled module, the shared memory, the thread id and the
// argument the guest prepared -- and keeps only what it cannot delegate,
// which is handing out the id.
//
// False when there is none free. The host takes that as an answer rather than
// a failure and runs the thread on the scheduler, so a page that warmed fewer
// agents than the program asks for still runs it.
const spawn = (request) => {
  const worker = idle.pop();
  if (!worker) {
    post("meta", `thread ${request.tid}: no idle agent, running on the main scheduler`);
    return false;
  }
  post("meta", `thread ${request.tid}: handed to an agent (${idle.length} left idle)`);
  worker.postMessage(request);
  return true;
};

// Where a frame goes, and it is not here. HashLink runs in this worker, which
// is inside the call into the program for as long as the program runs and so
// never returns to its event loop -- and a canvas reaches the page at the end
// of a task. Painting from here produced exactly one frame, after every
// thread had finished.
//
// The framebuffer is in shared memory, so nothing has to be copied out of it
// on this thread. This describes it to the page once, the page forwards that
// to `display.js`, and that agent paints from it at display rate while the
// program keeps drawing. Repeating the description every frame would be
// pointless -- it is the same buffer -- so only a change is sent.
let display = false;
let described = "";
self.ashPresent = ({ memory, address, width, height }) => {
  if (!display) return false;
  const key = `${address}:${width}:${height}`;
  if (key !== described) {
    try {
      self.postMessage({ kind: "display", memory, address, width, height });
    } catch (e) {
      // A memory that is not shared cannot be handed to another agent, which
      // is a single-threaded module: it has no display here.
      display = false;
      post("meta", `no display: ${e.message}`);
      return false;
    }
    described = key;
  }
  return true;
};

self.onmessage = async (event) => {
  const { module, args, environ, display: wanted } = event.data;
  display = !!wanted;
  try {
    await init();
    // Before the program runs, and before it can ask for a thread.
    const wanted = Math.max(1, (navigator.hardwareConcurrency || 4) - 1);
    await warmAgents(wanted);
    post("meta", `${wanted} agents ready`);
    const response = await fetch(module);
    if (!response.ok) throw new Error(`fetching ${module}: ${response.status}`);
    const bytes = new Uint8Array(await response.arrayBuffer());
    post("meta", `loaded ${module}, ${bytes.length.toLocaleString()} bytes`);

    const started = performance.now();
    const outcome = await run(bytes, args ?? [module], environ ?? [], spawn);
    const took = Math.round(performance.now() - started);

    if (outcome.trapped) post("err", `trapped: ${outcome.trapped}`);
    else post("meta", `exited with ${outcome.status}, in ${took}ms`);
    self.postMessage({ kind: "done", status: outcome.status, trapped: outcome.trapped });
  } catch (e) {
    post("err", String(e && e.message ? e.message : e));
    self.postMessage({ kind: "done", status: -1, trapped: String(e) });
  } finally {
    // The run owns every agent, even one still executing after a sibling's
    // fatal exit. No guest context may outlive its program.
    for (const worker of agents) worker.terminate();
    agents.length = 0;
    idle.length = 0;
  }
};
