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

import init, { run, prepare_program, prepare_library } from "./ash_browser.js";

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
//
// The page normally makes them and passes one end of a MessageChannel per
// agent: some browsers (Chrome on Android) cannot start a Worker from inside a
// Worker at all. Without ports, this worker makes its own.
const idle = [];
const agents = [];

// Modules an agent has been asked to prepare, by request number.
const preparing = new Map();
let prepareCount = 0;

function answerPrepare(data) {
  const done = preparing.get(data.id);
  preparing.delete(data.id);
  if (done) done(data);
}

// Ask an idle agent to read, rewrite and compile what was downloaded, so the
// work runs beside the other downloads and not on this thread, which has to
// keep reading them. The answer carries `prepared` (or an `error`) and the
// bytes back. Nothing when no agent is free; the caller prepares it here.
function prepareOnAgent(what, bytes, threads) {
  const agent = idle.pop();
  if (!agent) return undefined;
  const id = ++prepareCount;
  return new Promise((resolve) => {
    preparing.set(id, resolve);
    agent.postMessage({ kind: "prepare", what, id, bytes: bytes.buffer, threads }, [bytes.buffer]);
  });
}

function adoptAgents(ports) {
  for (const port of ports) {
    port.onmessage = ({ data }) => {
      if (data.kind === "prepared") answerPrepare(data);
      else if (data.kind === "agent" && data.state === "idle") idle.push(port);
    };
    idle.push(port);
  }
  return ports.length;
}

async function warmAgents(count) {
  const ready = [];
  for (let i = 0; i < count; i++) {
    const worker = new Worker(new URL("./thread.js", import.meta.url), { type: "module" });
    let settle;
    ready.push(new Promise((resolve) => (settle = resolve)));
    worker.onmessage = ({ data }) => {
      if (data.kind === "prepared") {
        answerPrepare(data);
        return;
      }
      if (data.kind !== "agent") {
        self.postMessage(data);
        return;
      }
      if (data.state === "ready") settle(true);
      idle.push(worker);
    };
    // Settled, not left pending: an agent that never starts must not keep the
    // program from starting. Its error has no message; the likeliest cause is
    // a browser that cannot start a module Worker from inside a Worker.
    worker.onerror = (e) => {
      post("meta", `agent: ${e.message || "thread.js did not start (module Workers inside a Worker unsupported?)"}`);
      settle(false);
    };
    agents.push(worker);
  }
  return (await Promise.all(ready)).filter(Boolean).length;
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

// A service the program wants running beside it (`ash_host_agent`), started
// by the page -- which loads the library's `<name>.mjs` shim -- for the same
// reason display.js is: this agent never returns to its event loop, so it
// cannot create a Worker. The host has already checked the name is a plain
// identifier and the memory is shared.
self.ashAgent = ({ name, memory, address }) => {
  try {
    self.postMessage({ kind: "spawn-agent", name, memory, address });
    return true;
  } catch (e) {
    post("meta", `agent ${name}: ${e.message}`);
    return false;
  }
};

// The native libraries beside the program, wasm side modules `ash --build`
// listed in `libraries.json`, fetched before the program runs: a guest that
// asks for a library is inside a synchronous call and cannot wait for one.
// No manifest is a program without libraries.
//
// Every library is requested as soon as the manifest names it, together and
// alongside the program's own download, not after it. `since` stamps each
// step with the milliseconds since the load began.
//
// Each is also read, rewritten and compiled the moment its bytes are in
// (`ready` is the host finishing its own start-up, which that needs), so the
// work happens while the rest of the program is still downloading instead of
// after the last byte. A library that will not prepare is handed on as bytes,
// and the host reports why when it loads it.
async function fetchLibraries(module, since, ready) {
  const base = new URL(module, self.location.href);
  let names = [];
  try {
    const listed = await fetch(new URL("libraries.json", base));
    if (listed.ok) names = await listed.json();
  } catch {
    return undefined;
  }
  if (!Array.isArray(names) || names.length === 0) return undefined;
  // A program built with threads needs shared memory, which only a page that
  // is cross-origin isolated has; the module a thread's instance uses is
  // made now for any page that could run one.
  const threads = self.crossOriginIsolated === true;
  const fetched = await Promise.all(
    names.map(async (name) => {
      const response = await fetch(new URL(`${name}.wasm`, base));
      if (!response.ok) {
        post("err", `fetching library ${name}: ${response.status}`);
        return undefined;
      }
      const bytes = new Uint8Array(await response.arrayBuffer());
      post("meta", `loaded library ${name}, ${bytes.length.toLocaleString()} bytes, at ${since()}`);
      const size = bytes.length;
      return [name, await prepared("library", name, bytes, threads, ready, since, size)];
    }),
  );
  // Listed order, whichever finished first.
  return Object.fromEntries(fetched.filter(Boolean));
}

// A downloaded module, prepared on an idle agent, or here when there is none,
// and handed on as its bytes when it will not prepare: the host reports why
// when it loads it.
async function prepared(what, name, bytes, threads, ready, since, size) {
  await ready;
  const began = performance.now();
  const done = (where) =>
    post("meta", `prepared ${what} ${name} (${size.toLocaleString()} bytes) ${where} in ${Math.round(performance.now() - began)}ms, at ${since()}`);
  const asked = prepareOnAgent(what, bytes, threads);
  if (asked) {
    const answer = await asked;
    if (answer.error === undefined) {
      done("on an agent");
      return answer.prepared;
    }
    post("meta", `preparing ${what} ${name}: ${answer.error}; handing it on as bytes`);
    return new Uint8Array(answer.bytes);
  }
  try {
    const result = what === "program" ? prepare_program(bytes) : prepare_library(bytes, threads);
    done("here");
    return result;
  } catch (e) {
    post("meta", `preparing ${what} ${name}: ${e && e.message ? e.message : e}; handing it on as bytes`);
    return bytes;
  }
}

self.onmessage = async (event) => {
  const { module, args, environ, display: wanted, agents: ports } = event.data;
  display = !!wanted;
  try {
    // Every download starts before anything is awaited, so the host's own
    // module, the program and the libraries share the connection and the
    // round trips overlap. The host's goes first: nothing runs without it.
    const begun = performance.now();
    const since = () => `${Math.round(performance.now() - begun)}ms`;
    post("meta", `fetching ${module} and its libraries`);
    const initialising = init();
    // The agents the work below borrows are made once the host has started.
    const agentsReady = (async () => {
      await initialising;
      post("meta", `host ready at ${since()}`);
      // Before the program runs, and before it can ask for a thread.
      const count = Array.isArray(ports)
        ? adoptAgents(ports)
        : await warmAgents(Math.max(1, (navigator.hardwareConcurrency || 4) - 1));
      post("meta", `${count} agents ready at ${since()}`);
      return count;
    })();
    // The body is read as it arrives, not when the program gets to it: a
    // response nobody reads stalls the download behind it.
    const programRequest = fetch(module).then(async (response) => {
      if (!response.ok) throw new Error(`fetching ${module}: ${response.status}`);
      const body = new Uint8Array(await response.arrayBuffer());
      post("meta", `loaded ${module}, ${body.length.toLocaleString()} bytes, at ${since()}`);
      return body;
    });
    const libraryRequest = fetchLibraries(module, since, agentsReady);
    // Settled by the awaits below; this only keeps an early failure from
    // being reported twice.
    for (const pending of [initialising, agentsReady, programRequest, libraryRequest]) {
      pending.catch(() => {});
    }

    await agentsReady;
    const bytes = await programRequest;
    // Rewritten and compiled now, before the libraries are waited for.
    const program = await prepared("program", module, bytes, false, agentsReady, since, bytes.length);
    const libraries = await libraryRequest;
    post("meta", `starting the program at ${since()}`);

    const started = performance.now();
    const outcome = await run(program, args ?? [module], environ ?? [], spawn, libraries);
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
    // Agents the page made are the page's to stop, once it sees "done".
    for (const worker of agents) worker.terminate();
    agents.length = 0;
    idle.length = 0;
  }
};
