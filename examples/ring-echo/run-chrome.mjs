// Run the built example in a real browser: serve out/ with `ash serve`, open it
// in headless Chrome over the DevTools protocol, and wait for the page to say
// it is done. Exit 0 only if the program printed "ring-echo ok".
//
//     node examples/ring-echo/run-chrome.mjs [path to ash] [path to Chrome]
//
// Node 22 or later (global fetch and WebSocket).
import { spawn } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const root = join(here, "../..");
const ash = process.argv[2] ?? join(root, "target/release/ash");
const chrome =
  process.argv[3] ?? "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const webPort = 8741;
const debugPort = 9341;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const profile = mkdtempSync(join(tmpdir(), "ring-echo-"));
const server = spawn(ash, ["serve", process.env.CHANNEL_TEST_DIR ?? join(here, "out"), "--port", String(webPort)], {
  stdio: "ignore",
});
const browser = spawn(
  chrome,
  [
    "--headless=new",
    `--remote-debugging-port=${debugPort}`,
    `--user-data-dir=${profile}`,
    "--no-first-run",
    "--disable-gpu",
    "about:blank",
  ],
  { stdio: "ignore" },
);
const cleanup = () => {
  server.kill();
  browser.kill();
  try {
    rmSync(profile, { recursive: true, force: true });
  } catch {}
};

let exit = 1;
try {
  let target;
  for (let i = 0; i < 100 && !target; i++) {
    try {
      const response = await fetch(
        `http://127.0.0.1:${debugPort}/json/new?http://localhost:${webPort}/?${process.env.CHANNEL_TEST_QUERY ?? ""}`,
        { method: "PUT" },
      );
      if (response.ok) target = await response.json();
    } catch {
      await sleep(200);
    }
  }
  if (!target) throw new Error("Chrome did not start");

  const socket = new WebSocket(target.webSocketDebuggerUrl);
  await new Promise((resolve, reject) => {
    socket.onopen = resolve;
    socket.onerror = reject;
  });
  let nextId = 1;
  const waiting = new Map();
  const console_ = [];
  socket.onmessage = (event) => {
    const message = JSON.parse(event.data);
    if (message.id && waiting.has(message.id)) {
      waiting.get(message.id)(message);
      waiting.delete(message.id);
    } else if (message.method === "Runtime.consoleAPICalled") {
      console_.push(message.params.args.map((a) => a.value ?? a.description).join(" "));
    } else if (message.method === "Runtime.exceptionThrown") {
      console_.push("exception: " + (message.params.exceptionDetails.exception?.description ?? "?"));
    }
  };
  const call = (method, params = {}) =>
    new Promise((resolve) => {
      const id = nextId++;
      waiting.set(id, resolve);
      socket.send(JSON.stringify({ id, method, params }));
    });
  await call("Runtime.enable");

  const deadline = Date.now() + 90_000;
  let title = "";
  let out = "";
  while (Date.now() < deadline) {
    const state = await call("Runtime.evaluate", {
      expression: `JSON.stringify({ title: document.title, out: document.getElementById("out")?.textContent ?? "", isolated: self.crossOriginIsolated })`,
      returnByValue: true,
    });
    try {
      const value = JSON.parse(state.result.result.value);
      title = value.title;
      out = value.out;
      if (/done|trapped/.test(title)) {
        console.log(`crossOriginIsolated: ${value.isolated}`);
        break;
      }
    } catch {}
    await sleep(500);
  }
  console.log(`title: ${title}`);
  console.log(out.trim());
  if (console_.length) console.log("--- console\n" + console_.join("\n"));
  exit = /done/.test(title) && out.includes(process.env.CHANNEL_TEST_EXPECT ?? "ring-echo ok") ? 0 : 1;
  console.log(exit === 0 ? "PASS" : "FAIL");
} catch (e) {
  console.log("error: " + e.message);
} finally {
  cleanup();
}
process.exit(exit);
