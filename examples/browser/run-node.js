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
//
// The flag is not optional. ash's exception handling is `setjmp`, lowered
// into the standardised exception-handling instructions, so a module carries
// `exnref` and node refuses one without it. Browsers that have shipped the
// standardised proposal need no flag.

const fs = require("fs");
const { run } = require("./ash_browser.js");

const path = process.argv[2];
if (!path) {
  console.error("usage: node --experimental-wasm-exnref run-node.js <prog.wasm> [args...]");
  process.exit(2);
}

(async () => {
  const bytes = new Uint8Array(fs.readFileSync(path));
  const outcome = await run(bytes, [path, ...process.argv.slice(3)], []);
  if (outcome.trapped) {
    console.error(`trapped: ${outcome.trapped}`);
    process.exit(1);
  }
  process.exit(outcome.status);
})();
