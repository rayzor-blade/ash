// Run an ash wasm module under node: `node run.mjs <module.wasm> [export] [args...]`
import { instantiate } from "./host.mjs";

const [, , path, name = "hl_entry", ...rest] = process.argv;
if (!path) {
  console.error("usage: run.mjs <module.wasm> [export] [args...]");
  process.exit(2);
}
const instance = await instantiate(path);
const fn = instance.exports[name];
if (typeof fn !== "function") {
  console.error(
    `no export '${name}'. available: ${Object.keys(instance.exports).join(", ")}`,
  );
  process.exit(1);
}
const result = fn(...rest.map(Number));
if (result !== undefined) console.log(String(result));
