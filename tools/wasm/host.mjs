// The host ABI for an ash wasm module, and the seed of Phase 3.
//
// Deliberately tiny: stdout, a clock, and random bytes. Everything a program
// needs beyond this — canvas, WebGL, audio, input — belongs to the embedder.
// Keeping it this small is what makes the same .wasm run under both wasmtime
// and a browser.
export function hostImports(getMemory) {
  const dec = new TextDecoder();
  let out = "";
  const readStr = (ptr, len) =>
    dec.decode(new Uint8Array(getMemory().buffer, ptr, len));
  return {
    env: {
      // Write `len` bytes at `ptr` to stdout.
      ash_host_write: (ptr, len) => {
        out += readStr(ptr, len);
        let nl;
        while ((nl = out.indexOf("\n")) >= 0) {
          console.log(out.slice(0, nl));
          out = out.slice(nl + 1);
        }
      },
      // Milliseconds since an arbitrary epoch, monotonic.
      ash_host_now: () => performance.now(),
      // Fill `len` bytes at `ptr` with randomness.
      ash_host_random: (ptr, len) => {
        crypto.getRandomValues(new Uint8Array(getMemory().buffer, ptr, len));
      },
      // Abort with a message; the module is not expected to resume.
      ash_host_abort: (ptr, len) => {
        throw new Error("ash abort: " + readStr(ptr, len));
      },
    },
  };
}

export async function instantiate(path) {
  const { readFile } = await import("node:fs/promises");
  const bytes = await readFile(path);
  let instance;
  const imports = hostImports(() => instance.exports.memory);
  ({ instance } = await WebAssembly.instantiate(bytes, imports));
  return instance;
}
