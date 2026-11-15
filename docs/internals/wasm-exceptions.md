# wasm: exceptions and setjmp

ash's trap model is `setjmp`/`longjmp`. On wasm a throw is still a
`longjmp`, which the exception-handling proposal carries as a throw of the
`__c_longjmp` tag. Compiled Haxe code does not call `setjmp`. A `try` is a
region that the direct backend wraps in `try_table` handlers (next section),
and so is the outermost boundary around `main`. The only `setjmp` calls left
are the runtime's own, which rustc builds with LLVM's SJLJ lowering (last
section).

## Haxe try lowering

`crates/ash_wasm_emit/src/traps.rs`. Entering a `try` arms the trap in the
runtime's chain as everywhere else (`hlp_setup_trap_jit`), keeps the returned
buffer in a local, writes the buffer's own address into its first word, and
writes -1 into the second word, the label `setjmp` keeps there, so the buffer
cannot match a frame lowered by `setjmp`. Every instruction in the region that
can throw runs inside a `try_table` that catches `__c_longjmp`. The tag's
payload points to a pair: the buffer the jump was thrown to, and a value. The
catch compares the first word of that buffer with the local. A match restores the body's stack
pointer and branches to the handler block, which disarms the trap and stores
the exception in the cell the region wrote it to. Anything else is thrown on,
with the same buffer and value, by `__wasm_longjmp`: inside the enclosing
trap's `try_table`, or out to the caller if there is none. An export's
boundary (`AotRequest::exports`) is the same handler.

`throw` and `rethrow` call `hlp_throw` and `hlp_rethrow`, which do not return.

- **The first word, not the address.** The runtime's throw jumps with a copy
  of the buffer, so the copy's address never matches the local, but the word
  stored inside it does.
- **Thrown anew, not rethrown.** A rethrow keeps the caught exception in an
  `exnref` local, and the fiber transform refuses to resume a call while one
  might be live: the threads suite trapped. The jump goes on through
  `__wasm_longjmp`, which throws the tag again.
- **Entering a region costs no `setjmp`**: it is a call to
  `hlp_setup_trap_jit` and two stores.

## The runtime's setjmp lowering

The runtime is C and Rust, and it calls `setjmp`. WASI's `libsetjmp.a` defines
`__wasm_setjmp`, `__wasm_setjmp_test` and `__wasm_longjmp` instead of
`_setjmp`/`longjmp`; these are the forms LLVM's WebAssembly SJLJ pass rewrites
a `setjmp` call into. rustc builds `ash_std` with that pass on, through the
per-target `rustflags` in `.cargo/config.toml`:
`-Ctarget-feature=+exception-handling`, `-Cllvm-args=-wasm-enable-sjlj` and
`-Cllvm-args=-wasm-use-legacy-eh=false`. The last one selects the standardised
`try_table` encoding, because LLVM still defaults to the withdrawn
`try`/`catch`, which no current engine accepts.

Both halves of the rewrite are needed. An object that references
`__wasm_setjmp` and not `__wasm_setjmp_test` links and runs, and prints
"thrown Wasm exception" instead of catching.
