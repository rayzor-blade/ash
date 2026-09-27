# wasm: exceptions and setjmp

ash's trap model is `setjmp`/`longjmp`. On wasm a throw is still a
`longjmp`, which the exception-handling proposal carries as a throw of the
`__c_longjmp` tag. A Haxe `try` in compiled code is not a `setjmp` there: it
is an exception handler (next section). The `setjmp`s left are the runtime's
own and the outermost boundary around `main`, and those go through LLVM's
SJLJ lowering, where two halves have to agree.

## A Haxe try is a handler

`crates/ash/src/llvm/function/wasm_traps.rs`. Entering a `try` arms the trap
in the runtime's chain as everywhere else (`hlp_setup_trap_jit`), writes the
buffer's own address into its first word, and keeps the buffer in a stack
slot. Every call inside the region is an `invoke` whose unwind edge goes to
the trap's catch block, which catches `__c_longjmp`, reads the first word of
the thrown buffer and compares it with the slot. A match leaves the catch and
runs the handler; anything else is thrown on, with the same buffer, to the
enclosing trap's catch block or the caller. An export's boundary
(`AotRequest::exports`) is the same handler.

- **The first word, not the address.** The runtime's throw jumps with a copy
  of the buffer, so the address never matches; the word inside it does.
- **One catch block per trap.** With one per function, a handler's own calls
  unwind into the catch block that just caught, and the backend emits code
  that does not validate for that shape.
- **Thrown anew, not rethrown.** A rethrow keeps the caught exception in an
  `exnref` local, and the fiber transform refuses to resume a call while one
  might be live: the threads suite trapped. The jump goes on through
  `ash_trap_pass`, which calls `longjmp` again.
- **Entry costs no `setjmp`**, and the exceptional edges are real CFG edges,
  so a function holding a `try` is optimized like any other; natively it is
  kept out of the middle end.
- **`-wasm-enable-eh`** is needed alongside `-wasm-enable-sjlj`: without it
  the backend drops every `invoke`'s unwind edge.
- The personality the IR must name, `__gxx_wasm_personality_v0`, is never
  called; the module defines it weakly, under that name, which the backend
  recognises.

## The setjmp lowering

WASI's `libsetjmp.a` does not define `_setjmp`/`longjmp`. It defines
`__wasm_setjmp`, `__wasm_setjmp_test` and `__wasm_longjmp`, the forms LLVM's
WebAssembly SJLJ pass rewrites a `setjmp` call into. So both halves need
`-wasm-enable-sjlj` and `+exception-handling`; the backend refuses one
without the other.

**An object with only half the rewrite links and runs.** One that referenced
`__wasm_setjmp` and not `__wasm_setjmp_test` was correct until it threw, then
printed "thrown Wasm exception" instead of catching.

The catch half cannot be requested with a flag. It needs the target
machine's exception model to be `wasm`, because `TargetPassConfig` adds
`LowerInvoke` whenever the asm info reports no exception handling, and that
pass deletes the `invoke`s the rewrite just created. `llc` takes
`-exception-model=wasm`; the LLVM C API has no equivalent, so neither
llvm-sys nor inkwell can set it. The WebAssembly target tries to infer it,
but `basicCheckForEHAndSjLj` runs after `initAsmInfo()`, so the asm info
keeps `ExceptionHandling::None`.

ash carries one C++ translation unit,
`crates/ash/cpp/wasm_exception_model.cpp`, that sets both fields after
construction. Without it a wasm build is refused rather than emitted wrong.
ash also passes `-wasm-use-legacy-eh=false`: LLVM still defaults to the
withdrawn `try`/`catch`, which no current engine accepts.

The regression test asserts on both halves of the lowering, because the
broken form is the one that looks fine.
