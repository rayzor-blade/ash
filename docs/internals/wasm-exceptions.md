# wasm: exceptions and setjmp

ash's trap model is `setjmp`/`longjmp`. On wasm that becomes LLVM's SJLJ
lowering over the exception-handling proposal, and two halves have to agree.

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
