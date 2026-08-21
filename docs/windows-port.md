# Windows port plan (x86_64-pc-windows-msvc)

Status: **does not compile yet.** The runtime is written against the unix
family in ~20 load-bearing places, and two of them are architectural. The
release workflow carries a `windows-x86_64` matrix entry behind the
`try_windows` dispatch input so the job is one click away once the tiers
below land; until then it does not run.

The inventory below is from a full-workspace audit (2026-08-21). File/line
references drift; the API names are the anchor.

## The two architectural forks — decide these first

1. **The setjmp/longjmp exception model.** `call_setjmp_opaque`
   (crates/ash_interp/src/interpreter.rs) calls `_setjmp` through a
   function pointer; on MSVC `_setjmp` is a compiler intrinsic that must be
   expanded inline, and Win64 `longjmp` performs a real SEH unwind that
   requires `.pdata`/`.xdata` for every frame between throw and trap —
   which JIT-emitted frames don't have. Either both JIT tiers emit Win64
   unwind info and the trap chain moves to a small C shim around the real
   intrinsics, or the trap chain is replaced with a non-unwinding mechanism
   (explicit result propagation / saved-`CONTEXT` restore). This constrains
   everything else. Same family: `native_recovery.rs`'s
   `sigsetjmp`/`siglongjmp` (no MSVC equivalent; the VEH shape is
   `AddVectoredExceptionHandler` + `RtlRestoreContext`, stub-able to
   "never recover" for first light).

2. **HDLL symbol binding.** The loader relies on
   `dlopen(RTLD_NOW | RTLD_GLOBAL)` so HDLLs resolve `hl_*` against the
   already-loaded ash_std. Windows binds DLL imports to a *named* DLL at
   load time and has no `RTLD_GLOBAL`. Either ash_std ships an import
   library named what HDLLs link against, or interposition is replaced
   with explicit function-table registration at HDLL init.

## Mechanical tiers

- **Build system** (partly fixed already): MSVC names the cdylib
  `ash_std.dll` (no `lib` prefix) — build.rs handles this now; libclang
  lives in `bin\` not `lib\` on Windows LLVM — handled; `ash_sdl` links
  nothing on Windows (needs `SDL2.lib` or crate exclusion); llvm-sys
  needs a static LLVM 21 MSVC toolchain.
- **std/src/thread.rs** — the biggest chunk (~600 lines, zero cfgs):
  pthread mutex/cond/semaphore → `CRITICAL_SECTION` +
  `CONDITION_VARIABLE` (or `std::sync` + reentrancy counter),
  `nanosleep` → `Sleep`.
- **GC memory**: `mmap`/`munmap` → `VirtualAlloc(MEM_RESERVE)` +
  per-block `MEM_COMMIT` (maps *better* onto the demand-committed design
  than mmap does); `madvise(MADV_DONTNEED)` → `DiscardVirtualMemory`;
  `pthread_self` thread token → `GetCurrentThreadId`.
- **Stack bounds**: `thread_stack_base()`'s fallback guesses +8 MB;
  Windows default stacks are 1 MB, so the conservative scan would read
  unmapped memory. Use `GetCurrentThreadStackLimits`.
- **`dlsym(RTLD_DEFAULT, ...)`** (sys.rs resolve, native_lib staleness
  probe, interpreter SDL pump): no Win32 primitive searches every module —
  needs `EnumProcessModules` + `GetProcAddress`, or a registration table.
  Do all three with one helper.
- **`ash_static_call`** (std/src/fun.rs): the x86_64 arm hardcodes the
  System V ABI (rdi/rsi/... with split int/float counters). Win64 wants
  rcx/rdx/r8/r9 with a unified counter and 32 bytes of shadow space, and
  xmm6-15 are non-volatile. Compiles-but-corrupts without a third arm.
- **Profiler** (crates/ash/src/profile.rs): SIGPROF-based sampling has no
  Windows equivalent; first light is `#[cfg(unix)]` with no-op
  `init`/`report`, the real port is SuspendThread/GetThreadContext.
- **ash_cli signal handlers**: `sigaction` for SIGSEGV/SIGBUS/SIGABRT is
  ungated; gate it, VEH later.
- **Fibers**: krio-fiber already carries a Win64 context switch; its
  `cfg(not(unix))` stacks lack a guard page (upstream fix).

## Ranked order

1. setjmp/longjmp model → 2. HDLL binding → 3. build system → 4.
thread.rs → 5. GC memory + stack bounds → 6. dlsym helper → 7.
native_recovery VEH stub → 8. profiler cfg-gate → 9. ash_static_call
Win64 arm → 10. ash_sdl link + krio guard pages.
