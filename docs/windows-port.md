# Windows port plan (x86_64-pc-windows-msvc)

Status: **ash_std compiles clean on windows-2022** (release workflow's
`windows-experimental` job, dispatch-gated behind `try_windows`).
`cargo build --release -p ash` then stops in ash_core, and ash_core was
swept 2026-08-21: every hl_type_kind width error (Class A) and ungated
unix-ism (Class B) the CI run named is fixed — plus reload.rs, which the
truncated error list missed but carried the same class — except one hunk
parked for sequencing: `emit_comparison_jump`'s `a_kind: u32` in
jit/function.rs, contested with the in-flight SafeCast workflow
(`/tmp/win_contested.patch` on the dev machine; apply with that merge).
Until it lands, Windows CI keeps reporting exactly the
function.rs:2011..2148 + 4872-4874 errors; after it, the build front
moves to ash_interp, then the `ash` CLI crate. The inventory below is
the audit of those two (2026-08-21). File/line references drift; the API
names are the anchor.

## The rule that closed ash_core Class A

bindgen types the C `hl_type_kind` enum u32 under clang and i32 under
MSVC, so a kind value's type is `hl::hl_type_kind` — the alias — never a
bare u32/i32: struct fields, `Vec<...>`, params, closure params, match
scrutinees. Alias-vs-alias comparisons compile on both platforms, and
`as usize`/`as i64` casts stay fine. Nothing repr(C) or serialized
carries a kind (CompiledFunctionMeta, LoweredMeta, ObjLayout are
in-process Rust structs), so the width flip is layout-safe. The check:

    grep -rnE "(kind|_kind)s?\s*:\s*(Vec<)?u32|u32.*=.*\.kind" crates/ash/src --include='*.rs' | grep -v hl_bindings

Empty today except the parked hunk. (jit/tbaa.rs `kind_id: u32` is an
LLVM metadata kind id from inkwell — not this enum.)

## The two architectural forks — decide these first

1. **The setjmp/longjmp exception model.** The catching side has one
   chokepoint: `call_setjmp_opaque` (crates/ash_interp/src/interpreter.rs
   :92) — all four setjmp sites route through it. MSVC *does* export
   `_setjmp` from ucrtbase, but it records an SEH frame pointer in the
   jmp_buf, and Win64 `longjmp` performs a real SEH unwind that requires
   `.pdata`/`.xdata` for every frame between throw and trap — which
   JIT-emitted frames don't have. HashLink's own Windows port zeroes the
   jmp_buf's frame slot so longjmp degrades to a register restore; ours
   must do the same or move the trap chain to a non-unwinding mechanism
   (explicit result propagation / saved-`CONTEXT` restore). The throwing
   side is ash_std's `hlp_throw`, which calls `_longjmp` — a symbol MSVC
   does not export at all — so the two crates must flip together. This
   constrains everything else.

2. **HDLL symbol binding.** The loader relies on
   `dlopen(RTLD_NOW | RTLD_GLOBAL)` so HDLLs resolve `hl_*` against the
   already-loaded ash_std. Windows binds DLL imports to a *named* DLL at
   load time and has no `RTLD_GLOBAL`. Either ash_std ships an import
   library named what HDLLs link against, or interposition is replaced
   with explicit function-table registration at HDLL init. (The lookup
   half is done: std/src/sys.rs now walks `EnumProcessModules` +
   `GetProcAddress` where unix used `dlsym(RTLD_DEFAULT)`.)

## Landed

- **Build system**: cdylib name (`ash_std.dll`, no `lib` prefix), embed
  archive name (`libash_std.a` regardless of target), LLVM 21 MSVC
  bundle + libclang path in the workflow.
- **std/src/thread.rs, gc.rs, sys.rs**: ported behind per-concern
  `#[cfg]` sys modules — CRITICAL_SECTION/CONDITION_VARIABLE,
  VirtualAlloc(MEM_RESERVE/MEM_COMMIT) + DiscardVirtualMemory,
  EnumProcessModules for the whole-process symbol search. This is the
  pattern the remaining crates copy.
- **ash_core Class A** (rule above), minus the parked hunk.
- **ash_core native_lib.rs**: the system-libhl canary probe
  (dlopen/dlsym/dlclose) is `#[cfg(unix)]`; Windows takes the
  embedded/on-disk path unconditionally — no /usr/local/lib concept, and
  DLL imports bind by name at load time, so the shared-instance argument
  cannot arise. The HDLL load path's `#[cfg(not(unix))]` Library::new
  branches were already present.
- **ash_core profile.rs**: the SIGPROF + pthread_kill sampler is
  `#[cfg(unix)]` as a unit; on Windows `start()` errors, `init()` says so
  once on stderr, and the phase-tree profiler keeps working. The real
  port (SuspendThread + GetThreadContext) stays out of scope.

## Next: ash_interp (fails first)

Class A — same alias rule, ~34 sites, all in interpreter.rs:
CompiledFunctionEntry's `arg_kinds: [u32; 8]` / `ret_kind: u32` /
`fn args -> &[u32]` (:279-:287), `sigs: Vec<Option<([u32; 8], u8, u32)>>`
(:454), `let mut arg_kinds = [0u32; 8]` (:3419), and `kind: u32` params
on is_primitive_or_bytes_kind / is_unboxable_primitive_kind /
is_numeric_or_bool_kind (:1927-:1956), coerce_value_for_static_kind,
unbox_dynamic_to_kind, dynamic_to_value_for_kind, numeric_as_f64/i64/u64,
value_to_i64, wrap_native_result, read/write_value_at/_from_ptr/_to_ptr,
two `|k: u32|` float-kind closures, plus assorted
dst_kind/src_kind/obj_kind/reg_kind params. Arrays become
`[hl::hl_type_kind; 8]`; the `kind as i32` casts stay valid either way.
This file belongs to the SafeCast workflow — sweep it after their merge,
alongside the parked ash_core hunk.

Class B:

- **native_recovery.rs** — extern `sigsetjmp`/`siglongjmp` (link error:
  no such symbols in any Windows CRT) and `pthread_self`. Cfg-out the
  module with API-preserving stubs (`arm_native_recovery` → 0,
  `try_recover_from_signal` → false, disarms no-op) so the interpreter
  call sites compile untouched. Degradation: a native-call AV crashes the
  process instead of being swallowed — acceptable; the mechanism exists
  for macOS GL driver bugs. Real recovery would be a VEH that rewrites
  `CONTEXT` to a resume shim — never longjmp, which SEH-unwinds through
  frames that never established SEH state.
- **pump_events_and_swap** (:8990) — `dlsym(RTLD_DEFAULT)` probing SDL;
  `#[allow(dead_code)]`, kept for the Heaps work. `#[cfg(unix)]` the fn;
  when it revives, the Windows shape is `GetModuleHandleW("SDL2.dll")` +
  `GetProcAddress` — there is no whole-process search, the module must be
  named.
- **thread_stack_base** (:1737) — both arms are cfg'd macos/linux, so on
  Windows it silently falls back to SP + 8 MB. Default Windows stacks are
  1 MB: the fabricated base overshoots the guard page and the GC's
  conservative scan walks unmapped memory → AV. Add a
  `GetCurrentThreadStackLimits` arm. Runtime blocker, not a build one —
  but GC-fatal.
- **call_setjmp_opaque** (:92) — fork 1's catching side; see above.
- Recovery-signal names (:8507) hardcode macOS numbering — dead code once
  recovery is stubbed; no action beyond the stub.
- **lower_own_priority** (:9658) — compiles as a no-op on Windows; a
  `SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_LOWEST)` arm
  when convenient. Until then the LLVM chase thread competes at normal
  priority — correct, just impolite, exactly as the doc comment frames
  failure.
- The macos-only GLSL `#version` patch compiles out; Windows GL drivers
  serve `#version 130`. No action.
- Clean: air.rs, ssa.rs, frame.rs, values.rs, lib.rs.

## Next: the `ash` CLI crate (builds last)

One unit, one class (unix signals). Move the crash-handler complex under
`#[cfg(unix)]` unchanged and write the Windows counterpart as SEH — a
different architecture, not a per-line substitution: the `sigaction`
install (:85-93), `errno()`'s `__errno_location` arm (:110-118),
`write_stderr`'s `STDERR_FILENO` (:124-140), and
`crash_handler_siginfo`'s `siginfo_t`/`si_addr`/`SIGBUS` (:302-378).
Windows: `SetUnhandledExceptionFilter` (VEH only if it must fire before
debuggers); CRT `signal(SIGABRT, …)` works as-is; fault address from
`ExceptionRecord->ExceptionInformation[1]`; pc/fp/sp straight from
`CONTEXT` (Rip/Rbp/Rsp) instead of the hand-rolled UContext64 (already
cfg'd, falls to None); `WriteFile(GetStdHandle(STD_ERROR_HANDLE))` — the
async-signal-safety constraint becomes "no CRT locks inside an exception
filter"; return `EXCEPTION_CONTINUE_SEARCH` instead of re-raising.
SIGBUS has no analogue (`EXCEPTION_IN_PAGE_ERROR` is closest). The
jemalloc global_allocator and the `CARGO_MANIFEST_DIR` default path need
no action.

## Still open in ash_std

- **ash_static_call** (std/src/fun.rs): the x86_64 asm arm hardcodes
  System V (rdi/rsi/… with split int/float counters). Win64 wants
  rcx/rdx/r8/r9 with a unified counter, 32 bytes of shadow space, and
  xmm6-15 non-volatile. Compiles-but-corrupts without a third arm —
  a runtime blocker for Reflect/constructors that CI cannot see.
- **hlp_throw's `_longjmp`** — fork 1's throwing side.
- `ash_sdl` links nothing on Windows (needs `SDL2.lib` or crate
  exclusion).
- krio-fiber's `cfg(not(unix))` stacks lack a guard page (upstream fix).

## Ranked order

1. the parked `a_kind` hunk (lands with the SafeCast merge) → 2.
ash_interp Class A alias sweep → 3. native_recovery stubs + the SDL
dlsym gate (workspace compiles) → 4. `ash` CLI SEH handler (workspace
+ CLI compile) → 5. setjmp/longjmp model — fork 1, the first thing that
blocks *running* rather than building → 6. `thread_stack_base` via
GetCurrentThreadStackLimits (GC-fatal) → 7. HDLL binding — fork 2 → 8.
ash_static_call Win64 arm → 9. thread priority, ash_sdl link, krio
guard pages.
