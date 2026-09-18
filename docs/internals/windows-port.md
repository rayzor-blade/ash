# Windows (x86_64-pc-windows-msvc)

ash builds, runs and ships on Windows. `cargo build --release -p ash_std` then
`cargo build --release -p ash` produces a working `ash.exe`; the release
workflow's `windows` job gates publish like any other platform, and lint's
`windows-check` links the whole CLI against real Windows headers on every push.
`examples/heaps_base2d` loads HashLink's own `fmt`/`ui`/`uv`/`sdl` hdlls and
runs its update loop indefinitely in both `--mode interp` and `--mode hybrid`.

## Kind values use the alias, never a bare integer

bindgen types the C `hl_type_kind` enum as u32 under clang and i32 under MSVC.
So a kind is always `hl::hl_type_kind` — in struct fields, `Vec<...>`, params,
closure params and match scrutinees. Alias-to-alias comparisons compile on both
platforms, and `as usize` / `as i64` casts are fine.

Nothing repr(C) or serialized carries a kind (`CompiledFunctionMeta`,
`LoweredMeta` and `ObjLayout` are in-process Rust structs), so the width flip is
layout-safe. The check, which should stay empty:

```
grep -rnE "(kind|_kind)s?\s*:\s*(Vec<)?u32|u32.*=.*\.kind" crates/ash/src --include='*.rs' | grep -v hl_bindings
```

(`jit/tbaa.rs`'s `kind_id: u32` is an LLVM metadata kind id from inkwell, not
this enum.)

## How an HDLL reaches the runtime

This differs most from the other platforms, and each piece below was a separate
bug.

**One runtime, named `libhl.dll`.** A PE import table names the DLL a symbol
comes from and the loader binds it to *that module*, never to the executable —
so the ELF trick of linking ash_std in and letting HDLLs find it does not exist
here. Windows does what macOS does instead: with hdlls in the bytecode
directory, `choose_std_linkage` selects the dynamic runtime and
`init_std_library` loads `<exe_dir>\libhl.dll`, the exact module the hdlls name,
so both sides get one `HMODULE` and its `hlp_gc_init` runs. Statically linked,
the hdlls got a second copy of ash_std whose GC nobody had started.

`crates/ash_cli/build.rs` stages `ash_std.dll` under that name, and as
`libhl.1.dll` for HashLink 1.x CMake builds. Copies, not symlinks — a symlink
needs Developer Mode or admin rights.

**Every import must resolve before a module maps.** There is no lazy binding:
one missing export fails the whole library with `ERROR_PROC_NOT_FOUND` (127),
naming nothing. `ui.hdll` alone wanted `hl_get_thread`, `hl_thread_start`,
`hl_detect_debugger`, `hl_dyn_geti` and `hl_dyn_getp`; `mysql.hdll` wanted
`hl_dyn_seti64`. When an hdll will not load, diff its import table against
`libhl.dll`'s exports — the loader will not say which symbol it wanted.

**An HDLL's dependencies do not resolve from its own directory.** The default
search order starts at the *executable's* directory and never includes the
loaded module's, so `sdl.hdll` beside the bytecode would not find an `SDL3.dll`
next to it. HashLink never meets this because `hl.exe` ships in that directory.
ash loads hdlls with `LOAD_WITH_ALTERED_SEARCH_PATH`, which puts the hdll's own
directory first; `PATH` is still consulted after, so an installed HashLink on
`PATH` keeps working.

**`SDL3.dll` and `OpenAL32.dll` are the operator's problem.** Nothing stages
them. They must sit beside `ash.exe`, in the game directory, or on `PATH`.

## Deliberate differences

- **`hl_get_thread` returns a real per-thread record.** Null is not a graceful
  "ash keeps no registry": ui.hdll's sentinel stores the pointer and polls
  `main_thread->gc_blocking` from elsewhere, so null is an access violation the
  moment a sentinel starts. `hlp_blocking` publishes the field; the tail is
  zeroed padding to upstream's size.
- **`hl_thread_start` returns null**, which is upstream's own "no thread was
  started". ash has no OS threads to hand out — Haxe threads are krio fibers —
  and `sentinel_loop` cannot be one either: it sleeps in a loop that never
  yields, and when it fires it calls SuspendThread/SetThreadContext to rewrite
  the watched thread's pc. A program asking for a sentinel runs without a
  watchdog.
- **`thread_stack_base` asks `GetCurrentThreadStackLimits`.** It used to fall
  back to `current_stack_addr() + 8MB`; a default Windows stack is 1 MB, so the
  conservative scan read unmapped memory on every collection — an access
  violation at a different moment every run (5.7s, 9.5s, 15.6s, 23.6s, 47.9s on
  one binary and one program), while anything short enough never to collect was
  unaffected.
- **`hlp_throw` calls `longjmp`, not `_longjmp`.** darwin and glibc export the
  no-signal-mask variant; MSVC's setjmp.h declares only `longjmp`, and Windows
  longjmp never touches signal masks. Same operation, one underscore apart.
- **`native_recovery.rs` and the sampling profiler are `#[cfg(unix)]`**, with
  API-preserving stubs. A native-call access violation crashes the process
  instead of being swallowed, and `ASH_PROFILE=sample` errors while the phase
  tree keeps working.

## Still open

- **Win64 longjmp/SEH is untested, not resolved.** Win64 `longjmp` performs a
  real SEH unwind, which wants `.pdata`/`.xdata` for every frame between throw
  and trap. JIT frames do carry it — nothing under `crates/ash/src/llvm/` sets
  `nounwind` and `frame-pointer=all` is on every function — so the question is
  whether that data is correct and reachable, not whether it exists. HashLink
  zeroes the jmp_buf's frame slot so longjmp degrades to a register restore;
  ash does not. Nothing run on Windows has thrown across a JIT frame yet. The
  first program that does is the test.
- **No crash handler.** `main.rs`'s handler complex is `#[cfg(unix)]`, so a
  fault prints nothing — every diagnosis in this port came from timings and
  bisects instead of a stack. The shape is `SetUnhandledExceptionFilter`, fault
  address from `ExceptionRecord->ExceptionInformation[1]`, pc/fp/sp from
  `CONTEXT` (Rip/Rbp/Rsp), `WriteFile(GetStdHandle(STD_ERROR_HANDLE))`, and
  `EXCEPTION_CONTINUE_SEARCH` rather than re-raising. The async-signal-safety
  constraint becomes "no CRT locks inside an exception filter". CRT
  `signal(SIGABRT, …)` works as-is; SIGBUS has no analogue
  (`EXCEPTION_IN_PAGE_ERROR` is closest).
- **krio-fiber's `cfg(not(unix))` stacks lack a guard page** (upstream fix).
  Its Windows x86_64 switcher does save and restore TEB.StackBase/StackLimit,
  so native code on a fiber is otherwise legitimate.
- **`pump_events_and_swap` is `#[cfg(unix)]`** — it probes SDL through
  `dlsym(RTLD_DEFAULT)`. Windows has no whole-process search; the shape is
  `GetModuleHandleW("SDL2.dll")` + `GetProcAddress`.
- **`lower_own_priority` is a no-op.** The LLVM chase thread competes at normal
  priority — correct, just impolite. Wants
  `SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_LOWEST)`.

## Building locally

The same pieces the release job installs: rustup nightly (msvc host), VS Build
Tools with the C++ workload and a Windows SDK, and LLVM 21 development files
from conda-forge (`llvmdev=21.1.8 zlib libxml2-devel`, plus `clangdev` for
bindgen — the CI runner gets libclang from its preinstalled LLVM).

```powershell
$env:LLVM_SYS_211_PREFIX = "<prefix>\Library"
$env:LIBCLANG_PATH       = "<prefix>\Library\bin"
$env:PATH                = "<prefix>\Library\bin;$env:PATH"
```

The `PATH` entry is not optional. conda's `libclang.dll` is a small *forwarder*
to `libclang-13.dll` beside it, and Windows resolves a forwarder against the
normal search order rather than the forwarding DLL's directory. Without it every
`GetProcAddress` fails and bindgen reports "a `libclang` function was called
that is not supported by the loaded `libclang` instance", which sounds like a
version problem and is not one.

Build `ash_std` before `ash`: the latter's build.rs embeds the former's cdylib
and fails if it is missing.
