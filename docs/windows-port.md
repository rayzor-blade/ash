# Windows (x86_64-pc-windows-msvc)

Status: **builds, runs, and ships.** `cargo build --release -p ash_std` then
`cargo build --release -p ash` produces a working `ash.exe`; the release
workflow's `windows` job is an ordinary leg that gates publish like any
other platform, and lint's `windows-check` compiles and links the whole CLI
against real Windows headers on every push. `examples/heaps_base2d` loads
HashLink's own `fmt`/`ui`/`uv`/`sdl` hdlls and runs its update loop
indefinitely in both `--mode interp` and `--mode hybrid`.

What remains is listed at the end. None of it stops a program from running;
all of it is reachable by a program this port has not run yet.

## The rule that closed the kind-width errors

bindgen types the C `hl_type_kind` enum u32 under clang and i32 under MSVC,
so a kind value's type is `hl::hl_type_kind` — the alias — never a bare
u32/i32: struct fields, `Vec<...>`, params, closure params, match
scrutinees. Alias-vs-alias comparisons compile on both platforms, and
`as usize`/`as i64` casts stay fine. Nothing repr(C) or serialized carries a
kind (CompiledFunctionMeta, LoweredMeta, ObjLayout are in-process Rust
structs), so the width flip is layout-safe. The check:

    grep -rnE "(kind|_kind)s?\s*:\s*(Vec<)?u32|u32.*=.*\.kind" crates/ash/src --include='*.rs' | grep -v hl_bindings

Empty today. (jit/tbaa.rs `kind_id: u32` is an LLVM metadata kind id from
inkwell — not this enum.)

## How an HDLL reaches the runtime here

This is the part that differs most from the other platforms, and every piece
of it was a separate bug.

**One runtime, named `libhl.dll`.** A PE import table names the DLL a symbol
comes from, and the loader binds it to *that module* — never to the
executable, the way ELF lets an executable's exports preempt a
`libhl.so` dependency. So the ELF trick of linking ash_std in and letting
HDLLs find it does not exist here. Windows instead does what macOS does:
when the bytecode directory contains hdlls, `choose_std_linkage` selects the
dynamic runtime and `init_std_library` loads `<exe_dir>\libhl.dll` — the
exact module the hdlls' import tables name, so the loader hands both sides
one `HMODULE`, and its `hlp_gc_init` runs. Statically linked, the hdlls got
a second copy of ash_std whose GC nobody had started.

`crates/ash_cli/build.rs` stages `ash_std.dll` under that name (and
`libhl.1.dll`, which HashLink 1.x CMake builds import) beside the
executable. Copies, not symlinks: creating a symlink on Windows needs
Developer Mode or admin rights. The release job checks the staging happened
rather than repeating it.

**Every import must resolve before a module maps.** There is no lazy
binding: one missing export fails the whole library with
`ERROR_PROC_NOT_FOUND` (127), naming nothing. `ui.hdll` alone wanted
`hl_get_thread`, `hl_thread_start`, `hl_detect_debugger`, `hl_dyn_geti` and
`hl_dyn_getp`; `mysql.hdll` wanted `hl_dyn_seti64`. When an hdll will not
load, diff its import table against `libhl.dll`'s exports — the loader will
not tell you which symbol it wanted.

**An HDLL's own dependencies do not resolve from its own directory.** The
default search order starts at the *executable's* directory and never
includes the loaded module's, so `sdl.hdll` next to the bytecode would not
find an `SDL3.dll` sitting beside it. HashLink never meets this because
`hl.exe` ships inside that directory. ash loads hdlls with
`LOAD_WITH_ALTERED_SEARCH_PATH`, which puts the hdll's own directory at the
head of the search; `PATH` is still consulted after, so an installed
HashLink on `PATH` keeps working too.

**`SDL3.dll` and `OpenAL32.dll` are still the operator's problem.** Nothing
in the build or the installer stages them. They must sit beside `ash.exe`,
in the game directory, or on `PATH`.

## Landed

- **Build system**: cdylib name (`ash_std.dll`, no `lib` prefix), embed
  archive name (`libash_std.a` regardless of target), LLVM 21 MSVC bundle +
  libclang path in the workflows. `native_lib`'s on-disk fallback looks for
  `ash_std.dll`, not the `libash_std.dll` MSVC never produces.
- **std/src/thread.rs, gc.rs, sys.rs**: ported behind per-concern `#[cfg]`
  sys modules — CRITICAL_SECTION/CONDITION_VARIABLE,
  VirtualAlloc(MEM_RESERVE/MEM_COMMIT) + DiscardVirtualMemory,
  EnumProcessModules for the whole-process symbol search.
- **The `hl_type_kind` alias sweep** across ash_core and ash_interp.
- **HDLL loading**, above.
- **`hl_get_thread`** returns a real per-thread record. Null is not a
  graceful "ash keeps no registry": ui.hdll's sentinel stores the pointer and
  polls `main_thread->gc_blocking` from elsewhere, so null is an access
  violation the moment a sentinel starts. `hlp_blocking` publishes the field;
  the tail is zeroed padding to upstream's size.
- **`hl_thread_start`** returns null — upstream's own "no thread was
  started", which `ui_start_sentinel` only stores. ash has no OS threads to
  hand out (Haxe threads are krio fibers), and `sentinel_loop` cannot be one
  of those either: it sleeps in a loop that never yields, and when it fires
  it calls SuspendThread/SetThreadContext on the thread it watches to rewrite
  that thread's pc. A program asking for a sentinel runs without a watchdog.
- **`thread_stack_base`** asks `GetCurrentThreadStackLimits`. It used to fall
  through to `current_stack_addr() + 8MB`; a default Windows stack is 1 MB,
  so the conservative scan read unmapped memory on every collection — an
  access violation at a different moment on every run (5.7s, 9.5s, 15.6s,
  23.6s, 47.9s on one binary and one program), while anything short enough
  never to collect was unaffected.
- **`native_recovery.rs`** is `#[cfg(unix)]` as a unit, with API-preserving
  stubs. Degradation: a native-call AV crashes the process instead of being
  swallowed. The mechanism exists for macOS GL driver bugs.
- **profile.rs**: the SIGPROF + pthread_kill sampler is `#[cfg(unix)]`; on
  Windows `start()` errors, `init()` says so once, and the phase-tree
  profiler keeps working.
- **hlp_throw's `_longjmp`**: darwin and glibc export the no-signal-mask
  variant, MSVC's setjmp.h declares only `longjmp`, and Windows longjmp never
  touches signal masks — so the bindings differ by exactly that underscore
  and the two calls are the same operation.

## Still open

- **`ash_static_call` (std/src/fun.rs)**: the x86_64 asm arm still hardcodes
  System V — rdi/rsi/… with split int/float counters. Win64 wants
  rcx/rdx/r8/r9 with a unified counter, 32 bytes of shadow space, and
  xmm6-15 non-volatile. Compiles-but-corrupts, so it is a runtime blocker for
  Reflect and constructors that neither CI nor the Heaps example reaches.
  This is the largest remaining item.
- **The Win64 longjmp/SEH question is untested, not resolved.** Win64
  `longjmp` performs a real SEH unwind, which wants `.pdata`/`.xdata` for
  every frame between throw and trap — which JIT-emitted frames do not have.
  HashLink zeroes the jmp_buf's frame slot so longjmp degrades to a register
  restore; ash does not, and nothing run on Windows so far throws across a
  JIT frame. The first program that does is the test.
- **No crash handler.** `main.rs`'s handler complex is `#[cfg(unix)]`, so a
  fault prints nothing at all — every diagnosis in this port came from
  timings and bisects instead of a stack. The Windows shape is
  `SetUnhandledExceptionFilter`, fault address from
  `ExceptionRecord->ExceptionInformation[1]`, pc/fp/sp from `CONTEXT`
  (Rip/Rbp/Rsp), `WriteFile(GetStdHandle(STD_ERROR_HANDLE))`, and
  `EXCEPTION_CONTINUE_SEARCH` rather than re-raising; the
  async-signal-safety constraint becomes "no CRT locks inside an exception
  filter". CRT `signal(SIGABRT, …)` works as-is. SIGBUS has no analogue
  (`EXCEPTION_IN_PAGE_ERROR` is closest).
- **krio-fiber's `cfg(not(unix))` stacks lack a guard page** (upstream fix).
  Its Windows x86_64 switcher does save and restore TEB.StackBase/StackLimit,
  so native code on a fiber is otherwise legitimate.
- **`pump_events_and_swap`** is `#[cfg(unix)]` — it probes SDL through
  `dlsym(RTLD_DEFAULT)`. Windows has no whole-process search; the shape is
  `GetModuleHandleW("SDL2.dll")` + `GetProcAddress`.
- **`lower_own_priority`** is a no-op; a
  `SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_LOWEST)` arm when
  convenient. The LLVM chase thread competes at normal priority — correct,
  just impolite.

## Building it locally

Same pieces the release job installs: rustup nightly (msvc host), VS Build
Tools with the C++ workload and a Windows SDK, and LLVM 21 development files
from conda-forge (`llvmdev=21.1.8 zlib libxml2-devel`, plus `clangdev` for
bindgen — the CI runner gets libclang from its preinstalled LLVM).

    $env:LLVM_SYS_211_PREFIX = "<prefix>\Library"
    $env:LIBCLANG_PATH       = "<prefix>\Library\bin"
    $env:PATH                = "<prefix>\Library\bin;$env:PATH"

The `PATH` entry is not optional. conda's `libclang.dll` is a small
*forwarder* to `libclang-13.dll` beside it, and Windows resolves a forwarder
against the normal search order rather than the forwarding DLL's own
directory — so without it every `GetProcAddress` fails and bindgen reports
"a `libclang` function was called that is not supported by the loaded
`libclang` instance", which sounds like a version problem and is not one.

Build `ash_std` before `ash`: the latter's build.rs embeds the former's
cdylib and fails if it is missing.
