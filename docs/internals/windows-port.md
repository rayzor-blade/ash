# Windows (x86_64-pc-windows-msvc)

ash builds, runs and ships on Windows. The release workflow's `windows` job
gates publishing, and lint's `windows-check` links the whole CLI against
real Windows headers on every push. `examples/heaps_base2d` loads HashLink's
`fmt`/`ui`/`uv`/`sdl` hdlls and runs its update loop in both `interp` and
`hybrid`.

## Kind values are the alias, never a bare integer

bindgen types the C `hl_type_kind` enum as `u32` under clang and `i32`
under MSVC. A kind is therefore always `hl::hl_type_kind` — in struct
fields, `Vec<...>`, parameters, closure parameters and match scrutinees.
Alias-to-alias comparisons compile on both; `as usize` / `as i64` casts are
fine. Nothing `repr(C)` or serialized carries a kind, so the width flip is
layout-safe. The check, which should stay empty:

```
grep -rnE "(kind|_kind)s?\s*:\s*(Vec<)?u32|u32.*=.*\.kind" crates/ash/src --include='*.rs' | grep -v hl_bindings
```

(`jit/tbaa.rs`'s `kind_id: u32` is an LLVM metadata kind id, not this enum.)

## How an HDLL reaches the runtime

**One runtime, named `libhl.dll`.** A PE import table names the DLL a symbol
comes from and the loader binds to that module, never to the executable, so
the ELF trick of linking ash_std into the binary and letting HDLLs find it
does not exist. With hdlls in the bytecode directory, `choose_std_linkage`
selects the dynamic runtime and `init_std_library` loads
`<exe_dir>\libhl.dll`, the module the hdlls name, so both sides get one
`HMODULE` and one `hlp_gc_init`. `crates/ash_cli/build.rs` stages
`ash_std.dll` under that name, and as `libhl.1.dll` for HashLink 1.x CMake
builds — copies, since a symlink needs Developer Mode.

**Every import must resolve before a module maps.** No lazy binding: one
missing export fails the whole library with `ERROR_PROC_NOT_FOUND` (127),
naming nothing. When an hdll will not load, diff its import table against
`libhl.dll`'s exports.

**An HDLL's dependencies do not resolve from its own directory** by default;
the search starts at the executable's directory. ash loads hdlls with
`LOAD_WITH_ALTERED_SEARCH_PATH`, which puts the hdll's own directory first;
`PATH` is still consulted after.

**`SDL3.dll` and `OpenAL32.dll` are not staged.** Beside `ash.exe`, in the
game directory, or on `PATH`.

## Deliberate differences

- `hl_get_thread` returns a real per-thread record. ui.hdll's sentinel stores
  the pointer and polls `main_thread->gc_blocking`, so null is an access
  violation the moment a sentinel starts. `hlp_blocking` publishes the
  field; the tail is zeroed padding to upstream's size.
- `hl_thread_start` returns null, upstream's own "no thread was started".
  Haxe threads are fibers; `sentinel_loop` cannot be one either, since it
  sleeps without yielding and rewrites another thread's pc with
  `SuspendThread`/`SetThreadContext`.
- `thread_stack_base` asks `GetCurrentThreadStackLimits`. The former
  `current_stack_addr() + 8MB` fallback read unmapped memory on every
  collection, because a default Windows stack is 1 MB.
- `hlp_throw` calls `longjmp`, not `_longjmp`: MSVC's setjmp.h declares only
  `longjmp`, and Windows longjmp never touches signal masks.
- `native_recovery.rs` and the sampling profiler are `#[cfg(unix)]`, with
  API-preserving stubs. A native access violation crashes the process, and
  `ASH_PROFILE=sample` errors while the phase tree keeps working.

## Open

- **Win64 longjmp/SEH is untested.** Win64 `longjmp` performs a real SEH
  unwind, which wants `.pdata`/`.xdata` for every frame between throw and
  trap. JIT frames carry it (nothing under `crates/ash/src/llvm/` sets
  `nounwind`, `frame-pointer=all` is on every function), so the question is
  whether that data is correct and reachable. HashLink zeroes the jmp_buf's
  frame slot so longjmp degrades to a register restore; ash does not. The
  first program that throws across a JIT frame on Windows is the test.
- **No crash handler.** `main.rs`'s handler complex is `#[cfg(unix)]`. The
  shape: `SetUnhandledExceptionFilter`, fault address from
  `ExceptionRecord->ExceptionInformation[1]`, pc/fp/sp from `CONTEXT`,
  `WriteFile(GetStdHandle(STD_ERROR_HANDLE))`, `EXCEPTION_CONTINUE_SEARCH`.
  No CRT locks inside the filter. `signal(SIGABRT, …)` works as-is; SIGBUS
  has no analogue.
- **krio-fiber's `cfg(not(unix))` stacks lack a guard page** (upstream fix).
- **`pump_events_and_swap` is `#[cfg(unix)]`**: it probes SDL through
  `dlsym(RTLD_DEFAULT)`. The Windows shape is `GetModuleHandleW("SDL2.dll")`
  + `GetProcAddress`.
- **`lower_own_priority` is a no-op.** Wants
  `SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_LOWEST)`.

## Building locally

The same pieces the release job installs: rustup nightly (msvc host), VS
Build Tools with the C++ workload and a Windows SDK, LLVM 21 development
files from conda-forge (`llvmdev=21.1.8 zlib libxml2-devel`, plus `clangdev`
for bindgen).

```powershell
$env:LLVM_SYS_211_PREFIX = "<prefix>\Library"
$env:LIBCLANG_PATH       = "<prefix>\Library\bin"
$env:PATH                = "<prefix>\Library\bin;$env:PATH"
```

The `PATH` entry is required: conda's `libclang.dll` is a forwarder to
`libclang-13.dll` beside it, and Windows resolves a forwarder against the
normal search order. Without it every `GetProcAddress` fails and bindgen
reports "a `libclang` function was called that is not supported by the
loaded `libclang` instance", which is not a version problem.

Build `ash_std` before `ash`.
