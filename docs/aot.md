# Ahead-of-time compilation

`ash --build` compiles a HashLink program to a native executable or a wasm
module. The result contains no bytecode and no interpreter; nothing compiles
at run time.

The cost moves to the build: a large program is a real optimising compile,
not a JIT promotion. Most of this page is about that cost and its dials.

## Native

```
ash --build prog prog.hl
./prog
```

`ash` emits the code, links it and finds the runtime on its own: beside the
`ash` binary first, then the usual library directories. `--runtime <path>` or
`ASH_RUNTIME` names one explicitly. `--emit-aot prog.o` stops at the object
file for a caller that links itself.

### Programs that load HDLLs

An HDLL and the program must share one runtime. Two copies in one process
means two garbage collectors, and they crash as soon as one meets the other's
objects. `ash` detects that the program loads HDLLs, links the runtime as a
shared library, and stages it beside the executable under both names an HDLL
may import: `libhl.1.dylib` (HashLink 1.x builds) and the bare
`libhl.dylib`. `--abi-version 2` stages `libhl.2.dylib` for HDLLs built
against HashLink 2.

You supply the `.hdll` files. An AOT binary looks for them in its own
directory, then the working directory, and nowhere else.

If no runtime is installed anywhere, `ash` writes out the copy it carries
inside itself, so a machine that has never built the workspace still works.

The reverse case fails deliberately: linking a program that loads no HDLL
against the shared runtime stops with `fixup error ... does not have
address`. Such a program is emitted in static mode and references the
runtime directly.

### Build time and memory

For a native target, the emitter shards the program across threads. Each shard keeps a slice of
the functions, reduces the rest to declarations, and runs LLVM's optimiser
and code generator on its slice; the emitter writes the object holding the
program's data itself. `--build` hands every object to the linker at once;
`--emit-aot` joins them into the one object file it writes.

An 8,577-function game on a ten-core Apple M-series machine:

| shards | wall clock | peak memory |
|---|---|---|
| 1 | 350 s | 2.5 GB |
| 2 | 57 s | 3.0 GB |
| 4 | 42 s | 4.3 GB |
| 8 | 45 s | 4.9 GB |

One shard is the single-module path, where
the optimiser sees all 9,021 bodies at once and its cost grows faster than
linearly in module size; splitting in two cuts the work by six, not two.
Beyond two shards the gain is ordinary parallelism, and it flattens because
every shard reads the whole program before discarding what it does not own.
Memory scales with shard count for the same reason.

The default is half the machine's cores, clamped to [2, 6]. Set `ASH_AOT_SHARDS=2`
on a memory-constrained machine, or `1` for the smallest footprint.

### Persistent AIR reuse

Native and WASM builds cache lowered and optimized AIR per function on disk,
under a 512 MiB budget. `--air-cache-stats` reports reuse and stage timings;
`--air-cache-dir PATH` selects a directory, `--air-cache-mb N` changes the
budget, and `--no-air-cache` disables persistence. `ash air-cache` inspects the
cache; `ash air-cache --clear` clears it. A change to the compiler, target, ABI,
profile, options or dependencies invalidates the cached entry.

A native build still runs LLVM IR emission, optimization, code generation and
linking each time, and a wasm build still runs the direct emitter and the link.
The [Ashui measurements](wasm/air-cache-build-times.md), taken when wasm builds
still used LLVM, show the cost of those stages separately. See [the cache contract](debugging.md#persistent-air-cache-for-aot)
for invalidation details and environment equivalents.

### Optimization options

`-O0`, `-O1`, `-O2`, `-O3`, `-Os` and `-Oz` select LLVM IR optimization
and its matching codegen level. The long form is `--opt-level`.
`-Os` and `-Oz` set size attributes and use codegen level 2.
AIR keeps its own level, normally 3, so its inlining and allocation removal
remain available independently of LLVM's settings.

| Option | Controls |
|---|---|
| `-O`, `--opt-level 0\|1\|2\|3\|s\|z` | LLVM IR pipeline and matching codegen effort (native targets) |
| `--air-opt-level 0\|1\|2\|3` | AIR lowering/optimization configuration (native and wasm targets) |
| `--codegen-opt-level 0\|1\|2\|3` | Override machine codegen effort independently of `-O` (native targets) |

A wasm build is made by the direct emitter, not LLVM, so `-O` and
`--codegen-opt-level` do not change its output; `--air-opt-level` does. The
[LLVM WASM research](wasm/llvm-build-times.md) measured codegen levels on wasm
builds from before the direct backend.

Examples:

```sh
ash -O1 --build app app.hl
ash -Oz --build app app.hl
ash --air-opt-level 0 --build app.wasm --target wasm32-wasip1-threads app.hl
```

Explicit options override the corresponding environment settings. With no
options, the defaults and environment variables apply. A selected `-O` also
overrides `ASH_AOT_CODEGEN_LEVEL` unless `--codegen-opt-level` is specified.
Options apply to `--build` and `--emit-aot`. The settings belong to one build
and are inherited by shard workers. Library callers can use
`emit_aot_with_options` without changing existing `AotRequest` values or
`emit_aot` calls.

### Dials

| variable | effect |
|---|---|
| `ASH_AOT_SHARDS=N` | shard count. `1` is the single-module path, and the first thing to try when a sharded build misbehaves |
| `ASH_AOT_CODEGEN_LEVEL=0..3` | LLVM machine-codegen effort for native builds (default 3), independent of AIR and IR optimization. A wasm build ignores it |
| `ASH_AOT_NO_OPT=1` | skip the optimiser: fast builds, readable stacks, much slower code |
| `ASH_AOT_DUMP_IR=<path>` | write the LLVM IR. A directory takes `module.ll`; anything else is the file. Each shard's IR lands beside it |
| `ASH_LLVM_PASSES=<spec>` | replace the optimiser pipeline (`default<O2>`, …); `off` skips it |
| `ASH_AIR_LEVEL=0..3` | how hard the AIR optimiser works before LLVM sees the code |
| `ASH_AIR_NO_WIDEN=1` | turn off loop widening, the one AIR pass that rewrites arithmetic |
| `ASH_MIDDLE_END_LOG=1` | report how many functions were shielded from the optimiser |
| `ASH_AOT_NO_STATIC_DEVIRT=1` | do not guess method-call targets from the classes the program instantiates; only a `--pgo` profile devirtualises. Applies to native and wasm targets alike. Safe; slower on monomorphic dispatch |
| `ASH_WASM_DIRECT=nopolls,noshadow` | wasm targets only, which are always built by the direct AIR-to-wasm backend, with no LLVM module. `nopolls` leaves out the safepoint test at loop heads and `noshadow` the shadow frames: measurement switches, and a module built with either is not sound to run. Any other value is an error |

Flags:

- `--target <triple>` cross-compiles. A named target, the build machine's
  included, is built for a generic CPU. Native cross targets use one shard
  because the host's `ld` reads one object format. A wasm build is not
  sharded: the direct emitter writes one object. See
  [Cross-compiling](#cross-compiling) for which triples a build accepts.
- `--allow-refused` emits even when some functions could not be lowered; each
  becomes a throw at the point it is reached. Without it a refusal stops the
  build.
- `--pgo=<profile>` devirtualises from a call-site profile recorded by running
  the program once with `ASH_AOT_PROFILE_OUT` set. Every guard re-checks its
  target, so a stale profile costs a compare. The `=` is required.

### Reading the emitter's output

```
[aot] lowering 17279ms total; slowest:
[aot] sharding: 9021 bodies, 4 shards, shielded 51, stream 61 MB, prepared in 2388ms
[aot] shard 0: kept 2828, carried 3031, stripped 3162, swept 58406, folded 30169, declared 57996; prepare 2982ms, middle end 6656ms, codegen 7639ms
[aot] data object: 9021 bodies stripped; prepare 547ms, codegen 369ms
```

- **lowering** — bytecode to LLVM IR, ash's own work, single-threaded. The
  floor on build time.
- **shielded** — functions kept out of the optimiser because they catch
  exceptions. A trap is a `setjmp`, and a local promoted out of memory has an
  indeterminate value after the jump.
- **kept** — bodies this shard emits; **carried** — small bodies it may
  inline but will not emit; **stripped** — reduced to declarations;
  **swept** — deleted as unreachable; **folded** / **declared** — data it can
  read versus data it can only reference.
- **prepare** — dominated by reading the stream, which every shard does in
  full; the reason more shards stop helping.

### Exception stacks

`haxe.CallStack` works in an AOT binary. Every function keeps its frame
pointer, the object registers a table of body addresses and Haxe names at
startup, and the runtime walks the frame chain through that table.

Do not strip the binary. The table is the primary source, so stacks survive
stripping, but the runtime consults `dladdr` to keep runtime frames out of the
trace, and without symbols that filter stops working. Optimised builds show fewer
frames than unoptimised ones; inlining removed them.

### Troubleshooting

| symptom | cause | fix |
|---|---|---|
| `fixup error ... does not have address` at link | a static-mode object linked against the shared runtime | link the static `libash_std.a`, or emit a program that actually loads HDLLs |
| `Native library 'ssl' not loaded` at startup | the `.hdll` is not beside the binary or in the working directory | copy the HDLLs next to the binary |
| `no LC_RPATH's found` when an HDLL loads | the binary has no rpath for its own directory | build with `--build`, which adds it |
| GC crash shortly after an HDLL loads | two runtimes in one process | link the shared runtime and stage `libhl` beside the binary |
| `ld -r failed` | shards emitted for a foreign object format | guarded now; if it appears, set `ASH_AOT_SHARDS=1` and report it |
| build killed, machine swapping | too many shards for the memory | lower `ASH_AOT_SHARDS` |
| a wrong answer only in the AOT binary | an optimiser or lowering bug | bisect: `ASH_AOT_SHARDS=1`, then `ASH_AIR_NO_WIDEN=1`, then `ASH_LLVM_PASSES=off`, then `ASH_AOT_NO_OPT=1` |

`cargo test -p ash --test aot_smoke` compiles a corpus with `--build` and
compares each binary's output with the JIT's, byte for byte.

### Limits of an AOT binary

An AOT binary has no tiers, no on-stack replacement and no hot reload. The
call-site profile is advisory. There is no bytecode in the binary, so nothing
can be reloaded into it.

### Cross-compiling

`--emit-aot out.o --target <triple>` writes an object for another
architecture; linking it needs that architecture's toolchain and a runtime
built for it, which is why the platform table calls these "objects only".

The release binary registers the LLVM backends for the targets ash claims:
x86 (64- and 32-bit), AArch64, ARM, RISC-V 32 and 64, PowerPC64LE and s390x.
`cargo test -p ash --test cross_targets` emits an object for each and checks
its ELF header, so those triples are gated. wasm32 is not an LLVM target here:
the direct backend builds it, and `wasm_target` tests it. A triple outside the
set fails with `no target for <triple>`.

The `-dev` tarballs (`ash-linux-x86_64-dev`, `ash-macos-aarch64-dev`;
`ASH_DEV=1` on the install line) are the same ash built with
`--features all-targets`, which registers every backend the LLVM it links
was built with — MIPS, LoongArch, SPARC, Hexagon and the rest. Nothing
checks those objects beyond emitting them; `ash --version` says
`(all LLVM targets)` when a binary is that build. The default build leaves
them out because their instruction tables are a third of the binary.

```
ASH_DEV=1 curl -fsSL https://ash.rayzor.tech/install.sh | sh
ash --emit-aot game.o --target mips64el-unknown-linux-gnuabi64 game.hl
```

## Android and iOS

`--build` links a program for a phone using only the platform SDK; nothing
needs to be set.

```sh
ash --build app --target aarch64-apple-ios-sim app.hl
ash --build app --target aarch64-linux-android app.hl
```

ash looks for the target's `libash_std.a` in `<ash directory>/<triple>/`, as
it does for the other cross runtimes, and links it into the program. The
release bundles them: the Android ones (`aarch64-linux-android`,
`armv7-linux-androideabi`, `x86_64-linux-android`) in every tarball, and the iOS ones
(`aarch64-apple-ios`, `aarch64-apple-ios-sim`, `x86_64-apple-ios`) in the
macOS tarballs, since only a Mac can link for iOS. A source build has none:
build one (below) and put it there or name it with `--runtime`.

- iOS uses Xcode's compiler and SDK, found with `xcrun`. The device
  (`aarch64-apple-ios`) and the simulator (`aarch64-apple-ios-sim`,
  `x86_64-apple-ios`) are different outputs: the object records which it is
  for, and `IPHONEOS_DEPLOYMENT_TARGET` sets the minimum version (14.0 when
  unset).
- Android uses the NDK's clang. ash finds the NDK at `ANDROID_NDK_HOME`, else
  the newest under `ANDROID_HOME` or `ANDROID_SDK_ROOT`, else the newest under
  Android Studio's default SDK location. `ASH_ANDROID_API` sets the API level
  (24 when unset).
- ash links the static runtime into a phone program whether or not it names
  HDLLs, because a phone has no library to open beside it. ash ignores a bare
  `CC` for these targets, because it names the host's compiler;
  `ASH_LINKER_<triple>` still overrides the driver.
- A program cannot open an HDLL on a phone. Pass each library the program
  needs as `--native-lib libfoo.a` (repeatable), and ash links the whole
  static library into the program. At startup the program finds each primitive
  it calls through the library's `hlp_<name>` entry, the same entry an HDLL
  exports. If no library defines that entry, calling the primitive raises an
  error, and a program that never calls it runs normally. Build the library
  for the same target as the program.
- A Rust static library contains its own copy of the standard library. Two
  copies in one link both define `rust_eh_personality`, and Apple's linker has
  no option to accept the duplicate. When a program links a Rust library with
  `--native-lib`, ash first merges that library, and the runtime if it exports
  the same internals, into one object that keeps only its C interface global.
  On Apple targets one more symbol stays visible to the link: the personality
  routine, which Rust uses to unwind a panic. Apple's linker accepts only a few
  of these in a program, so the link keeps one copy for all the libraries.
  This needs the tools the SDK already provides: Xcode's compiler driver and
  `nm`, or the NDK's `llvm-nm` and `llvm-objcopy`.
- On iOS, UIKit owns the main thread and calls the app once per pass of its
  loop. When the program links hlwindow, `main` therefore hands the program to
  the window host's loop, `xwindow_run_turns`, and the program becomes a fiber
  on the main thread. Each turn of the loop resumes the fiber. When the
  program waits on a window with nothing to return, the host suspends the
  fiber until the next turn. When the program's entry returns, the process
  exits with its status. A program that links no window host runs on the main
  thread as it does on every other target. On Android, hlwindow's
  `android_main` calls the `xwindow_main` that ash defines, and the program
  runs on the activity's thread. When the program's entry returns,
  `xwindow_main` returns, and the host then finishes the activity. Android can
  call `android_main` again in the same process, and the runtime cannot start a
  second time. In that case the process exits, and Android starts the activity
  again in a new process. `Sys.exit` also ends the process, so Android starts an
  activity that is in front again. To quit an app, return from the entry.
- ash writes an executable, which runs under `xcrun simctl spawn` and
  `adb shell`, or an Android shared library (below). It does not write an app
  bundle or an APK. A UIKit app needs a bundle: put the executable and an
  `Info.plist` in a `.app` directory and install it with
  `xcrun simctl install`.
- An Android app loads a shared library, not an executable. For an Android
  target, give `--build` a name that ends in `.so`:

  ```sh
  ash --build libmain.so --target aarch64-linux-android \
    --native-lib libhost.a app.hl
  ```

  The window host in `libhost.a` must define `ANativeActivity_onCreate` and an
  `android_main` that calls `xwindow_main`. The library exports only the names
  that Android looks up: `ANativeActivity_*`, `JNI_On*` and `Java_*`. If it
  exports none of them, the build fails. ash links `liblog` and `libandroid`.
  Name other system libraries in `ASH_LINK_ARGS`. If your host uses
  `android-activity`, set `ASH_ANDROID_API=26`. The crate calls a function from
  API level 26. At the default level, 24, the link fails. For a `NativeActivity`
  app, put the file in the APK as `lib/<abi>/libmain.so` and name it `main` in
  the manifest.
- Build a runtime per target with
  `cargo build --release -p ash_std --target aarch64-apple-ios-sim`, and for
  Android with `cargo rustc -p ash_std --target aarch64-linux-android
  --release --crate-type staticlib`, because the library's shared build would
  need the NDK as cargo's linker. The headers come from the same SDK.

## Native libraries on a desktop

A desktop program that names an HDLL loads it when the program runs, and it
takes the shared runtime. To ship one executable instead, pass each library as
`--native-lib libfoo.a` (repeatable):

```sh
ash --build app --native-lib libgreeter.a app.hl
```

If the archives define a primitive of every library the program names, ash
links the whole archive into the program and uses the static runtime. The
program finds each primitive through the library's `hlp_<name>` entry, as on a
phone. No `.hdll` and no `libhl` sit beside the executable.

- A library that no archive defines a primitive for stays an HDLL. In that
  case ash keeps the shared runtime and ignores the archives.
- If an archive lacks a primitive that the program names, calling it raises an
  error. A program that never calls it runs normally.
- A Rust static library is merged first, exactly as on a phone. On Linux this
  needs `llvm-objcopy` or `objcopy` on the search path.
- Windows keeps loading HDLLs.

## The other macOS architecture

A Mac links for either macOS architecture with Apple's linker. The macOS
tarballs carry the runtime for the other architecture beside the binary, so
this needs no other flag:

```sh
ash --build app --target x86_64-apple-darwin app.hl
```

On an arm64 Mac the tarball holds `x86_64-apple-darwin/libash_std.a`. On an
Intel Mac it holds `aarch64-apple-darwin/libash_std.a`. A source build has
neither. Build the runtime with
`cargo build --release -p ash_std --target x86_64-apple-darwin`, and put it
there or name it with `--runtime`. Pass `--native-lib` archives built for the
same architecture as the program.

## WebAssembly

```sh
ash --build game.wasm --target wasm32-wasip1 game.hl
```

ash needs no external toolchain. The linker is `crates/ash_wasm_link`, and the
libc it links against was joined into the runtime object when ash was built; the
command above works with `PATH` unset. The one thing it looks for is that
runtime object, `ash_runtime.o`, in a directory named for the target beside
`ash`, then the usual library directories; `--runtime` or `ASH_RUNTIME` names
it.

**The result is a library, not a command.** It exports `main` and
`ash_module_init` and imports what only a host can provide: WASI, fiber
suspension, sockets. A wasm module cannot switch its own stacks, so an ash
program is suspended by whoever embeds it. [wasm/README.md](wasm/README.md)
has the hosts and the import contract.

**Only reachable code is emitted.** Functions nothing calls are dropped —
about a third of a module. Reachability is generous: all data is kept, so any
function survives if its address is written anywhere, because compiled code
calls most of the runtime through tables built in data. Debug sections are
dropped.

**A static library is linked the way a native linker links one.** `--native-lib`
takes wasm objects and `ar` archives of them, such as a Rust `staticlib` built for
`wasm32-wasip1` or `wasm32-wasip1-threads`. From an archive ash links only the
members the program needs: a member joins when something already linked refers to a
symbol that nothing linked defines, and the members it brings are searched in turn
for the symbols they refer to. The build output says how many members each archive
contributed.

A Rust library carries its own `std`, so a member can define a name the runtime
already defines, such as `rust_eh_personality` or `errno`. The earlier definition
wins, and the member's code uses it: the runtime's definition if it has one,
otherwise the first archive's. The build output lists the names dropped this way.
Two plain objects that define one name are an error; only an archive's definitions
give way.

If a member refers to a function that nothing defines and no host supplies, such as
`chdir` from `std`, the link turns the reference into a call that traps. A program
that never calls it is unaffected. The imports a host supplies are listed in
[wasm/host-abi.md](wasm/host-abi.md), and they stay imports. When a member joins the
link, the module imports only what reachable code uses, so a member's unused
functions add no imports. An archive that contributes no member changes nothing in
the output. A position-independent library also works: it reads a function's address
from a wasm global, and the linker gives that function a table slot and sets the
global to it.

A program finds a library's primitives the way it finds an HDLL's. Each primitive a
library defines has an entry `hlp_<name>`. The entry takes a pointer to a string
pointer, stores the primitive's signature through it, and returns the function. When
an archive defines that entry for a primitive the program calls, the program calls
the entry once at start-up and stores the function in a data word, the primitive's
slot, that every call goes through. No `.wasm` side module has to sit beside the
output, and the build prints no warning about a missing one. If no archive defines a
primitive, the program looks for it in the side modules the host loads at run time
([wasm/hdlls.md](wasm/hdlls.md)).

Building the runtime object for the target, when working from source:

```sh
cargo build --release -p ash_std --target wasm32-wasip1
scripts/build_wasm_runtime.py
```
