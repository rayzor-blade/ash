# Native libraries on wasm

A wasm module cannot `dlopen`, so a native library for a wasm build is its
own `.wasm`, placed beside the program and loaded at start-up. The
`@:hlNative` declarations and the `DEFINE_PRIM` resolvers are unchanged. A
minimal library is under a kilobyte; the SQLite one is 1.9 MB, and moving it
out of the runtime took a hello world from 3.96 MB to 2.29 MB.

## The mechanism

A library is a `dylink.0` side module — core wasm dynamic linking. It
imports the program's linear memory and function table, so it operates on
the same heap and a function pointer it returns is an index the program can
call, and it imports each runtime function it uses by name, exactly as an
HDLL links against `libhl`. The Component Model is not used: its canonical
ABI copies values across the boundary, and `DEFINE_PRIM` signatures pass
`vbyte*`, `varray*`, `vdynamic*` and `vclosure*` into a heap the collector
scans.

## Building one in C

```sh
clang --target=wasm32-wasi --sysroot=$WASI_SYSROOT -fPIC -c -o demo.o demo.c
wasm-ld --experimental-pic -shared --import-undefined --no-entry \
        --export=hlp_add --export=hlp_greet -o demo.wasm demo.o
```

`--export` names the resolvers: `hlp_<primitive>`, the name the Haxe side
uses, not prefixed with the library. `@:hlNative("demo", "add")` looks for
`hlp_add` in `demo.wasm`. The file's stem is the library name, as with
`demo.hdll`.

The module then imports:

```
env.memory                      the program's linear memory
env.__indirect_function_table   the program's function table
env.__stack_pointer
env.__memory_base               where the loader placed its data
env.__table_base                where the loader placed its function pointers
env.hlp_alloc_bytes             each runtime function it calls, by name
```

## Building one in Rust

`crates/ash_hdll_sqlite` is the worked example, built by
`scripts/build_wasm_hdll.py --only sqlite` into
`target/<profile>/wasm32-wasip1/hdll/sqlite.wasm`. It depends on
[hl_abi](https://github.com/rayzor-blade/hl_abi), which declares the C ABI —
the `#[repr(C)]` layouts and the runtime's `extern` functions — and on nothing
of ash's runtime. Depending on the runtime as a crate would link a second
copy of its exports into the library, and two strong definitions of one
symbol do not link.

Three things a Rust library needs:

- **The program's allocator.** Rust's default on wasm is its own `dlmalloc`
  over its own arena; the first pointer that crosses between two allocators
  is a bug far from its cause. `hl_abi::ProgramAllocator` forwards to the
  program's `malloc`.
- **Position-independent code.** The precompiled `std` is not, so:

  ```sh
  RUSTFLAGS="-C relocation-model=pic -C target-feature=+mutable-globals" \
    cargo +nightly build --release --target wasm32-wasip1 -Z build-std=std,panic_abort
  wasm-ld --experimental-pic -shared --unresolved-symbols=import-dynamic \
    --no-entry --export=hlp_greet -o rustlib.wasm libyourlib.a
  ```

  `--unresolved-symbols=import-dynamic` rather than `--import-undefined`:
  Rust's std needs the address of `errno`, which is data, and the latter
  covers functions only.
- **Size discipline.** A library pulling in all of `std` is around 770 KB,
  mostly `std`. `#![no_std]` over the program's allocator keeps one small.

## What the program exports

Exporting a function pins it — tree shaking cannot remove what a host is told
about — so the program's ABI is not widened speculatively. `ash --build`
looks for side modules beside the output, reads their imports, and exports
exactly those plus the memory, the table, the two base globals and `malloc`.
The table loses its maximum so a library's functions can be appended.

A program built with no library beside it keeps its three default exports. A
primitive it never reaches costs nothing; one it does reach raises the same
"not loaded" error a native binary raises for a missing HDLL.

## Where a library's imports are answered

A side module brings almost nothing with it: `wasm-ld -shared` imports
undefined symbols rather than pulling archive members, so even `-lc` adds
nothing and a library's libc comes from its host.

- **`env`** is the program: memory, table, and the runtime and libc
  functions it exports.
- **`wasi_snapshot_preview1`** is the host.
- **`GOT.mem.x` / `GOT.func.x`** are the global offset table: mutable
  globals holding where `x` ended up, filled by the loader.

A library can only use libc functions the runtime object contains, and the
build says so rather than letting the load fail:

```
a native library beside the output imports 9 function(s) this runtime does
not define, the first few being futimens, pread, pwrite, readv...
```

`LIBRARY_LIBC` in `scripts/build_wasm_runtime.py` lists libc entry points
force-included into the runtime object for libraries. It can be generous:
nothing references them in an ordinary program, so tree shaking drops them
again.

## A library whose work is the host's

A library that draws to a canvas or plays sound calls neither the runtime nor
libc; its primitives forward to the host. Declare the import in `env` under
the `ash_host_` prefix:

```c
extern int ash_host_fiber_state(void);
```

`env` resolves against the program first and the host second, and an
`ash_host_` name is never demanded of the program, so such a library widens
the program's ABI not at all. It runs only where the host answers those
imports; a host that lacks one refuses the load with the name in the message.

## Loading

`crates/ash_wasm_runtime/src/native/dylink.rs`, before the program's own
initialisation:

1. Read `dylink.0` for the data size and table slots the library wants.
2. Allocate the data from the program's `malloc`.
3. Grow the program's table by the slots.
4. Instantiate, supplying memory, table, the two bases, each named function
   from the program's exports, WASI from the host, and a zeroed mutable
   global per GOT entry.
5. Fill the GOT.
6. Call `__wasm_apply_data_relocs` and `__wasm_call_ctors` if exported.

The guest reaches the library through `ash_host_dlopen` (is it there?) and
`ash_host_dlsym` (a table index, which is what a function pointer is on
wasm); `aot_native.rs` then runs the `DEFINE_PRIM` protocol as it always
has.

## Not done

- The browser loader. The steps above run against `WebAssembly.instantiate`;
  fetching a library only when the program uses it is the reason to do it.
- Lazy loading: everything beside the program is loaded at start-up.
- Unloading.
- `fmt` stays compiled into the runtime: its digests and zlib streams are
  small pure computation with no other source in a sandbox.
