# Native libraries on wasm

A Haxe program that names an HDLL used to build for wasm and then raise at the
first primitive it reached, because a wasm module cannot `dlopen` anything.
The way out was to compile the library into the runtime -- which `fmt` and
`sqlite` still are -- and that does not scale: a library present in the
runtime object is in every module whether the program uses it or not, and
SQLite alone is 1.67 MB of a 3.96 MB hello world.

A native library is now its own `.wasm`, shipped beside the program and loaded
at start-up. The demo library in this document is **925 bytes**.

## Why not a component

The Component Model is the wrong boundary here, and not by a little.

`DEFINE_PRIM` signatures pass `vbyte*`, `varray*`, `vdynamic*` and
`vclosure*` -- addresses in the heap ash's collector scans. A library
allocates objects that collector must find, and calls closures back into the
VM. A component owns its linear memory and the canonical ABI *copies* values
across the boundary, so a `hl.Bytes` handed to a library would arrive as a
copy and writes would not land, an object the library allocated would be
invisible to the collector, and a callback would have nothing to call.

Components are the right boundary one level out -- ash itself as a component
talking to its host over WIT. At the HDLL seam they break the ABI.

## What is used instead

A `dylink.0` side module: core wasm dynamic linking, which is the same-memory
mechanism. It imports the program's memory and its function table, so it works
on the same heap and a function pointer it returns is an index the program can
call. It then imports each runtime function it uses **by name**, exactly as an
HDLL links against libhl.

That last point is why nothing about the `DEFINE_PRIM` protocol changed: the
resolver is found by name among the module's exports, and `aot_native.rs` runs
the protocol it always has.

## Building one

    clang --target=wasm32-wasi --sysroot=$WASI_SYSROOT -fPIC -c -o demo.o demo.c
    wasm-ld --experimental-pic -shared --import-undefined --no-entry \
            --export=hlp_add --export=hlp_greet -o demo.wasm demo.o

`--export` names the `DEFINE_PRIM` resolvers, which is `hlp_<primitive>` --
the primitive as the Haxe side names it, NOT prefixed with the library. A
`@:hlNative("demo", "add")` looks for `hlp_add` in `demo.wasm`.

The file's stem is the library name, the same rule `demo.hdll` follows.

What that module then declares it needs:

    env.memory                      the program's linear memory
    env.__indirect_function_table   the program's function table
    env.__stack_pointer
    env.__memory_base               where the loader put its data
    env.__table_base                where the loader put its function pointers
    env.hlp_alloc_bytes             one runtime function, imported by name

## What the program has to export, and when

Exporting a function pins it -- tree shaking cannot remove what the host is
told about -- so the ABI is not widened speculatively. `ash --build` looks for
side modules beside the output, reads what they import, and exports exactly
that, plus the memory, the table, the two base globals and `malloc`. The table
also loses its maximum, because a side module's functions are appended to it.

A program built with no library beside it keeps the three exports it always
had. It still builds and still runs; a primitive it never reaches costs it
nothing, and one it does reach raises the same "not loaded" a native binary
raises for a missing HDLL.

## Where a library gets everything else

A side module brings almost nothing with it. `wasm-ld -shared` resolves
undefined symbols by importing them rather than by pulling archive members, so
even `-lc` changes nothing: a library's libc comes from whoever hosts it. That
is the model, not a gap in it.

So three parties answer a library's imports:

- **`env`** is the program: its memory, its table, and the runtime and libc
  functions it exports.
- **`wasi_snapshot_preview1`** is the host, which answers a library exactly as
  it answers the program.
- **`GOT.mem.x` and `GOT.func.x`** are the global offset table: mutable
  globals holding where `x` ended up. A symbol the library defines itself is
  resolved from its own exports -- a data symbol is exported as a global
  holding its address, a function as a function that the loader gives a table
  slot. One it does not define is the program's.

A library therefore cannot use a libc function that is not in the runtime
object, and the runtime object only has what `ash_std` itself referenced. The
build says so rather than letting the load fail:

    a native library beside the output imports 9 function(s) this runtime does
    not define, the first few being futimens, pread, pwrite, readv...

`LIBRARY_LIBC` in `scripts/build_wasm_runtime.py` is the answer to that: a
list of libc entry points force-included into the runtime object for
libraries to use. It can be generous, because nothing references them and no
library asks for them in an ordinary program, so tree shaking drops them
again. They are named as archive members rather than forced with `-u`, which
a relocatable link refuses -- and `--whole-archive` on libc is not the way
either, because it pulls crt1 and the long-double printf, whose own undefined
symbols nothing provides.

## What the loader does

`crates/ash_wasm_runtime/src/native/dylink.rs`, before the program's own
initialisation runs -- so that nothing instantiates a module from inside a
call the guest is making:

1. Read `dylink.0` for the bytes of data and the table slots the library wants.
2. Take the bytes from the program's own `malloc`, so one allocator owns the
   whole heap.
3. Grow the program's table by the slots, and remember where they start.
4. Instantiate, supplying the memory, the table, the two bases, each named
   function from the program's exports, WASI from the host, and a zeroed
   mutable global for every GOT entry.
5. Fill in the GOT now that the library has been placed.
6. Call `__wasm_apply_data_relocs` and `__wasm_call_ctors` if it exports them.

The guest then reaches it through two imports that are `dlopen` and `dlsym`
under other names: `ash_host_dlopen` answers whether a library is there, and
`ash_host_dlsym` answers with a table index -- which is what a function
pointer already is in a wasm module.

## sqlite, as it is actually shipped

`crates/ash_hdll_sqlite` is the worked example, built by
`scripts/build_wasm_hdll.py` to
`target/<profile>/wasm32-wasip1/hdll/sqlite.wasm`. It is 1.9 MB, and taking it
out of the runtime took a hello world from **3,964,699 to 2,292,831 bytes --
42% smaller**, because a library compiled into the runtime object is in every
module whether the program uses it or not.

It depends on `ash_std` for nothing at all. `src/abi.rs` is the C ABI written
out -- the `#[repr(C)]` layouts and the `extern "C"` declarations -- because
depending on the runtime as a Rust crate would put a second copy of its
`#[no_mangle]` exports in the library's archive, and two strong definitions of
one name cannot be linked. Three things that seam needed:

- **`hlp_type_i32` and its four siblings**, added to `ash_std`. A library
  cannot call `hlt_i32()`, which is a Rust function, and must not use `hl.h`'s
  `hlt_i32` static: that is the plain descriptor, while allocations are made
  against the persistent GC-registered one, and they are not interchangeable.
- **The program's allocator as the library's.** Rust's default on wasm is its
  own `dlmalloc` over its own arena; two allocators on one linear memory do
  not corrupt each other, but nothing either allocates can be freed by the
  other, and the first pointer that crosses is a bug far from its cause.
- **The address of libc functions, not just calls to them.** sqlite builds a
  VFS out of function pointers, so `close`, `fcntl`, `fstat` and the rest
  arrive as `GOT.func` entries and the program has to export them for the
  loader to give them table slots.

## A library written in Rust

The same thing, with two extra flags, because the toolchain's precompiled
`core` and `std` are not position-independent:

    RUSTFLAGS="-C relocation-model=pic -C target-feature=+mutable-globals" \
      cargo +nightly build --release --target wasm32-wasip1 \
      -Z build-std=std,panic_abort
    wasm-ld --experimental-pic -shared --unresolved-symbols=import-dynamic \
      --no-entry --export=hlp_greet -o rustlib.wasm libyourlib.a

`--unresolved-symbols=import-dynamic` rather than `--import-undefined`: the
latter covers functions only, and Rust's std needs the address of `errno`,
which is data.

A library that pulls in all of `std` is around 770 KB, most of it `std` rather
than the library. `#![no_std]` with the runtime's own allocator is the way to
keep one small.

## A library whose work is the host's

The examples so far call the runtime: allocate, box, return. A library that
draws to a canvas or plays a sound calls neither the runtime nor libc -- it
calls the host, and its primitives are little more than forwarding.

That works, and it is the shape to reach for wherever the capability belongs
to the embedder rather than to the program. Declare the import in `env` under
the `ash_host_` prefix:

    extern int ash_host_fiber_state(void);

`env` resolves against the program first and the host second, and a name
beginning `ash_host_` is never demanded of the program -- it is the same
namespace the program itself reaches suspension and sockets through. So a
library like this needs nothing exported on its behalf, and the program's ABI
does not widen at all.

The cost is that such a library only runs where the host answers those
imports. That is the honest position for a capability a sandbox does not
have: the native host and the browser host each supply what they can, and a
library asking for something neither has fails to load with the name in the
message.

## What is not done yet

- **The browser half.** The loader is the native host's. In a page the same
  steps run against `WebAssembly.instantiate`, and the fetch is what makes
  this worth doing: `program.wasm` small, each library fetched only if used.
- **Lazy loading.** Everything beside the program is loaded at start-up.
  Loading on first use needs instantiation from inside a guest call, which is
  a knot worth tying only once there is a reason to.
- **Unloading.** Nothing frees a library's data or its table slots.
- **`fmt` is still compiled in**, and should stay: its digests and zlib
  streams are pure computation, small, and a program that hashes has no other
  way to get them in a sandbox.
