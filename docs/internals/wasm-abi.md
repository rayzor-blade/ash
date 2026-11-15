# wasm: target ABI and calls

How ash lays out a program for a 32-bit target, and how it calls a callee
known only at run time. User-facing material is in
[../wasm/README.md](../wasm/README.md).

## Target ABI

`TargetAbi` (`crates/ash/src/target_abi.rs`) holds the triple, the pointer
width, every HashLink layout derived from that width, and the target's
capabilities. It is chosen before anything is decoded.
`BytecodeDecoder::decode_for_abi` takes it, so enum offsets are computed for
the target rather than inherited from the compiler process; lowering reads
field offsets and array element sizes from it; the module's triple and data
layout are set before a body is emitted.

`cargo test -p ash --test wasm_target` fails if an import outside the
runtime's own surface appears.

**No relocation may reach an undefined data symbol.** `--allow-undefined`
covers functions only. `ash_fiber_poll_epoch`, the loop safepoint's word,
was referenced directly and broke the link; generated code reads it through
a pointer `ash_late_init` fills from the runtime's getter, the same
indirection a Mach-O dylib needs. `TargetAbi::direct_data_relocations`
records this.

The following are still derived from the host, and each is a bug waiting on a
32-bit target: AOT constants assuming an eight-byte object header, AOT helper
signatures using `i64` where the runtime takes `usize`, AOT data reading
`hl_runtime_obj` offsets with host `offset_of!`, `RefData` using the host
`size_of::<varray>()`, and static closure emission assuming a 32-byte
`vclosure` with `stackCount`.

## Function pointers on wasm

The tiered runtime names a not-yet-compiled body with a `findex + 1` sentinel
and distinguishes it from real code by magnitude: below `0x100000` is a
sentinel, because no native code address is. On wasm a function pointer is a
table index in the low hundreds, so the check classified every real function
as a sentinel: `hlp_call_method` handed one to the closure runner, which
called back into `hlp_call_method`, until the shadow stack wrapped.

Now only `fiber::is_stub_sentinel` makes this check, and on wasm it returns
false: nothing there creates a sentinel, since the target has no interpreter
and no tiers. Whether a code pointer can be a small integer is a property of
the target, not of whether a stub resolver was installed (`--mode jit`
creates sentinels without one).

## Dynamic calls use trampolines

`ash_static_call` places values in registers and jumps. WebAssembly has no
registers and type-checks every indirect call, so a call whose signature is
known only at run time cannot be assembled.

The compiler sees every function type in the program, so it emits one
trampoline per distinct signature — `(fun, args, out) -> ptr`, unpacking
arguments, making one statically typed call, storing the result — registered
under a key computed from the `hl_type`
(`crates/ash/src/llvm/aot_trampoline.rs`). A miss reports its key and
argument count.

Three constraints: a method's closure form is not in the type table, so the
emitter registers the `hlp_get_closure_type` shape as well; registration
belongs in `ash_late_init`, not `emit_module_init`, which runs during `build`
before trampolines exist; and the count is a `usize` — declaring it `i64`
gives `rust-lld: warning: function signature mismatch`: the linker only
warns, and the call is corrupt.
