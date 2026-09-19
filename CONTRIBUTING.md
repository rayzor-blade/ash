# Contributing

For changing ASH. Users of ASH need the [README](README.md) and
[docs/](docs/README.md), not this.

## Layout

A Rust workspace. In the order a program meets them:

| Crate | Directory | Role |
|-------|-----------|------|
| **ash** | `crates/ash_cli` | The binary. Command line, mode selection, the AOT driver. |
| **ash_core** | `crates/ash` | Bytecode decoder, native symbol resolution, the Cranelift and LLVM backends, the AOT and wasm emitters. |
| **ash_interp** | `crates/ash_interp` | The interpreter and the promotion path into the compilers. |
| **air** | `crates/air` | The IR both backends lower, and its passes. |
| **ash_std** | `std` | HashLink's standard library in Rust. Compiled to a shared library and embedded into the binary. |
| **ash_simd**, **ash_hdll_simd** | `crates/ash_simd`, `crates/ash_hdll_simd` | The ash-simd primitives; the second crate is `simd.hdll` for stock HashLink. |
| **ash_wasm_link**, **ash_wasm_runtime**, **ash_browser** | `crates/…` | The wasm linker, the in-module runtime half plus native host, the browser host. |
| **ash_macro**, **ash_native_call**, **ash_trace** | `crates/…` | FFI symbol loading, the native-call dispatch table, exception trace rendering. |

**AIR** is a typed phi-SSA IR built from HL bytecode. It carries trap regions,
an effect classification per instruction (pure / reads memory / writes memory
/ may throw / clobbers all) and an alias model over fields, array data,
globals, raw bytes and cells. GVN, LICM, SROA, inlining, the FMA peephole,
bounds-check elimination (`redundant-guard-elim`: a loop counter proven
non-negative makes the unsigned array guard the loop's own exit test, which
GVN has already unified) and the loop widener run over it, and last
`stripmine`, which names the
induction variable of each innermost loop so a backend can test its low
bits and poll once per strip of iterations instead of on every one. Both
backends lower it directly; the interpreter can walk it (`ASH_AIR=v2`);
`serialize` turns it back into HL opcodes, which is what `--emit-optimized`
writes and what the opcode interpreter executes.

**Tiers.** The interpreter runs everything and counts calls. `beadie` brokers
promotion: it compiles a hot function with Cranelift on a background thread
and publishes the code pointer atomically. Hot Cranelift code is recompiled by
LLVM. Transfers into compiled code happen at the next call or, for a running
loop, at a loop header (OSR); Cranelift → LLVM transfers use typed SSA
snapshots of the live values.

**Runtime.** `ash_std` implements the `hlp_*` primitives and the GC (Immix,
conservative stack scanning, per-thread TLABs, adaptive trigger). Haxe threads
are krio fibers on one OS thread. Every native the program can reach is
resolved once into a process-global `lib@symbol → address` table used by all
tiers.

## Prerequisites

- Rust nightly (`rust-toolchain.toml`).
- LLVM 21. `llvm-sys` reads `LLVM_SYS_211_PREFIX`, else `llvm-config` on `PATH`.

  ```bash
  brew install llvm                                   # macOS
  export LLVM_SYS_211_PREFIX=/opt/homebrew/opt/llvm

  apt install llvm-21-dev                             # Debian/Ubuntu
  export LLVM_SYS_211_PREFIX=/usr/lib/llvm-21
  ```

- libclang, for bindgen over the runtime headers. Point `LIBCLANG_PATH` at
  LLVM 21's `lib`; a newer libclang degrades every struct to an opaque type
  and the build fails with hundreds of "no field" errors.
- Haxe 4.3.x, only to recompile test sources. The `.hl` files are committed.

Windows: [docs/internals/windows-port.md](docs/internals/windows-port.md).

## Building

```bash
cargo build -p ash_std
cargo build -p ash
```

`ash_std` first, always, after any change under `std/`. The runtime is
embedded into the binary as bytes by `crates/ash/build.rs`, which cargo does
not model as a dependency. The build script warns when the embedded library
is older than the sources. Release builds follow the same order; if no
release `ash_std` exists the script falls back to the debug one and warns,
and that binary must not be measured.

The `llvm` feature (default on) gates the LLVM tier and the AOT compiler.
`--no-default-features` links no LLVM and refuses `--build`, `--emit-aot`,
`--hot-reload` and `--jit-tier=llvm`.

`make` builds the host target in release with LTO; `make all` builds every
installed target.

The wasm build links `ash_runtime.o` — `ash_std` for `wasm32-wasip1` plus
wasi-libc and libsetjmp in one relocatable object. `scripts/build_wasm_runtime.py`
builds it, for both profiles; nothing rebuilds it automatically.
`cargo test -p ash --test wasm_runtime_fresh` detects a stale one and names
the missing export.

## Tests

```bash
cargo test -p air                                    # IR and passes
cargo test -p ash_core --lib
cargo test -p ash_std
cargo test -p ash --test parity_matrix               # interp and hybrid vs HashLink
cargo test -p ash --test aot_smoke                   # AOT binaries vs the JIT, byte for byte
```

**Parity.** `crates/ash/test/tests/parity_cases.toml` names programs whose
output must equal stock HashLink's. The reference is an oracle bundle
recorded in CI (`ASH_PARITY_ORACLE_DIR`), because stock HashLink does not run
on Apple Silicon; without one the harness falls back to `haxe --interp`, which
disagrees with HashLink on float formatting and integer overflow. A case that
needs a library sets `classpath`.

**Conformance.** `scripts/haxe_conformance.py` builds the upstream `unit`,
`sys`, `threads` and `misc/eventLoop` suites and runs them per case under each
engine. [docs/internals/conformance-suites.md](docs/internals/conformance-suites.md)
records which upstream suites execute on a VM at all.

**Benchmarks.** `scripts/ash_bench.py` over `bench/benchmarks.toml`;
[bench/README.md](bench/README.md). Numbers only mean something from an
otherwise idle machine, and only within one sweep; the published page is fed
by CI's nightly sweep, which A/Bs HEAD against the previous commit on the same
runner.

Single programs:

```bash
cargo run -p ash -- --mode interp crates/ash/test/tests/test_basic.hl
cargo run -p ash -- --mode hybrid --jit-threshold 1 crates/ash/test/tests/test_basic.hl
```

The Mandelbrot checksums are engine fingerprints: the interpreter rounds
multiply and add separately, compiled code fuses them.

| | 298² | 875×500 |
|---|---|---|
| unfused | 22816350 | 112790102 |
| fused | 22825041 | 112798515 |

Source: `crates/ash/test/tests/Mandelbrot_reference.c`.

## CI

- **lint** — `cargo clippy --workspace --all-targets -- -D warnings`, the
  same for the wasm targets, and a full Windows link.
- **test** — unit tests, the HDLL callback corpus on three OSes, an AOT
  executable on four platforms.
- **parity_oracle**, **parity_gate** — record stock HashLink's output (with
  `simd.hdll` installed), then compare the interpreter and hybrid arms.
- **haxe-conformance** — the upstream suites on interp, AOT, wasm,
  wasm-fibers.
- **benchmarks** — nightly sweep; the push trigger only redeploys the site.

A red check is fixed in the same change.

## Conventions

- Comments state what the code does and the constraint that shaped it. No
  measurements, no history of attempts — those belong in the issue.
- Commits: short subject, body only when the change is not self-evident,
  `git-bug: <64-character id>` trailer. Not the short id; a forge autolinks it
  as a commit.
- No planning or status documents in the tree. File an issue.
- `cargo fmt --all`; clippy clean with `-D warnings`.

## Issues

[git-bug](https://github.com/git-bug/git-bug). Issues are git objects under
`refs/bugs/*`: they travel with a clone and never touch the working tree.

```sh
brew install git-bug                 # or: go install github.com/git-bug/git-bug@latest
git-bug pull                         # a plain `git pull` does not fetch them
git-bug bug                          # id, status, title
git-bug bug --label area:gc --status open
git-bug bug show <id>
git-bug termui                       # or `git-bug webui`
```

Once per clone:

```sh
git-bug user new -n "<name>" -e "<email>" --non-interactive
printf '%s\n\n%s\n' "<title>" "<body>" > /tmp/issue.md
git-bug bug new -F /tmp/issue.md --non-interactive
git-bug bug label new <id> bug area:gc
```

`-F` is parsed like a commit message: title, blank line, body. `-t` is
ignored when `-F` is present. Labels: a kind (`bug`, `perf`, `debt`), an area
(`area:gc`, `area:air`, `area:jit`, `area:aot`, `area:cranelift`,
`area:runtime`, `area:interp`, `area:tooling`, `area:ci`, `area:conformance`,
`area:portability`), `priority:high` when it blocks other work. Filter with
`--label`; the query parser does not accept the colon in `label:area:gc`.
Close the issue when the work lands and cite it in the commit. `git-bug push`
publishes; `git push` does not.

## Internals

| | |
|---|---|
| [docs/internals/air-vectorization.md](docs/internals/air-vectorization.md) | the loop widener: rules, blockers, next steps |
| [docs/internals/wasm-abi.md](docs/internals/wasm-abi.md) | the 32-bit target ABI, dynamic calls on wasm |
| [docs/internals/wasm-exceptions.md](docs/internals/wasm-exceptions.md) | setjmp exceptions on wasm and the LLVM exception-model patch |
| [docs/internals/wasm-fibers.md](docs/internals/wasm-fibers.md) | the link-time transform that suspends a fiber inside a module |
| [docs/internals/wasm-threads.md](docs/internals/wasm-threads.md) | shared memory, Workers, threads on wasm |
| [docs/internals/windows-port.md](docs/internals/windows-port.md) | Windows: runtime loading, deliberate differences, open items |
| [docs/internals/conformance-suites.md](docs/internals/conformance-suites.md) | which upstream suites run on a VM |
| [docs/internals/parity-backlog.md](docs/internals/parity-backlog.md) | open parity deltas |
| [bench/README.md](bench/README.md) | the benchmark runner |
