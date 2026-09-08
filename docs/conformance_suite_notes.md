# Which Haxe suites can run on ash

Only four suites execute on the HL VM upstream, so only four can be conformance
rows here. The rest test the compiler, not a program.

Ground truth is upstream's own CI: `tests/runci/targets/Hl.hx` says what runs on
HashLink, `Macro.hx` and `Js.hx` say what runs elsewhere. Swept 2026-08-28
against the Haxe 4.3.6 checkout in
`~/.cache/ash-haxe-conformance/haxe-tests-4.3.6/tests`.

## Runs on the VM

| Suite | Note |
|---|---|
| `unit` | in our suite |
| `threads` | in our suite |
| `sys` | in our suite |
| `misc/eventLoop` | the only newly-integrated VM execution |

Everything else in `misc` is compile-only, including `misc/hl` — upstream's CI
compiles those four projects and never runs them. `misc/hl/reserved-keywords`
and the compile-hlc steps are the HL/C native path, not bytecode, and run only
on Linux CI.

`optimization`, `nullsafety`, `server` and `sourcemaps` do not appear in
`Hl.hx` at all:

| Suite | Runner | What is under test |
|---|---|---|
| `optimization` | `Js.hx` | the compiler's analyzer, plus expectations that are literal JS source strings in `@:js` metadata |
| `nullsafety` | `Macro.hx` | a compile-time diagnostic pass; emits no artifact |
| `server` | `Js.hx` + node | the compilation server over the IDE protocol |
| `sourcemaps` | `Macro.hx` | the emitted source map; HL emits none |

## Gaps found

**`misc/eventLoop`: none.** Built with haxe 4.3.6 and run under
`target/release/ash`: exit 0 in both interp and jit, stdout byte-identical to
upstream's expectation file — all 23 lines in order.

**`misc/hl/projects/Issue11196`: advisory.** Its `out.hl` exits 0, but the
loader reports three unresolved natives: `std@hlp_bytes_subtract`,
`std@hlp_bytes_address`, `std@hlp_bytes_from_address`. Upstream CI never
executes this file, so it is not a conformance failure — but real HashLink
resolves them, and any `-dce no` program that calls `haxe.io.Bytes` address ops
will fail on ash. Worth implementing.

## The EMPTY classification is a tripwire

No case in these suites gates its `main` away from HL. Sweeping every `.hx`
under `misc/projects`, `misc/resolution`, `misc/compiler_loops`,
`misc/eventLoop`, `nullsafety/src`, `optimization/src`, `sourcemaps/src` and
`server/src`: `misc/eventLoop/Main.hx` has three `#if sys` blocks, all active on
HL; about 25 other hits are `#if macro`; one is part of a compile-fail fixture's
expected error.

Target-specificity here is expressed with hxml target flags and whole
per-target directories, never `#if <target>` around `main`. So the pipeline's
EMPTY bucket has no current members — keep it for future cases.

## Method

Counts and gates come from direct sweeps of the checkout (target `#if`, hxml
target flags, `--cmd` lines) plus reading `runci/targets/`. The eventLoop and
Issue11196 verdicts come from building with haxe 4.3.6 and running under a
snapshot of `target/release/ash`, snapshotted because another session relinks
it.
