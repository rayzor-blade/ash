# Which Haxe suites run on a VM

Only four of the upstream test suites execute on the HL VM, so only four can
be conformance rows. The rest test the compiler. Ground truth is upstream's
CI: `tests/runci/targets/Hl.hx` says what runs on HashLink, `Macro.hx` and
`Js.hx` what runs elsewhere. Swept against the Haxe 4.3.6 checkout the
conformance script maintains under `~/.cache/ash-haxe-conformance/`.

## Runs on the VM

| Suite | |
|---|---|
| `unit` | in the suite |
| `threads` | in the suite |
| `sys` | in the suite |
| `misc/eventLoop` | in the suite; the only other VM execution upstream performs |

Everything else in `misc` is compile-only, including `misc/hl`: upstream's
CI compiles those projects and never runs them. `misc/hl/reserved-keywords`
and the compile-hlc steps are the HL/C path, Linux CI only.

`optimization`, `nullsafety`, `server` and `sourcemaps` do not appear in
`Hl.hx`:

| Suite | Runner | Under test |
|---|---|---|
| `optimization` | `Js.hx` | the analyzer, with expectations as literal JS source in `@:js` metadata |
| `nullsafety` | `Macro.hx` | a compile-time diagnostic pass; no artifact |
| `server` | `Js.hx` + node | the compilation server over the IDE protocol |
| `sourcemaps` | `Macro.hx` | the emitted source map; HL emits none |

## Notes

`misc/hl/projects/Issue11196` exits 0 but reports three unresolved natives —
`std@hlp_bytes_subtract`, `std@hlp_bytes_address`,
`std@hlp_bytes_from_address`. Upstream never executes it, so it is not a
conformance failure, but real HashLink resolves them and a `-dce no` program
using `haxe.io.Bytes` address operations would fail on ash.

No case in these suites gates its `main` behind a target `#if`; target
specificity is expressed with hxml flags and per-target directories. The
pipeline's EMPTY bucket therefore has no members. Keep it.
