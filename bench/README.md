# Benchmark suite

`scripts/ash_bench.py` sweeps the corpus in [`benchmarks.toml`](benchmarks.toml)
across ash's execution modes, checks that every run produced the right
answer, and writes a table plus a JSON baseline a later run can be gated
against. `scripts/run_perf_matrix.py` is a compatibility shim that forwards
here; do not add features to it.

```sh
cargo build --release -p ash_std && cargo build --release -p ash

./scripts/ash_bench.py --out-json bench/baselines/local.json     # sweep, record
./scripts/ash_bench.py --baseline bench/baselines/local.json     # gate; non-zero on regression
./scripts/ash_bench.py --benchmarks tiered_hotloop --gc-stats    # one benchmark, GC counters
./scripts/ash_bench.py --list
```

`--build` runs the cargo builds in the right order. The order matters:
`ash_std` is embedded into `ash`, so building `ash` against a stale
`ash_std` measures last week's runtime.

## Modes

| Mode | Flags | |
|---|---|---|
| `interp` | `--mode interp` | Interpreter only. The parity-validated reference and the speedup denominator. |
| `hybrid-auto` | `--mode hybrid --jit-tier auto` | The full ladder. |
| `hybrid-cranelift` | `--mode hybrid --jit-tier cranelift` | Ladder pinned to Cranelift. |
| `hybrid-llvm` | `--mode hybrid --jit-tier llvm` | Ladder pinned to LLVM. |
| `full-jit` | `--mode jit` | No interpreter: every function compiled at its first call, LLVM re-tiering the hot ones. |
| `hybrid-off` | `--mode hybrid --jit-tier off` | Control: tiering present, promotion disabled. Not in the default set. |
| `hybrid-eager` | `--jit-threshold 1 …` | Historical policy, kept so old numbers stay comparable. Not in the default set. |

`--modes a,b,c` selects; non-default modes run only when named. The manifest
also defines policy variants (`hybrid-auto-P-osr`, `interp-O1`, …) for
specific investigations; `--list` prints them.

## Metrics

**`MEDIAN` / `MIN` / `MAX` / `SD%`** — whole-process wall time in
milliseconds over `--iterations` runs (default 5) after `--warmups` (default
1). Read `SD%` before believing a delta; above ~5% the machine was busy.
This includes startup, bytecode load and, for JIT modes, compilation — the
number a user experiences. The `basic` row is the startup floor.

**`RSS`** — peak resident set size from a separate instrumented run under
`/usr/bin/time` (`-l` on macOS, `-v` on Linux). Separate because
`/usr/bin/time` adds a process, and because BSD `time(1)` reports a
signal-terminated child's status through `WEXITSTATUS`, which would erase a
SIGSEGV the runner needs to record.

**`SPEEDUP`** — this row's median against the first comparable mode's,
normally `interp`. Rows that are not `OK` get none.

**`TIER c/l`** — Cranelift / LLVM promotions, parsed from the `[tiered]`
summary line under `--jit-log`. A trailing `(+c/l)` means a tier engaged
after the counter was sampled: the counter is read when the entrypoint
returns, and LLVM installs on a background thread, so on a short benchmark
the install often lands after the sample. The runner collects the install
lines separately because "never engaged" and "engaged too late" are
different bugs.

**`GC`** — collections, with `--gc-stats`. The JSON also carries reclaimed
blocks, live blocks, allocated MB and pause times from ash's own report.
The allocation unit is MiB despite the runtime's `MB` suffix. With TLABs
enabled this counts capacity handed out at region refill, not exact bytes
allocated per object. Small measurement windows can report zero while
objects are allocated; see git-bug `7e9d728`. It is cumulative allocation
pressure, distinct from live memory and peak RSS.

The instrumented process is checked against the same correctness expectation
as the timed processes. Incorrect, failed, or timed-out diagnostics fail the
row and supply no GC/RSS counters. `--ash-base` gets its own checked diagnostic
run under `base.instrumented`; those runs remain outside timed samples.
`--diagnostics-dir DIR` retains their stdout/stderr, including tier logs.
CI keeps these files with the benchmark artifacts.
When present, `tier_installs[].installed_at_run_ms` records the runtime's
installation timestamp separately from the compilation duration. Installation
does not prove that the body was entered; use OSR/transfer logs as well.

`--ash PATH` selects an explicit executable. JSON records SHA-256 and size
for both executables, each bytecode input, and its main Haxe source when
available. The checkout revision alone does not identify a previously built
binary or its embedded runtime.

### Binary-tree allocation investigation

Git-bug `b002f3f` tracks generic allocation elimination; `ab70136` tracks the
baseline. Keep `BenchBinaryTrees.hx` and `bench_binary_trees.hl` unchanged:
depth 16, the existing iterations, and checksum `-104864`.

```sh
./scripts/ash_bench.py --ash target/release/ash \
  --benchmarks binary_trees --modes hybrid-auto --iterations 7 --warmups 1 \
  --gc-stats --tier-log --diagnostics-dir /tmp/trees-baseline/logs \
  --out-json /tmp/trees-baseline/hybrid.json
```

For a later comparison, add `--ash-base PATH --base-commit REV`; the runner
interleaves clean head/base processes on the same host. Keep each binary's
build revision, toolchain, and build commands alongside the result. AOT is
a separate control; hybrid must benefit during the actual workload, including
startup and promotion. Local Mac results are not comparable to CI timings.

The source constructs `2^(d+1)-1` nodes per depth-d tree, with
`2 * 2^(20-d)` trees at each even depth from 4 through 16, plus a depth-17
stretch tree and a depth-16 long-lived tree: 29,578,590 nodes in total.
At 32 aligned bytes per node on a 64-bit target this is 946,514,880 bytes
(902.67 MiB) before optimization and other runtime allocations. This is a
source-level model, not an exact prediction of the region counter: existing
scalar replacement already removes some objects. The retained depth-16 tree
alone is 131,071 nodes, approximately 4 MiB.

Capture static allocation evidence separately from execution:

```sh
ASH_VERIFY_AIR=only target/release/ash --quiet crates/ash/test/tests/bench_binary_trees.hl
ASH_SROA_WHY=1 ASH_VERIFY_AIR=dump:24 target/release/ash --quiet \
  crates/ash/test/tests/bench_binary_trees.hl 2>/tmp/trees-builder.air
ASH_SROA_WHY=1 ASH_VERIFY_AIR=dump:25 target/release/ash --quiet \
  crates/ash/test/tests/bench_binary_trees.hl 2>/tmp/trees-main.air
```

Indices 24/25 refer to the current fixture's builder/main; verify them against
the input and debug positions. These dumps show the diagnostic O3 pipeline,
not a count of dynamically allocated objects or proof of which body ran in a
particular tier. Returned roots and child stores currently block local SROA;
the immediate producer-consumer cases require analysis across those calls.

The [2026-10-12 baseline](baselines/binary-trees-allocation-2026-10-12.json)
contains input/binary hashes, toolchains, checked hybrid/AOT runs, diagnostic
controls, static AIR evidence, and the historical CI artifact with its limits.
Hybrid and AOT report 900.1 MiB; disabling AIR inlining reports 902.7 MiB;
normal optimization with TLABs disabled reports 900.0 MiB. Every completed
execution returns `-104864`. The static O3 model removes 87,378 roots (one
positive-item root per inner iteration, plus stretch and long-lived roots),
leaving 900.0002 MiB of tree storage before other runtime allocations. These
controls reconcile the rounded allocation figures without treating them as an
exact object counter. TLAB-disabled runs change collector behavior and are
accounting controls only. The local hybrid timings were noisy (16.5% relative
standard deviation); this checkpoint makes no speedup claim.

**`NOTE`** — which accepted checksum came back, or the failure reason.

## Correctness gate

A fast wrong answer is `INVALID`, excluded from all speedup and regression
arithmetic, and fails the gate. Every timed iteration is checked, not only
the last: promotion runs on a background thread, so a JIT mode can answer
differently on different runs of the same command, which is exactly the bug
to catch.

The runner also fingerprints each run's output and reports `output_stable`:

| Verdict | Reading |
|---|---|
| all correct, stable | `OK` |
| all wrong, stable | a deterministic miscompile |
| all wrong, unstable | uninitialized memory, not a miscompile |
| some wrong | a race, most likely against background promotion |
| all correct, unstable | `OK` with `WARNING: output varied across runs` |

Expectations use `parity_cases.toml`'s vocabulary, and a benchmark that
names a `parity_case` reads them from it:

- `exact` — normalized stdout equals a reference (`default` / `json_ws` /
  `none`, as in `crates/ash_cli/tests/common/mod.rs`).
- `checksum` — the `Checksum: N` line is in an accepted set declared in the
  manifest.
- `exit_only` — exit code 0.

The reference is a HashLink oracle bundle (`--oracle <dir>`, from
`scripts/generate_parity_oracle.py`), else an untimed `interp` run;
`reference_source` in the JSON says which. Stock HashLink does not run on
Apple Silicon, so locally the oracle is a CI artifact or nothing.

### Mandelbrot's checksums

Both Mandelbrot benchmarks accept two answers and the runner records which
one came back. Every engine fuses multiply-add pairs by default and agrees
on the result; `--no-fma` rounds every operation and matches HashLink:

| Benchmark | Checksum | Label | |
|---|---|---|---|
| `mandelbrot_small` (298²) | `22825041` | `fused` | the default, every engine; also `clang -ffp-contract=on` |
| `mandelbrot_small` | `22816350` | `unfused` | `--no-fma`; HashLink's JIT and the C reference at `-ffp-contract=off` |
| `mandelbrot` (875×500) | `112798587` | `fused` | the default, every engine |
| `mandelbrot` | `112790102` | `unfused` | `--no-fma`; HashLink's JIT |

A checksum outside the set is `INVALID`, and a label change against a
baseline is reported even when the time is flat.

## Statuses

| | |
|---|---|
| `OK` | ran, exited 0, expected answer; the only status that enters comparisons |
| `INVALID` | exited 0 with the wrong answer; always fails the gate |
| `FAIL` | non-zero exit or fatal signal, named |
| `TIMEOUT` | exceeded the per-run time box |
| `SKIP` | missing `.hl`, or a mode outside the benchmark's allowlist |

A failing cell never aborts the sweep.

## Load sensitivity

Runs are sequential and there is no `--jobs`. A concurrent build on the same
machine inflated a 6.7 s JIT pre-warm to 18.5 s, and because LLVM compiles on
a background thread a loaded machine changes which tier a short benchmark
finishes in — bimodal, not merely noisy. The runner reads the 1-minute load
average before starting and warns above `--load-threshold` × cores (default
0.4); `--ignore-load` silences it; both loads are recorded in the JSON.
Baselines record `system.cpu_model`, and the runner refuses to read a
comparison across different models as meaningful.

## Baselines and gating

```sh
./scripts/ash_bench.py --out-json bench/baselines/m1pro-2026-08-17.json   # record
./scripts/ash_bench.py --baseline bench/baselines/m1pro-2026-08-17.json   # gate
```

Per `(benchmark, mode)` on the median. A regression is a median worse by
more than `--regress-threshold` (default 10%) **and** more than
`--regress-min-ms` (default 5.0), or a status that got worse. Both bars,
because most of the corpus finishes in 15–25 ms of mostly startup and a
relative-only gate flags noise; a pair over the relative bar but under the
absolute one prints `noise (+N ms < 5ms floor)`. `INVALID` always fails.
`--min-speedup X` additionally requires each benchmark's best JIT mode to
beat `interp` by `X`; `--no-gate` reports and exits 0. Baseline files carry
`schema_version`; a mismatch is an error.

A bimodal sample — a tiered run whose compile lands before the loop ends on
some runs and not others — is split at its widest gap and both modes are
reported (`bimodal 3x886/4x1187`; `wall_ms.modes` in the JSON); the gate
compares fast mode against fast mode.

## A/B against a second binary

```sh
./scripts/ash_bench.py --ash-base /path/to/other/ash --base-commit abc1234
```

Every timed run of head is followed by one of base, so both see the same
machine in the same minute. The table gets a `vs base ±N%` note, an `A/B
against base` section follows, and each row's JSON carries the base numbers
under `base`. This is what the CI sweep does on every leg against the commit
the previous sweep measured, and it is the only cross-commit number on the
published page that means anything: the milliseconds come from whichever
runner the leg drew.

## Environment

The runner strips `ASH_GC_STATS`, `ASH_TIER_LOG` and `ASH_TIER` from the
inherited environment and exposes them as flags, so a run's configuration is
in its JSON:

| Flag | Env | |
|---|---|---|
| `--gc-stats` | `ASH_GC_STATS=1` | GC counters in the instrumented run |
| `--tier-log` | `ASH_TIER_LOG=1` | per-function install lines |
| `--gc-heap-mb N` | `ASH_GC_HEAP_MB=N` | heap reservation (default 512 MB); applies to timed runs too |

`--jit-log` goes to the instrumented run only.

## The corpus

`benchmarks.toml`, in run order. A bench with a `parity_case` inherits its
`.hl`, expectation, normalization, timeout and `slow` flag.

- `basic` — startup floor.
- `tiered_hotloop` — the ladder's designated exercise.
- `mandelbrot_small`, `mandelbrot` — FP kernels; `mandelbrot` is `slow` and
  needs `--include-slow` and a reduced iteration count.
- `array_push`, `gc`, `binary_trees` — allocation.
- `stdlib`, `jsonparse`, `jsonmin*`, `jsonarr`, `mapiter`, `map_simple`.
- `nbody`, `deltablue`, `fib`, the call benches — dispatch and calls.
- `simd_dot`, `simd_particles` — ash-simd.

`heaps_game` is not a benchmark: it opens a window and never returns. It is
carried under `--include-windowed`, restricted to `full-jit`, to keep a
machine-readable record of whether that build still crashes on it.

## JSON (schema v1)

Top level: `schema_version`, `generated_unix`, `generated_iso`,
`sweep_seconds`, `git`, `system`, `load`, `config`, `binaries`, `results`.
Each result:

```json
{
  "benchmark": "tiered_hotloop",
  "mode": "hybrid-cranelift",
  "group": "jit",
  "hl": "test_tiered_hotloop.hl",
  "command": "…/ash --mode hybrid --jit-tier cranelift --quiet …",
  "expectation": "exact",
  "status": "OK",
  "detail": "stdout matches interp",
  "reference_source": "interp",
  "wall_ms": { "min_ms": …, "median_ms": …, "mean_ms": …, "max_ms": …, "stddev_ms": …, "runs": 5 },
  "samples_ms": [ … ],
  "peak_rss_bytes": 34603008,
  "exit_code": 0,
  "signal": null,
  "checksum": { "value": "22816350", "label": "unfused", "accepted": true },
  "output_stable": true,
  "tiered": { "attempted": 1, "succeeded": 1, "failed": 0, "compiled_calls": 19886, "fallbacks": 0, "cranelift": 1, "llvm": 0 },
  "tier_installs": [ { "findex": 255, "tier": "cranelift", "compile_ms": 5.84 } ],
  "gc": { "collections": 0, "blocks_reclaimed": 0, "live_blocks": 0, … }
}
```

Raw `samples_ms` are kept so a later analysis can compute what this runner
did not.

## Related tools

| | |
|---|---|
| `scripts/hl_bench.py` | the HashLink JIT, HashLink/C and JVM lanes on the same corpus, for the published page |
| `scripts/aot_bench.py` | the AOT lane |
| `scripts/generate_parity_oracle.py` | the HashLink oracle bundle `--oracle` consumes; runs in CI |
| `crates/ash_cli/tests/parity_matrix.rs` | the parity gate proper; `ash_bench.py` borrows its case metadata and normalization |

## Opt-in recursive tree fusion experiment

This experiment has its own artifact-only CI job; it does not change the public
benchmark rows or enable the compiler optimization by default. Dispatch the
`benchmarks` workflow with `tree_fusion_only=true` to run only its build and
measurement jobs, without site publication. Ordinary scheduled sweeps also run
the experiment on a separate runner using the same prebuilt toolchain.

```sh
python3 scripts/bench_tree_fusion.py --ash target/release/ash \
  --source local --source-revision "$(git rev-parse HEAD)" \
  --pairs 7 --warmups 1 --diagnostics 2 --out /tmp/tree-fusion-results
```

`--source-revision` must name the source actually compiled into that binary.
The harness hashes the executable, source and bytecode; refuses a changed
benchmark artifact; alternates off/on and on/off pairs; and requires checksum
`-104864` from every executed guest. Both variants use the same release binary
and default hybrid settings. Timing includes process startup and compilation,
plus the development option's normal preparation messages. It excludes explicit
GC, JIT-log and profiler instrumentation. Inherited `ASH_*` overrides are removed.

Separate accounting runs disable TLABs on both sides, include managed and
external cumulative allocation, and retain GC counts/time, RSS, compiler phase
profiles and per-function tier installations. Each allocation counter is rounded
to 0.1 MiB, so the sum has up to ±0.1 MiB rounding uncertainty. RSS is a different
metric. Phase totals from background compiler threads overlap and must not be
summed into process wall time. An additional AIR dump verifies recursive helper
calls and absence of node/field operations; this inspection does not execute the
guest, so it supplies no checksum or timing evidence.

`result.json` retains all samples, commands, sanitized environment settings,
CPU/kernel provenance, binary/input hashes, paired ratios and a descriptive
bootstrap interval. Raw stdout/stderr accompanies each run, including failures.
Failed correctness/accounting/IR gates produce no performance summary. The
allocation gate requires substantial elimination while retaining approximately
4 MiB for the long-lived tree. Timing has no hard speed threshold on shared CI.

NUC results use `--source nuc` and stay distinct from actual CI evidence.
Whole-program latency and helper IR/compile evidence are reported separately;
this harness makes no isolated-helper throughput claim. Native AOT and wasm
fusion remain unsupported. Checkpoint `1499bef` tracks results and limitations.
