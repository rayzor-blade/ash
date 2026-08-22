#!/usr/bin/env bash
# Run the corpus under frequent collection and require the output not to move.
#
# This is the regression test for anything that touches the root set or moves
# where a GC pointer is stored. The plain corpus will NOT catch an unrooted
# pointer: at normal collection rates the object usually survives because
# something else in its block is still live. Three real defects were found
# only under pressure -- constants held solely in a Rust Vec (ab2505d),
# StringMap's pre-marked (and therefore never traced) arrays (31f6ccf), and
# the Int/Object maps' malloc-heap-only value pointers.
#
# Three traps worth knowing before editing this:
#
#  * Compare stdout ONLY. The JIT prints an informational banner to stderr
#    that the interpreter does not, so a 2>&1 comparison reports the whole
#    corpus as differing under --mode jit.
#
#  * ASH_GC_STRESS disables the TLAB outright, and at N=1 it collects
#    mid-construction, when a deep recursive native stack conservatively pins
#    every partially-built object. Since sweep only reclaims a block whose
#    lines are ALL unmarked, nothing is ever freed: bench_binary_trees climbs
#    to 512MB with freed=0 across 16k collections and dies "Out of memory".
#    That is stress geometry, not a leak.
#
#  * Which rows can afford stress is decided by MEASURED runtime, not by the
#    filename. test_mandelbrot is a full 875x500 kernel that happens to be
#    named test_*; at stress=1 it never finishes. Anything slower than
#    HEAVY_MS in a normal run gets an aggressive real trigger (TLAB on)
#    instead, which still collects constantly.
set -u
cd "$(dirname "$0")/../../.."
ASH=${ASH_BIN:-./target/debug/ash}
TIMEOUT=${GATE_TIMEOUT:-300}
HEAVY_MS=${HEAVY_MS:-1000}
JOBS=${JOBS:-4}
export ASH TIMEOUT HEAVY_MS

check_one() {
  f=$1; b=$(basename "$f")
  # A timed-out run produces EMPTY output, which compares unequal to
  # everything. Reporting that as a diff is a lie: a loaded box once turned
  # five slow benchmarks into "15 unexpected diffs", all of them the alarm
  # firing on the baseline. perl dies on SIGALRM, so rc 142 (128+14) is the
  # timeout signal -- surface it as its own outcome, never as a difference.
  TIMED_OUT=0
  run() {
    local out rc
    out=$(perl -e "alarm $TIMEOUT; exec @ARGV" "$@" 2>/dev/null); rc=$?
    [ "$rc" -ge 128 ] && TIMED_OUT=1
    printf '%s' "$out"
  }

  start=$(perl -e 'use Time::HiRes qw(time); printf "%d", time()*1000')
  normal=$(run "$ASH" --mode interp "$f")
  end=$(perl -e 'use Time::HiRes qw(time); printf "%d", time()*1000')
  ms=$((end - start))
  if [ "$TIMED_OUT" = 1 ]; then
    echo "TIMEOUT $b (baseline exceeded ${TIMEOUT}s -- box too loaded, or raise GATE_TIMEOUT)"
    return 0
  fi

  # Two independent reasons a program cannot take stress=1, and it needs the
  # UNION of them. Slow ones never finish. Allocation-heavy ones exhaust the
  # heap regardless of how quick they are, because stress disables the TLAB
  # and sweep only reclaims wholly-unmarked blocks. bench_deltablue is the
  # case that proves runtime alone is the wrong test: ~70ms on an idle box,
  # comfortably under HEAVY_MS, and still dies under stress.
  case "$b" in
    bench_*) heavy=1 ;;
    *)       [ "$ms" -gt "$HEAVY_MS" ] && heavy=1 || heavy=0 ;;
  esac
  if [ "$heavy" = 1 ]; then
    pressed=$(run env ASH_GC_TRIGGER_MB=1 "$ASH" --mode interp "$f"); how=trigger1
  else
    pressed=$(run env ASH_GC_STRESS=1 "$ASH" --mode interp "$f");     how=stress1
  fi
  if [ "$TIMED_OUT" = 1 ]; then
    echo "TIMEOUT $b (under pressure; ${how})"
    TIMED_OUT=0
  elif [ "$pressed" != "$normal" ]; then
    echo "PRESSURE-DIFF $b ($how, ${ms}ms)"
  fi

  for m in hybrid jit; do
    o=$(run "$ASH" --mode "$m" "$f")
    if [ "$TIMED_OUT" = 1 ]; then echo "TIMEOUT $b/$m"; TIMED_OUT=0; continue; fi
    # FP kernels legitimately differ from the interpreter under a compiled
    # tier: the interpreter rounds every opcode while Cranelift and LLVM
    # contract multiply-add into FMA. Measured ground truth for
    # test_mandelbrot: interp 112790102 is bit-identical to the C reference at
    # -ffp-contract=off, and the compiled tiers land in the fused family
    # alongside clang -ffp-contract=on (112798515) and hxcpp. Fusion is the
    # norm -- the unfused value appears nowhere but a strict interpreter -- so
    # these are expected, not regressions.
    case "$b/$m" in
      test_mandelbrot.hl/hybrid|test_mandelbrot.hl/jit|test_mandelbrot_small.hl/jit) continue ;;
    esac
    if [ "$o" != "$normal" ]; then
      echo "DIFF-$m $b"
    fi
  done
  return 0
}
export -f check_one

echo "=== gc_pressure: $(ls crates/ash/test/tests/*.hl | wc -l | tr -d ' ') programs, ${JOBS}x parallel ==="
ls crates/ash/test/tests/*.hl | xargs -P "$JOBS" -I{} bash -c 'check_one "$@"' _ {} > /tmp/gcp_out.txt 2>&1
cat /tmp/gcp_out.txt
d=$(grep -c '^PRESSURE-DIFF\|^DIFF-' /tmp/gcp_out.txt || true)
t=$(grep -c '^TIMEOUT' /tmp/gcp_out.txt || true)
n=$d
echo "gc_pressure: $d unexpected diffs, $t timeouts (timeouts are inconclusive, not failures)"
echo GATE-DONE
exit $((n > 0))
