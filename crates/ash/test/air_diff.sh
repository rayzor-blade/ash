#!/bin/zsh
# Differential harness for the AIR v2 SSA interpreter.
#
# Runs each program under three configurations and diffs stdout:
#
#   off          the opcode interpreter — the reference
#   v2           the SSA IR executed directly (crates/ash_interp/src/ssa.rs)
#   v2-serialize lower -> optimize -> serialize back to opcodes -> interpret
#
# The third column is what makes a disagreement cheap to localize. `serialize`
# is a test oracle rather than a production path, and it shares the lowering and
# the optimizer with `v2` while sharing the *dispatcher* with `off`. So:
#
#   v2 differs, serialize differs  -> lowering or an optimizer pass
#   v2 differs, serialize agrees   -> the SSA dispatcher
#   v2 agrees,  serialize differs  -> the serializer
#
# A differing exit code counts as a difference even when stdout matches: a
# program that dies halfway can still have printed identical output up to that
# point. For each difference the first diverging line is printed, with the
# reference on the `<` side.
#
# Roughly 1% of interpreter runs on allocation-heavy programs are not
# reproducible — a pre-existing defect that hits `off`, `v2` and `v2-serialize`
# at indistinguishable rates (measured 5/400, 4/400, 4/400), so a single
# disagreeing run is not evidence of a real divergence. Anything that differs is
# therefore re-run `CONFIRM` times and only reported as DIFFER if it reproduces
# every time; otherwise it is reported as FLAKY and does not fail the run.
#
# Usage:
#   crates/ash/test/air_diff.sh                  # whole corpus, ASH_AIR_LEVEL=O2
#   crates/ash/test/air_diff.sh path/to/prog.hl  # one program
#   LEVELS="O0 O1 O2 O3" crates/ash/test/air_diff.sh
#   SLOW=1 crates/ash/test/air_diff.sh           # include mandelbrot/nbody
#   CONFIRM=5 crates/ash/test/air_diff.sh        # re-runs before calling it real
#
# Expects `cargo build -p ash_cli` to have run; see the build-order note in
# CLAUDE.md, because ash_std is embedded with no cargo dependency edge.

set -u
root=${0:A:h:h:h:h}
cli=$root/target/debug/ash_cli
[[ -x $cli ]] || { print -u2 "no ash_cli at $cli — build it first"; exit 2; }

work=${TMPDIR:-/tmp}/air_diff.$$
mkdir -p $work
trap 'rm -rf $work' EXIT

timeout_bin=$(command -v timeout || command -v gtimeout || true)
TIMEOUT=${TIMEOUT:-180}
LEVELS=${LEVELS:-O2}
CONFIRM=${CONFIRM:-3}

if (( $# > 0 )); then
  progs=($@)
else
  progs=($root/crates/ash/test/tests/*.hl)
  if [[ -z ${SLOW:-} ]]; then
    # Excluded by runtime, not by expectation: interpreting either takes
    # minutes. `SLOW=1` puts them back.
    progs=(${progs:#*test_mandelbrot.hl})
    progs=(${progs:#*test_nbody.hl})
  fi
fi

run() {  # run <program> <ASH_AIR value> <output tag>
  local out=$work/$3
  if [[ -n $timeout_bin ]]; then
    ASH_AIR=$2 ASH_AIR_LEVEL=$level $timeout_bin $TIMEOUT \
      $cli --quiet --mode interp $1 >$out.stdout 2>$out.stderr
  else
    ASH_AIR=$2 ASH_AIR_LEVEL=$level \
      $cli --quiet --mode interp $1 >$out.stdout 2>$out.stderr
  fi
  print $? >$out.rc
}

# First line on which two files disagree, as a two-line diff excerpt.
first_divergence() {
  diff $1 $2 2>/dev/null | grep -E '^[<>]' | head -2 | sed 's/^/      /'
}

# Re-run the reference and one candidate config and report whether they still
# disagree. Used to tell a real divergence from the ~1% flake described above.
reproduces() {  # reproduces <program> <ASH_AIR value> <tag>
  local p=$1 cfg=$2 tag=$3 i
  for (( i = 0; i < CONFIRM; i++ )); do
    run $p off ref2
    run $p $cfg $tag
    if cmp -s $work/ref2.stdout $work/$tag.stdout &&
       [[ $(<$work/ref2.rc) == $(<$work/$tag.rc) ]]; then
      return 1   # agreed on a re-run, so the first disagreement was noise
    fi
  done
  return 0
}

rc_all=0
for level in ${=LEVELS}; do
  print "=== ASH_AIR_LEVEL=$level ==="
  printf "%-34s %-8s %-8s %s\n" PROGRAM v2 serialize "exit off/v2/ser"
  same=0 differ=0
  for p in $progs; do
    name=${p:t:r}
    run $p off ref
    run $p v2 ssa
    run $p v2-serialize ser
    rc_ref=$(<$work/ref.rc) rc_ssa=$(<$work/ssa.rc) rc_ser=$(<$work/ser.rc)

    if cmp -s $work/ref.stdout $work/ssa.stdout && [[ $rc_ref == $rc_ssa ]]; then
      v2=agree; same=$((same+1))
    elif reproduces $p v2 ssa2; then
      v2=DIFFER; differ=$((differ+1)); rc_all=1
    else
      v2=FLAKY; same=$((same+1))
    fi
    if cmp -s $work/ref.stdout $work/ser.stdout && [[ $rc_ref == $rc_ser ]]; then
      sr=agree
    elif reproduces $p v2-serialize ser2; then
      sr=DIFFER; rc_all=1
    else
      sr=FLAKY
    fi

    printf "%-34s %-8s %-8s %s/%s/%s\n" $name $v2 $sr $rc_ref $rc_ssa $rc_ser
    [[ $v2 == DIFFER ]] && { print "    v2 first divergence (< = reference):"
                             first_divergence $work/ref.stdout $work/ssa.stdout }
    [[ $sr == DIFFER ]] && { print "    serialize first divergence (< = reference):"
                             first_divergence $work/ref.stdout $work/ser.stdout }
  done
  printf -- "--- v2 vs reference: %s agree / %s differ ---\n\n" $same $differ
done
exit $rc_all
