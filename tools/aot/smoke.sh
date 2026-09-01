#!/usr/bin/env bash
# End-to-end check that the AOT toolchain works: emit -> link -> run, and
# require the binary to say exactly what the JIT says for the same program.
#
# Byte-identical stdout is the gate, not "it exited 0". An AOT module that
# starts, runs and prints something slightly different is the failure mode
# worth catching -- the constant pool, the type table and the global slots all
# have to be right before a single line comes out the same.
set -euo pipefail
cd "$(dirname "$0")/../.."
tmp="${TMPDIR:-/tmp}/ash-aot-smoke"
mkdir -p "$tmp"

# The shipped command, not the example. `ash --emit-aot` is how anyone
# actually compiles ahead of time, so it is what the gate should exercise --
# and requiring a separate `cargo build --example` was enough to make this
# whole step unrunnable on a machine that had only built the binary.
spike=target/debug/ash
[ -x "$spike" ] || spike=target/release/ash
[ -x "$spike" ] || { echo "build first: cargo build -p ash" >&2; exit 1; }
ash=target/debug/ash
[ -x "$ash" ] || ash=target/release/ash
[ -x "$ash" ] || { echo "build first: cargo build -p ash" >&2; exit 1; }

programs=("$@")
if [ ${#programs[@]} -eq 0 ]; then
  programs=(
    crates/ash/test/tests/bench_fib.hl
    crates/ash/test/tests/test_basic.hl
    crates/ash/test/tests/test_stdlib.hl
    crates/ash/test/tests/bench_deltablue.hl
    # Anonymous structures. AOT baked a virtual type's `lookup` as null while
    # baking `indexes`, so the lazy-init guard never fired and every
    # hash-keyed field access failed -- Reflect.field and plain `dyn.name`
    # returning null, hasField answering false, Std.string aborting -- on
    # bytecode the interpreter and the JIT both ran correctly. Nothing in the
    # old list touched a virtual, which is why it went unnoticed.
    crates/ash/test/tests/test_feature_typedef_anon.hl
    crates/ash/test/tests/test_std_reflect_type.hl
  )
fi

failed=0
for hl in "${programs[@]}"; do
  name="$(basename "$hl" .hl)"
  "$spike" --emit-aot "$tmp/$name.o" "$hl" > "$tmp/$name.emit" 2>&1 ||
    { echo "$name: EMIT FAILED"; sed -n '1,3p' "$tmp/$name.emit"; failed=1; continue; }
  ./tools/aot/link.sh "$tmp/$name.o" "$tmp/$name" >/dev/null ||
    { echo "$name: LINK FAILED"; failed=1; continue; }

  "$ash" --mode jit "$hl" 2>&1 | grep -v "returned: Void" > "$tmp/$name.jit"
  "$tmp/$name" > "$tmp/$name.aot" 2>&1 || true
  if diff -q "$tmp/$name.jit" "$tmp/$name.aot" >/dev/null; then
    echo "$name: OK ($(grep -o 'lowered [0-9/]*' "$tmp/$name.emit" | head -1))"
  else
    echo "$name: DIFFERS from the JIT"
    diff "$tmp/$name.jit" "$tmp/$name.aot" | head -6
    failed=1
  fi
done

[ "$failed" -eq 0 ] || { echo "SMOKE FAILED"; exit 1; }
echo "smoke OK -- every AOT binary matched the JIT byte for byte"
