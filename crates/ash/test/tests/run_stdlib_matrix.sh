#!/usr/bin/env bash
set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../../../../" && pwd)"

TESTS=(
  "TestStdStringTools:test_std_stringtools.hl"
  "TestStdDate:test_std_date.hl"
  "TestStdBytes:test_std_bytes.hl"
  "TestStdEReg:test_std_ereg.hl"
  "TestStdReflectType:test_std_reflect_type.hl"
)

echo "==> Compiling Haxe tests to .hl"
for test in "${TESTS[@]}"; do
  IFS=":" read -r main hl <<< "$test"
  echo "  - $main -> $hl"
  haxe --cwd "$SCRIPT_DIR" -main "$main" -hl "$hl"
done

echo
echo "==> Baseline check with Haxe interpreter"
baseline_fail=0
for test in "${TESTS[@]}"; do
  IFS=":" read -r main _ <<< "$test"
  if haxe --cwd "$SCRIPT_DIR" -main "$main" --interp; then
    echo "  [BASELINE PASS] $main"
  else
    echo "  [BASELINE FAIL] $main"
    baseline_fail=1
  fi
done

echo
echo "==> Running ash_cli interpreter"
cd "$ROOT_DIR" || exit 1
cargo build -q -p ash_cli
BIN="$(find "$ROOT_DIR/target" -type f -name ash_cli | head -n1)"
if [[ -z "$BIN" ]]; then
  echo "Unable to locate ash_cli binary under $ROOT_DIR/target"
  exit 1
fi

ash_fail=0
for test in "${TESTS[@]}"; do
  IFS=":" read -r main hl <<< "$test"
  hl_path="$SCRIPT_DIR/$hl"
  echo "  - $main ($hl)"
  if "$BIN" --mode interp "$hl_path"; then
    echo "    [ASH PASS]"
  else
    echo "    [ASH FAIL]"
    ash_fail=1
  fi
done

echo
echo "==> Summary"
echo "  baseline_fail=$baseline_fail"
echo "  ash_fail=$ash_fail"

if [[ $baseline_fail -ne 0 || $ash_fail -ne 0 ]]; then
  exit 1
fi
