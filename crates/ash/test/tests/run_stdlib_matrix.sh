#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../../../../" && pwd)"
PARITY_CASES="$SCRIPT_DIR/parity_cases.toml"
INCLUDE_SLOW="${ASH_STDLIB_INCLUDE_SLOW:-0}"

if [[ ! -f "$PARITY_CASES" ]]; then
  echo "Missing parity case file: $PARITY_CASES" >&2
  exit 1
fi

readarray -t CASE_LINES < <(python3 - <<'PY' "$PARITY_CASES" "$INCLUDE_SLOW"
import sys
import tomllib

cases_file = sys.argv[1]
include_slow = sys.argv[2] in ("1", "true", "True", "TRUE")
with open(cases_file, "rb") as f:
    data = tomllib.load(f)

for c in data.get("case", []):
    if c.get("slow", False) and not include_slow:
        continue
    name = c["name"]
    main = c.get("main", "")
    hl = c["hl"]
    compile_enabled = "1" if c.get("compile", True) else "0"
    sanity_enabled = "1" if c.get("sanity_interp", True) else "0"
    print(f"{name}|{main}|{hl}|{compile_enabled}|{sanity_enabled}")
PY
)

echo "==> Compiling Haxe tests to .hl"
compile_fail=0
for line in "${CASE_LINES[@]}"; do
  IFS='|' read -r name main hl compile_enabled _ <<< "$line"
  if [[ "$compile_enabled" != "1" ]]; then
    echo "  - $name -> $hl (precompiled)"
    continue
  fi
  echo "  - $main -> $hl"
  if ! haxe --cwd "$SCRIPT_DIR" -main "$main" -hl "$hl"; then
    echo "    [COMPILE FAIL] $name"
    compile_fail=1
  fi
done

echo
echo "==> Baseline check with Haxe interpreter"
baseline_fail=0
for line in "${CASE_LINES[@]}"; do
  IFS='|' read -r name main _ _ sanity_enabled <<< "$line"
  if [[ "$sanity_enabled" != "1" ]]; then
    echo "  [BASELINE SKIP] $name (sanity_interp=false)"
    continue
  fi
  if haxe --cwd "$SCRIPT_DIR" -main "$main" --interp; then
    echo "  [BASELINE PASS] $name"
  else
    echo "  [BASELINE FAIL] $name"
    baseline_fail=1
  fi
done

echo
echo "==> Running ash_cli interpreter"
cd "$ROOT_DIR"
cargo build -q -p ash_cli
BIN="$(find "$ROOT_DIR/target" -type f -name ash_cli | head -n1)"
if [[ -z "$BIN" ]]; then
  echo "Unable to locate ash_cli binary under $ROOT_DIR/target" >&2
  exit 1
fi

ash_fail=0
for line in "${CASE_LINES[@]}"; do
  IFS='|' read -r name _ hl _ _ <<< "$line"
  hl_path="$SCRIPT_DIR/$hl"
  echo "  - $name ($hl)"
  if "$BIN" --mode interp --quiet "$hl_path"; then
    echo "    [ASH PASS]"
  else
    echo "    [ASH FAIL]"
    ash_fail=1
  fi
done

echo
echo "==> Summary"
echo "  compile_fail=$compile_fail"
echo "  baseline_fail=$baseline_fail"
echo "  ash_fail=$ash_fail"

if [[ $compile_fail -ne 0 || $baseline_fail -ne 0 || $ash_fail -ne 0 ]]; then
  exit 1
fi
