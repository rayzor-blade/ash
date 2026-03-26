#!/usr/bin/env bash
# Compare ash interpreter output against HashLink for a given test case.
#
# Usage (inside Docker):
#   ./scripts/docker_compare.sh TestStdlib
#   ./scripts/docker_compare.sh --all          # run all non-slow cases
#   ./scripts/docker_compare.sh --all-slow     # run all cases including slow
#
# Usage (from host via Docker Compose):
#   docker compose -f docker/docker-compose.yml run --rm compare TestStdlib
#   docker compose -f docker/docker-compose.yml run --rm compare --all
set -euo pipefail

TESTS_DIR="/work/crates/ash/test/tests"
CASES_FILE="$TESTS_DIR/parity_cases.toml"
PASS=0
FAIL=0
SKIP=0

run_case() {
    local name="$1"
    local hl_file="$2"
    local main="$3"
    local slow="$4"
    local expectation="$5"
    local hl_path="$TESTS_DIR/$hl_file"

    # Compile if main is set
    if [[ -n "$main" ]]; then
        if ! haxe --cwd "$TESTS_DIR" -main "$main" -hl "$hl_file" 2>/dev/null; then
            echo "  COMPILE FAIL: $name"
            FAIL=$((FAIL + 1))
            return
        fi
    fi

    if [[ ! -f "$hl_path" ]]; then
        echo "  MISSING: $hl_path"
        SKIP=$((SKIP + 1))
        return
    fi

    # Run through HashLink
    local hl_out hl_exit
    hl_out=$(hl "$hl_path" 2>&1) || true
    hl_exit=$?

    if [[ "$expectation" == "exit_only" ]]; then
        if [[ $hl_exit -eq 0 ]]; then
            printf "  %-40s HL exit=0  PASS (exit_only)\n" "$name"
        else
            printf "  %-40s HL exit=%d\n" "$name" "$hl_exit"
        fi
        PASS=$((PASS + 1))
        return
    fi

    # Print result
    local status_icon
    if [[ $hl_exit -eq 0 ]]; then
        status_icon="OK"
        PASS=$((PASS + 1))
    else
        status_icon="FAIL"
        FAIL=$((FAIL + 1))
    fi

    local preview
    preview=$(echo "$hl_out" | head -1 | cut -c1-60)
    printf "  %-40s HL exit=%-3d %s  %s\n" "$name" "$hl_exit" "$status_icon" "$preview"

    if [[ $hl_exit -ne 0 ]]; then
        echo "    stderr: $(echo "$hl_out" | tail -3)"
    fi
}

parse_cases() {
    local include_slow="$1"
    # Simple TOML parser for parity_cases.toml
    local name="" hl="" main="" slow="false" expectation="exact"

    while IFS= read -r line; do
        line="${line%%#*}"  # strip comments
        line="${line## }"   # trim leading spaces
        [[ -z "$line" ]] && continue

        if [[ "$line" == "[[case]]" ]]; then
            # Emit previous case if any
            if [[ -n "$name" ]]; then
                if [[ "$slow" == "true" && "$include_slow" != "true" ]]; then
                    SKIP=$((SKIP + 1))
                else
                    run_case "$name" "$hl" "$main" "$slow" "$expectation"
                fi
            fi
            name="" hl="" main="" slow="false" expectation="exact"
            continue
        fi

        case "$line" in
            name\ =*)    name=$(echo "$line" | sed 's/name = "\(.*\)"/\1/') ;;
            hl\ =*)      hl=$(echo "$line" | sed 's/hl = "\(.*\)"/\1/') ;;
            main\ =*)    main=$(echo "$line" | sed 's/main = "\(.*\)"/\1/') ;;
            slow\ =*)    slow=$(echo "$line" | sed 's/slow = //') ;;
            expectation\ =*) expectation=$(echo "$line" | sed 's/expectation = "\(.*\)"/\1/') ;;
        esac
    done < "$CASES_FILE"

    # Emit last case
    if [[ -n "$name" ]]; then
        if [[ "$slow" == "true" && "$include_slow" != "true" ]]; then
            SKIP=$((SKIP + 1))
        else
            run_case "$name" "$hl" "$main" "$slow" "$expectation"
        fi
    fi
}

# --- Main ---
case "${1:-}" in
    --all)
        echo "Running all parity cases through HashLink (excluding slow)..."
        echo ""
        parse_cases "false"
        echo ""
        echo "Results: $PASS passed, $FAIL failed, $SKIP skipped"
        [[ $FAIL -eq 0 ]] && exit 0 || exit 1
        ;;
    --all-slow)
        echo "Running ALL parity cases through HashLink (including slow)..."
        echo ""
        parse_cases "true"
        echo ""
        echo "Results: $PASS passed, $FAIL failed, $SKIP skipped"
        [[ $FAIL -eq 0 ]] && exit 0 || exit 1
        ;;
    "")
        echo "Usage: $0 <TestName> | --all | --all-slow"
        exit 1
        ;;
    *)
        # Single test case by name — find it in the TOML
        TEST_NAME="$1"
        # Find the hl file and main for this case
        HL_FILE=$(grep -A5 "name = \"$TEST_NAME\"" "$CASES_FILE" | grep "^hl = " | sed 's/hl = "\(.*\)"/\1/' | head -1)
        MAIN=$(grep -A5 "name = \"$TEST_NAME\"" "$CASES_FILE" | grep "^main = " | sed 's/main = "\(.*\)"/\1/' | head -1)

        if [[ -z "$HL_FILE" ]]; then
            echo "Case '$TEST_NAME' not found in $CASES_FILE"
            exit 1
        fi

        echo "Running $TEST_NAME through HashLink..."
        run_case "$TEST_NAME" "$HL_FILE" "$MAIN" "false" "exact"
        ;;
esac
