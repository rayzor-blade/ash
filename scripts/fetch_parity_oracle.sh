#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT_DIR="${OUT_DIR:-$ROOT_DIR/target/parity-oracle}"
WORKFLOW_FILE="${WORKFLOW_FILE:-parity_oracle.yml}"
ARTIFACT_NAME="${ARTIFACT_NAME:-parity-oracle-main-latest}"

if ! command -v gh >/dev/null 2>&1; then
  echo "gh CLI is required. Install from https://cli.github.com/" >&2
  exit 1
fi

REPO="${REPO:-}"
if [[ -z "$REPO" ]]; then
  REPO="$(gh repo view --json nameWithOwner -q .nameWithOwner)"
fi

RUN_ID="${RUN_ID:-}"
if [[ -z "$RUN_ID" ]]; then
  RUN_ID="$(gh run list \
    --repo "$REPO" \
    --workflow "$WORKFLOW_FILE" \
    --branch main \
    --status success \
    --limit 1 \
    --json databaseId \
    -q '.[0].databaseId')"
fi

if [[ -z "$RUN_ID" || "$RUN_ID" == "null" ]]; then
  echo "No successful run found for workflow=$WORKFLOW_FILE repo=$REPO" >&2
  exit 1
fi

rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"

echo "Downloading artifact '$ARTIFACT_NAME' from run $RUN_ID (repo $REPO) ..."
gh run download "$RUN_ID" --repo "$REPO" --name "$ARTIFACT_NAME" --dir "$OUT_DIR"

echo "Oracle bundle downloaded to: $OUT_DIR"
echo "Set ASH_PARITY_ORACLE_DIR=$OUT_DIR to consume it in parity_matrix tests."
