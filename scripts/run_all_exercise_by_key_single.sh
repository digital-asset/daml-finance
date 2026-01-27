#!/usr/bin/env bash
set -euo pipefail

: "${DAR_GLOB:?Need DAR_GLOB}"
: "${OWNER_ID:?Need OWNER_ID}"

: "${OBS1_ID:?Need OBS1_ID (counterparty)}"
CP_ID="${CP_ID:-$OBS1_ID}"

N="${1:-3}"

P1_LEDGER_PORT="${P1_LEDGER_PORT:-6865}"
P2_LEDGER_PORT="${P2_LEDGER_PORT:-6965}"

CANTON_LOG="${CANTON_LOG:-/tmp/canton-debug/canton.log}"
[[ -f "$CANTON_LOG" ]] || { echo "ERROR: canton log not found at $CANTON_LOG"; exit 1; }

file_size() {
  local f="$1"
  if stat -f%z "$f" >/dev/null 2>&1; then stat -f%z "$f"; else stat -c%s "$f"; fi
}

run() {
  local name="$1"
  local template_variant="$2"
  local ledger_port="$3"
  shift 3

  mkdir -p /tmp/canton-debug

  local start_bytes slice run_out rc
  start_bytes="$(file_size "$CANTON_LOG")"
  slice="/tmp/canton-debug/slice_${name}.log"
  run_out="/tmp/canton-debug/out_${name}.txt"

  echo
  echo "=== RUN: $name ==="
  echo "LEDGER_PORT=$ledger_port"
  echo "TEMPLATE_VARIANT=$template_variant"
  echo "CANTON_LOG=$CANTON_LOG"
  echo "OWNER_ID=$OWNER_ID"
  echo "CP_ID=$CP_ID"
  echo "CMD: $*"
  echo

  # Run benchmark and show output live in terminal, while also saving it to a file.
  set +e
  LOG="$CANTON_LOG" LEDGER_PORT="$ledger_port" TEMPLATE_VARIANT="$template_variant" \
    "$@" 2>&1 | tee "$run_out"
  rc=${PIPESTATUS[0]}
  set -e

  # Slice only the new log bytes produced by this run
  tail -c +"$((start_bytes+1))" "$CANTON_LOG" > "$slice" || true

  if [[ $rc -ne 0 ]]; then
    echo "ERROR: benchmark '$name' failed with exit code $rc" >&2
    echo "Wrote full output to: $run_out" >&2
    echo "Wrote canton slice to: $slice" >&2
    return $rc
  fi

  LOG_FILE="$slice" ./scripts/analyze_sequencer_traffic_1_2p.sh > "${name}_analysis.txt"
  echo "[ok] wrote ${name}_analysis.txt"
}

run owner_ctrl BenchKeyed_OwnerCtrl "$P1_LEDGER_PORT" \
  ./scripts/run_benchmark_n.sh "$N" \
    Daml.Finance.Benchmark.Test.ExerciseByKey_N:runOwnerCtrl \
    "$DAR_GLOB" "$OWNER_ID"

run cp_controls BenchKeyed_CpControls "$P2_LEDGER_PORT" \
  ./scripts/run_benchmark_n.sh "$N" \
    Daml.Finance.Benchmark.Test.ExerciseByKey_N:runCpControls \
    "$DAR_GLOB" "$OWNER_ID" "$CP_ID"

