#!/usr/bin/env bash
set -euo pipefail

# -----------------------------------------------------------------------------
# Run multi-party EXERCISE-by-key benchmarks (TwoSignatories + JointCtrl)
# and slice Canton log per run for deterministic analysis.
#
# Usage:
#   DAR_GLOB=... OWNER_ID=... OBS1_ID=... ./scripts/run_all_exercise_by_key_multi.sh 3
#
# Optional env:
#   CP_ID=...              (defaults to OBS1_ID)
#   P2_LEDGER_PORT=6965
#   CANTON_LOG=/path/to/canton.log   (MUST match your multi conf --log-file-name)
# -----------------------------------------------------------------------------

: "${DAR_GLOB:?Need DAR_GLOB}"
: "${OWNER_ID:?Need OWNER_ID}"

CP_ID="${CP_ID:-${OBS1_ID:-}}"
: "${CP_ID:?Need CP_ID (set CP_ID or OBS1_ID)}"

N="${1:-3}"

# In your multi setup you typically run these from participant2
P2_LEDGER_PORT="${P2_LEDGER_PORT:-6965}"

# MUST match your canton --log-file-name (single vs multi can differ!)
CANTON_LOG="${CANTON_LOG:-/tmp/canton-debug/canton_multi.log}"
[[ -f "$CANTON_LOG" ]] || { echo "ERROR: canton log not found at $CANTON_LOG"; exit 1; }

file_size() {
  local f="$1"
  if stat -f%z "$f" >/dev/null 2>&1; then
    stat -f%z "$f"   # macOS
  else
    stat -c%s "$f"   # Linux
  fi
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
  echo "CMD: $*"
  echo

  set +e
  LOG="$CANTON_LOG" LEDGER_PORT="$ledger_port" TEMPLATE_VARIANT="$template_variant" \
    "$@" >"$run_out" 2>&1
  rc=$?
  set -e

  # Slice canton log for this run (deterministic analysis input)
  tail -c +"$((start_bytes+1))" "$CANTON_LOG" > "$slice" || true

  echo "----- benchmark output (full) -----"
  cat "$run_out" || true
  echo "-----------------------------------"

  if [[ $rc -ne 0 ]]; then
    echo "ERROR: benchmark '$name' failed with exit code $rc" >&2
    echo "Full output:  $run_out" >&2
    echo "Canton slice: $slice" >&2
    return $rc
  fi

  LOG_FILE="$slice" ./scripts/analyze_sequencer_traffic.sh > "${name}_analysis.txt"
  echo "[ok] wrote ${name}_analysis.txt"
}

run two_signatories BenchKeyed_TwoSignatories "$P2_LEDGER_PORT" \
  ./scripts/run_benchmark_n.sh "$N" \
    Daml.Finance.Benchmark.Test.ExerciseByKey_N:runTwoSignatories \
    "$DAR_GLOB" "$OWNER_ID"

run joint_ctrl BenchKeyed_JointCtrl "$P2_LEDGER_PORT" \
  ./scripts/run_benchmark_n.sh "$N" \
    Daml.Finance.Benchmark.Test.ExerciseByKey_N:runJointCtrl \
    "$DAR_GLOB" "$OWNER_ID" "$CP_ID"

run three_signatories BenchKeyed_ThreeSignatories "$P2_LEDGER_PORT" \
  ./scripts/run_benchmark_n.sh "$N" \
    Daml.Finance.Benchmark.Test.ExerciseByKey_N:runThreeSignatories \
    "$DAR_GLOB" "$OWNER_ID"
