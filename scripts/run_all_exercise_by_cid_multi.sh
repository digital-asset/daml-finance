#!/usr/bin/env bash
set -euo pipefail

: "${DAR_GLOB:?Need DAR_GLOB}"
: "${OWNER_ID:?Need OWNER_ID}"

CP_ID="${CP_ID:-${OBS1_ID:-}}"
: "${CP_ID:?Need CP_ID (set CP_ID or OBS1_ID)}"

: "${CID_TWO_SIGNATORIES:?Need CID_TWO_SIGNATORIES}"
: "${CID_JOINT_CTRL:?Need CID_JOINT_CTRL}"
: "${CID_THREE_SIGNATORIES:?Need CID_THREE_SIGNATORIES}"

N="${1:-3}"
P2_LEDGER_PORT="${P2_LEDGER_PORT:-6965}"

# IMPORTANT: multi may use a different log file depending on your dev-protocol-multi.conf
CANTON_LOG="${CANTON_LOG:-/tmp/canton-debug/canton_multi.log}"
[[ -f "$CANTON_LOG" ]] || { echo "ERROR: canton log not found at $CANTON_LOG"; exit 1; }

file_size() {
  if stat -f%z "$1" >/dev/null 2>&1; then
    stat -f%z "$1"   # macOS
  else
    stat -c%s "$1"   # Linux
  fi
}

run() {
  local name="$1" template_variant="$2" ledger_port="$3"
  shift 3

  mkdir -p /tmp/canton-debug
  local start_bytes slice out rc
  start_bytes="$(file_size "$CANTON_LOG")"
  slice="/tmp/canton-debug/slice_${name}.log"
  out="/tmp/canton-debug/out_${name}.txt"

  echo
  echo "=== RUN: $name ==="
  echo "LEDGER_PORT=$ledger_port"
  echo "TEMPLATE_VARIANT=$template_variant"
  echo "CANTON_LOG=$CANTON_LOG"
  echo "OWNER_ID=$OWNER_ID"
  echo "CP_ID=$CP_ID"
  echo "CMD: $*"
  echo

  set +e
  LOG="$CANTON_LOG" LEDGER_PORT="$ledger_port" TEMPLATE_VARIANT="$template_variant" \
    "$@" >"$out" 2>&1
  rc=$?
  set -e

  # Slice canton log for this run
  tail -c +"$((start_bytes+1))" "$CANTON_LOG" > "$slice" || true

  echo "----- benchmark output (full) -----"
  cat "$out" || true
  echo "-----------------------------------"

  if [[ $rc -ne 0 ]]; then
    echo "ERROR: '$name' failed (rc=$rc). Output: $out  Slice: $slice" >&2
    return $rc
  fi

  LOG_FILE="$slice" ./scripts/analyze_sequencer_traffic.sh > "${name}_analysis.txt"
  echo "[ok] wrote ${name}_analysis.txt"
}

# ExerciseByCid signatures (from your module):
#   runTwoSignatories       : (Text, ContractId BenchKeyed_TwoSignatories) -> Script ()
#   runJointCtrl            : (Text, Text, ContractId BenchKeyed_JointCtrl) -> Script ()
#   runThreeSignatories     : (Text, Text, ContractId BenchKeyed_ThreeSignatories) -> Script ()

run exercise_by_cid_two_signatories BenchKeyed_TwoSignatories "$P2_LEDGER_PORT" \
  ./scripts/run_benchmark_n.sh "$N" \
    Daml.Finance.Benchmark.Test.ExerciseByCid_N:runTwoSignatories \
    "$DAR_GLOB" "$OWNER_ID" "$CID_TWO_SIGNATORIES"

run exercise_by_cid_joint_ctrl BenchKeyed_JointCtrl "$P2_LEDGER_PORT" \
  ./scripts/run_benchmark_n.sh "$N" \
    Daml.Finance.Benchmark.Test.ExerciseByCid_N:runJointCtrl \
    "$DAR_GLOB" "$OWNER_ID" "$CP_ID" "$CID_JOINT_CTRL"

run exercise_by_cid_three_signatories BenchKeyed_ThreeSignatories "$P2_LEDGER_PORT" \
  ./scripts/run_benchmark_n.sh "$N" \
    Daml.Finance.Benchmark.Test.ExerciseByCid_N:runThreeSignatories \
    "$DAR_GLOB" "$OWNER_ID" "$CID_THREE_SIGNATORIES"
