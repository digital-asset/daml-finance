#!/usr/bin/env bash
set -euo pipefail

: "${DAR_GLOB:?Set DAR_GLOB}"
: "${OWNER_ID:?Set OWNER_ID}"
: "${CP_ID:?Set CP_ID}"
: "${CP2_ID:?Set CP2_ID}"

OBSERVERS_STR="${OBSERVERS_STR:-}"
PACKED="${OWNER_ID}||${OBSERVERS_STR}"
N="${1:-3}"

# These must match dev-protocol-multi.conf
P1_LEDGER_PORT="${P1_LEDGER_PORT:-6865}"
P2_LEDGER_PORT="${P2_LEDGER_PORT:-6965}"

# IMPORTANT: use the MULTI log file (not the single one)
CANTON_LOG_MULTI="${CANTON_LOG_MULTI:-/tmp/canton-debug/canton_multi.log}"
[[ -f "$CANTON_LOG_MULTI" ]] || { echo "ERROR: multi canton log not found at $CANTON_LOG_MULTI"; exit 1; }

file_size() { if stat -f%z "$1" >/dev/null 2>&1; then stat -f%z "$1"; else stat -c%s "$1"; fi; }

run() {
  local name="$1" template_variant="$2" ledger_port="$3"
  shift 3

  mkdir -p /tmp/canton-debug
  local start_bytes slice out rc
  start_bytes="$(file_size "$CANTON_LOG_MULTI")"
  slice="/tmp/canton-debug/slice_${name}.log"
  out="/tmp/canton-debug/out_${name}.txt"

  echo
  echo "=== RUN: $name ==="
  echo "LEDGER_PORT=$ledger_port"
  echo "TEMPLATE_VARIANT=$template_variant"
  echo "CANTON_LOG_MULTI=$CANTON_LOG_MULTI"
  echo "CMD: $*"
  echo

  set +e
  LOG="$CANTON_LOG_MULTI" LEDGER_PORT="$ledger_port" TEMPLATE_VARIANT="$template_variant" \
    "$@" >"$out" 2>&1
  rc=$?
  set -e

  tail -c +"$((start_bytes+1))" "$CANTON_LOG_MULTI" > "$slice" || true

  # Print full benchmark output (no tail truncation)
  cat "$out" || true

  if [[ $rc -ne 0 ]]; then
    echo "ERROR: '$name' failed (rc=$rc). Output: $out  Slice: $slice" >&2
    return $rc
  fi

  LOG_FILE="$slice" ./scripts/analyze_sequencer_traffic.sh > "${name}_analysis.txt"
  echo "[ok] wrote ${name}_analysis.txt"
}

SUFFIX_BASE="$(date +%Y%m%d-%H%M%S)"

# TwoSignatories: submit [owner, cp] -> MUST run on the participant where multi-submit works in the MULTI setup.
run one_create_two_signatories BenchKeyed_TwoSignatories "$P2_LEDGER_PORT" \
  ./scripts/run_benchmark_n.sh "$N" \
    Daml.Finance.Benchmark.Test.OneCreate_N:runTwoSignatories \
    "$DAR_GLOB" \
    "$PACKED" \
    "$CP_ID" \
    "${SUFFIX_BASE}-two"

# JointCtrl: create is submit owner (only owner is signatory)
# In multi mode, you may still run it on P1 (owner host) to keep things deterministic.
run one_create_joint_ctrl BenchKeyed_JointCtrl "$P1_LEDGER_PORT" \
  ./scripts/run_benchmark_n.sh "$N" \
    Daml.Finance.Benchmark.Test.OneCreate_N:runJointCtrl \
    "$DAR_GLOB" \
    "$PACKED" \
    "$CP_ID" \
    "${SUFFIX_BASE}-joint"

# ThreeSignatories: submit [owner, cp] -> MUST run on the participant where multi-submit works in the MULTI setup.
run one_create_three_signatories BenchKeyed_ThreeSignatories "$P2_LEDGER_PORT" \
  ./scripts/run_benchmark_n.sh "$N" \
    Daml.Finance.Benchmark.Test.OneCreate_N:runThreeSignatories \
    "$DAR_GLOB" \
    "$PACKED" \
    "$CP_ID" \
    "$CP2_ID" \
    "${SUFFIX_BASE}-three"
