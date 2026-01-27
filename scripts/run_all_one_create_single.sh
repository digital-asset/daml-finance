#!/usr/bin/env bash
set -euo pipefail

: "${DAR_GLOB:?Set DAR_GLOB}"
: "${OWNER_ID:?Set OWNER_ID}"

# CP_ID needed for CpControls
: "${OBS1_ID:?Need OBS1_ID (counterparty)}"
CP_ID="${CP_ID:-$OBS1_ID}"
: "${CP_ID:?Need CP_ID (set CP_ID or OBS1_ID)}"

# Optional observers list: "obs1;obs2;obs3"
OBSERVERS_STR="${OBSERVERS_STR:-}"
PACKED="${OWNER_ID}||${OBSERVERS_STR}"

N="${1:-3}"

# Ports
P1_LEDGER_PORT="${P1_LEDGER_PORT:-6865}"
P2_LEDGER_PORT="${P2_LEDGER_PORT:-6965}"

# Canton flat log (the one Canton actually writes)
CANTON_LOG="${CANTON_LOG:-/tmp/canton-debug/canton.log}"
[[ -f "$CANTON_LOG" ]] || { echo "ERROR: canton log not found at $CANTON_LOG"; exit 1; }

file_size() { if stat -f%z "$1" >/dev/null 2>&1; then stat -f%z "$1"; else stat -c%s "$1"; fi; }

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
  echo "CMD: $*"
  echo

  set +e
  LOG="$CANTON_LOG" LEDGER_PORT="$ledger_port" TEMPLATE_VARIANT="$template_variant" \
    "$@" >"$out" 2>&1
  rc=$?
  set -e

  tail -c +"$((start_bytes+1))" "$CANTON_LOG" > "$slice" || true
  tail -n 200 "$out" || true

  if [[ $rc -ne 0 ]]; then
    echo "ERROR: '$name' failed (rc=$rc). Output: $out  Slice: $slice" >&2
    return $rc
  fi

  LOG_FILE="$slice" ./scripts/analyze_sequencer_traffic_1_2p.sh > "${name}_analysis.txt"
  echo "[ok] wrote ${name}_analysis.txt"
}

# Unique suffix per script invocation
SUFFIX_BASE="$(date +%Y%m%d-%H%M%S)"

run one_create_owner_ctrl BenchKeyed_OwnerCtrl "$P1_LEDGER_PORT" \
  ./scripts/run_benchmark_n.sh "$N" \
    Daml.Finance.Benchmark.Test.OneCreate_N:runOwnerCtrl \
    "$DAR_GLOB" \
    "$PACKED" \
    "${SUFFIX_BASE}-owner"

# CpControls CREATE must run where OWNER can submit -> participant1/6865
run one_create_cp_controls BenchKeyed_CpControls "$P1_LEDGER_PORT" \
  ./scripts/run_benchmark_n.sh "$N" \
    Daml.Finance.Benchmark.Test.OneCreate_N:runCpControls \
    "$DAR_GLOB" \
    "$PACKED" \
    "$CP_ID" \
    "${SUFFIX_BASE}-cp"
