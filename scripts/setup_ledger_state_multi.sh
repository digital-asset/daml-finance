#!/usr/bin/env bash
set -euo pipefail

# -----------------------------------------------------------------------------
# scripts/setup_ledger_state_multi.sh  (PHASE 2)
#
# Creates MULTI-SUBMIT benchmark contracts and exports their CIDs:
#   - BenchKeyed_TwoSignatories  (submit [owner, cp])
#   - BenchKeyed_ThreeSignatories (submit [owner, cp, cp2])
#
# Requirements:
#   - Canton multi setup already running with multi-hosting enabled (bootstrap-N-multi.sc)
#   - LEDGER_PORT should point to the participant where multi-submit works (default 6965)
#
# Required env:
#   DAR_GLOB   : path to .dar (or glob that run_one_benchmark.sh can resolve)
#   OWNER_ID   : party id text
#   OBS1_ID    : party id text (used as CP_ID)
#
# Optional env:
#   OBS2_ID           : party id text (used as CP2_ID for 3-signatories). REQUIRED for 3-signatories.
#   LEDGER_PORT       : default 6965
#   CANTON_LOG_MULTI  : path to multi canton log file (must match --log-file-name)
#   OBSERVERS_STR     : additional observers beyond cp/cp2 (semicolon-separated). Default "".
# -----------------------------------------------------------------------------

# Required environment
: "${DAR_GLOB:?Need DAR_GLOB (can be a single .dar path)}"
: "${OWNER_ID:?Need OWNER_ID}"
: "${OBS1_ID:?Need OBS1_ID (counterparty / CP_ID)}"

LEDGER_PORT="${LEDGER_PORT:-6965}"

# IMPORTANT: use the MULTI log file (not the single one)
CANTON_LOG_MULTI="${CANTON_LOG_MULTI:-/tmp/canton-debug/canton_multi.log}"
mkdir -p /tmp/canton-debug
[[ -f "$CANTON_LOG_MULTI" ]] || {
  echo "ERROR: multi canton log not found at $CANTON_LOG_MULTI" >&2
  echo "Hint: start Canton multi with --log-file-name $CANTON_LOG_MULTI" >&2
  exit 1
}

# Parties
CP_ID="$OBS1_ID"
OBS2_ID="${OBS2_ID:-}"
CP2_ID="$OBS2_ID"

# ObserversText passed to setup scripts:
# - Do NOT automatically include CP_ID/CP2_ID here; templates already add them where needed.
# - Use OBSERVERS_STR only for "extra observers" (semicolon-separated), default empty.
OBSERVERS_STR="${OBSERVERS_STR:-}"
OBSERVERS_TEXT="$OBSERVERS_STR"

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

file_size() {
  local f="$1"
  if stat -f%z "$f" >/dev/null 2>&1; then
    stat -f%z "$f"   # macOS
  else
    stat -c%s "$f"   # Linux
  fi
}

extract_cid() {
  # Extract first CID=... from logs (handles debug "CID=<...>")
  sed -nE 's/.*\bCID=([^[:space:]]+).*/\1/p' \
    | head -n 1 \
    | tr -d '\r' \
    | sed 's/\\"//g; s/"//g'
}

run_setup_and_get_cid() {
  local label="$1"; shift
  local script_name="$1"; shift

  local port="$LEDGER_PORT"
  if [[ "${1:-}" =~ ^[0-9]+$ ]]; then
    port="$1"
    shift
  fi

  local start_bytes slice out rc cid
  start_bytes="$(file_size "$CANTON_LOG_MULTI")"
  slice="/tmp/canton-debug/slice_setup_${label}.log"
  out="/tmp/canton-debug/out_setup_${label}.txt"

  echo >&2
  echo "=== SETUP: $label ===" >&2
  echo "LEDGER_PORT=$port" >&2
  echo "TEMPLATE_VARIANT=$label" >&2
  echo "CANTON_LOG_MULTI=$CANTON_LOG_MULTI" >&2
  echo "SCRIPT=$script_name" >&2
  echo "ARGS: $*" >&2
  echo >&2

  set +e
  LOG="$CANTON_LOG_MULTI" LEDGER_PORT="$port" TEMPLATE_VARIANT="$label" \
    ./scripts/run_one_benchmark.sh "$script_name" "$DAR_GLOB" "$@" >"$out" 2>&1
  rc=$?
  set -e

  # Slice log for this setup action (optional but useful for debugging)
  tail -c +"$((start_bytes+1))" "$CANTON_LOG_MULTI" > "$slice" 2>/dev/null || true

  # Print full output so you see errors (no tail truncation)
  cat "$out" >&2 || true

  if [[ $rc -ne 0 ]]; then
    echo "ERROR: setup '$label' failed (rc=$rc)" >&2
    echo "Output: $out" >&2
    echo "Log slice: $slice" >&2
    return $rc
  fi

  cid="$(extract_cid <"$out")"
  if [[ -z "$cid" ]]; then
    echo "ERROR: Failed to extract CID for $label (see $out)" >&2
    exit 1
  fi

  echo "$cid"
}

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

echo
echo "=== [PHASE 2] Creating MULTI-SUBMIT benchmark contracts and exporting CIDs ==="
echo "OWNER_ID=$OWNER_ID"
echo "CP_ID=$CP_ID"
echo "CP2_ID=${CP2_ID:-<unset>}"
echo "OBSERVERS_TEXT(extra)=${OBSERVERS_TEXT:-<empty>}"
echo

# Variant 2: BenchKeyed_TwoSignatories
# SetupLedgerState_N:runTwoSignatories expects (OWNER_ID, CP_ID, observersText)
CID_TWO_SIGNATORIES="$(
  run_setup_and_get_cid "BenchKeyed_TwoSignatories" \
    Daml.Finance.Benchmark.Test.SetupLedgerState_N:runTwoSignatories \
    "$LEDGER_PORT" \
    "$OWNER_ID" \
    "$CP_ID" \
    "$OBSERVERS_TEXT"
)"

# Variant 4: BenchKeyed_JointCtrl create 
CID_JOINT_CTRL="$(
  run_setup_and_get_cid "BenchKeyed_JointCtrl" \
    Daml.Finance.Benchmark.Test.SetupLedgerState_N:runJointCtrl \
    "$LEDGER_PORT" \
    "$OWNER_ID" \
    "$CP_ID" \
    "$OBSERVERS_TEXT"
)"

# Variant 5: BenchKeyed_ThreeSignatories
# SetupLedgerState_N:runThreeSignatories expects (OWNER_ID, CP_ID, CP2_ID, observersText)
: "${CP2_ID:?Need OBS2_ID (used as CP2_ID) to create BenchKeyed_ThreeSignatories}"

CID_THREE_SIGNATORIES="$(
  run_setup_and_get_cid "BenchKeyed_ThreeSignatories" \
    Daml.Finance.Benchmark.Test.SetupLedgerState_N:runThreeSignatories \
    "$LEDGER_PORT" \
    "$OWNER_ID" \
    "$CP_ID" \
    "$CP2_ID" \
    "$OBSERVERS_TEXT"
)"

export CID_TWO_SIGNATORIES CID_JOINT_CTRL CID_THREE_SIGNATORIES

CIDS_ENV="/tmp/canton-debug/cids.env"
{
  printf "export CID_TWO_SIGNATORIES='%s'\n" "$CID_TWO_SIGNATORIES"
  printf "export CID_JOINT_CTRL='%s'\n" "$CID_JOINT_CTRL"
  printf "export CID_THREE_SIGNATORIES='%s'\n" "$CID_THREE_SIGNATORIES"
} > "$CIDS_ENV"

echo
echo "[ok] wrote: $CIDS_ENV"
echo "Now run: source $CIDS_ENV"

