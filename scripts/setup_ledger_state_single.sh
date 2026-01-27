#!/usr/bin/env bash
set -euo pipefail

# Required environment
: "${DAR_GLOB:?Need DAR_GLOB (can be a single .dar path)}"
: "${OWNER_ID:?Need OWNER_ID}"
: "${OBS1_ID:?Need OBS1_ID (counterparty)}"
: "${LEDGER_PORT:=6865}"

# Optional second observer
OBS2_ID="${OBS2_ID:-}"

# Convention: CP_ID = OBS1_ID
CP_ID="$OBS1_ID"

# observersText = "obs1;obs2" (obs2 optional)
OBSERVERS="${OBS1_ID}${OBS2_ID:+;${OBS2_ID}}"

# Helper: extract CID from DAML Script output
extract_cid() {
  sed -n 's/.*CID=\(.*\)$/\1/p' \
  | head -n 1 \
  | tr -d '\r' \
  | sed 's/\\"//g; s/"//g'
}

# Helper: run setup script once and capture CID
run_setup_and_get_cid() {
  local label="$1"
  shift
  local script_name="$1"
  shift

  # optional numeric port as next arg
  local port="$LEDGER_PORT"
  if [[ "${1:-}" =~ ^[0-9]+$ ]]; then
    port="$1"
    shift
  fi

  local cid
  cid="$(
    LOG=/tmp/canton-debug/canton.log \
    LEDGER_PORT="$port" \
    TEMPLATE_VARIANT="$label" \
    ./scripts/run_one_benchmark.sh \
      "$script_name" \
      "$DAR_GLOB" \
      "$@" \
    | tee /dev/tty \
    | extract_cid
  )"

  if [[ -z "$cid" ]]; then
    echo "ERROR: Failed to extract CID for $label" >&2
    exit 1
  fi

  echo "$cid"
}

echo
echo "=== [PHASE 1] Creating SINGLE-SUBMIT benchmark contracts and exporting CIDs ==="
echo "OWNER_ID=$OWNER_ID"
echo "CP_ID=$CP_ID"
echo "OBSERVERS=$OBSERVERS"
echo

# Variant 1: OwnerCtrl create (submit as OWNER on 6865)
CID_OWNER_CTRL="$(
  run_setup_and_get_cid "BenchKeyed_OwnerCtrl" \
    Daml.Finance.Benchmark.Test.SetupLedgerState_N:runOwnerCtrl \
    6865 \
    "$OWNER_ID" \
    "$OBSERVERS"
)"

# Variant 3: CpControls create (still submit as OWNER on 6865)
CID_CP_CONTROLS="$(
  run_setup_and_get_cid "BenchKeyed_CpControls" \
    Daml.Finance.Benchmark.Test.SetupLedgerState_N:runCpControls \
    6865 \
    "$OWNER_ID" \
    "$CP_ID" \
    "$OBSERVERS"
)"

export CID_OWNER_CTRL CID_CP_CONTROLS 

echo
echo "=== Exported CIDs (PHASE 1) ==="
echo "CID_OWNER_CTRL=$CID_OWNER_CTRL"
echo "CID_CP_CONTROLS=$CID_CP_CONTROLS"

get_var() {
  if [[ -n "${BASH_VERSION:-}" ]]; then
    printf '%s' "${!1-}"
  else
    # zsh (and others): use eval indirection
    eval 'printf "%s" "${'"$1"':-}"'
  fi
}

for v in CID_OWNER_CTRL CID_CP_CONTROLS; do
  val="$(get_var "$v")"
  [[ -n "$val" ]] || { echo "ERROR: $v is empty"; return 1 2>/dev/null || exit 1; }
done