#!/usr/bin/env bash
# -----------------------------------------------------------------------------
# run_one_benchmark.sh
#
# Purpose:
#   Run ONE DAML Script benchmark, then analyze ONLY the new Canton log bytes
#   appended during the run (byte-offset slicing) to estimate "elementary choice"
#   cost (Create/Exercise) as deterministically as possible.
#
# Usage:
#   LOG=/path/to/my-canton.log \
#   TEMPLATE_VARIANT=BenchKeyed_OwnerCtrl \
#   ./scripts/run_one_benchmark.sh <Module:run> <dar_glob_or_path> [script_args...]
#
# Notes:
#   - Assumes Canton is writing to LOG (default: my-canton.log).
#   - Creates a unique per-run slice log under /tmp/canton-debug/.
#   - Calls ./scripts/analyze_sequencer_traffic.sh on the slice log.
#   - Supports 0..3 script args and encodes them as JSON for --input-file.
# -----------------------------------------------------------------------------

set -euo pipefail

LOG="${LOG:-my-canton.log}"

SCRIPT_NAME="${1:?Usage: run_one_benchmark.sh <Module:run> <dar_glob> [script_args...]}"
DAR_GLOB="${2:?Usage: run_one_benchmark.sh <Module:run> <dar_glob> [script_args...]}"
shift 2

DAR="$(ls -1 ${DAR_GLOB} 2>/dev/null | head -n 1 || true)"
if [[ -z "${DAR}" ]]; then
  echo "ERROR: No DAR matched glob: ${DAR_GLOB}"
  exit 1
fi

echo "Using DAR: ${DAR}"
echo "Running script: ${SCRIPT_NAME}"

mkdir -p /tmp/canton-debug

file_size() {
  local f="$1"
  if [[ ! -f "$f" ]]; then
    echo "0"
    return
  fi
  if stat -f%z "$f" >/dev/null 2>&1; then
    stat -f%z "$f"        # macOS
  else
    stat -c%s "$f"        # Linux
  fi
}

START_BYTES="$(file_size "$LOG")"

INPUT_FLAG=()
TMP_INPUT=""
cleanup() { [[ -n "${TMP_INPUT:-}" ]] && rm -f "$TMP_INPUT" || true; }
trap cleanup EXIT

if [[ "$#" -gt 0 ]]; then
  TMP_INPUT="$(mktemp -t daml-script-input.XXXXXX)"

  if [[ "$#" -eq 1 ]]; then
    python3 -c 'import json,sys; print(json.dumps(sys.argv[1]))' "$1" > "$TMP_INPUT"
  else
    python3 -c 'import json,sys; print(json.dumps(sys.argv[1:]))' "$@" > "$TMP_INPUT"
  fi

  INPUT_FLAG=(--input-file "$TMP_INPUT")
fi

# IMPORTANT: --input-file must come BEFORE --script-name in some versions
#change back ledger port to 6965 for 2 and 3 participants
# Add:   --ledger-user "bench-bob-bobBank" \

dpm script \
  --ledger-host 127.0.0.1 \
  --ledger-port "${LEDGER_PORT:-6865}" \
  --dar "$DAR" \
  "${INPUT_FLAG[@]}" \
  --script-name "$SCRIPT_NAME"

END_BYTES="$(file_size "$LOG")"
if [[ "$END_BYTES" -lt "$START_BYTES" ]]; then
  echo "ERROR: Log file shrank during run (rotation/truncation?). LOG=$LOG" >&2
  exit 1
fi
if [[ "$END_BYTES" -eq "$START_BYTES" ]]; then
  echo "ERROR: Log file did not grow during run. Is LOG correct? LOG=$LOG" >&2
  exit 1
fi

# Unique per-run slice log (avoid collisions across runs)
TS="$(date +%Y%m%d-%H%M%S)"
SAFE_SCRIPT="${SCRIPT_NAME//[:]/_}"
RUN_LOG="${RUN_LOG:-/tmp/canton-debug/run-${SAFE_SCRIPT}-${TS}.log}"

tail -c +"$((START_BYTES+1))" "$LOG" > "$RUN_LOG"

# Run analysis on the slice log. You can tighten expectations via env if desired.
LOG_FILE="$RUN_LOG" \
TEMPLATE_VARIANT="${TEMPLATE_VARIANT:-unknown}" \
./scripts/analyze_sequencer_traffic-3p.sh # For 3 participants, use: analyze_sequencer_traffic_3p.sh
