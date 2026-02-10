#!/usr/bin/env bash
set -euo pipefail

N="${1:?Usage: run_benchmark_n.sh <N> <Module:run> <dar_glob> [--pass-i] [script_args...]}"
SCRIPT_NAME="${2:?Usage: run_benchmark_n.sh <N> <Module:run> <dar_glob> [--pass-i] [script_args...]}"
DAR_GLOB="${3:?Usage: run_benchmark_n.sh <N> <Module:run> <dar_glob> [--pass-i] [script_args...]}"
shift 3

PASS_I="no"
if [[ "${1:-}" == "--pass-i" ]]; then
  PASS_I="yes"
  shift 1
fi

SCRIPT_ARGS=("$@")
OUT_CSV="${OUT_CSV:-benchmark_runs.csv}"
TODAY="$(date +%F)"

mkdir -p /tmp/canton-debug

# CSV header once
if [[ ! -f "$OUT_CSV" ]]; then
  echo "run_id,run_date,script_name,template_variant,total_network_bytes,submission_count,bytes_per_submission,transactions_created,views_created_total,views_per_transaction,tx_accepted_count,bytes_per_transaction,bytes_per_tx_accepted,bytes_per_view,participants_involved,participant_count,business_parties,network_mb_total,cost_usd_total,request_line_count_diagnostic,req_list_participants_diagnostic,req_num_participants_diagnostic,tx_tid" \
    > "$OUT_CSV"
fi

extract_csv_line() {
  awk '
    BEGIN {in_csv=0; saw_header=0}
    /^=== CSV \(machine/ {in_csv=1; next}
    in_csv && /^template_variant,/ {saw_header=1; next}
    in_csv && saw_header {
      if ($0 ~ /^[[:space:]]*$/) next
      print $0
      exit
    }
  '
}

# Print the analyzer's “Excel friendly” table section (ends at "valid")
extract_human_table() {
  awk '
    /^=== CSV \(Excel friendly/ {in_tbl=1; print; next}
    in_tbl {
      print
      if ($0 ~ /^valid[[:space:]]+/) exit
    }
  '
}

for i in $(seq 1 "$N"); do
  echo "=============================="
  echo "RUN $i / $N"
  echo "=============================="

  RUN_ID="${SCRIPT_NAME//[:]/_}_run${i}"
  out="/tmp/canton-debug/out_${RUN_ID}.txt"
  err="/tmp/canton-debug/err_${RUN_ID}.txt"

  set +e
  if [[ "$PASS_I" == "yes" ]]; then
    ./scripts/run_one_benchmark.sh "$SCRIPT_NAME" "$DAR_GLOB" "${SCRIPT_ARGS[@]}" "$i" >"$out" 2>"$err"
  else
    ./scripts/run_one_benchmark.sh "$SCRIPT_NAME" "$DAR_GLOB" "${SCRIPT_ARGS[@]}" >"$out" 2>"$err"
  fi
  rc=$?
  set -e

  if [[ $rc -ne 0 ]]; then
    echo "ERROR: run_one_benchmark failed for $RUN_ID (rc=$rc)"
    echo "--- stderr (tail) ---"
    tail -n 120 "$err" || true
    echo "--- stdout (tail) ---"
    tail -n 120 "$out" || true
    exit $rc
  fi

  CSV_LINE="$(extract_csv_line < "$out" || true)"
  if [[ -z "${CSV_LINE:-}" ]]; then
    echo "ERROR: Could not extract CSV metrics for $RUN_ID."
    echo "Full output: $out"
    echo "--- stdout (tail) ---"
    tail -n 200 "$out" || true
    exit 1
  fi

  echo "${RUN_ID},${TODAY},${SCRIPT_NAME},${CSV_LINE}" >> "$OUT_CSV"
  echo "Appended to: $OUT_CSV"

  echo
  echo "=== RUN SUMMARY ==="
  printf "%-26s %s\n" "run_id" "$RUN_ID"
  printf "%-26s %s\n" "script_name" "$SCRIPT_NAME"
  echo
  extract_human_table < "$out" || true
  echo
done

echo
echo "Done. Results in: $OUT_CSV"
