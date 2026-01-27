#!/usr/bin/env bash
# -----------------------------------------------------------------------------
# scripts/analyze_sequencer_traffic_1_2p.sh
#
# For 1–2 participant setups (e.g., sandbox only, or sandbox + participant1).
#
# Produces the same metrics as analyze_sequencer_traffic.sh, but is robust to:
#   - 0 matches for grep patterns (won't exit under set -euo pipefail)
#   - participant names that are not "participantN" (e.g., sandbox, bankA)
#
# Metrics:
#   - request_bytes_total: sum of "sends request ... of size N bytes"
#   - submission_count: UNIQUE command IDs observed in slice
#   - tx_tree_computed_count: count of "Computed transaction tree..."
#   - tx_views_total/avg/distribution
#   - tx_accepted_count: count SequencedTransactionAccepted (proxy)
#   - participants (Option A): tid-scoped unique participant names for first tx-tree tid
# -----------------------------------------------------------------------------

set -euo pipefail

LOG_FILE="${LOG_FILE:-my-canton.log}"
TEMPLATE_VARIANT="${TEMPLATE_VARIANT:-unknown}"

PRICE_USD_PER_MB="${PRICE_USD_PER_MB:-60}"
MB_BYTES="${MB_BYTES:-1000000}"

die() { echo "ERROR: $*" >&2; exit 1; }
need_file() { [[ -f "$1" ]] || die "$1 not found. (Hint: produce a per-run slice log)"; }

fmt_div2() { awk 'NR==1{t=$1} NR==2{c=$1} END{ if(c>0) printf "%.2f", t/c; else print "NA" }'; }
fmt_div4() { awk 'NR==1{t=$1} NR==2{c=$1} END{ if(c>0) printf "%.4f", t/c; else print "NA" }'; }

need_file "$LOG_FILE"

echo "=== ANALYZING ${LOG_FILE} ==="
echo

# -----------------------------------------------------------------------------
# 0) Patterns
# -----------------------------------------------------------------------------
REQ_RE='sends request .* of size [0-9]+ bytes'
VIEWS_RE='Computed transaction tree with total=[0-9]+ views'
ACCEPTED_RE='SequencedTransactionAccepted'

# command-id is a UUID in your logs.
# Keep commandId = <hex> as fallback (some structured logs).
# CommandId(<uuid>) appears in some structured dumps.
SUBMIT_RE='command-id [0-9a-f-]{36}|commandId = [0-9a-f]+|CommandId\([0-9a-f-]{36}\)'

# Generic participant name after "participant=" in JSON logs:
# examples:
#   logger_name "...:participant=sandbox"
#   message "... participant=participant1/psid=..."
#   message "... participant=participant1 tid:...."
PART_RE='\bparticipant=([A-Za-z0-9_-]+)(\/|[[:space:]]|$)'

# -----------------------------------------------------------------------------
# 1) Wire-bytes proxy
# -----------------------------------------------------------------------------
REQUEST_BYTES_TOTAL="$(
  (grep -E "$REQ_RE" "$LOG_FILE" 2>/dev/null || true) \
  | sed -E 's/.* of size ([0-9]+) bytes.*/\1/' \
  | awk 'BEGIN{s=0} /^[0-9]+$/ {s+=$1} END{print s+0}'
)"

REQUEST_LINE_COUNT_DIAGNOSTIC="$(
  (grep -E "$REQ_RE" "$LOG_FILE" 2>/dev/null || true) | wc -l | tr -d ' '
)"

# -----------------------------------------------------------------------------
# 2) Submission count (UNIQUE command IDs)
# -----------------------------------------------------------------------------
SUBMISSION_COUNT="$(
  (grep -Eo "$SUBMIT_RE" "$LOG_FILE" 2>/dev/null || true) \
  | awk '
      {
        gsub(/^command-id[[:space:]]+/, "", $0)
        gsub(/^commandId[[:space:]]*=[[:space:]]*/, "", $0)
        gsub(/^CommandId\(/, "", $0)
        gsub(/\)$/, "", $0)
        print $0
      }
    ' \
  | sort -u | wc -l | tr -d ' '
)"

REQUEST_BYTES_AVG_PER_SUBMISSION="$(
  printf "%s\n%s\n" "$REQUEST_BYTES_TOTAL" "$SUBMISSION_COUNT" | fmt_div2
)"

# -----------------------------------------------------------------------------
# 3) Semantic tx count + views
# -----------------------------------------------------------------------------
TX_TREE_COMPUTED_COUNT="$(
  (grep -E "$VIEWS_RE" "$LOG_FILE" 2>/dev/null || true) | wc -l | tr -d ' '
)"

TX_TREE_VIEWS_TOTAL="$(
  (grep -E "$VIEWS_RE" "$LOG_FILE" 2>/dev/null || true) \
  | sed -E 's/.*total=([0-9]+) views.*/\1/' \
  | awk 'BEGIN{s=0} /^[0-9]+$/ {s+=$1} END{print s+0}'
)"

TX_TREE_VIEWS_AVG="$(
  printf "%s\n%s\n" "$TX_TREE_VIEWS_TOTAL" "$TX_TREE_COMPUTED_COUNT" | fmt_div4
)"

TX_TREE_VIEWS_DISTRIBUTION="$(
  (grep -E "$VIEWS_RE" "$LOG_FILE" 2>/dev/null || true) \
  | sed -E 's/.*total=([0-9]+) views.*/\1/' \
  | awk '/^[0-9]+$/{print $1}' \
  | sort -n | uniq -c | sed -E 's/^[[:space:]]+//'
)"

# -----------------------------------------------------------------------------
# 4) Accepted tx count (proxy)
# -----------------------------------------------------------------------------
TX_ACCEPTED_COUNT="$(
  (grep -F "$ACCEPTED_RE" "$LOG_FILE" 2>/dev/null || true) | wc -l | tr -d ' '
)"

# -----------------------------------------------------------------------------
# 5) Participants involved (Option A: tid-scoped)
# -----------------------------------------------------------------------------
TX_TREE_LINE="$(
  (grep -m 1 -E "$VIEWS_RE" "$LOG_FILE" 2>/dev/null || true)
)"

TX_TID="$(
  printf "%s\n" "$TX_TREE_LINE" \
  | sed -nE 's/.*\btid:([0-9a-f]+)\b.*/\1/p'
)"

if [[ -n "${TX_TID:-}" ]]; then
  # Extract participant=<name> on lines that contain THIS tid.
  TID_PARTICIPANTS_LIST="$(
    (grep -F "$TX_TID" "$LOG_FILE" 2>/dev/null || true) \
    | sed -nE "s/.*${PART_RE}.*/\1/p" \
    | sort -u | tr '\n' ' ' | sed -E 's/[[:space:]]+$//'
  )"

  TID_NUM_PARTICIPANTS="$(
    (grep -F "$TX_TID" "$LOG_FILE" 2>/dev/null || true) \
    | sed -nE "s/.*${PART_RE}.*/\1/p" \
    | sort -u | wc -l | tr -d ' '
  )"
else
  TID_PARTICIPANTS_LIST="none"
  TID_NUM_PARTICIPANTS="0"
fi

if [[ -z "${TID_PARTICIPANTS_LIST:-}" ]]; then
  TID_PARTICIPANTS_LIST="none"
  TID_NUM_PARTICIPANTS="0"
fi

# -----------------------------------------------------------------------------
# 5b) Participants from request lines (diagnostic only)
# -----------------------------------------------------------------------------
REQ_PARTICIPANTS_LIST="$(
  (grep -E "$REQ_RE" "$LOG_FILE" 2>/dev/null || true) \
  | sed -nE "s/.*${PART_RE}.*/\1/p" \
  | sort -u | tr '\n' ' ' | sed -E 's/[[:space:]]+$//'
)"

REQ_NUM_PARTICIPANTS="$(
  (grep -E "$REQ_RE" "$LOG_FILE" 2>/dev/null || true) \
  | sed -nE "s/.*${PART_RE}.*/\1/p" \
  | sort -u | wc -l | tr -d ' '
)"

if [[ -z "${REQ_PARTICIPANTS_LIST:-}" ]]; then
  REQ_PARTICIPANTS_LIST="none"
  REQ_NUM_PARTICIPANTS="0"
fi

# -----------------------------------------------------------------------------
# 6) Derived metrics
# -----------------------------------------------------------------------------
BYTES_PER_TX_COMPUTED="$(
  printf "%s\n%s\n" "$REQUEST_BYTES_TOTAL" "$TX_TREE_COMPUTED_COUNT" | fmt_div2
)"

BYTES_PER_TX_ACCEPTED="$(
  printf "%s\n%s\n" "$REQUEST_BYTES_TOTAL" "$TX_ACCEPTED_COUNT" | fmt_div2
)"

BYTES_PER_VIEW="$(
  printf "%s\n%s\n" "$REQUEST_BYTES_TOTAL" "$TX_TREE_VIEWS_TOTAL" | fmt_div2
)"

# -----------------------------------------------------------------------------
# 7) Pricing ($/MB, decimal)
# -----------------------------------------------------------------------------
TRAFFIC_MB_TOTAL="$(
  awk -v b="$REQUEST_BYTES_TOTAL" -v mb="$MB_BYTES" 'BEGIN{ if(mb>0) printf "%.6f", b/mb; else print "NA" }'
)"

COST_USD_TOTAL="$(
  awk -v m="$TRAFFIC_MB_TOTAL" -v p="$PRICE_USD_PER_MB" 'BEGIN{ if(m!="NA") printf "%.6f", m*p; else print "NA" }'
)"

# -----------------------------------------------------------------------------
# 8) Output (same as 3p script)
# -----------------------------------------------------------------------------
echo "=== REQUEST TRAFFIC (wire-bytes proxy; not guaranteed sequencer-only) ==="
echo "REQUEST_BYTES_TOTAL=${REQUEST_BYTES_TOTAL}"
echo "REQUEST_LINE_COUNT_DIAGNOSTIC=${REQUEST_LINE_COUNT_DIAGNOSTIC}"
echo

echo "=== SUBMISSIONS (proxy; UNIQUE command ids) ==="
echo "SUBMIT_RE=${SUBMIT_RE}"
echo "SUBMISSION_COUNT=${SUBMISSION_COUNT}"
echo "REQUEST_BYTES_AVG_PER_SUBMISSION=${REQUEST_BYTES_AVG_PER_SUBMISSION}"
echo

echo "=== SEMANTIC COUNTS ==="
echo "TX_TREE_COMPUTED_COUNT=${TX_TREE_COMPUTED_COUNT}"
echo "TX_ACCEPTED_COUNT(proxy)=${TX_ACCEPTED_COUNT}"
echo

echo "=== TRANSACTION VIEWS ==="
echo "TX_TREE_VIEWS_TOTAL=${TX_TREE_VIEWS_TOTAL}"
echo "TX_TREE_VIEWS_AVG=${TX_TREE_VIEWS_AVG}"
echo "TX_TREE_VIEWS_DISTRIBUTION (count views):"
echo "${TX_TREE_VIEWS_DISTRIBUTION:-none}"
echo

echo "=== PARTICIPANTS INVOLVED (Option A: tid-scoped; preferred) ==="
echo "TX_TID=${TX_TID:-none}"
echo "LIST_PARTICIPANTS=${TID_PARTICIPANTS_LIST}"
echo "NUM_PARTICIPANTS=${TID_NUM_PARTICIPANTS}"
echo

echo "=== PARTICIPANTS (from request lines; diagnostic only) ==="
echo "REQ_LIST_PARTICIPANTS=${REQ_PARTICIPANTS_LIST}"
echo "REQ_NUM_PARTICIPANTS=${REQ_NUM_PARTICIPANTS}"
echo

echo "=== DERIVED METRICS ==="
echo "BYTES_PER_TX_COMPUTED=${BYTES_PER_TX_COMPUTED}"
echo "BYTES_PER_TX_ACCEPTED=${BYTES_PER_TX_ACCEPTED}"
echo "BYTES_PER_VIEW=${BYTES_PER_VIEW}"
echo

echo "=== PRICING ==="
echo "PRICE_USD_PER_MB=${PRICE_USD_PER_MB}"
echo "TRAFFIC_MB_TOTAL=${TRAFFIC_MB_TOTAL}"
echo "COST_USD_TOTAL=${COST_USD_TOTAL}"
echo

echo "=== CSV (Excel friendly – human readable) ==="
printf "%-34s %-12s\n" "metric" "value"
printf "%-34s %-12s\n" "----------------------------------" "------------"
printf "%-34s %-12s\n" "template_variant"                  "$TEMPLATE_VARIANT"
printf "%-34s %-12s\n" "request_bytes_total"               "$REQUEST_BYTES_TOTAL"
printf "%-34s %-12s\n" "submission_count"                  "$SUBMISSION_COUNT"
printf "%-34s %-12s\n" "bytes_avg_per_submission"          "$REQUEST_BYTES_AVG_PER_SUBMISSION"
printf "%-34s %-12s\n" "tx_tree_computed_count"            "$TX_TREE_COMPUTED_COUNT"
printf "%-34s %-12s\n" "tx_views_total"                    "$TX_TREE_VIEWS_TOTAL"
printf "%-34s %-12s\n" "tx_views_avg"                      "$TX_TREE_VIEWS_AVG"
printf "%-34s %-12s\n" "tx_accepted_count"                 "$TX_ACCEPTED_COUNT"
printf "%-34s %-12s\n" "bytes_per_tx_computed"             "$BYTES_PER_TX_COMPUTED"
printf "%-34s %-12s\n" "bytes_per_tx_accepted"             "$BYTES_PER_TX_ACCEPTED"
printf "%-34s %-12s\n" "bytes_per_view"                    "$BYTES_PER_VIEW"
printf "%-34s %-12s\n" "list_participants"                 "$TID_PARTICIPANTS_LIST"
printf "%-34s %-12s\n" "number_unique_participants"        "$TID_NUM_PARTICIPANTS"
printf "%-34s %-12s\n" "traffic_mb_total"                  "$TRAFFIC_MB_TOTAL"
printf "%-34s %-12s\n" "cost_usd_total"                    "$COST_USD_TOTAL"
echo

echo "=== CSV (machine / Excel) ==="
echo "template_variant,request_bytes_total,submission_count,bytes_avg_per_submission,tx_tree_computed_count,tx_views_total,tx_views_avg,tx_accepted_count,bytes_per_tx_computed,bytes_per_tx_accepted,bytes_per_view,list_participants,number_unique_participants,traffic_mb_total,cost_usd_total"
echo "\"$TEMPLATE_VARIANT\",$REQUEST_BYTES_TOTAL,$SUBMISSION_COUNT,$REQUEST_BYTES_AVG_PER_SUBMISSION,$TX_TREE_COMPUTED_COUNT,$TX_TREE_VIEWS_TOTAL,$TX_TREE_VIEWS_AVG,$TX_ACCEPTED_COUNT,$BYTES_PER_TX_COMPUTED,$BYTES_PER_TX_ACCEPTED,$BYTES_PER_VIEW,\"$TID_PARTICIPANTS_LIST\",$TID_NUM_PARTICIPANTS,$TRAFFIC_MB_TOTAL,$COST_USD_TOTAL"
