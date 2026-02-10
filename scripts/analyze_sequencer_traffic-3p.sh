#!/usr/bin/env bash
# -----------------------------------------------------------------------------
# scripts/analyze_sequencer_traffic.sh
#
# Per-run slice log recommended.
#
# Metrics:
#   - request_bytes_total: sum of "sends request ... of size N bytes" (wire-bytes proxy)
#   - submission_count: UNIQUE command IDs observed in slice (proxy for "command submitted")
#   - tx_tree_computed_count: count of "Computed transaction tree..." (semantic tx proxy)
#   - tx_views_total/avg/distribution: extracted from tx-tree lines
#   - tx_accepted_count: count SequencedTransactionAccepted (proxy; noisy)
#   - participants (Option A): tid-scoped unique participants for the first tx-tree tid
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

# -----------------------------------------------------------------------------
# 1) Wire-bytes proxy
# -----------------------------------------------------------------------------
REQUEST_BYTES_TOTAL="$(
  (grep -a -E "$REQ_RE" "$LOG_FILE" 2>/dev/null || true) \
  | sed -E 's/.* of size ([0-9]+) bytes.*/\1/' \
  | awk 'BEGIN{s=0} /^[0-9]+$/ {s+=$1} END{print s+0}'
)"

REQUEST_LINE_COUNT_DIAGNOSTIC="$(
  (grep -a -E "$REQ_RE" "$LOG_FILE" 2>/dev/null || true) | wc -l | tr -d ' '
)"

# -----------------------------------------------------------------------------
# 2) Submission count - unique command-id from Transaction: "Preparing request submitters ... command-id ...""
SUBMISSION_IDS="$(
  (grep -a -Eo 'Preparing request submitters .* command-id [0-9a-fA-F-]{36}' "$LOG_FILE" 2>/dev/null || true) \
  | sed -E 's/.* command-id ([0-9a-fA-F-]{36}).*/\1/' \
  | sort -u
)"

SUBMISSION_COUNT="$(
  printf "%s\n" "$SUBMISSION_IDS" | awk 'NF{c++} END{print c+0}'
)"

REQUEST_BYTES_AVG_PER_SUBMISSION="$(
  printf "%s\n%s\n" "$REQUEST_BYTES_TOTAL" "$SUBMISSION_COUNT" | fmt_div2
)"

# -----------------------------------------------------------------------------
# 3) Semantic tx count + views
# -----------------------------------------------------------------------------
TX_TREE_COMPUTED_COUNT="$(
  (grep -a -E "$VIEWS_RE" "$LOG_FILE" 2>/dev/null || true) | wc -l | tr -d ' '
)"

TX_TREE_VIEWS_TOTAL="$(
  (grep -a -E "$VIEWS_RE" "$LOG_FILE" 2>/dev/null || true) \
  | sed -E 's/.*total=([0-9]+) views.*/\1/' \
  | awk 'BEGIN{s=0} /^[0-9]+$/ {s+=$1} END{print s+0}'
)"

TX_TREE_VIEWS_AVG="$(
  printf "%s\n%s\n" "$TX_TREE_VIEWS_TOTAL" "$TX_TREE_COMPUTED_COUNT" | fmt_div4
)"

TX_TREE_VIEWS_DISTRIBUTION="$(
  (grep -a -E "$VIEWS_RE" "$LOG_FILE" 2>/dev/null || true) \
  | sed -E 's/.*total=([0-9]+) views.*/\1/' \
  | awk '/^[0-9]+$/{print $1}' \
  | sort -n | uniq -c | sed -E 's/^[[:space:]]+//'
)"

# -----------------------------------------------------------------------------
# 4) Accepted tx count - count of ParallelIndexerSubscription Phase 7: Storing ... SequencedTransactionAccepted( (proxy; should be ~1 per accepted tx)
# -----------------------------------------------------------------------------
TX_ACCEPTED_COUNT="$(
  (grep -a -E 'Phase 7: Storing.*SequencedTransactionAccepted\(' "$LOG_FILE" 2>/dev/null || true) \
  | wc -l | tr -d ' '
)"

TX_ACCEPTED_PUBLISHER_COUNT="$(
  (grep -a -E 'RecordOrderPublisher:.*with event:SequencedTransactionAccepted\(' "$LOG_FILE" 2>/dev/null || true) \
  | wc -l | tr -d ' '
)"

# -----------------------------------------------------------------------------
# 5) Participants involved (Option A, FIXED: tid-scoped, robust parsing)
# -----------------------------------------------------------------------------
TX_TREE_LINE="$(
  grep -a -m 1 -E "$VIEWS_RE" "$LOG_FILE" 2>/dev/null || true
)"

# tid can appear like: "tid:11f4..." OR "Request tid:11f4..."
TX_TID="$(
  printf "%s\n" "$TX_TREE_LINE" \
  | sed -nE 's/.*\btid:([0-9a-f]+)\b.*/\1/p'
)"

# Participants involved = participants that computed the transaction tree for this TX_TID
if [[ -n "${TX_TID:-}" ]]; then
  TID_PARTICIPANTS_LIST="$(
    (grep -a -F "$TX_TID" "$LOG_FILE" 2>/dev/null || true) \
    | grep -a -F 'TransactionTreeFactoryImpl' \
    | sed -nE 's/.*\bparticipant=(participant[0-9]+).*/\1/p' \
    | sort -u | tr '\n' ' ' | sed -E 's/[[:space:]]+$//'
  )"

  TID_NUM_PARTICIPANTS="$(
    (grep -a -F "$TX_TID" "$LOG_FILE" 2>/dev/null || true) \
    | grep -a -F 'TransactionTreeFactoryImpl' \
    | sed -nE 's/.*\bparticipant=(participant[0-9]+).*/\1/p' \
    | sort -u | wc -l | tr -d ' '
  )"
fi

# Fallback if nothing matched (avoid empty output)
if [[ -z "${TID_PARTICIPANTS_LIST:-}" ]]; then
  TID_PARTICIPANTS_LIST="none"
  TID_NUM_PARTICIPANTS="0"
fi

# -----------------------------------------------------------------------------
# 5b) Participants from request lines (diagnostic only)
# -----------------------------------------------------------------------------
REQ_PARTICIPANTS_LIST="$(
  (grep -a -E "$REQ_RE" "$LOG_FILE" 2>/dev/null || true) \
  | sed -nE "s/.*'PAR::(participant[0-9]+)::.*'.*/\1/p" \
  | sort -u | tr '\n' ' ' | sed -E 's/[[:space:]]+$//'
)"

REQ_NUM_PARTICIPANTS="$(
  (grep -a -E "$REQ_RE" "$LOG_FILE" 2>/dev/null || true) \
  | sed -nE "s/.*'PAR::(participant[0-9]+)::.*'.*/\1/p" \
  | sort -u | wc -l | tr -d ' '
)"

if [[ -z "${REQ_PARTICIPANTS_LIST:-}" ]]; then
  REQ_PARTICIPANTS_LIST="none"
  REQ_NUM_PARTICIPANTS="0"
fi

PARTIES_LIST="$(
  grep -a -Eo '(aliceBank|alice|bobBank|bob|centralBank)::[0-9a-f]{64}' "$LOG_FILE" 2>/dev/null \
  | sed 's/::.*//' \
  | sort -u \
  | tr '\n' ' ' \
  | sed -E 's/[[:space:]]+$//'
)"

if [[ -z "${PARTIES_LIST:-}" ]]; then
  PARTIES_LIST="none"
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
# 8) Output
# -----------------------------------------------------------------------------
echo "=== REQUEST TRAFFIC (wire-bytes proxy; not guaranteed sequencer-only) ==="
echo "REQUEST_BYTES_TOTAL=${REQUEST_BYTES_TOTAL}"
echo "REQUEST_LINE_COUNT_DIAGNOSTIC=${REQUEST_LINE_COUNT_DIAGNOSTIC}"
echo

echo "=== SUBMISSIONS (proxy; UNIQUE command ids) ==="
echo "SUBMISSION_COUNT=${SUBMISSION_COUNT}"
echo "REQUEST_BYTES_AVG_PER_SUBMISSION=${REQUEST_BYTES_AVG_PER_SUBMISSION}"
echo

echo "=== SEMANTIC COUNTS ==="
echo "TX_TREE_COMPUTED_COUNT=${TX_TREE_COMPUTED_COUNT}"
echo "TX_ACCEPTED_COUNT(proxy,indexer)=${TX_ACCEPTED_COUNT}"
echo "TX_ACCEPTED_PUBLISHER_COUNT(diagnostic)=${TX_ACCEPTED_PUBLISHER_COUNT}"
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
echo "PARTIES_LIST=${PARTIES_LIST}"
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
printf "%-34s %-12s\n" "total_network_bytes"               "$REQUEST_BYTES_TOTAL"
printf "%-34s %-12s\n" "submission_count"                  "$SUBMISSION_COUNT"
printf "%-34s %-12s\n" "bytes_per_submission"              "$REQUEST_BYTES_AVG_PER_SUBMISSION"
printf "%-34s %-12s\n" "transactions_created"              "$TX_TREE_COMPUTED_COUNT"
printf "%-34s %-12s\n" "views_created_total"               "$TX_TREE_VIEWS_TOTAL"
printf "%-34s %-12s\n" "views_per_transaction"             "$TX_TREE_VIEWS_AVG"
printf "%-34s %-12s\n" "tx_accepted_count"                 "$TX_ACCEPTED_COUNT"
printf "%-34s %-12s\n" "bytes_per_transaction"             "$BYTES_PER_TX_COMPUTED"
printf "%-34s %-12s\n" "bytes_per_tx_accepted"             "$BYTES_PER_TX_ACCEPTED"
printf "%-34s %-12s\n" "bytes_per_view"                    "$BYTES_PER_VIEW"
printf "%-34s %-12s\n" "runner_participant_count"          "$TID_PARTICIPANTS_LIST"
printf "%-34s %-12s\n" "participant_count"                 "$TID_NUM_PARTICIPANTS"
printf "%-34s %-12s\n" "business_parties"                  "$PARTIES_LIST"
printf "%-34s %-12s\n" "network_mb_total"                  "$TRAFFIC_MB_TOTAL"
printf "%-34s %-12s\n" "cost_usd_total"                    "$COST_USD_TOTAL"
echo

echo "=== CSV (machine / Excel) ==="
echo "template_variant,total_network_bytes,submission_count,bytes_per_submission,transactions_created,views_created_total,views_per_transaction,tx_accepted_count,bytes_per_transaction,bytes_per_tx_accepted,bytes_per_view,runner_participant,runner_participant_count,business_parties,network_mb_total,cost_usd_total,request_line_count_diagnostic,req_list_participants_diagnostic,req_num_participants_diagnostic,tx_tid"
echo "\"${TEMPLATE_VARIANT}\",${REQUEST_BYTES_TOTAL},${SUBMISSION_COUNT},${REQUEST_BYTES_AVG_PER_SUBMISSION},${TX_TREE_COMPUTED_COUNT},${TX_TREE_VIEWS_TOTAL},${TX_TREE_VIEWS_AVG},${TX_ACCEPTED_COUNT},${BYTES_PER_TX_COMPUTED},${BYTES_PER_TX_ACCEPTED},${BYTES_PER_VIEW},\"${TID_PARTICIPANTS_LIST}\",${TID_NUM_PARTICIPANTS},\"${PARTIES_LIST}\",${TRAFFIC_MB_TOTAL},${COST_USD_TOTAL},${REQUEST_LINE_COUNT_DIAGNOSTIC},\"${REQ_PARTICIPANTS_LIST}\",${REQ_NUM_PARTICIPANTS},\"${TX_TID:-}\""
