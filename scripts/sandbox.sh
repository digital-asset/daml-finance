#!/usr/bin/env bash
# Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates.
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail

echo "Building all DAML packages..."
dpm build --all

echo "Collecting DARs..."
mapfile -t DARS < <(
  find package/main/daml package/test/daml \
    -path "*/.daml/dist/*.dar" \
    -type f \
  | sort
)

if [ "${#DARS[@]}" -eq 0 ]; then
  echo "No DAR files found. Build must have failed."
  exit 1
fi

echo "(Starting Canton sandbox with ${#DARS[@]} DARs)"

BENCH_DAR="$(ls -1 package/test/daml/Daml.Finance.Benchmark.Test/.daml/dist/*.dar | head -n 1)"

CMD=(dpm sandbox
  --dev
  --ledger-api-port 6865
  --json-api-port 7575
  --log-encoder json
  --log-file-name my-canton.log
  --log-truncate
  --debug
  --log-last-errors true
  -c dev-protocol.conf
  --dar "$BENCH_DAR"
)

exec "${CMD[@]}"

