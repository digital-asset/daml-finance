#!/usr/bin/env bash
# -----------------------------------------------------------------------------
# Start Canton Sandbox (clean)
#
# This script:
#  1) Ensures we are in the daml-finance repo
#  2) Cleans previous build artifacts
#  3) Rebuilds everything
#  4) Deletes previous Canton logs
#  5) Starts the sandbox (via sandbox.sh)
# -----------------------------------------------------------------------------

set -euo pipefail

echo "=== Starting clean Canton sandbox ==="

# Ensure we are at repo root
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

echo "Repository root: $REPO_ROOT"

# Clean previous builds
echo "Running make clean..."
make clean

echo "Running make install..."
make install

# Remove previous Canton log
LOG_FILE="my-canton.log"
if [[ -f "$LOG_FILE" ]]; then
  echo "Removing old log file: $LOG_FILE"
  rm -f "$LOG_FILE"
fi

# Start sandbox
echo "Starting Canton sandbox..."
echo "(sandbox.sh already runs 'dpm build --all')"
echo

exec ./scripts/sandbox.sh
