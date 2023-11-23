#!/usr/bin/env bash
# Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# This script tests each template in the following list with the latest SDK snapshot release.
# It ensures that each template works correctly with the current version of the SDK.
# For each template in the list, the script performs the following actions:
# 1. Creates a new DAML project based on the template.
# 2. Retrieves any necessary dependencies and builds the project.
# 3. Runs tests to verify the integrity and functionality of the template.
#
# The templates tested by this script include:
# - quickstart-finance
# - finance-upgrades
# - finance-settlement
# - finance-lifecycling
# - finance-payoff-modeling
#
# The script is designed to be run from the user's home directory. It will create a temporary
# directory which will be removed upon completion.

# Configuration
TEMPLATES=("quickstart-finance" "finance-upgrades" "finance-settlement" "finance-lifecycling" "finance-payoff-modeling")
FOLDER="tmp-daml-finance-test-quickstarter-and-tutorials"

# Check if the current directory is your home directory
if [ "$(pwd)" != "$HOME" ]; then
  echo "==> This script must be run from your home directory ~/."
  exit 1
fi

# Start testing
echo "========================================"
echo "DAML Finance Template Testing Initiated"
echo "========================================"
echo "Using the newest SDK snapshot version to test the following templates:"
printf -v joined '%s, ' "${TEMPLATES[@]}"
joined=${joined%, }
echo "$joined"
echo "========================================"

# Create a new test folder
echo "==> Create a new ${FOLDER} folder."
find "$HOME" -name "${FOLDER}" -type d -maxdepth 1 -exec rm -rf {} \; 2>/dev/null
FOLDER_PATH="${HOME}/${FOLDER}"
mkdir -p "$FOLDER_PATH"

# Function to clean up the created folder
cleanup() {
  echo "==> Cleaning up $1"
  rm -rf "$1"
}

# Install the latest SDK snapshot version and extract the version
echo "==> Installing Latest SDK Snapshot Version"
SDK_VERSION=$(daml install latest --snapshots=yes | awk '/Installing SDK version/{print $4}')
echo "==> Downloaded SDK $SDK_VERSION"

for TEMPLATE in "${TEMPLATES[@]}"; do
  echo "==> Testing $TEMPLATE"

  # Create a new project
  cd "$FOLDER_PATH"
  daml new --template=$TEMPLATE $TEMPLATE || {
      echo "==> Failed to create DAML project with template $TEMPLATE"
      cleanup "$FOLDER_PATH"
      exit 1
    }
  PROJECT_PATH="${FOLDER_PATH}/${TEMPLATE}"

  # Get dependencies, build the project, and run tests, handling any failure
  ( cd "$PROJECT_PATH" &&
    ./get-dependencies.sh &&
    daml build &&
    daml test
  ) || {
    echo "==> Failed testing $TEMPLATE"
    cleanup "$FOLDER_PATH"
    exit 1
  }

  echo "==> Test for $TEMPLATE finished successfully"
  echo
done

echo
echo "==> Testing completed successfully (for all templates)"
