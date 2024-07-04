#!/usr/bin/env bash
# Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# Script: Test Daml Finance Templates with Latest SDK
#
# Purpose:
#   This script automates the testing of various Daml Finance templates using the "latest" SDK
#   version. It is intended to ensure that these templates function correctly for the SDK version.
#
# Templates Tested:
#   - quickstart-finance
#   - finance-upgrades
#   - finance-settlement
#   - finance-lifecycling
#   - finance-payoff-modeling
#
# Workflow:
#   1. Creates a new Daml project for each listed template.
#   2. Retrieves necessary dependencies and builds the project.
#   3. Executes tests to verify the template's functionality.
#
# Environment Setup:
#   - Requires a GitHub access token in the .envrc.private file (GITHUB_TOKEN=<your-token>) to
#     fetch the "latest" SDK version.
#   - The script is designed to run from a directory outside of the daml-finance repository
#     (e.g., ~/).
#
# Temporary Directory:
#   - Creates and utilizes a temporary directory for testing, which is removed upon script
#     completion.
#
# Filters for SDK Version Retrieval:
#   - require_asset_name_substring: Filters SDK versions based on asset name substring.
#   - exclude_asset_name_substring_if_non_empty: Excludes SDK versions with specific asset name
#     substrings.
#   - require_release_tag_name_substring: Filters SDK versions based on release tag name substring.
#   - exclude_release_tag_name_substring_if_non_empty: Excludes SDK versions with specific release
#     tag name substrings.
#
# Output:
#   - Prints detailed logs of the testing process.
#   - Provides clear error messages and exits with an error code if any test fails.

# Daml Finance templates
TEMPLATES=("quickstart-finance" "finance-upgrades" "finance-settlement" "finance-lifecycling" "finance-payoff-modeling")

# Filters for finding "latest" SDK version (from the daml GitHub releases page)
require_asset_name_substring="2.9.0"               # e.g. "2.8.0"
exclude_asset_name_substring_if_non_empty=""       # e.g. "2.9.0"
require_release_tag_name_substring="rc"            # e.g. "rc"
exclude_release_tag_name_substring_if_non_empty="" # e.g. "snapshot"

# Initiate the test.
echo "==============================="
echo "Test SDK Daml Finance Templates"
echo "==============================="
printf -v joined '%s, ' "${TEMPLATES[@]}"
joined=${joined%, }
echo "templates: $joined"
echo "require_asset_name_substring: $require_asset_name_substring"
echo "exclude_asset_name_substring_if_non_empty: $exclude_asset_name_substring_if_non_empty"
echo "require_release_tag_name_substring: $require_release_tag_name_substring"
echo "exclude_release_tag_name_substring_if_non_empty: $exclude_release_tag_name_substring_if_non_empty"
echo "==============================="

# Get absolute path of this file.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$SCRIPT_DIR/.."

# Get SDK version
echo "==> Get \"latest\" SDK version"
source "$SCRIPT_DIR/utility/get-latest-sdk-version.sh"
SDK_AND_DAML_VERSION=$(get_latest_sdk_version "$require_asset_name_substring" "$exclude_asset_name_substring_if_non_empty" "$require_release_tag_name_substring" "$exclude_release_tag_name_substring_if_non_empty")
# Set IFS to : for splitting
IFS=":" read -r SDK_VERSION DAML_VERSION <<< "$SDK_AND_DAML_VERSION"
# Check if the command failed or if SDK_VERSION is empty
if [ $? -ne 0 ] || [ -z "$SDK_VERSION" ]; then
  echo "Error: Failed to get the SDK version."
  exit 1
fi
echo "Using SDK Version: $SDK_VERSION"
echo "Using Daml Version: $DAML_VERSION"

# Create a temporary test folder
FOLDER="tmp-daml-finance-test-quickstarter-and-tutorials"
echo "==> Create a new ${FOLDER} folder."
find "$HOME" -name "${FOLDER}" -type d -maxdepth 1 -exec rm -rf {} \; 2>/dev/null
FOLDER_PATH="${HOME}/${FOLDER}"
mkdir -p "$FOLDER_PATH"

# Start testing
echo "==> Installing SDK. "
source "$SCRIPT_DIR/utility/install-sdk.sh"
install_sdk "$SDK_VERSION" "$DAML_VERSION" "macos"
exit_status=$?
if [ $exit_status -ne 0 ]; then
  echo "Failed to download and install SDK. Exiting script."
  exit $exit_status
fi

# Function to clean up the created folder
cleanup() {
  echo "==> Cleaning up $1"
  rm -rf "$1"
}

for TEMPLATE in "${TEMPLATES[@]}"; do
  echo "==> Testing $TEMPLATE"

  # Create a new project
  cd "$FOLDER_PATH"
  daml new --template=$TEMPLATE $TEMPLATE || {
      echo "==> Failed to create Daml project with template $TEMPLATE"
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
