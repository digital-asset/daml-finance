#!/usr/bin/env bash
# Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail

# Function: test_quickstarter_and_tutorials
#
# Purpose:
# This script tests the quickstarter and tutorials using the specified SDK version (argument 1) and
# Daml version (argument 2). It is intended to be executed from a directory outside of the Daml
# Finance nix environment. It assumes that the 'daml' command is available, i.e., a Daml SDK is
# pre-installed.
#
# Parameters:
#   SDK_VERSION: The specific version of the SDK to install.
#   DAML_VERSION: (Optional) Daml version is only used when the SDK is sourced from GitHub (see
#                 `get_os` below). It refers to the version number of the files of the release
#                 folder. For example, if you use a release candidate like:
#                 https://github.com/digital-asset/daml/releases/tag/v2.9.0-rc1
#                 the daml-version would be: 2.9.0-snapshot.20240619.12850.0.v0cfddd39
#                 On the other hand, if you use a regular release like:
#                 https://github.com/digital-asset/daml/releases/tag/v2.8.0
#                 the daml-version would simply be: 2.8.0
#
# Templates Tested:
#   - quickstart-finance
#   - finance-upgrades
#   - finance-settlement
#   - finance-lifecycling
#   - finance-payoff-modeling
#
# The script performs the following steps for each template:
#   1. Creates a new Daml project for the template.
#   2. Retrieves necessary dependencies and builds the project.
#   3. Executes tests to verify the template's functionality.
test_quickstarter_and_tutorials() {
  SDK_VERSION=$1
  DAML_VERSION=${2:-$SDK_VERSION} # Use SDK version as default if DAML version is not provided
  TEMPLATES=("quickstart-finance" "finance-upgrades" "finance-settlement" "finance-lifecycling" "finance-payoff-modeling")
  FAILED_TEMPLATES=()

  # Ensure 'daml' command is available
  if ! command -v daml &> /dev/null; then
    echo "Error: 'daml' command not found. Please ensure a Daml SDK is pre-installed."
    exit 1
  fi

  # Initiate the test.
  echo "======================================================="
  echo "Test Daml Finance Quickstarter and Tutorials"
  printf -v joined '%s, ' "${TEMPLATES[@]}"
  joined=${joined%, }
  echo "TEMPLATES: $joined"
  echo "SDK_VERSION: $SDK_VERSION"
  echo "DAML_VERSION: $DAML_VERSION"
  echo "======================================================="

  # Get absolute path of this file.
  SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  ROOT_DIR="$SCRIPT_DIR/.."

  # Create a temporary directory for the testing
  FOLDER_PATH=$(mktemp -d) # Automatically generate a system-managed temporary folder
  echo "Temporary folder created at: $FOLDER_PATH"
  # Ensure the temporary folder is cleaned up on script exit
  trap 'echo "Cleaning up"; rm -rf "$FOLDER_PATH"' EXIT

  # Install SDK
  echo "==> Installing SDK. "
  source "$SCRIPT_DIR/install-daml-sdk-version.sh"
  install_daml_sdk_version "macos" "$SDK_VERSION" "$DAML_VERSION"

  # Test all templates
  for TEMPLATE in "${TEMPLATES[@]}"; do
    echo "==> Creating new project for $TEMPLATE"
    cd "$FOLDER_PATH"
    if ! daml new --template=$TEMPLATE $TEMPLATE; then
      echo "==> Failed to create Daml project with template $TEMPLATE"
      FAILED_TEMPLATES+=("$TEMPLATE")
      continue  # Continue to the next template
    fi
    PROJECT_PATH="${FOLDER_PATH}/${TEMPLATE}"
    cd "$PROJECT_PATH"

    echo "==> Fetching dependencies for $TEMPLATE"
    if ! ./get-dependencies.sh; then
      echo "==> Failed fetching dependencies for $TEMPLATE"
      FAILED_TEMPLATES+=("$TEMPLATE")
      continue
    fi

    echo "==> Building project for $TEMPLATE"
    if ! daml build; then
      echo "==> Failed building project for $TEMPLATE"
      FAILED_TEMPLATES+=("$TEMPLATE")
      continue
    fi

    echo "==> Running tests for $TEMPLATE"
    if ! daml test; then
      echo "==> Failed running tests for $TEMPLATE"
      FAILED_TEMPLATES+=("$TEMPLATE")
      continue
    fi

    echo "==> Test for $TEMPLATE finished successfully"
    echo
  done

  # Summary of failed templates
  if [ "${#FAILED_TEMPLATES[@]}" -gt 0 ]; then
    echo "======================================================="
    echo "Failed Templates:"
    for TEMPLATE in "${FAILED_TEMPLATES[@]}"; do
      echo "- $TEMPLATE"
    done
    echo "======================================================="
    exit 1
  fi

  echo
  echo "==> Testing completed successfully (for all templates)"
  echo "======================================================="
}

# Check if the script is being executed directly or sourced
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  # Run the function if the script is executed directly
  if [[ $# -lt 1 ]]; then
    echo "Usage: $0 <SDK_VERSION> [<DAML_VERSION>]"
    exit 1
  fi
  test_quickstarter_and_tutorials "$1" "${2:-}"
fi
