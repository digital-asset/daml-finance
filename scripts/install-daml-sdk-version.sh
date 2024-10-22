#!/usr/bin/env bash
# Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail

# Function: install_daml_sdk_version
#
# Purpose:
#   Attempts to install a specific version of the SDK from the open-source (OS) repository on
#   GitHub. If it fails, it falls back to downloading from the enterprise edition (EE) from JFrog.
#   It is intended to be executed from a directory outside of the Daml Finance nix environment.
#
# Parameters:
#   OS: The operating system for which the SDK is to be installed.
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
# Implementation Details:
#   - The function downloads and installs the SDK. It first tries the open-source GitHub repo and
#     then falls back to the enterprise edition (EE) on failure.
#   - Assumes that a pre-installed Daml SDK is already available.
#
# Output:
#   - Returns 0 on successful installation, 1 on failure.
install_daml_sdk_version() {
  OS=$1
  SDK_VERSION=$2
  DAML_VERSION=${3:-$SDK_VERSION} # Use SDK version as default if DAML version is not provided

  # Ensure 'daml' command is available
  if ! command -v daml &> /dev/null; then
    echo "Error: 'daml' command not found. Please ensure a Daml SDK is pre-installed."
    exit 1
  fi

  # Get absolute path of this file.
  SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  ROOT_DIR="$SCRIPT_DIR/.."

  # Load environment variables from .envrc.private in the parent directory
  if [ -f "$ROOT_DIR/.envrc.private" ]; then
    source "$ROOT_DIR/.envrc.private"
  fi

  # Check if the version is already installed
  if daml version | grep -q "$SDK_VERSION"; then
    echo "SDK version $SDK_VERSION is already installed. " >&2
    return 0
  fi

    # Create a temporary directory for the SDK download
  OUT_DIR=$(mktemp -d)
  echo "Temporary folder created at: $OUT_DIR"
  # Ensure the temporary folder is cleaned up on script exit
  trap 'echo "Cleaning up"; rm -rf "$OUT_DIR"' EXIT

  # Tarball location
  OUT="$OUT_DIR/daml-sdk.tar.gz"

  # Download SDK function with fallback
  download_sdk() {
    # First try GitHub open-source download
    echo "Trying GitHub..."
    if curl --location --fail \
      "https://github.com/digital-asset/daml/releases/download/v${DAML_VERSION}/daml-sdk-${SDK_VERSION}-${OS}.tar.gz" \
      --output "$OUT"; then
      return 0
    fi

    # If GitHub download fails, try Enterprise Edition (EE) repository
    echo "GitHub failed, trying EE..."
    if [ -n "${ARTIFACTORY_USERNAME:-}" ] && [ -n "${ARTIFACTORY_PASSWORD:-}" ]; then
      curl -u "${ARTIFACTORY_USERNAME}:${ARTIFACTORY_PASSWORD}" \
        "https://digitalasset.jfrog.io/artifactory/assembly/daml/${SDK_VERSION}/daml-sdk-${SDK_VERSION}-${OS}.tar.gz" \
        --output "$OUT"
    else
      echo "Artifactory credentials (ARTIFACTORY_USERNAME and ARTIFACTORY_PASSWORD) must be set." >&2
      return 1
    fi
  }

  # Download and handle failure
  if ! download_sdk; then
    echo "Failed to download the SDK. Exiting."
    exit 1
  fi

  # Install SDK
  if ! daml install --install-assistant yes --install-with-internal-version yes "$OUT"; then
    rm -rf "$OUT_DIR"
    echo "Installation failed." >&2
    return 1 # Return a non-zero exit code to indicate failure
  fi
  echo "SDK version $SDK_VERSION installation completed successfully."

  return 0
}

# Check if the script is being executed directly or sourced
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  # Run the function if the script is executed directly
  if [[ $# -lt 2 ]]; then
    echo "Usage: $0 <OS> <SDK_VERSION> [<DAML_VERSION>]"
    exit 1
  fi
  install_daml_sdk_version "$1" "$2" "${3:-}"
fi
