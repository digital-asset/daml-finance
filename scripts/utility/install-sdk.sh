# Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# Function: install_sdk
# Purpose:
#   Attempts to install a specific version of the SDK from the open-source (OS) repository on
#   GitHub. If it fails, it falls back to downloading from the enterprise edition (EE).
#
# Parameters:
#   SDK_VERSION: The specific version of the SDK to install.
#   OS: The operating system for which the SDK is to be installed.
#
# Implementation Details:
#   - Determines paths and loads environment variables.
#   - Creates a temporary directory for the SDK download.
#   - Contains two functions, get_os and get_ee, to handle downloading from different sources.
#       - get_os: Downloads from the open-source GitHub repository.
#       - get_ee: Downloads from the enterprise edition repository, requiring credentials.
#   - Checks if the specified SDK version is already installed.
#   - Attempts to download and install the SDK using get_os, and if it fails, tries get_ee.
#   - Extracts and installs the SDK from the downloaded tarball.
#   - Handles cleanup and error reporting throughout the process.
#
# Output:
#   - Prints status messages during the installation process.
#   - Returns 0 on successful installation, 1 on failure.
install_sdk() {
  SDK_VERSION=$1
  OS=$2

  # Get absolute path of this file.
  SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  ROOT_DIR="$SCRIPT_DIR/../.."

  # Load environment variables from .envrc.private in the parent directory
  if [ -f "$ROOT_DIR/.envrc.private" ]; then
    source "$ROOT_DIR/.envrc.private"
  fi

  # Create a temporary directory for the SDK download
  TMP_DIR=$(mktemp -d)

  get_os() {
    curl --location \
         --fail \
         https://github.com/digital-asset/daml/releases/download/v${SDK_VERSION}/daml-sdk-${SDK_VERSION}-${OS}.tar.gz \
      > "$TMP_DIR/daml-sdk.tar.gz"
  }

  get_ee() {
    if [ -n "${ARTIFACTORY_USERNAME}" ] && [ -n "${ARTIFACTORY_PASSWORD}" ]; then
      curl -u "${ARTIFACTORY_USERNAME}:${ARTIFACTORY_PASSWORD}" \
          "https://digitalasset.jfrog.io/artifactory/assembly/daml/${SDK_VERSION}/daml-sdk-${SDK_VERSION}-${OS}.tar.gz" \
        > "$TMP_DIR/daml-sdk.tar.gz"
        # curl -sSL "https://get.daml.com" | sh -s "${SDK_VERSION}"
    else
      echo "ARTIFACTORY_USERNAME and ARTIFACTORY_PASSWORD must be set." >&2
      return 1
    fi
  }

  # Check if the version is already installed
  if daml version | grep -q "$SDK_VERSION"; then
    echo "SDK version $SDK_VERSION is already installed. " >&2
  else
    echo "SDK version $SDK_VERSION is not installed. Installing now. " >&2
    if get_os; then
      echo "Downloaded using get_os. " >&2
    else
      echo "Failed to download using get_os, trying get_ee. " >&2
      if ! get_ee; then
        echo "Failed to download using get_ee. " >&2
        rm -rf "$TMP_DIR"
        echo "Cleaned up, now exiting. " >&2
        return 1 # Return a non-zero exit code to indicate failure
      fi
    fi
    (
      set -e
      cd "$TMP_DIR"
      tar xzf daml-sdk.tar.gz --strip-components 1
      echo "Extraction completed. " >&2
      ./install.sh --install-with-internal-version yes # Aternatively: curl -sSL https://get.daml.com/ | sh -s 2.8.0-rc2
      # Clean up
      rm -rf "$TMP_DIR"
    )

    # Check the exit status of the subshell
    if [ $? -ne 0 ]; then
      echo "Installation failed. " >&2
      rm -rf "$TMP_DIR"
      return 1 # Return a non-zero exit code to indicate failure
    fi
  fi

  return 0
}

# Test
# TEST=$(install_sdk "2.8.0-snapshot.20231206.12407.0.vb5eee4a3" "macos")
# echo $TEST
