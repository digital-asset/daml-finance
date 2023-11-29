# Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# Attempts to install the SDK version from the open-source (OS) repository on GitHub. On failure,
# it falls back to downloading from the enterprise edition (EE).
install-sdk() {
  SDK_VERSION=$1
  OS=$2

  # Load environment variables from .envrc.private in the parent directory
  SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
  if [ -f "$SCRIPT_DIR/../.envrc.private" ]; then
    source "$SCRIPT_DIR/../.envrc.private"
  fi

  # Create a temporary directory for the SDK download
  TMP_DIR=$(mktemp -d)
  echo "Created temporary directory: $TMP_DIR"

  get_os() {
    curl --location \
          --fail \
          https://github.com/digital-asset/daml/releases/download/v${SDK_VERSION}/daml-sdk-${SDK_VERSION}-${OS}.tar.gz \
      > "$TMP_DIR/daml-sdk.tar.gz"
  }

  get_ee() {
    if [ -n "${ARTIFACTORY_PASSWORD}" ]; then
      curl -u "${ARTIFACTORY_USERNAME}:${ARTIFACTORY_PASSWORD}" \
          "https://digitalasset.jfrog.io/artifactory/assembly/daml/${SDK_VERSION}/daml-sdk-${SDK_VERSION}-${OS}.tar.gz" \
        > "$TMP_DIR/daml-sdk.tar.gz"
    else
      echo "ARTIFACTORY_USERNAME and ARTIFACTORY_PASSWORD must be set." >&2
      exit 1
    fi
  }

  # Check if the version is already installed
  if daml version | grep -q "$SDK_VERSION"; then
    echo "SDK version $SDK_VERSION is already installed."
  else
    echo "SDK version $SDK_VERSION is not installed. Installing now."
    if get_os; then
      echo "Downloaded using get_os"
    else
      echo "Failed to download using get_os, trying get_ee"
      if ! get_ee; then
        echo "Failed to download using get_ee"
        rm -rf "$TMP_DIR"
        echo "Cleaned up, now exiting"
        return 1 # Return a non-zero exit code to indicate failure
      fi
    fi
    (
      set -e
      cd "$TMP_DIR"
      tar xzf daml-sdk.tar.gz --strip-components 1
      echo "Extraction completed. Contents of $TMP_DIR:"
      ls "$TMP_DIR"
      ./install.sh --allow-install-non-release=yes
      # Clean up
      rm -rf "$TMP_DIR"
    )

    # Check the exit status of the subshell
    if [ $? -ne 0 ]; then
      echo "Installation failed"
      rm -rf "$TMP_DIR"
      return 1 # Return a non-zero exit code to indicate failure
    fi
  fi
}
