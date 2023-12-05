#!/usr/bin/env bash
# Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

# Function: version_gt
# Purpose:
#   Compares two semantic version numbers to determine if the first one is greater than the second.
# Usage:
#   version_gt [version1] [version2]
# The function checks if the first argument ($1) is not the lower version, implying it's greater.
# Output:
#   Returns true (0) if the first version is greater, false (1) otherwise.
version_gt() {
  test "$(printf '%s\n' "$@" | sort -V | head -n 1)" != "$1";
}

# Function: get_latest_sdk_version
# Purpose:
#   Fetches the latest SDK version from a specified GitHub repository, taking into account various filter parameters.
#
# Parameters:
#   require_asset_name_substring:
#       A substring that must be present in the asset name. Defaults to an empty string if not provided.
#   exclude_asset_name_substring_if_non_empty:
#       A substring that, if present, excludes the asset. Defaults to an empty string if not provided.
#   require_release_tag_name_substring:
#       A substring that must be present in the release tag name. Defaults to an empty string if not provided.
#   exclude_release_tag_name_substring_if_non_empty:
#       A substring that, if present, excludes the release tag. Defaults to an empty string if not provided.
#
# Implementation Details:
#   - Determines paths and loads environment variables.
#   - Checks for the presence of a GITHUB_TOKEN in the environment.
#   - Defines the GitHub API URL for listing releases.
#   - Fetches release information using curl and processes the JSON response.
#   - Iterates over each release, applying filters based on the provided substrings.
#   - Extracts the asset name, tag name, and release name, performing necessary string manipulations.
#   - Uses the version_gt function to identify the highest version.
#
# Output:
#   - Prints details of the highest version release.
#   - Returns the name of the highest version asset.
get_latest_sdk_version() {
  require_asset_name_substring=${1:-""}                    # Defaults to empty string if not provided
  exclude_asset_name_substring_if_non_empty=${2:-""}       # Defaults to empty string if not provided
  require_release_tag_name_substring=${3:-""}              # Defaults to empty string if not provided
  exclude_release_tag_name_substring_if_non_empty=${4:-""} # Defaults to empty string if not provided

  # Get absolute path of this file.
  SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  ROOT_DIR="$SCRIPT_DIR/../.."

  # Load environment variables from .envrc.private in the parent directory
  if [ -f "$ROOT_DIR/.envrc.private" ]; then
    source "$ROOT_DIR/.envrc.private"
  fi
  if [ -z "$GITHUB_TOKEN" ]; then
    echo "Error: No GITHUB_TOKEN set in .envrc.private." >&2
    return 1
  fi

  # Define the repository owner and name
  owner="digital-asset"
  repo="daml"

  # GitHub API URL for listing releases
  releases_url="https://api.github.com/repos/$owner/$repo/releases"

  # Get the list of releases using the GitHub token
  releases_json=$(curl -s -H "Authorization: token $GITHUB_TOKEN" $releases_url)

  # echo "DEBUG: $releases_json"

  # Check if the response is valid JSON
  if ! jq empty <<<"$releases_json"; then
    echo "Failed to parse JSON response" >&2
    return 1
  fi

  # Initialize variables for the highest version release
  highest_version="0.0.0"
  highest_version_release_tag_name=""
  highest_version_release_name=""
  highest_version_asset_name=""

  # Process each release
  while IFS= read -r line; do
    asset_name=$(echo "$line" | jq -r '.assets[] | select(.name | endswith("macos.tar.gz")).name')

    # Check if the asset_name contains the required string
    if [[ "$asset_name" == *"$require_asset_name_substring"* ]]; then

      # Check that the asset_name does not contain the string
      if [[ -n "$exclude_asset_name_substring_if_non_empty" ]] && [[ "$asset_name" == *"$exclude_asset_name_substring_if_non_empty"* ]]; then
        continue
      fi

      asset_name="${asset_name#daml-sdk-}"      # Remove prefix
      asset_name="${asset_name%-macos.tar.gz}"  # Remove suffix

      # Continue if no macOS asset is found
      [ -z "$asset_name" ] && continue

      release_tag_name=$(echo "$line" | jq -r '.tag_name')
      release_name=$(echo "$line" | jq -r '.name')

      # Check if the release_tag_name contains the exclude_tag_name_substring (if set)
      if [[ "$release_tag_name" == *"$require_release_tag_name_substring"* ]]; then

        # Check that the release_tag_name does not contain the string
        if [[ -n "$exclude_release_tag_name_substring_if_non_empty" ]] && [[ "$release_tag_name" == *"$exclude_release_tag_name_substring_if_non_empty"* ]]; then
          continue
        fi

        # Remove 'v' prefix from tag name for version comparison
        version=$(echo "$release_tag_name" | sed 's/^v//')

        # Check and update the highest version release name
        if version_gt "$version" "$highest_version"; then
          highest_version=$version
          highest_version_release_tag_name=$release_tag_name
          highest_version_release_name=$release_name
          highest_version_asset_name=$asset_name
        fi
      fi
    fi
  done < <(echo "$releases_json" | jq -c '.[]')

  echo "highest_version:                  $highest_version" >&2
  echo "highest_version_release_tag_name: $highest_version_release_tag_name" >&2
  echo "highest_version_release_name:     $highest_version_release_name" >&2
  echo "highest_version_asset_name:       $highest_version_asset_name" >&2
  echo "" >&2

  # Do not echo, just set the value to be captured by command substitution
  printf "%s" "$highest_version_asset_name"
}

# Test
# SDK_VERSION=$(get_latest_sdk_version "" "2.9.0" "rc" "")
# echo $SDK_VERSION
