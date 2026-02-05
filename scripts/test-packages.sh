#!/usr/bin/env bash
# Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail

root_dir=$(cd "$(dirname "$0")"; cd ..; pwd -P)

echo "Running package tests..."

# Parse packages.yaml to get test package paths
packages_yaml="${root_dir}/package/packages.yaml"
test_package_paths=($(yq e '.local.packages | to_entries | .[].value.package.path | select(. == "test/daml*")' "${packages_yaml}"))

for test_package_path in "${test_package_paths[@]}"; do
  pkg="${root_dir}/package/${test_package_path}"
  echo "Testing package: $pkg"
  (cd "$pkg" && dpm test)
done

echo ""
echo "All tests ran successfully!"