#!/usr/bin/env bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail

# Use absolute paths to allow this script to be called from any location
root_dir=$(cd "$(dirname $0)"; cd ..; pwd -P)

echo "Running package tests..."

# Run tests for all test packages (in package/test/*) defined in the packages config file
packages_yaml=${root_dir}/package/packages.yaml
test_package_paths=($(yq e '.local.packages | to_entries | map(.value.package.path) | .[] | select(. == "test/daml*")' ${packages_yaml}))
for test_package_path in "${test_package_paths[@]}"; do
  daml test --project-root ${root_dir}/package/${test_package_path}
done

echo ""
echo "All tests ran successfully!"
