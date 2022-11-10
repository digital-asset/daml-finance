#!/usr/bin/env bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail

# Use absolute paths to allow this script to be called from any location
script_dir=$(cd "$(dirname $0)"; pwd -P)
root_dir=$(cd ${script_dir}; cd ..; pwd -P)

# Run validation for released packages (in package/main/*) defined in the packages config file
packages_yaml=${root_dir}/package/packages.yaml
main_package_paths=($(yq e '.local.packages | to_entries | map(.value.package.path) | .[] | select(. == "main/daml*")' ${packages_yaml}))
for main_package_path in "${main_package_paths[@]}"; do
  ${script_dir}/validate-package.sh ${root_dir}/package/${main_package_path}
done

echo -e "\nAll packages are valid!"
