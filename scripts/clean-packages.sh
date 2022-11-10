#!/usr/bin/env bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

root_dir=$(cd "$(dirname $0)"; cd ..; pwd -P)

# Remove .lib directories in packages
echo "Removing .lib/ directories in all packages"
rm -r ${root_dir}/package/*/daml/*/.lib/ 1> /dev/null 2>&1

echo "Removing .dars/ directory"
rm -r ${root_dir}/.dars 1> /dev/null 2>&1

# Clean each package defined in the package config file.
packages_yaml=${root_dir}/package/packages.yaml
package_paths=($(yq e '.local.packages | to_entries | map(.value.package.path) | .[]' ${packages_yaml}))
for package_path in "${package_paths[@]}"; do
  daml clean --project-root ${root_dir}/package/${package_path}
done
