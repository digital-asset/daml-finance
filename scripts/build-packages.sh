#!/usr/bin/env bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail

# Use absolute paths to allow this script to be called from any location
script_dir=$(cd "$(dirname $0)"; pwd -P)
root_dir=$(cd ${script_dir}; cd ..; pwd -P)

# Remove any existing .lib/ directories from packages
if ls package/*/daml/*/.lib/ 1> /dev/null 2>&1; then
  rm -r ${root_dir}/package/*/daml/*/.lib/
fi

# Read the list of packages in order from the package config file and build each package
packages_yaml=${root_dir}/package/packages.yaml
package_paths=($(yq e '.local.packages | to_entries | map(.value.package.path) | .[]' ${packages_yaml}))
for package_path in "${package_paths[@]}"; do
  ${script_dir}/build-package.sh ${root_dir}/package/${package_path}
done

# Copy package dars into a dedicated folder
if [[ -d ${root_dir}/.dars ]]; then
  rm -r ${root_dir}/.dars
fi
mkdir ${root_dir}/.dars
cp ${root_dir}/package/main/daml/*/.daml/dist/* ${root_dir}/.dars/

boldCyan='\033[1;96m'
colour_off='\033[0m'
echo -e "\n${boldCyan}All packages successfully built!${colour_off}"
