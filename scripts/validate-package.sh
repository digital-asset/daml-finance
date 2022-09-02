#!/bin/bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -eu

package_root_dir=$1
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")"; pwd -P)
root_dir=$(cd ${script_dir}; cd ..; pwd -P)

package_name=$(yq e '.name' ${package_root_dir}/daml.yaml)
package_version=$(yq e '.version' ${package_root_dir}/daml.yaml)

package_dar="${package_name}-${package_version}.dar"
package_dar_path="${package_root_dir}/.daml/dist/${package_dar}"
package_dar_tag="`awk '{ n=split($0,array,"/"); print array[n] }' <<< ${package_root_dir}`/${package_version}"

cache_package_dir="${root_dir}/.cache/daml-finance/${package_dar_tag}"
cache_package_dar="${cache_package_dir}/${package_dar}"

green='\033[0;32m'
red='\033[0;31m'
colour_off='\033[0m'

echo -e "\nValidating package ${green}${package_name}${colour_off}..."

# Check if the package has been build
if [[ ! -a ${package_dar_path} ]]; then
  echo -e "${red}ERROR: Cannot locate dar for package ${package_name}. Ensure this package has been build successfully${colour_off}"
  exit 1
fi

# Check if the version of this package has already been released on GitHub
if [[ -a ${cache_package_dar} ]]; then
  echo "Using cached dependency at ${cache_package_dar}"
elif [[ `curl -L -o /dev/null --silent -I -w '%{http_code}' https://github.com/digital-asset/daml-finance/releases/download/${package_dar_tag}/${package_dar}` == "200" ]]; then
  echo "Downloading ${package_dar} from Github repository at https://github.com/digital-asset/daml-finance/releases/download/${package_dar_tag}/${package_dar}..."
  curl -Lf# https://github.com/digital-asset/daml-finance/releases/download/${package_dar_tag}/${package_dar} --create-dirs -o ${cache_package_dar}
  echo -e "\nDar ${package_dar} added to the cache..."
else
  echo "Package ${package_name} at version ${package_version} has not been released."
  exit 0
fi

# Extract the package id of the local
released_dar_package_id=`daml damlc inspect-dar ${cache_package_dar} --json | jq -r .main_package_id`
local_dar_package_id=`daml damlc inspect-dar ${package_dar_path} --json | jq -r .main_package_id`

if [[ ${released_dar_package_id} == ${local_dar_package_id} ]]; then
  echo "The package ${package_name} is valid! Local build matches released dar."
else
  echo -e "${red}ERROR: The package ${package_name} is not valid. The local build does not match the released build.${colour_off}"
  echo -e "${red}ERROR: If this package's source code or a dependency version has been modified, update the version of the package in the daml.yaml file!${colour_off}"
  echo -e "${red}ERROR: Released package id - ${released_dar_package_id}.${colour_off}"
  echo -e "${red}ERROR: Local package id - ${local_dar_package_id}.${colour_off}"
  exit 1
fi
