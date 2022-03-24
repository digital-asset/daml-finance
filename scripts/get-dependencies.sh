#!/bin/bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -eu

daml_yaml_file=$1
root_dir="$(dirname "${daml_yaml_file}")"

dependencies=($(yq e '.data-dependencies[]' ${daml_yaml_file}))
for dependency_path in "${dependencies[@]}"; do

  echo "Processing dependency ${dependency_path}"

  if [[ -a ${root_dir}/${dependency_path} ]]; then
    echo "Dependency ${dependency_path} already exists. Skipping..."
  else
    # Take the full path, split on the unix file separator and take the last element in array
    file_name=`awk '{ n=split($0,array,"/"); print array[n] }' <<< ${dependency_path}`

    # From the end of the file name, remove the file extension and the version to get the repo name
    repo_name=`awk '{ sub(/\-[0-9]*\..*$/, "") ; print }' <<< ${file_name}`

    # Match from the major version until the end of the file name; keep what was matched from the input string; remove the file extension
    version=`awk '{ match($0, /[0-9a-zA-Z]+\..*/) ; $0=substr($0, RSTART, RLENGTH); sub(/.[a-z]+$/, ""); print }' <<< ${file_name}`

    echo "Downloading ${file_name} from Github repository at https://github.com/digital-asset/${repo_name}/releases/download/v${version}/${file_name}."
    curl -Lf# https://github.com/digital-asset/${repo_name}/releases/download/v${version}/${file_name} -o ${root_dir}/${dependency_path}

    echo -e "\nDependency ${file_name} downloaded successfully and saved to ${root_dir}/${dependency_path}.\n"
  fi

done

