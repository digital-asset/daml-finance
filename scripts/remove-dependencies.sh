#!/bin/bash
set -eu

daml_yaml_file=$1
root_dir="$(dirname "${daml_yaml_file}")"

dependencies=($(yq e '.data-dependencies[]' ${daml_yaml_file}))
for dependency_path in "${dependencies[@]}"; do

  echo "Processing dependency ${dependency_path}"

  if [[ -a ${root_dir}/${dependency_path} ]]; then
    rm -f ${root_dir}/${dependency_path}
    echo "Dependency ${dependency_path} removed."
  else
    echo "Dependency ${dependency_path} is not installed. Ignoring..."
  fi

done
