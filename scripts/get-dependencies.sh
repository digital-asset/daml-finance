#!/usr/bin/env bash
# Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail

daml_yaml_file=$1
project_name=$(yq e '.name' ${daml_yaml_file})

# Use absolute paths to allow this script to be called from any location
root_dir=$(cd "$(dirname $0)"; cd ..; pwd -P)
cache_dir="${root_dir}/.cache"
package_root_dir="${root_dir}/package/*/daml"
project_root_dir=$(cd "$(dirname "${daml_yaml_file}")"; pwd -P)
project_lib_dir="${project_root_dir}/.lib"

red='\033[0;31m'
colour_off='\033[0m'

dependencies=($(yq e '.data-dependencies[]' ${daml_yaml_file}))
if [[ -z ${dependencies:-} ]]; then
  echo "Project ${project_name} has no dependencies. Ignoring..."
else
  for dependency_path in "${dependencies[@]}"; do

    echo "Processing dependency ${dependency_path}"

    # TODO: Adjust the regex to match the new major-version-in-path structure
    # isValidPath=`awk '{ match($0, /^.lib\/[a-zA-Z\-]*\/([a-zA-Z\.]*\/v?[0-9\.]*|v?[0-9\.]*)\/[a-zA-Z0-9\.\-]*\.dar$/); print RLENGTH }' <<< ${dependency_path}`
    # if [[ ${isValidPath} -eq -1 ]]; then
    #   echo -e "${red}ERROR: Dependency ${dependency_path} does not match the expected format.

    #           Dependency syntax :
    #             .lib/<repo_name>/<tag>/<file_name>
    #           <tag> syntax :
    #             <version> | <project_name>/<version>

    #           Regex format : ^\.lib\/[a-zA-Z\-]*\/([a-zA-Z\.]*\/v?[0-9\.]*|v?[0-9\.]*)\/[a-zA-Z0-9\.\-]*\.dar$ ${colour_off}"
    #   exit 1
    # fi

    if [[ -a ${project_root_dir}/${dependency_path} ]]; then
      echo -e "Dependency ${dependency_path} already setup. Skipping.\n"
    else
      # Extract the dependency details from dependency path
      read repo_name file_name tag <<<$(awk '{
          n = split($0,array,"/");
          repo = array[2];
          file = array[n];
          for (i = 3; i < n; i++) {
            tag = sprintf("%s%s", tag, array[i]);
            if (i != n - 1)
              tag = sprintf("%s/", tag);
            }
          }
          END { print repo, file, tag }' <<< ${dependency_path})

      # Check if the dependency exists in the following order :
      # 1 - cache
      # 2 - GitHub
      # 3 - local build
      cache_dependency_path=${cache_dir}/${repo_name}/${tag}

      if [[ -a ${cache_dependency_path}/${file_name} ]]; then
        echo "Using cached dependency at ${cache_dependency_path}/${file_name}"
        mkdir -p ${project_lib_dir}/${repo_name}/${tag} && cp ${cache_dependency_path}/${file_name} ${project_root_dir}/${dependency_path}
      elif [[ `curl -L -o /dev/null --silent -I -w '%{http_code}' https://github.com/digital-asset/${repo_name}/releases/download/${tag}/${file_name}` == "200" ]]; then
        echo "Downloading ${file_name} from Github repository at https://github.com/digital-asset/${repo_name}/releases/download/${tag}/${file_name}..."
        curl -Lf# https://github.com/digital-asset/${repo_name}/releases/download/${tag}/${file_name} --create-dirs -o ${project_root_dir}/${dependency_path}

        echo -e "\nAdding ${file_name} to the cache..."
        mkdir -p ${cache_dependency_path} && cp ${project_root_dir}/${dependency_path} ${cache_dependency_path}/${file_name}
      else
        echo "Attempting to get dependency ${file_name} locally..."
        package_name=$(echo ${tag} | cut -f1 -d/)
        if ( ls ${package_root_dir}/${package_name}/.daml/dist/${file_name} 1> /dev/null 2>&1 ); then
          mkdir -p ${project_lib_dir}/${repo_name}/${tag} && cp ${package_root_dir}/${package_name}/.daml/dist/${file_name} ${project_root_dir}/${dependency_path}
        else
          echo -e "${red}ERROR: Unable to locally locate dependency ${file_name}. Ensure this dependency has been successfully built.${colour_off}"
          exit 1
        fi
      fi

      echo -e "Dependency ${file_name} installed successfully and saved to ${project_root_dir}/${dependency_path}.\n"
    fi

  done
fi
