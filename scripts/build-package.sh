#!/bin/bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -eu

project=$1
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")"; pwd -P)
project_name=$(yq e '.name' ${project}/daml.yaml)

colour_off='\033[0m'
green='\033[0;32m'

project_string="Building package - ${project_name}"
bar=""
for str in $(seq 1 `echo ${project_string} | wc -c`); do bar+="-"; done

echo -e "\n${green}${bar}${colour_off}"
echo -e "${green}${project_string}${colour_off}"
echo -e "${green}${bar}${colour_off}\n"

${script_dir}/get-dependencies.sh ${project}/daml.yaml

echo "Compiling ${project_name}..."
DAML_PROJECT=${project} daml build

echo -e "\Successfully built package ${project_name}."
