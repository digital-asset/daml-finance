#!/bin/bash
# Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -eu

project=$1
script_dir=$(cd "$(dirname $0)"; pwd -P)
project_name=$(yq e '.name' ${project}/daml.yaml)

green='\033[0;32m'
boldGreen='\033[1;92m'
cyan='\033[0;36m'
boldWhite='\033[1;97m'
colour_off='\033[0m'

project_string="Building package - ${project_name}"
bar=""
for str in $(seq 1 `echo ${project_string} | wc -c`); do bar+="-"; done

echo -e "\n${boldGreen}${bar}${colour_off}"
echo -e "${boldGreen}${project_string}${colour_off}"
echo -e "${boldGreen}${bar}${colour_off}\n"

echo -e "${boldWhite}Extracting dependencies for library ${project_name}...${colour_off}\n"
${script_dir}/get-dependencies.sh ${project}/daml.yaml

echo -e "${boldWhite}Compiling ${project_name}...${colour_off}"
DAML_PROJECT=${project} daml build

echo -e "\n${cyan}Successfully built package ${project_name}.${colour_off}"
